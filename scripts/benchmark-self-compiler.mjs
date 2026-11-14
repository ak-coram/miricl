// Compile the compiler once, then benchmark it compiling the original source.
// Benchmark output is measured and instantiated, never installed as a compiler.
// node scripts/benchmark-self-compiler.mjs OUTPUT IMAGE [SOURCES.lisp] [--module|--extract]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';
import {startWasmProfile} from './wasm-profile.mjs';
import {enableCompilationStatistics, disableCompilationStatistics, readCompilationStatistics, writeCompilationStatistics} from './compilation-statistics.mjs';

const moduleMode = process.argv.includes('--module');
const extractOnly = process.argv.includes('--extract');
const profileEnabled = process.argv.includes('--profile');
const synchronous = process.argv.includes('--sync');
const individualBootstrap = process.argv.includes('--individual-bootstrap');
const statisticsEnabled = process.argv.includes('--statistics');
for (const option of process.argv.slice(2).filter(arg => arg.startsWith('--')))
  if (!['--module', '--extract', '--profile', '--sync', '--individual-bootstrap', '--statistics'].includes(option))
    throw new Error(`Unknown benchmark option: ${option}`);
const warmupRounds = Number(process.env.MIRICL_BENCHMARK_WARMUPS ?? 1);
const rounds = Number(process.env.MIRICL_BENCHMARK_ROUNDS ?? 3);
assert.ok(Number.isSafeInteger(warmupRounds) && warmupRounds > 0, 'Expected positive warmup count');
assert.ok(Number.isSafeInteger(rounds) && rounds > 0, 'Expected positive sample count');
const [output, imagePath, sourcesPath] = process.argv.slice(2).filter(arg => !arg.startsWith('--'));
if (!output || !imagePath) throw new Error('Expected OUTPUT IMAGE [SOURCES.lisp]');
await fs.mkdir(output, {recursive: true});
const image = await fs.readFile(imagePath);
let vm;
if (synchronous) {
  const module = new WebAssembly.Module(image);
  assert.equal(WebAssembly.Module.customSections(module, 'miricl.core').length, 0,
    'Synchronous benchmarking expects a runtime image, not a saved heap');
  vm = new Miricl(new WebAssembly.Instance(module));
} else vm = await Miricl.instantiate(image);
const samples = [], sources = sourcesPath && await fs.readFile(sourcesPath);
const timed = source => {
  const cpu = process.cpuUsage(), before = performance.now();
  const result = vm.evaluate(`(progn ${source} (values))`);
  assert.equal(result.output, '');
  const elapsed = process.cpuUsage(cpu);
  return {wallMs: performance.now() - before, cpuMs: (elapsed.user + elapsed.system) / 1000};
};
benchmark: try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (values)');
  if (individualBootstrap) {
    assert.equal(vm.evaluate('(eq *compiler-state* :cold)').output, 'T\n',
      'Individual bootstrap comparison requires a cold compiler image');
    vm.evaluate('(setq *native-compiler-blocks* nil) (values)');
  }
  if (sources) {
    vm.mountFile('/self-sources.lisp', sources.toString('utf8'));
    vm.evaluate('(let ((*compile-evaluations* nil)) (load "/self-sources.lisp")) (values)');
  } else {
    vm.evaluate(`(defparameter *self-benchmark-sources*
      (mapcar (lambda (name)
        (let ((source (function-lambda-expression (symbol-function name))))
          (unless source (error "Missing source for ~S; supply SOURCES.lisp for a precompiled image" name))
          (list name source))) *native-compiler-seeds*))
      (with-open-file (out "/self-sources.lisp" :direction :output)
        (let ((*print-readably* t) (*print-pretty* nil) (*print-circle* t)
              (seen (make-hash-table :test 'eq)) (symbols nil))
          ;; Bootstrap primitives are uninterned symbols with ABI identities.
          ;; Preserve those separately from ordinary lexical gensyms when a
          ;; corpus is read into another heap.
          (labels ((walk (form)
            (cond ((consp form) (walk (car form)) (walk (cdr form)))
                  ((and (symbolp form) (null (symbol-package form)) (not (gethash form seen))
                        (or (fboundp form) (boundp form) (special-operator-p form) (macro-function form)
                            (and (>= (length (symbol-name form)) 2)
                                 (string= (symbol-name form) "%%" :end1 2))))
                   (setf (gethash form seen) t)
                   (let ((namespace (compiler-primitive 9 form)))
                     (when (and (plusp namespace)
                                (eq form (compiler-primitive 10 namespace (symbol-name form))))
                       (push (list form namespace (symbol-name form)) symbols)))))))
            (walk *self-benchmark-sources*))
          (write-string "(in-package :miricl-compiler)" out) (terpri out)
          (write (list 'defparameter '*self-benchmark-sources*
            (list 'sublis
              (list 'mapcar '(lambda (entry)
                (cons (first entry) (compiler-primitive 10 (second entry) (third entry))))
                (list 'quote symbols))
              (list 'quote *self-benchmark-sources*))) :stream out)))
      (values)`);
  }
  const corpus = sources ?? await vm.readFile('/self-sources.lisp');
  await fs.writeFile(path.join(output, 'sources.lisp'), corpus);
  if (extractOnly) {console.log(`Wrote ${path.join(output, 'sources.lisp')}`); break benchmark;}
  vm.evaluate(`(setq *self-benchmark-sources*
    (mapcar (lambda (entry) (list (first entry) (compiler-source-policy (second entry))))
      *self-benchmark-sources*)) (values)`);
  const compilerPreparationMode = vm.evaluate('(eq *compiler-state* :ready)').output.trim() === 'T'
    ? 'precompiled' : 'first-use';
  const bootstrapProfile = profileEnabled && compilerPreparationMode === 'first-use'
    && await startWasmProfile(vm, image, path.join(output, 'bootstrap'));
  const bootstrap = timed('(ensure-compiler)');
  const bootstrapHotFrames = bootstrapProfile && await bootstrapProfile();
  console.log(compilerPreparationMode === 'precompiled' ? 'Precompiled compiler ready.'
    : `First-use compiler compilation: ${bootstrap.wallMs.toFixed(0)} ms`);
  assert.equal(vm.evaluate(`(every (lambda (name) (compiled-function-p (symbol-function name)))
    *native-compiler-seeds*)`).output, 'T\n');
  let compilerModule;
  if (moduleMode) {
    vm.evaluate(`(defparameter *self-definitions* (mapcar (lambda (entry)
      (list* 'defun (first entry) (cdr (second entry)))) *self-benchmark-sources*)) (values)`);
  }
  const frontend = moduleMode
    ? `(let ((*gensym-counter* 0))
         (defparameter *self-benchmark-units* (list (compile-module-to-hir *self-definitions* :inline nil))))`
    : `(let ((*gensym-counter* 0)) (defparameter *self-benchmark-units*
        (mapcar (lambda (entry) (compile-to-hir (second entry) :name (first entry))) *self-benchmark-sources*)))`;
  const emission = `(defparameter *self-benchmark-modules*
    (mapcar (lambda (unit) (multiple-value-list (emit-native-module unit))) *self-benchmark-units*))`;
  const installation = `(mapc (lambda (entry)
    (multiple-value-bind (function native) (compiler-primitive 17 (first entry) (second entry) nil)
      (unless (and native (compiled-function-p function)) (error "Native installation failed"))
      ${moduleMode ? '(defparameter *self-module-entries* (funcall function))' : ''}))
    *self-benchmark-modules*)`;
  // Allow longer warmup when profiling exposes background JIT compilation.
  // Timed rounds use newly lowered HIR and newly emitted code.
  for (let round = 0; round < warmupRounds; round++) {
    timed(frontend); timed(emission); timed(installation);
    console.log(`Compiler warmup ${round + 1}/${warmupRounds} completed.`);
  }
  for (let round = 0; round < rounds; round++) {
    const sample = {round, frontend: timed(frontend), emission: timed(emission), installation: timed(installation)};
    samples.push(sample);
    console.log(`Round ${round + 1}: frontend ${sample.frontend.wallMs.toFixed(0)} ms, emission ${sample.emission.wallMs.toFixed(0)} ms, Wasm installation ${sample.installation.wallMs.toFixed(0)} ms`);
  }
  const count = Number(vm.evaluate('(length *self-benchmark-sources*)').output.trim());
  const moduleBytes = Number(vm.evaluate('(reduce #\'+ *self-benchmark-modules* :key (lambda (entry) (length (first entry))))').output.trim());
  let hotFrames;
  if (profileEnabled) {
    hotFrames = {bootstrap: bootstrapHotFrames};
    for (const [stage, source] of [['frontend', frontend], ['emission', emission]]) {
      const finish = await startWasmProfile(vm, image, path.join(output, stage));
      try {timed(source);} finally {hotFrames[stage] = await finish();}
    }
  }
  let statistics;
  if (statisticsEnabled) {
    statistics = {};
    // Collect in separate repetitions. Counter bookkeeping must not pollute
    // the ordinary timing samples above or be presented as native throughput.
    for (const [stage, source] of [['frontend', frontend], ['emission', emission]]) {
      enableCompilationStatistics(vm);
      try {
        const measurement = timed(source);
        statistics[stage] = await readCompilationStatistics(vm);
        await writeCompilationStatistics({workload: 'compiler-corpus', phase: stage, status: 'complete',
          counters: statistics[stage], elapsedMs: measurement.wallMs, timingIncludesCollection: true,
          sourceSha256: createHash('sha256').update(corpus).digest('hex')});
      }
      finally {disableCompilationStatistics(vm);}
    }
    // Validate/instantiate this output too, without installing a compiler.
    timed(installation);
    await fs.writeFile(path.join(output, 'statistics.json'), JSON.stringify(statistics, null, 2) + '\n');
  }
  vm.evaluate(`(with-open-file (out "/self-module.wasm" :direction :output :if-exists :supersede
                 :element-type '(unsigned-byte 8))
    (write-sequence (first (first *self-benchmark-modules*)) out)) (values)`);
  await fs.writeFile(path.join(output, 'module.wasm'), await vm.readFile('/self-module.wasm'));
  if (moduleMode) compilerModule = {bytes: moduleBytes, image: measureWasm(path.join(output, 'module.wasm')),
    functions: Number(vm.evaluate('(length (hir-unit-functions (first *self-benchmark-units*)))').output.trim())};
  // Benchmark output is validated but never installed as the next compiler.
  const moduleCode = await fs.readFile(path.join(output, 'module.wasm'));
  assert.ok(WebAssembly.validate(moduleCode), 'The bootstrapped compiler emitted an invalid module');
  if (moduleMode) assert.equal(vm.evaluate('(length *self-module-entries*)').output.trim(), String(count));
  const median = values => values.toSorted((a, b) => a - b)[Math.floor(values.length / 2)];
  const medians = Object.fromEntries(['frontend', 'emission', 'installation'].map(stage =>
    [stage, Object.fromEntries(['wallMs', 'cpuMs'].map(metric => [metric, median(samples.map(sample => sample[stage][metric]))]))]));
  const report = {imagePath, imageSha256: createHash('sha256').update(image).digest('hex'),
    sourceSha256: createHash('sha256').update(corpus).digest('hex'), compilerSourcePolicy: true,
    gensymCounter: 0, optimizationPipeline: 'standard',
    compilerPreparationMode, bootstrapBlocks: !individualBootstrap, count, moduleBytes,
    node: process.version, execArgv: process.execArgv, synchronous,
    platform: process.platform, architecture: process.arch, warmupRounds, rounds,
    image: measureWasm(imagePath), bootstrap, compilerModule, samples, medians, hotFrames, statistics, emittedModuleVerified: true};
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify(report, null, 2) + '\n');
  console.log(`${count} compiler functions; ${moduleBytes} emitted module bytes; output module verified.`);
} finally {vm.close();}
