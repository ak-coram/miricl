// Benchmark native compiler source as data, with identical input across images.
// First run a source-bearing image; reuse its sources.lisp for later comparisons.
// node scripts/benchmark-self-compiler.mjs OUTPUT IMAGE [SOURCES.lisp] [--module|--extract]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import inspector from 'node:inspector';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';

const moduleMode = process.argv.includes('--module');
const extractOnly = process.argv.includes('--extract');
const profileEnabled = process.argv.includes('--profile');
const synchronous = process.argv.includes('--sync');
const warmupRounds = Number(process.env.MIRICL_BENCHMARK_WARMUPS ?? 1);
const rounds = Number(process.env.MIRICL_BENCHMARK_ROUNDS ?? 3);
assert.ok(Number.isSafeInteger(warmupRounds) && warmupRounds > 0, 'Expected positive warmup count');
assert.ok(Number.isSafeInteger(rounds) && rounds > 0, 'Expected positive sample count');
const [output, imagePath, sourcesPath] = process.argv.slice(2).filter(arg => !['--module', '--extract', '--profile', '--sync'].includes(arg));
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
  const bootstrap = timed('(ensure-compiler)');
  assert.equal(vm.evaluate(`(every (lambda (name) (compiled-function-p (symbol-function name)))
    *native-compiler-seeds*)`).output, 'T\n');
  let compilerModule;
  if (moduleMode) {
    const compilation = timed(`(multiple-value-bind (entries bytes unit)
      (compile-native-module (mapcar (lambda (entry)
        (list* 'defun (first entry) (cdr (second entry)))) *self-benchmark-sources*))
      (defparameter *self-module-entries* entries)
      (defparameter *self-module-bytes* (length bytes))
      (defparameter *self-module-functions* (length (hir-unit-functions unit))))`);
    compilerModule = {compilation,
      bytes: Number(vm.evaluate('*self-module-bytes*').output.trim()),
      functions: Number(vm.evaluate('*self-module-functions*').output.trim())};
    timed(`(dolist (entry *self-module-entries*) (setf (symbol-function (car entry)) (cdr entry)))`);
    console.log(`Compiler module: ${compilerModule.bytes} bytes, ${compilerModule.functions} bodies.`);
  }
  const frontend = `(defparameter *self-benchmark-units*
    (mapcar (lambda (entry) (compile-to-hir (second entry) :name (first entry))) *self-benchmark-sources*))`;
  const emission = `(defparameter *self-benchmark-modules*
    (mapcar (lambda (unit) (multiple-value-list (emit-native-module unit))) *self-benchmark-units*))`;
  const installation = `(mapc (lambda (entry)
    (multiple-value-bind (function native) (compiler-primitive 17 (first entry) (second entry) nil)
      (unless (and native (compiled-function-p function)) (error "Native installation failed"))))
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
    const profiler = new inspector.Session(); profiler.connect();
    const post = (method, params = {}) => new Promise((resolve, reject) =>
      profiler.post(method, params, (error, result) => error ? reject(error) : resolve(result)));
    try {
      await post('Profiler.enable'); await post('Profiler.start'); timed(emission);
      const {profile} = await post('Profiler.stop');
      await fs.writeFile(path.join(output, 'emission.cpuprofile'), JSON.stringify(profile));
      const nodes = new Map(profile.nodes.map(node => [node.id, node])), frames = new Map();
      const exports = new Map(Object.entries(vm.vm).filter(([, value]) => typeof value === 'function')
        .map(([name, value]) => [value.name, name]));
      profile.samples.forEach((id, index) => {
        const frame = nodes.get(id).callFrame, key = JSON.stringify(frame);
        const item = frames.get(key) ?? {...frame,
          export: exports.get(frame.functionName.replace(/^wasm-function\[(\d+)\]$/u, '$1')), milliseconds: 0};
        item.milliseconds += profile.timeDeltas[index] / 1000; frames.set(key, item);
      });
      hotFrames = [...frames.values()].sort((a, b) => b.milliseconds - a.milliseconds).slice(0, 40);
    } finally {profiler.disconnect();}
  }
  // Install one complete emitted generation, then exercise that compiler.
  vm.evaluate(`(mapc (lambda (source module unit)
    (let* ((name (first source)) (function (compiler-primitive 17 (first module) (second module) name))
           (arity (native-fixed-arity (first (hir-unit-functions unit)))))
      (setf (symbol-function name) (if arity (compiler-primitive 30 function arity) function))))
    *self-benchmark-sources* *self-benchmark-modules* *self-benchmark-units*) (values)`);
  assert.equal(vm.evaluate(`(equal (multiple-value-list
    (funcall (compile-native '(lambda (x) (values (+ x 2) :compiled))) 40)) '(42 :compiled))`).output, 'T\n');
  const median = values => values.toSorted((a, b) => a - b)[Math.floor(values.length / 2)];
  const medians = Object.fromEntries(['frontend', 'emission', 'installation'].map(stage =>
    [stage, Object.fromEntries(['wallMs', 'cpuMs'].map(metric => [metric, median(samples.map(sample => sample[stage][metric]))]))]));
  const report = {imagePath, imageSha256: createHash('sha256').update(image).digest('hex'),
    sourceSha256: createHash('sha256').update(corpus).digest('hex'), count, moduleBytes,
    node: process.version, execArgv: process.execArgv, synchronous,
    platform: process.platform, architecture: process.arch, warmupRounds, rounds,
    image: measureWasm(imagePath), bootstrap, compilerModule, samples, medians, hotFrames, nextGenerationVerified: true};
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify(report, null, 2) + '\n');
  console.log(`${count} compiler functions; ${moduleBytes} emitted module bytes; next compiler generation verified.`);
} finally {vm.close();}
