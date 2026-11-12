// Measure prepared HIR execution, then compile a fixed original-source corpus
// once. Prepare payloads against their image; compare instruction hashes when
// build-local diagnostic symbol names differ. Never install another compiler.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';
import {startWasmProfile} from './wasm-profile.mjs';

const profile = process.argv.includes('--profile');
const args = process.argv.slice(2).filter(arg => arg !== '--profile');
const [output, imagePath, codePath, sourcesPath, compare, otherImage, otherCode] = args;
if (!output || !imagePath || !codePath || !sourcesPath)
  throw new Error('Expected OUTPUT IMAGE PREPARED-HIR.wasm ORIGINAL-SOURCES.lisp [--profile] [--compare IMAGE HIR]');
assert.ok(args.length === 4 || (args.length === 7 && compare === '--compare' && otherImage && otherCode),
  'Expected --compare IMAGE HIR as the optional second image');
const rounds = Number(process.env.MIRICL_BENCHMARK_ROUNDS ?? 3);
assert.ok(Number.isSafeInteger(rounds) && rounds > 0, 'Expected positive sample count');
const variants = [{imagePath, codePath}, ...(compare ? [{imagePath: otherImage, codePath: otherCode}] : [])];
await Promise.all(variants.map(async variant => {
  [variant.image, variant.code] = await Promise.all([variant.imagePath, variant.codePath].map(p => fs.readFile(p)));
}));
const sources = await fs.readFile(sourcesPath);
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const sharedPayload = variants.every(variant => hash(variant.code) === hash(variants[0].code));
const evaluate = (vm, source) => assert.equal(vm.evaluate(`(progn ${source} (values))`).output, '');
const timed = action => {
  const cpu = process.cpuUsage(), before = performance.now();
  action();
  const elapsed = process.cpuUsage(cpu);
  return {wallMs: performance.now() - before, cpuMs: (elapsed.user + elapsed.system) / 1000};
};
await fs.mkdir(output, {recursive: true});
const samples = [];
let moduleSha256;
const instructionHashes = [];
for (let round = 0; round < rounds; round++) {
  // Alternate AB/BA to reduce drift from runtime warmup and host load. Each
  // sample still uses a fresh heap; compression runs after all timed samples.
  const order = variants.map((_, i) => i);
  if (round % 2) order.reverse();
  for (const variant of order) {
    const {image, code} = variants[variant];
    const vm = await Miricl.instantiate(image);
    try {
      vm.evaluate('(in-package :miricl-compiler) (values)');
      evaluate(vm, '(setq *compile-evaluations* nil)');
      vm.mountFile('/prepared-hir.wasm', code);
      const installation = timed(() => evaluate(vm, '(load "/prepared-hir.wasm" :verbose nil)'));
      assert.equal(vm.evaluate(`(and (eq *compiler-state* :cold)
        (every (lambda (name) (null (function-lambda-expression (symbol-function name))))
               *native-compiler-seeds*))`).output, 'T\n');
      // Diagnostic names can contain host-generated lexical gensyms. Hash the
      // executable block tables separately so comparisons can establish that
      // identical instructions are executing despite those names differing.
      evaluate(vm, `(with-open-file (out "/instructions.bin" :direction :output
        :if-exists :supersede :element-type '(unsigned-byte 8))
        (dolist (name *native-compiler-seeds*)
          (let ((payload (compiler-primitive 43 (symbol-function name))))
            (unless payload (error "Expected prepared HIR for ~S" name))
            (loop for function across (svref payload 1) for bytes = (svref function 5) do
              (dotimes (i 4) (write-byte (ldb (byte 8 (* 8 i)) (length bytes)) out))
              (loop for char across bytes do (write-byte (char-code char) out))))))`);
      const instructions = hash(await vm.readFile('/instructions.bin'));
      if (instructionHashes[variant]) assert.equal(instructions, instructionHashes[variant]);
      if (sharedPayload && instructionHashes[0]) assert.equal(instructions, instructionHashes[0]);
      instructionHashes[variant] = instructions;
      const stopProfile = profile && round === 0
        && await startWasmProfile(vm, image, path.join(output, `bootstrap-${variant}`));
      const bootstrap = timed(() => evaluate(vm, '(ensure-compiler)'));
      if (stopProfile) await stopProfile();
      assert.equal(vm.evaluate('(eq *compiler-state* :ready)').output, 'T\n');
      vm.mountFile('/originals.lisp', sources);
      evaluate(vm, '(load "/originals.lisp" :verbose nil)');
      const compilation = timed(() => evaluate(vm, `
        (multiple-value-bind (functions bytes)
            (compile-native-module (mapcar (lambda (entry)
              (list* 'defun (first entry) (cdr (compiler-source-policy (second entry)))))
              *self-benchmark-sources*) :inline nil)
          (declare (ignore functions))
          (with-open-file (out "/originals.wasm" :direction :output
            :if-exists :supersede :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`));
      const module = await vm.readFile('/originals.wasm');
      assert.ok(WebAssembly.validate(module));
      const digest = hash(module);
      if (moduleSha256) assert.equal(digest, moduleSha256, 'Repeated compilation must emit identical code');
      moduleSha256 = digest;
      samples.push({variant, installation, bootstrap, compilation, moduleBytes: module.length, moduleSha256});
      console.log(`Round ${round + 1}, image ${variant}: HIR load ${installation.wallMs.toFixed(0)} ms, bootstrap ${bootstrap.wallMs.toFixed(0)} ms, originals ${compilation.wallMs.toFixed(0)} ms, ${module.length} bytes`);
    } finally {vm.close();}
  }
}
const sameInstructions = instructionHashes.every(digest => digest === instructionHashes[0]);
if (!sameInstructions) console.log('HIR instruction streams differ; fixed original-source output remains byte-identical.');
const images = variants.map(({imagePath, codePath, image, code}) => ({imagePath,
  imageSha256: hash(image), codePath, codeSha256: hash(code), sizes: measureWasm(imagePath)}));
await fs.writeFile(path.join(output, 'results.json'), JSON.stringify({
  images, sourcesPath, sourceSha256: hash(sources), instructionSha256: instructionHashes[0],
  instructionHashes, sameInstructions, samples,
}, null, 2) + '\n');
