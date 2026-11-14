// Compare initial compiler bootstrap with source and compact HIR kernels.
// HIR preparation is measured separately, never hidden in the bootstrap time.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';

const [output, imagePath = 'miricl-cold.wasm', sourcesPath, nativePath] = process.argv.slice(2);
if (!output) throw new Error('Expected OUTPUT [IMAGE] [SOURCES.lisp] [PRECOMPILED-IMAGE]');
await fs.mkdir(output, {recursive: true});
const image = await fs.readFile(imagePath);
const originals = sourcesPath && await fs.readFile(sourcesPath);
const timed = action => {
  const cpu = process.cpuUsage(), start = performance.now(), result = action();
  const elapsed = process.cpuUsage(cpu);
  return {result, wallMs: performance.now() - start, cpuMs: (elapsed.user + elapsed.system) / 1000};
};
const evaluate = (vm, form) => {
  const r = vm.evaluate(`(progn ${form} (values))`); assert.equal(r.output, ''); return r;
};
const prepareOriginals = vm => {
  if (originals) {
    vm.mountFile('/hir-original-sources.lisp', originals.toString('utf8'));
    evaluate(vm, '(load "/hir-original-sources.lisp" :verbose nil)');
  }
  evaluate(vm, `(defparameter *hir-benchmark-originals*
    ${originals ? '*self-benchmark-sources*' : '*hir-bootstrap-sources*'})`);
};
const compileOriginals = async vm => {
  const measurement = timed(() => evaluate(vm, `
    (multiple-value-bind (functions bytes)
        (compile-native-module
          (mapcar (lambda (entry) (list* 'defun (first entry) (cdr (compiler-source-policy (second entry)))))
            *hir-benchmark-originals*) :inline nil)
      (declare (ignore functions))
      (with-open-file (out "/hir-originals-module.wasm" :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`));
  delete measurement.result;
  const bytes = await vm.readFile('/hir-originals-module.wasm');
  assert.ok(WebAssembly.validate(bytes));
  return {...measurement, bytes: bytes.length, sha256: createHash('sha256').update(bytes).digest('hex')};
};
let preparation = await Miricl.instantiate(image);
let code, codeOnly, sourceBootstrap, sourceOriginals, hirOriginals, prepare, sourceCapture, preparationImageSha256;
try {
  preparation.evaluate('(in-package :miricl-compiler) (values)');
  evaluate(preparation, `(setq *compile-evaluations* nil)
    (defparameter *hir-bootstrap-sources*
      (mapcar (lambda (name) (list name (function-lambda-expression (symbol-function name)))) *native-compiler-seeds*))`);
  if (nativePath) {
    // Capture the current compiler's source with the ordinary object-graph
    // codec, then prepare it with the already compiled compiler. This avoids
    // repeating cold source bootstrap solely to construct a benchmark input.
    sourceCapture = timed(() => evaluate(preparation, `(progv (mapcar (lambda (name) (compiler-primitive 10 1 name))
        '("%FASL-RECORDS" "%FASL-SEEN" "%FILE-LOAD-VALUES"))
        (list (make-array 32 :adjustable t :fill-pointer 0) (make-hash-table :test 'eq) nil)
      (let ((root (funcall (symbol-function (compiler-primitive 10 1 "%FASL-DUMP"))
                    (list 'defparameter '*hir-bootstrap-sources* (list 'quote *hir-bootstrap-sources*)))))
        (funcall (symbol-function (compiler-primitive 10 1 "%FASL-WRITE"))
          "/kernel-sources.wasm" (list root) (symbol-value (compiler-primitive 10 1 "%FASL-RECORDS")))))`));
    delete sourceCapture.result;
    const kernels = await preparation.readFile('/kernel-sources.wasm');
    preparation.close();
    const nativeImage = await fs.readFile(nativePath);
    preparationImageSha256 = createHash('sha256').update(nativeImage).digest('hex');
    preparation = await Miricl.instantiate(nativeImage);
    preparation.evaluate('(in-package :miricl-compiler) (values)');
    evaluate(preparation, '(setq *compile-evaluations* nil)');
    preparation.mountFile('/kernel-sources.wasm', kernels);
    evaluate(preparation, '(load "/kernel-sources.wasm" :verbose nil)');
    assert.equal(preparation.evaluate('(eq *compiler-state* :ready)').output, 'T\n');
    console.log(`HIR preparation uses ${nativePath}; cold source bootstrap skipped`);
    sourceBootstrap = null;
  } else {
    sourceBootstrap = timed(() => evaluate(preparation, '(ensure-compiler)'));
    delete sourceBootstrap.result;
    console.log(`Source bootstrap: ${sourceBootstrap.wallMs.toFixed(0)} ms`);
  }
  prepareOriginals(preparation);
  sourceOriginals = await compileOriginals(preparation);
  prepare = timed(() => evaluate(preparation, `
    (progv (mapcar (lambda (name) (compiler-primitive 10 1 name))
             '("%FASL-RECORDS" "%FASL-SEEN" "%FILE-LOAD-VALUES"))
        (list (make-array 32 :adjustable t :fill-pointer 0) (make-hash-table :test 'eq) nil)
     (let ((roots nil) (forms nil) (dump (symbol-function (compiler-primitive 10 1 "%FASL-DUMP"))))
      (push (funcall dump (list 'defparameter '*hir-bootstrap-sources*
        (list 'quote *hir-bootstrap-sources*))) roots)
      (dolist (entry *hir-bootstrap-sources*)
        (let* ((name (first entry)) (source (compiler-source-policy (second entry)))
               (payload (emit-hir-code (compile-to-hir source :name name)))
               (form (list 'setf (list 'symbol-function (list 'quote name))
                       (list 'load-hir-code (list 'quote payload)))))
          (push form forms) (push (funcall dump form) roots)))
      (funcall (symbol-function (compiler-primitive 10 1 "%FASL-WRITE"))
        "/hir-bootstrap.wasm" (nreverse roots)
        (symbol-value (compiler-primitive 10 1 "%FASL-RECORDS")))
      ;; A separate distribution measurement omits the original-source corpus
      ;; retained solely for the second benchmark.
      (progv (mapcar (lambda (name) (compiler-primitive 10 1 name))
               '("%FASL-RECORDS" "%FASL-SEEN" "%FILE-LOAD-VALUES"))
          (list (make-array 32 :adjustable t :fill-pointer 0) (make-hash-table :test 'eq) nil)
        (funcall (symbol-function (compiler-primitive 10 1 "%FASL-WRITE"))
          "/hir-code-only.wasm" (mapcar dump (nreverse forms))
          (symbol-value (compiler-primitive 10 1 "%FASL-RECORDS"))))))`));
  delete prepare.result;
  code = await preparation.readFile('/hir-bootstrap.wasm');
  await fs.writeFile(path.join(output, 'hir-bootstrap.wasm'), code);
  codeOnly = await preparation.readFile('/hir-code-only.wasm');
  await fs.writeFile(path.join(output, 'hir-code-only.wasm'), codeOnly);
  console.log(`HIR preparation: ${prepare.wallMs.toFixed(0)} ms; payload ${code.length} bytes`);
} finally {preparation.close();}
const vm = await Miricl.instantiate(image);
let install, hirBootstrap;
try {
  vm.evaluate('(in-package :miricl-compiler) (values)');
  evaluate(vm, '(setq *compile-evaluations* nil)');
  vm.mountFile('/hir-bootstrap.wasm', code);
  install = timed(() => evaluate(vm, '(load "/hir-bootstrap.wasm" :verbose nil)')); delete install.result;
  console.log(`HIR installation: ${install.wallMs.toFixed(0)} ms; starting native compiler bootstrap`);
  prepareOriginals(vm);
  assert.equal(vm.evaluate('(eq *compiler-state* :ready)').output, 'NIL\n', 'Preparation must not install a native compiler');
  assert.equal(vm.evaluate('(every (lambda (name) (null (function-lambda-expression (symbol-function name)))) *native-compiler-seeds*)').output,
    'T\n', 'HIR bootstrap must not depend on retained source');
  hirBootstrap = timed(() => evaluate(vm, '(ensure-compiler)')); delete hirBootstrap.result;
  assert.equal(vm.evaluate('(eq *compiler-state* :ready)').output, 'T\n');
  hirOriginals = await compileOriginals(vm);
  assert.equal(hirOriginals.sha256, sourceOriginals.sha256, 'Both compilers must emit identical code for the original source');
  // Compile and execute a fixed original source with the resulting compiler.
  assert.equal(vm.evaluate(`(funcall (compile-native '(lambda (x) (values (+ x 1) (* x x)))) 9)`).output, '10\n81\n');
  console.log(`HIR bootstrap: ${hirBootstrap.wallMs.toFixed(0)} ms; install ${install.wallMs.toFixed(0)} ms`);
} finally {vm.close();}
const result = {image: measureWasm(imagePath), imageSha256: createHash('sha256').update(image).digest('hex'),
  sourcesPath, preparationImagePath: nativePath, preparationImageSha256, sourceCapture,
  sourceSha256: originals && createHash('sha256').update(originals).digest('hex'),
  sourceBootstrap, sourceOriginals, prepare, install, hirBootstrap, hirOriginals, payloadBytes: code.length,
  payloadSizes: measureWasm(path.join(output, 'hir-bootstrap.wasm'))};
result.codeOnlySizes = measureWasm(path.join(output, 'hir-code-only.wasm'));
await fs.writeFile(path.join(output, 'results.json'), `${JSON.stringify(result, null, 2)}\n`);
