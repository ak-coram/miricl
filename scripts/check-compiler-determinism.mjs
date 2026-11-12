// Compare a cold self-compiled compiler with the precompiled compiler on one
// fixed source corpus. Generated compiler entries are exercised, never installed.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';

const [output, coldImage, readyImage, sourcePath] = process.argv.slice(2);
if (!output || !coldImage || !readyImage || !sourcePath)
  throw new Error('Expected OUTPUT COLD-IMAGE PRECOMPILED-IMAGE SOURCES.lisp');
const source = await fs.readFile(sourcePath), variants = [];
const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
await fs.mkdir(output, {recursive: true});
for (const [name, imagePath] of [['cold', coldImage], ['precompiled', readyImage]]) {
  const image = await fs.readFile(imagePath), vm = await Miricl.instantiate(image);
  const evaluate = form => {
    const result = vm.evaluate(`(progn ${form} (values))`);
    assert.equal(result.output, '');
  };
  try {
    evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil)');
    const initialState = vm.evaluate('*compiler-state*').output.trim();
    assert.equal(initialState, name === 'cold' ? ':COLD' : ':READY');
    assert.equal(vm.evaluate(`(notany (lambda (name) (boundp (intern name :miricl-compiler)))
      '("*HIR-OPTIMIZE*" "*WASM-OPTIMIZE*" "*WASM-STRUCTURED-LOOPS*"
        "*WASM-PREDICATE-CSE*" "*WASM-LICM*"))`).output, 'T\n', 'Compiler pass settings have been removed');
    const start = performance.now(), cpu = process.cpuUsage();
    evaluate('(ensure-compiler)');
    const elapsed = process.cpuUsage(cpu);
    console.log(`${name}: compiler ready after ${(performance.now() - start).toFixed(0)} ms`);
    const bootstrap = {wallMs: performance.now() - start, cpuMs: (elapsed.user + elapsed.system) / 1000};
    assert.equal(vm.evaluate(`(every (lambda (name) (compiled-function-p (symbol-function name)))
      *native-compiler-seeds*)`).output, 'T\n');
    const policy = vm.evaluate(`(list (compiler-primitive 36 nil) (compiler-primitive 33 nil))`).output.trim();
    vm.mountFile('/fixed-sources.lisp', source);
    evaluate('(load "/fixed-sources.lisp" :verbose nil)');
    const before = performance.now();
    evaluate(`(let ((*gensym-counter* 0))
      (multiple-value-bind (bytes literals)
          (emit-native-module (compile-module-to-hir
            (mapcar (lambda (entry) (list* 'defun (first entry)
              (cdr (compiler-source-policy (second entry))))) *self-benchmark-sources*) :inline nil))
        (with-open-file (out "/compiler-output.wasm" :direction :output :if-exists :supersede
                         :element-type '(unsigned-byte 8)) (write-sequence bytes out))
        (progv (mapcar (lambda (name) (compiler-primitive 10 1 name))
            '("%FASL-RECORDS" "%FASL-SEEN" "%FILE-LOAD-VALUES"))
            (list (make-array 32 :adjustable t :fill-pointer 0) (make-hash-table :test 'eq) nil)
          (let ((root (funcall (symbol-function (compiler-primitive 10 1 "%FASL-DUMP")) literals)))
            (funcall (symbol-function (compiler-primitive 10 1 "%FASL-WRITE"))
              "/compiler-literals.wasm" (list root)
              (symbol-value (compiler-primitive 10 1 "%FASL-RECORDS")))))
        (multiple-value-bind (factory native) (compiler-primitive 17 bytes literals nil)
          (unless (and native (= (length (funcall factory)) (length *self-benchmark-sources*)))
            (error "Compiler output failed validation")))))`);
    const bytes = Buffer.from(await vm.readFile('/compiler-output.wasm'));
    const literals = Buffer.from(await vm.readFile('/compiler-literals.wasm'));
    assert.ok(WebAssembly.validate(bytes));
    await fs.writeFile(path.join(output, `${name}.wasm`), bytes);
    await fs.writeFile(path.join(output, `${name}-literals.wasm`), literals);
    variants.push({name, imagePath, imageSha256: sha256(image), initialState, policy,
      bootstrap, compilationWallMs: performance.now() - before, bytes, literals,
      moduleBytes: bytes.length, moduleSha256: sha256(bytes), literalsSha256: sha256(literals)});
    console.log(`${name}: ${bytes.length} module bytes, SHA256 ${sha256(bytes)}`);
  } finally {vm.close();}
}
const [cold, ready] = variants;
const equal = {policy: cold.policy === ready.policy, code: cold.bytes.equals(ready.bytes),
  literals: cold.literals.equals(ready.literals)};
await fs.writeFile(path.join(output, 'results.json'), JSON.stringify({sourcePath, sourceSha256: sha256(source),
  variants: variants.map(({bytes, literals, ...variant}) => ({...variant, literalsBytes: literals.length})), equal}, null, 2) + '\n');
assert.deepEqual(equal, {policy: true, code: true, literals: true}, 'Cold and precompiled compiler output must match');
console.log('Cold and precompiled compilers produce identical Wasm bytes and literal metadata.');
