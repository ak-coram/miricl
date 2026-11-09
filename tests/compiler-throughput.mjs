import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(miricl-compiler::ensure-compiler) (in-package :miricl-compiler) (values)');
  // The named records retain callable predicates, type tests and shallow copy
  // semantics as well as the field macros used by the self-compiled kernels.
  check(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 19))))
                (function (first (hir-unit-functions unit)))
                (value (first (hir-function-values function)))
                (copy (copy-hir-value value)) (calls 0))
    (setf (hir-value-type (progn (incf calls) copy)) 'integer)
    (and (typep unit 'hir-unit) (funcall #'hir-value-p value)
         (typep copy 'hir-value) (not (eq copy value))
         (eq (hir-value-producer copy) (hir-value-producer value))
         (= calls 1) (eq (hir-value-type copy) 'integer) (eq (hir-value-type value) t)))`);
  check(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 19))))
                (function (first (hir-unit-functions unit)))
                (value (first (hir-function-values function))))
    (setf (hir-value-id value) (hir-function-value-count function))
    (handler-case (progn (verify-hir unit) nil) (error () t)))`);
  check(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 19))))
                (function (first (hir-unit-functions unit)))
                (instruction (loop for block in (hir-function-blocks function)
                  thereis (find-if (lambda (op) (hir-instruction-inputs op))
                            (hir-block-instructions block))))
                (input (first (hir-instruction-inputs instruction))))
    (setf (first (hir-instruction-inputs instruction)) (copy-hir-value input))
    (handler-case (progn (verify-hir unit) nil) (error () t)))`);
  vm.evaluate(`(defun throughput-rewrite (unit)
    (let ((left nil) (right nil) (function (first (hir-unit-functions unit))))
      (dolist (value (hir-function-values function))
        (let ((producer (hir-value-producer value)))
          (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant))
            (case (first (hir-instruction-attributes producer)) (19 (setq left value)) (37 (setq right value))))))
      (assert (and left right))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (setf (hir-instruction-inputs instruction)
            (substitute left right (hir-instruction-inputs instruction) :test #'eq)))))
    unit)
    (defparameter *throughput-unit* (compile-to-hir '(lambda (x) (list (+ x 19) (+ x 37)))))
    (progn (emit-native-module *throughput-unit*)
      (throughput-rewrite *throughput-unit*) (values))`);
  // Reusing an HIR object after its uses change must give the same module as
  // a fresh, identically rewritten graph. Disable final size passes here so
  // they cannot mask stale lowering metadata by simplifying both results.
  for (const [name, unit] of [
    ['reused', '*throughput-unit*'],
    ['fresh', `(throughput-rewrite (compile-to-hir '(lambda (x) (list (+ x 19) (+ x 37)))))`],
  ]) {
    vm.evaluate(`(let ((*wasm-optimize* nil))
      (multiple-value-bind (bytes literals) (emit-native-module ${unit})
        (with-open-file (out "/${name}.wasm" :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8)) (write-sequence bytes out))
        (assert (equal (funcall (compiler-primitive 17 bytes literals nil) 3) '(22 22))))) (values)`);
  }
  assert.deepEqual(await vm.readFile('/reused.wasm'), await vm.readFile('/fresh.wasm'));
  const expressions = Array.from({length: 280}, (_, i) => `(+ n ${i})`).join(' ');
  vm.evaluate(`(defparameter *throughput-function* (compile-native '(lambda (n) (list ${expressions})))) (values)`);
  for (const n of [-5, 7]) check(`(equal (funcall *throughput-function* ${n})
    '(${Array.from({length: 280}, (_, i) => n + i).join(' ')}))`);
  console.log('Compiler throughput: record APIs, dense value validation, changed HIR reuse and 280 live temporaries pass.');
} finally {vm.close();}
