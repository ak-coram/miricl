import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(miricl-compiler::ensure-compiler) (in-package :miricl-compiler) (values)');
  check(`(every (lambda (entry)
    (let ((bytes (coerce (append '(99) (second entry) '(99)) 'vector)))
      (multiple-value-bind (value next) (funcall (first entry) bytes 1)
        (and (= value (third entry)) (= next (1+ (length (second entry))))))))
    (list (list #'native-read-uleb '(0) 0)
          (list #'native-read-uleb '(127) 127)
          (list #'native-read-uleb '(128 1) 128)
          (list #'native-read-uleb '(128 128 1) 16384)
          (list #'native-read-sleb '(64) -64)
          (list #'native-read-sleb '(127) -1)
          (list #'native-read-sleb '(0) 0)
          (list #'native-read-sleb '(63) 63)
          (list #'native-read-sleb '(192 0) 64)
          (list #'native-read-sleb '(191 127) -65)
          (list #'native-read-sleb '(128 128 128 128 120) -2147483648)
          (list #'native-read-sleb '(255 255 255 255 7) 2147483647)))`);
  // Sibling stores may be consumed in creation order. Eliminating the older
  // store inserts a real operand below the younger one; both must stackify.
  // A temporary real operand must block them only until it is consumed.
  for (const barrier of ['', '(65 99) (26 nil)']) {
    check(`(let* ((ops (map 'vector (lambda (entry)
                    (make-native-op (first entry) (second entry) 0 0))
                  '((65 1) (33 2) (65 2) (33 3) ${barrier}
                    (32 2) (32 3) (70 nil) (15 nil) (11 nil)))))
      (native-op-flow ops) (native-op-liveness ops 4 nil)
      (native-stackify-ops ops)
      (equal (loop for op across ops when (native-op-code op) collect (native-op-code op))
        '(65 65 ${barrier ? '65 26' : ''} 70 15 11)))`);
  }
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
  // Construct the tree in Lisp so this tests frontend recursion independently
  // of the text reader's nesting bound. Large dispatcher frames used to trap.
  check(`(let ((form '(values x :deep)))
    (dotimes (i 1000)
      (setq form (list 'let '((y x)) (list 'if 'y (list 'progn form) :absent))))
    (let ((function (compile-native (list 'lambda '(x) form))))
      (and (equal (multiple-value-list (funcall function 17)) '(17 :deep))
           (eq (funcall function nil) :absent))))`);
  // Each of these formerly overflowed at 1,000 levels on the default stack.
  // Calls must keep left-to-right effects; THE must preserve multiple values.
  check(`(let ((form 'x))
    (dotimes (i 2000) (setq form (list 'identity form)))
    (let ((function (compile-native (list 'lambda '(x) form))))
      (and (= (funcall function 42) 42) (null (funcall function nil)))))`);
  check(`(let ((form '(values x :second)))
    (dotimes (i 2000) (setq form (list 'the '(values integer keyword) form)))
    (equal (multiple-value-list (funcall (compile-native (list 'lambda '(x) form)) 42))
      '(42 :second)))`);
  check(`(let ((form '(values)) (zero '(values)))
    (dotimes (i 1000)
      (setq form (list 'if 'x form zero)))
    (let ((function (compile-native (list 'lambda '(x) form))))
      (and (null (multiple-value-list (funcall function t)))
           (null (multiple-value-list (funcall function nil))))))`);
  console.log('Compiler throughput: LEB boundaries, operand barriers, record APIs, dense value validation, changed HIR reuse, 280 live temporaries and deeply nested scopes pass.');
} finally {vm.close();}
