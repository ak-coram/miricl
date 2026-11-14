import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
function workerIndex(bytes) {
  let position = 8;
  const word = () => {let value = 0, scale = 1, byte; do {
    byte = bytes[position++]; value += (byte & 127) * scale; scale *= 128;
  } while (byte & 128); return value;};
  while (position < bytes.length) {
    const kind = bytes[position++], end = word() + position;
    if (kind === 7) {
      const count = word();
      for (let i = 0; i < count; i++) {
        const length = word(), name = new TextDecoder().decode(bytes.subarray(position, position + length));
        position += length;
        const type = bytes[position++], index = word();
        if (type === 0 && name === 'direct-entry-1') return index;
      }
    }
    position = end;
  }
  throw new Error('Missing direct entry for local-call fixture');
}
try {
  vm.evaluate('(miricl-compiler::ensure-compiler) (in-package :miricl-compiler) (values)');
  vm.evaluate(`(defun module-retains-function-p (unit name)
    (not (null (some (lambda (function)
      (some (lambda (binding)
        (let ((local (hir-binding-local binding)))
          (and (eq (hir-binding-name binding) name) local
               (member (hir-local-function local) (hir-unit-functions unit)))))
        (hir-function-bindings function))) (hir-unit-functions unit))))) (values)`);
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun helper (x) (+ x 1))
                            (defun unused () (error "Unreachable"))
                            (defun entry (x) (helper x))) :entry-points '(entry))
    (and (= (length entries) 1) (= (funcall (cdar entries) 41) 42)
         (module-retains-function-p unit 'entry) (not (module-retains-function-p unit 'helper))
         (not (module-retains-function-p unit 'unused)) (plusp (length bytes))
         (not (fboundp 'helper)) (not (fboundp 'entry))))`);
  // Keeping a shared helper, explicit NOTINLINE, and escaped function values
  // retain a separate body; unused functions disappear in each case.
  check(`(let ((definitions '((defun helper (x) (+ x 1)) (defun unused () :unused)
                             (defun entry (x) (helper x)))))
    (multiple-value-bind (entries bytes unit)
      (compile-native-module definitions :entry-points '(entry) :inline nil)
      (declare (ignore bytes))
      (and (= (funcall (cdar entries) 41) 42) (module-retains-function-p unit 'helper)
           (not (module-retains-function-p unit 'unused)))))`);
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun helper (x) (+ x 1))
                            (defun entry (x) (declare (notinline helper)) (helper x))) :entry-points '(entry))
    (declare (ignore bytes))
    (and (= (funcall (cdar entries) 41) 42) (module-retains-function-p unit 'helper)))`);
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun helper (x) (+ x 1))
                            (defun entry (x) (declare (inline helper)) (helper x)))
      :entry-points '(entry) :inline nil)
    (declare (ignore bytes))
    (and (= (funcall (cdar entries) 41) 42) (not (module-retains-function-p unit 'helper))))`);
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun helper (x) (+ x 1))
                            (defun entry () #'helper)) :entry-points '(entry))
    (declare (ignore bytes))
    (and (= (funcall (funcall (cdar entries)) 41) 42) (module-retains-function-p unit 'helper)))`);
  // Inline bodies use the definition's lexical variables and function bindings,
  // evaluate arguments left to right, and retain all return values.
  for (const [source, args, expected] of [
    [`(lambda (x) (flet ((helper (y) (+ x y))) (declare (inline helper))
       (let ((x 1000)) (helper 2))))`, '40', '42\n'],
    [`(lambda (x) (flet ((helper (y) (+ y 1))) (declare (inline helper))
       (flet ((+ (&rest args) (declare (ignore args)) 999)) (helper x))))`, '41', '42\n'],
    [`(lambda () (let ((events nil))
       (flet ((helper (a b) (values a b events))) (declare (inline helper))
         (helper (progn (push 1 events) 40) (progn (push 2 events) 2)))))`, '', '40\n2\n(2 1)\n'],
    [`(lambda () (flet ((helper () (return-from helper (values 42 :second))))
       (declare (inline helper)) (helper)))`, '', '42\n:SECOND\n'],
    [`(lambda () (block done (flet ((helper () (return-from done 42)))
       (declare (inline helper)) (helper)) 0))`, '', '42\n'],
    [`(lambda (x) (flet ((helper (y) (setq x y) (lambda () x)))
       (declare (inline helper)) (let ((f (helper 42))) (setq x 43) (funcall f))))`, '1', '43\n'],
    [`(lambda (x) (labels ((helper (n) (if (zerop n) x (helper (1- n)))))
       (declare (inline helper)) (helper 10)))`, '42', '42\n'],
    [`(lambda (n)
       (labels ((a (x) (if (zerop x) 42 (b (1- x))))
                (b (x) (if (zerop x) 42 (a (1- x)))))
         (declare (inline a b)) (a n)))`, '17', '42\n'],
    [`(lambda (x)
       (labels ((helper (n) (if (zerop n) x (funcall (lambda () (helper (1- n)))))))
         (declare (inline helper)) (helper 10)))`, '42', '42\n'],
    [`(lambda () (block done (flet ((helper () 42)) (return-from done (helper)))))`, '', '42\n'],
    [`(lambda () (flet ((unused () (error "Unreachable"))) 42))`, '', '42\n'],
  ]) {
    vm.evaluate(`(defparameter *module-test* (compile-native '${source}))`);
    assert.equal(vm.evaluate(`(funcall *module-test* ${args})`).output, expected, source);
  }
  // Two definitions can share executable conses without a structural cycle.
  check(`(let* ((body '(if (zerop n) :done (b (1- n))))
               (source (list 'lambda '(n)
                 (list 'labels (list (list 'a '(n) body) (list 'b '(n) body))
                   '(declare (inline a b)) '(a n)))))
    (eq (funcall (compile-native source) 10) :done))`);
  check(`(let ((f (compile-native '(lambda () (let ((x 0))
    (flet ((helper (a) a)) (declare (inline helper))
      (handler-case (helper (incf x) (incf x)) (program-error () x))))))))
    (= (funcall f) 2))`);
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun even-entry (n) (if (zerop n) t (odd-entry (1- n))))
                            (defun odd-entry (n) (if (zerop n) nil (even-entry (1- n)))))
      :entry-points '(even-entry odd-entry))
    (declare (ignore bytes unit))
    (and (funcall (cdr (assoc 'even-entry entries)) 20)
         (funcall (cdr (assoc 'odd-entry entries)) 21)))`);
  check(`(handler-case (progn (compile-module-to-hir '((defun a () 1)) :entry-points '(missing)) nil)
    (error () t))`);
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun a (x) (values x)) (defun b (x) (values x))
      (defun dispatch (flag x) (if flag (a x) (b x)))) :entry-points '(dispatch) :inline nil)
    (declare (ignore unit))
    (with-open-file (out "/shared-tail.wasm" :direction :output :element-type '(unsigned-byte 8))
      (write-sequence bytes out))
    (let ((f (cdar entries))) (and (= (funcall f t 41) 41) (= (funcall f nil 42) 42))))`);
  const bytes = await vm.readFile('/shared-tail.wasm');
  assert.ok(WebAssembly.validate(bytes));
  // Check the sharing relation itself. Extra adapters or workers are valid
  // additions and must not break this test by changing the module inventory.
  check(`(let* ((*native-functions* '(shared-a shared-b distinct))
                (*native-function-indices* (native-function-indices *native-functions*))
                (*wasm-body-keys* (make-hash-table :test 'eq)) (*wasm-worker-origins* nil))
    (multiple-value-bind (bodies indices changed)
        (native-coalesce-bodies (list (vector 0 32 1 11) (vector 0 32 1 11) (vector 0 208 111 11)))
      (declare (ignore bodies changed))
      (and (= (gethash 'shared-a indices) (gethash 'shared-b indices))
           (/= (gethash 'shared-a indices) (gethash 'distinct indices)))))`);
  check(`(let ((f (compile-native '(lambda (x)
    (let ((a x) (b (+ x 1))) (values (lambda (y) (+ a y)) (lambda (y) (+ b y))))))))
    (multiple-value-bind (a b) (funcall f 10)
      (and (= (funcall a 3) 13) (= (funcall b 3) 14))))`);
  // Inspect the actual entry, accepting unrelated workers/imports. Ordinary
  // calls and tail-frame setup require the normal calling boundary; a checked
  // primitive-only DEBUG 0 helper can use a direct non-tail Wasm call.
  vm.evaluate('(defvar *local-call-special* :outside) (values)');
  for (const [name, helper, direct] of [
    ['checked reader', '(declare (optimize (debug 0)) (inline car)) (values (car value))', true],
    ['debug reader', '(declare (optimize (debug 1)) (inline car)) (values (car value))', false],
    ['tail setup', '(declare (optimize (debug 0))) (funcall #\'car value)', false],
    ['dynamic scope', '(declare (optimize (debug 0)) (inline car)) (let ((*local-call-special* value)) (car *local-call-special*))', false],
  ]) {
    check(`(progn
      (defparameter *local-call-unit* (compile-to-hir
        '(lambda (object) (declare (optimize (debug 0)))
           (flet ((reader (value) ${helper})) (declare (notinline reader))
             (values (reader object))))))
      (multiple-value-bind (bytes literals) (emit-native-module *local-call-unit*)
        (defparameter *local-call-code* bytes)
        (defparameter *local-call-function* (compiler-primitive 17 bytes literals nil))
        (with-open-file (out "/local-call.wasm" :direction :output :if-exists :supersede
            :element-type '(unsigned-byte 8)) (write-sequence bytes out)))
      (and (equal (multiple-value-list (funcall *local-call-function* '(42))) '(42))
           (handler-case (progn (funcall *local-call-function* 17) nil)
             (type-error (condition) (eql (type-error-datum condition) 17)))
           (eq *local-call-special* :outside)
           (= (funcall *local-call-function* '(43)) 43)))`);
    const bytes = await vm.readFile('/local-call.wasm');
    assert.ok(WebAssembly.validate(bytes), name);
    const internalCall = () => vm.evaluate(`(multiple-value-bind (body imports)
        (native-disassembly-body *local-call-code* ${workerIndex(bytes)})
      (not (null (some (lambda (op) (and (= (native-op-code op) #x10)
                        (>= (native-op-argument op) (length imports)))) (native-read-ops body)))))`).output;
    assert.equal(internalCall(), direct ? 'T\n' : 'NIL\n', name);
    if (direct) {
      // Public HIR can change between emissions. Removing the inline contract
      // must invalidate eligibility and restore the ordinary call boundary.
      vm.evaluate(`(dolist (function (hir-unit-functions *local-call-unit*))
        (dolist (block (hir-function-blocks function))
          (dolist (instruction (hir-block-instructions block))
            (when (and (eq (hir-instruction-opcode instruction) :function-reference)
                       (eq (first (hir-instruction-attributes instruction)) 'car))
              (setf (second (hir-instruction-attributes instruction)) 'notinline)))))
        (multiple-value-bind (bytes literals) (emit-native-module *local-call-unit*)
          (setq *local-call-code* bytes *local-call-function* (compiler-primitive 17 bytes literals nil))
          (with-open-file (out "/local-call.wasm" :direction :output :if-exists :supersede
              :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values)`);
      const changed = await vm.readFile('/local-call.wasm');
      assert.equal(vm.evaluate(`(multiple-value-bind (body imports)
          (native-disassembly-body *local-call-code* ${workerIndex(changed)})
        (not (null (some (lambda (op) (and (= (native-op-code op) #x10)
                          (>= (native-op-argument op) (length imports)))) (native-read-ops body)))))`).output,
        'NIL\n', 'Changed HIR loses the direct-call proof');
      check('(= (funcall *local-call-function* \'(44)) 44)');
    }
  }
  check(`(let ((function (compile-native '(lambda (object) (declare (optimize (debug 0)))
      (flet ((first-value (x) (declare (optimize (debug 0)) (inline car cdr))
               (values (car x) (cdr x))))
        (declare (notinline first-value))
        (let ((events nil))
          (list (first-value (progn (push :first events) object))
                (multiple-value-list (first-value (progn (push :second events) object)))
                (handler-case (first-value (progn (push :arity events) object) :extra)
                  (program-error () :rejected))
                events)))))))
    (equal (funcall function '(42 . 43)) '(42 (42 43) :rejected (:arity :second :first))))`);
  check(`(let* ((object (list :identity))
               (function (compile-native '(lambda (object) (declare (optimize (debug 0)))
                 (let ((saved object))
                   (flet ((zero () (declare (optimize (debug 0))) (values saved))
                          (one (a) (declare (optimize (debug 0))) (values a))
                          (two (a b) (declare (ignore a) (optimize (debug 0))) (values b))
                          (three (a b c) (declare (ignore a b) (optimize (debug 0))) (values c))
                          (four (a b c d) (declare (ignore a b c) (optimize (debug 0))) (values d)))
                     (declare (notinline zero one two three four))
                     (let ((out (list (zero) (one object) (two :a object)
                                      (three :a :b object) (four :a :b :c object))))
                       (setq saved :changed) (list out (zero)))))))))
    (let ((result (funcall function object)))
      (and (every (lambda (value) (eq value object)) (first result))
           (eq (second result) :changed))))`);
  console.log('Compiler modules: entry points, helper elimination, shared bodies, NOTINLINE, escaped functions, lexical scope, evaluation order, multiple values, mutation, nonlocal exits, and recursion pass.');
} finally {vm.close();}
