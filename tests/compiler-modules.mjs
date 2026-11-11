import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(miricl-compiler::ensure-compiler) (in-package :miricl-compiler) (values)');
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun helper (x) (+ x 1))
                            (defun unused () (error "Unreachable"))
                            (defun entry (x) (helper x))) :entry-points '(entry))
    (and (= (length entries) 1) (= (funcall (cdar entries) 41) 42)
         (= (length (hir-unit-functions unit)) 2) (plusp (length bytes))
         (not (fboundp 'helper)) (not (fboundp 'entry))))`);
  // Keeping a shared helper, explicit NOTINLINE, and escaped function values
  // retain a separate body; unused functions disappear in each case.
  check(`(let ((definitions '((defun helper (x) (+ x 1)) (defun unused () :unused)
                             (defun entry (x) (helper x)))))
    (multiple-value-bind (entries bytes unit)
      (compile-native-module definitions :entry-points '(entry) :inline nil)
      (declare (ignore bytes))
      (and (= (funcall (cdar entries) 41) 42) (= (length (hir-unit-functions unit)) 3))))`);
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun helper (x) (+ x 1))
                            (defun entry (x) (declare (notinline helper)) (helper x))) :entry-points '(entry))
    (declare (ignore bytes))
    (and (= (funcall (cdar entries) 41) 42) (= (length (hir-unit-functions unit)) 3)))`);
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun helper (x) (+ x 1))
                            (defun entry (x) (declare (inline helper)) (helper x)))
      :entry-points '(entry) :inline nil)
    (declare (ignore bytes))
    (and (= (funcall (cdar entries) 41) 42) (= (length (hir-unit-functions unit)) 2)))`);
  check(`(multiple-value-bind (entries bytes unit)
    (compile-native-module '((defun helper (x) (+ x 1))
                            (defun entry () #'helper)) :entry-points '(entry))
    (declare (ignore bytes))
    (and (= (funcall (funcall (cdar entries)) 41) 42) (= (length (hir-unit-functions unit)) 3)))`);
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
  let position = 8, bodies;
  const u32 = () => {
    let value = 0, shift = 0, byte;
    do {byte = bytes[position++]; value += (byte & 127) * 2 ** shift; shift += 7;} while (byte & 128);
    return value;
  };
  while (position < bytes.length) {
    const id = bytes[position++], size = u32(), end = position + size;
    if (id === 3) bodies = u32();
    position = end;
  }
  assert.equal(bodies, 3, 'Factory, dispatch, and one shared body for A/B');
  check(`(let ((f (compile-native '(lambda (x)
    (let ((a x) (b (+ x 1))) (values (lambda (y) (+ a y)) (lambda (y) (+ b y))))))))
    (multiple-value-bind (a b) (funcall f 10)
      (and (= (funcall a 3) 13) (= (funcall b 3) 14))))`);
  console.log('Compiler modules: entry points, helper elimination, shared bodies, NOTINLINE, escaped functions, lexical scope, evaluation order, multiple values, mutation, nonlocal exits, and recursion pass.');
} finally {vm.close();}
