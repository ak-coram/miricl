import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const sourceCompiler = process.argv.includes('--source');
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (sourceCompiler) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('hir-types.lisp', 'utf8'));
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  for (const safety of [0, 3]) {
    for (const body of ['(symbolp x)', '(if (symbolp x) t nil)', '(not (symbolp x))']) {
      check(`(let ((f (miricl-compiler:compile-native
          '(lambda (x) (declare (inline symbolp not) (optimize (safety ${safety}))) ${body}))))
        (every (lambda (x)
          (equal (multiple-value-list (funcall f x))
                 (list ${body === '(not (symbolp x))' ? '(not (symbolp x))' : '(not (null (symbolp x)))'})))
          (list nil t :keyword 'car (make-symbol "FRESH") 0 -1 1073741824
                (ash 1 100) 3/2 1.25d0 #c(1 2) #\\a "" "abc" #(1 2) '(a b)
                (make-hash-table) #'car (make-array '(2 3)))))`);
    }
  }
  check(`(let ((calls 0)
        (f (miricl-compiler:compile-native '(lambda (callback)
             (declare (inline symbolp)) (symbolp (funcall callback))))))
    (and (funcall f (lambda () (incf calls) (values nil :extra))) (= calls 1)))`);
  check(`(let ((calls 0)
        (f (miricl-compiler:compile-native '(lambda (callback)
             (declare (inline symbolp)) (symbolp (funcall callback)) :done))))
    (and (eq :done (funcall f (lambda () (incf calls) :value))) (= calls 1)))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (x)
              (declare (inline symbolp))
              (let ((answer (symbolp x))) (values answer (symbolp x)))))))
    (and (equal (multiple-value-list (funcall f nil)) '(t t))
         (equal (multiple-value-list (funcall f 0)) '(nil nil))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (x)
              (declare (inline symbolp + 1+ <) (optimize (safety 0) (debug 0)))
              (let ((sum 0)) (declare (type (integer 0 4) sum))
                (dotimes (i 4 sum) (declare (type (integer 0 4) i))
                  (if (symbolp x) (incf sum))))))))
    (and (= (funcall f nil) 4) (= (funcall f :x) 4) (= (funcall f 0) 0)))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (x)
             (let ((calls 0))
               (flet ((symbolp (value) (declare (ignore value)) (incf calls)))
                 (declare (inline symbolp)) (values (symbolp x) calls)))))))
    (equal (multiple-value-list (funcall f nil)) '(1 1)))`);
  for (const args of ['', 'x x']) {
    check(`(let ((f (miricl-compiler:compile-native '(lambda (x)
               (declare (inline symbolp)) (symbolp ${args})))))
      (handler-case (progn (funcall f nil) nil) (program-error () t)))`);
  }
  if (!sourceCompiler) {
    // Primitive replacement must use the sealed compiler: the source compiler
    // itself calls SYMBOLP while parsing and emitting these lambdas.
    check(`(let ((ordinary (miricl-compiler:compile-native '(lambda (x) (symbolp x))))
          (primary (miricl-compiler:compile-native '(lambda (x) (values (symbolp x)))))
          (captured (miricl-compiler:compile-native '(lambda (x) (declare (inline symbolp)) (symbolp x))))
          (dynamic (miricl-compiler:compile-native '(lambda (x) (declare (notinline symbolp)) (symbolp x))))
          (original (symbol-function 'symbolp)))
      (unwind-protect
        (progn (setf (symbol-function 'symbolp)
                 (lambda (x) (declare (ignore x)) (values :replacement :extra)))
          (and (equal (multiple-value-list (funcall ordinary nil)) '(:replacement :extra))
               (equal (multiple-value-list (funcall primary nil)) '(:replacement))
               (equal (multiple-value-list (funcall dynamic nil)) '(:replacement :extra))
               (equal (multiple-value-list (funcall captured nil)) '(t))
               (equal (multiple-value-list (funcall captured 0)) '(nil))))
        (setf (symbol-function 'symbolp) original)))`);
  }
} finally { vm.close(); }
console.log('SYMBOLP: NIL, symbols, immediate/heap nonsymbols, branches, effects, value counts, loop invariants, shadowing and arity pass.');
