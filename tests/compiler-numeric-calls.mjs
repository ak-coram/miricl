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
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  check(`(progn
    (defparameter *numeric-primary* (miricl-compiler:compile-native
      '(lambda (function a b) (list (funcall function a b)))))
    (defparameter *numeric-full* (miricl-compiler:compile-native
      '(lambda (function a b) (funcall function a b))))
    (defparameter *numeric-drop* (miricl-compiler:compile-native
      '(lambda (function a b) (funcall function a b) :done)))
    (defparameter *numeric-three* (miricl-compiler:compile-native
      '(lambda (function a b c) (list (funcall function a b c)))))
    (defparameter *numeric-one* (miricl-compiler:compile-native
      '(lambda (function a) (list (funcall function a)))))
    (and (functionp *numeric-primary*) (functionp *numeric-full*)))`);

  // Full ordinary calls supply an independent result/value-count oracle.
  check(`(let ((functions (list #'/ #'truncate)))
    (every (lambda (function)
      (every (lambda (pair)
        (let* ((a (car pair)) (b (cadr pair))
               (expected (multiple-value-list (funcall function a b))))
          (and (equalp (funcall *numeric-primary* function a b) (list (car expected)))
               (equalp (multiple-value-list (funcall *numeric-full* function a b)) expected)
               (eq :done (funcall *numeric-drop* function a b)))))
        (list '(13 4) '(-13 4) '(13 -4) '(-13 -4) '(0 3)
              '(13/7 4/11) '(1.25 0.5) '(1.25d0 0.5d0)
              '(1.25 2.0d0) '(1/3 2.0) '(1.0d0 3/7)
              (list (ash 1 120) 7) (list (- (1+ (ash 1 120))) 17)
              '(1073741824 1) '(-1073741824 -1)))) functions))`);
  check(`(every (lambda (pair)
    (let ((a (car pair)) (b (cadr pair)))
      (equalp (funcall *numeric-primary* #'/ a b) (list (/ a b)))))
    '( (#c(1 2) #c(3 -4)) (7 #c(2 3)) (#c(1/3 2/5) 7/11)
       (#c(1.0d200 2.0d200) #c(3.0d200 -4.0d200))
       (#c(1.0d-200 2.0d-200) #c(3.0d-200 -4.0d-200)) ))`);
  check(`(and (equal (funcall *numeric-primary* #'/ 13 4) '(13/4))
              (equal (multiple-value-list (funcall *numeric-full* #'truncate 13 4)) '(3 1))
              (equal (funcall *numeric-one* #'/ 4) '(1/4))
              (equal (funcall *numeric-one* #'truncate 13/4) '(3))
              (equal (funcall *numeric-three* #'/ 60 2 3) '(10))
              (handler-case (progn (funcall *numeric-three* #'truncate 1 2 3) nil)
                (program-error () t)))`);
  // Floating quotient operations retain sign and contagion under the helper.
  check(`(let* ((one (funcall *numeric-primary* #'/ -0.0d0 1.0d0))
                (q (funcall *numeric-primary* #'truncate -0.0d0 1.0d0)))
     (and (typep (car one) 'double-float) (minusp (float-sign (car one)))
          (equal q '(0))))`);

  for (const name of ['/', 'truncate']) {
    for (const a of [':bad', 'nil', '"bad"']) {
      check(`(let ((bad ${a}))
                 (and (handler-case (progn (funcall *numeric-primary* #'${name} bad 2) nil)
                         (type-error (c) (eq (type-error-datum c) bad)))
                   (equal (funcall *numeric-primary* #'${name} 8 2) '(4))))`);
    }
    check(`(let ((expected (handler-case (progn (funcall #'${name} 8 0) nil)
                            (division-by-zero (c)
                              (list (arithmetic-error-operation c)
                                    (arithmetic-error-operands c))))))
             (and expected
                 (handler-case (progn (funcall *numeric-primary* #'${name} 8 :bad) nil)
                         (type-error (c) (eq (type-error-datum c) :bad)))
                 (handler-case (progn (funcall *numeric-drop* #'${name} 8 0) nil)
                   (division-by-zero (c)
                     (and (eq (arithmetic-error-operation c) (car expected))
                          (equal (arithmetic-error-operands c) (cadr expected))
                          (equal (arithmetic-error-operands c) '(8 0)))))
                 (equal (funcall *numeric-primary* #'${name} 8 2) '(4))))`);
    check(`(let ((saved (symbol-function '${name})))
       (unwind-protect
         (progn (setf (symbol-function '${name})
                  (lambda (a b) (declare (ignore a b)) (values :replacement :extra)))
           (and (equal (funcall *numeric-primary* (symbol-function '${name}) 8 2) '(:replacement))
                (equal (multiple-value-list (funcall *numeric-full* (symbol-function '${name}) 8 2))
                       '(:replacement :extra))
                (equal (funcall *numeric-primary* saved 8 2) '(4))))
         (setf (symbol-function '${name}) saved)))`);
  }
  check(`(and (handler-case (progn (funcall *numeric-primary* #'truncate #c(1 2) 2) nil)
                   (type-error () t))
              (handler-case (progn (funcall *numeric-primary* #'truncate 2 #c(1 2)) nil)
                   (type-error () t))
              (equal (funcall *numeric-primary* #'truncate 8 2) '(4)))`);
  check(`(let ((events nil)
     (f (miricl-compiler:compile-native '(lambda (function left right)
          (list (funcall function (funcall left) (funcall right)))))))
     (and (equal (funcall f #'/ (lambda () (push :left events) (values 8 :discarded))
                               (lambda () (push :right events) (values 2 :discarded))) '(4))
          (equal events '(:right :left))))`);
  check(`(let* ((f (lambda (&rest args) (values args :extra)))
                (a (car (funcall *numeric-primary* f :a :b)))
                (b (car (funcall *numeric-primary* f :a :b))))
     (and (equal a '(:a :b)) (equal b '(:a :b)) (not (eq a b))))`);
  check(`(equal (multiple-value-list
    (multiple-value-prog1 (values :kept :second)
      (funcall *numeric-primary* #'/ 13 4)
      (funcall *numeric-primary* #'truncate 13 4))) '(:kept :second))`);
  if (vm.vm['test-numeric-primary-packet']) {
    for (const mode of [0, 1]) assert.equal(vm.vm['test-numeric-primary-packet'](mode), 1);
    check('(= 42 (+ 20 22))');
  }
} finally { vm.close(); }
console.log('Saved numeric calls: exact/complex/float results, values, effects, replacement, arity and rejection/recovery pass.');
