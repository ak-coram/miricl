import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (process.argv.includes('--source')) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  check(`(progn
    (defparameter *round-primary* (miricl-compiler:compile-native '(lambda (f a b) (list (funcall f a b)))))
    (defparameter *round-one* (miricl-compiler:compile-native '(lambda (f a) (list (funcall f a)))))
    (defparameter *round-full* (miricl-compiler:compile-native '(lambda (f a b) (funcall f a b))))
    (defparameter *round-one-full* (miricl-compiler:compile-native '(lambda (f a) (funcall f a))))
    (defparameter *round-drop* (miricl-compiler:compile-native '(lambda (f a b) (funcall f a b) :done)))
    (defparameter *round-many* (miricl-compiler:compile-native '(lambda (f a b c) (list (funcall f a b c)))))
    (and (functionp *round-primary*) (functionp *round-one*)))`);
  // Ordinary full calls are the target oracle, independent of primary dispatch.
  for (const name of ['floor', 'ceiling', 'truncate', 'round']) {
    check(`(every (lambda (a)
      (and (equalp (funcall *round-one* #'${name} a) (list (${name} a)))
        (equalp (multiple-value-list (funcall *round-one-full* #'${name} a))
                (multiple-value-list (${name} a)))
        (every (lambda (b)
          (let ((expected (multiple-value-list (${name} a b))))
            (and (equalp (funcall *round-primary* #'${name} a b) (list (car expected)))
                 (equalp (multiple-value-list (funcall *round-full* #'${name} a b)) expected)
                 (eq (funcall *round-drop* #'${name} a b) :done))))
          '(-1073741824 -1073741823 -17 -4 -3 -2 -1 1 2 3 4 17 1073741823))))
      '(-1073741824 -1073741823 -1073741822 -17 -15 -14 -13 -10 -7 -6 -5 -3 -2 -1
        0 1 2 3 5 6 7 10 13 14 15 17 1073741821 1073741822 1073741823))`);
    check(`(every (lambda (pair)
      (let ((a (car pair)) (b (cadr pair)))
        (and (equalp (funcall *round-primary* #'${name} a b) (list (${name} a b)))
             (equalp (funcall *round-one* #'${name} a) (list (${name} a))))))
      (list '(13/7 4/11) '(-13/7 -4/11) '(1.25 0.5) '(-1.25d0 0.5d0)
            '(1/3 2.0) '(1.25 2.0d0) '(1.0d0 3/7) '(-0.0d0 1.0d0)
            (list (ash 1 120) 7) (list (- (1+ (ash 1 120))) -17)))`);
    check(`(and (equal (funcall *round-primary* #'${name} -1073741824 -1) '(1073741824))
      (handler-case (progn (funcall *round-many* #'${name} 1 2 3) nil) (program-error () t))
      (handler-case (progn (funcall *round-primary* #'${name} 3 0) nil)
        (division-by-zero (c) (equal (arithmetic-error-operands c) '(3 0))))
      (handler-case (progn (funcall *round-drop* #'${name} 3 0) nil) (division-by-zero () t))
      (equal (funcall *round-primary* #'${name} 8 2) '(4)))`);
    for (const bad of [':bad', 'nil', '"bad"', '#c(1 2)']) {
      check(`(let ((bad ${bad}))
        (and (handler-case (progn (funcall *round-primary* #'${name} bad 2) nil)
               (type-error (c) (eq (type-error-datum c) bad)))
             (handler-case (progn (funcall *round-primary* #'${name} 2 bad) nil)
               (type-error (c) (eq (type-error-datum c) bad)))
             (handler-case (progn (funcall *round-one* #'${name} bad) nil)
               (type-error (c) (eq (type-error-datum c) bad)))
             (equal (funcall *round-one* #'${name} 4) '(4))))`);
    }
    // Compile before replacement: the source compiler can itself use rounding.
    check(`(let ((saved (symbol-function '${name})))
      (unwind-protect
        (progn (setf (symbol-function '${name}) (lambda (&rest args) (values args :extra)))
          (let ((a (car (funcall *round-primary* (symbol-function '${name}) 8 2)))
                (b (car (funcall *round-primary* (symbol-function '${name}) 8 2))))
            (and (equal a '(8 2)) (equal b '(8 2)) (not (eq a b))
              (equal (funcall *round-one* (symbol-function '${name}) 8) '((8)))
              (equal (multiple-value-list (funcall *round-full* (symbol-function '${name}) 8 2)) '((8 2) :extra))
              (equal (funcall *round-primary* saved 8 2) '(4))
              (equal (funcall *round-one* saved 8) '(8)))))
        (setf (symbol-function '${name}) saved)))`);
    check(`(equal (multiple-value-list
      (multiple-value-prog1 (values :kept :second)
        (funcall *round-primary* #'${name} 13 4)
        (funcall *round-one* #'${name} 13/4))) '(:kept :second))`);
  }
  check(`(let ((events nil)
    (f (miricl-compiler:compile-native '(lambda (f left right)
      (list (funcall f (funcall left) (funcall right)))))))
    (and (equal (funcall f #'floor (lambda () (push :left events) (values -13 :extra))
                                  (lambda () (push :right events) (values 4 :extra))) '(-4))
         (equal events '(:right :left))))`);
  if (vm.vm['test-rounding-primary-packet']) {
    for (let mode = 0; mode < 4; mode++) {
      for (const unary of [0, 1]) {
        for (const kind of [0, 1, 2]) assert.equal(vm.vm['test-rounding-primary-packet'](mode, unary, kind), 1);
      }
    }
    check('(= (+ 20 22) 42)');
  }
} finally { vm.close(); }
console.log('Saved rounding calls: primary/full/unary, signs, ties, promotion, values, effects, redefinition, errors and recovery pass.');
