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
    (defparameter *numeric-primary* (miricl-compiler:compile-native
      '(lambda (f x) (list (funcall f x)))))
    (defparameter *numeric-full* (miricl-compiler:compile-native
      '(lambda (f x) (funcall f x))))
    (defparameter *numeric-drop* (miricl-compiler:compile-native
      '(lambda (f x) (funcall f x) :done)))
    (defparameter *numeric-zero* (miricl-compiler:compile-native
      '(lambda (f) (list (funcall f)))))
    (defparameter *numeric-two* (miricl-compiler:compile-native
      '(lambda (f x y) (list (funcall f x y)))))
    (defparameter *numeric-binary* (miricl-compiler:compile-native
      '(lambda (f x y) (list (funcall f x y))))) t)`);
  check(`(let ((numbers (list 0 1 -1 (ash 1 150) (- (ash 1 150))
                     3/7 -3/7 (/ (ash 1 200) 7) (- (/ (ash 1 200) 7))
                     0.0d0 -0.0d0 1.25d0 -1.25d0 1.0d-200 -1.0d200
                     #c(3 4) #c(-3 4) #c(3/7 -4/7)
                     #c(0.0d0 -0.0d0) #c(-0.0d0 0.0d0)
                     #c(1.0d200 -1.0d200) #c(1.0d-200 -1.0d-200))))
    (every (lambda (f)
      (every (lambda (x)
        (let ((expected (funcall f x)))
          (and (eql (car (funcall *numeric-primary* f x)) expected)
               (equal (multiple-value-list (funcall *numeric-full* f x)) (list expected))
               (eq (funcall *numeric-drop* f x) :done)
               (equal (multiple-value-list
                 (multiple-value-prog1 (values :kept :second)
                   (funcall *numeric-primary* f x))) '(:kept :second))))) numbers))
      (list #'abs #'signum)))`);
  check(`(and
    (= (car (funcall *numeric-primary* #'abs -19/7)) 19/7)
    (= (car (funcall *numeric-primary* #'signum (- (ash 1 150)))) -1)
    (eql (car (funcall *numeric-primary* #'signum (complex (ash 1 1200) (ash 1 1199))))
         (signum (complex (ash 1 1200) (ash 1 1199))))
    (= (car (funcall *numeric-primary* #'abs #c(3 4))) 5)
    (eql (car (funcall *numeric-primary* #'signum -0.0d0)) -0.0d0)
    (= (float-sign (car (funcall *numeric-primary* #'signum -0.0d0))) -1.0d0)
    (eql (car (funcall *numeric-primary* #'signum #c(0.0d0 -0.0d0))) #c(0.0d0 -0.0d0)))`);
  for (const [name, recovered] of [['abs', '3'], ['signum', '-1']]) {
    check(`(every (lambda (x)
      (and (handler-case (progn (funcall *numeric-primary* #'${name} x) nil) (type-error () t))
           (= (car (funcall *numeric-primary* #'${name} -3)) ${recovered})))
      (list nil t :bad #\\x "text" (list 1) (vector 1) #'car (make-hash-table)))`);
    check(`(and
      (handler-case (progn (funcall *numeric-zero* #'${name}) nil) (program-error () t))
      (handler-case (progn (funcall *numeric-two* #'${name} 1 2) nil) (program-error () t)))`);
  }
  check(`(handler-case (progn (funcall *numeric-primary* :not-a-function 1) nil) (error () t))`);
  for (const name of ['abs', 'signum']) {
    check(`(let ((saved (symbol-function '${name})))
      (unwind-protect
        (progn (setf (symbol-function '${name}) (lambda (&rest args) (values args :extra)))
          (let ((a (car (funcall *numeric-primary* (symbol-function '${name}) -3)))
                (b (car (funcall *numeric-primary* (symbol-function '${name}) -3))))
            (and (equal a '(-3)) (equal b '(-3)) (not (eq a b))
                 (equal (multiple-value-list (funcall *numeric-full* (symbol-function '${name}) -3))
                   '((-3) :extra))
                 (= (car (funcall *numeric-primary* saved -3)) ${name === 'abs' ? '3' : '-1'}))))
        (setf (symbol-function '${name}) saved)))`);
  }
  check(`(let ((events nil)
    (f (miricl-compiler:compile-native '(lambda (f thunk) (list (funcall (funcall f) (funcall thunk)))))))
    (and (= (car (funcall f (lambda () (push :function events) #'abs)
                           (lambda () (push :argument events) (values -3 :ignored)))) 3)
         (equal events '(:argument :function))))`);
  check(`(and (eq (catch 'numeric-escape
    (funcall *numeric-primary* (lambda (x) (throw 'numeric-escape x)) :escaped)) :escaped)
    (= (car (funcall *numeric-primary* #'abs -3)) 3))`);
  // These binary callbacks were already fast. Guard their existing all-number
  // and arbitrary-object contracts while extending adjacent unary dispatch.
  check(`(let ((numbers (list 0 1 -1 (ash 1 120) (- (ash 1 120)) 3/7 -3/7
                        0.0d0 -0.0d0 1.0d0 #c(1 2) #c(1.0d0 0.0d0))))
    (every (lambda (f) (every (lambda (x) (every (lambda (y)
      (eq (not (null (car (funcall *numeric-binary* f x y))))
          (not (null (funcall f x y))))) numbers)) numbers)) (list #'= #'/= #'eql)))`);
  check(`(let* ((cell (list :shared)) (cycle (cons nil nil))
                  (objects (list nil t :symbol #\\x "text" cell cycle (vector 1) #'car)))
    (setf (cdr cycle) cycle)
    (every (lambda (x) (every (lambda (y)
      (eq (not (null (car (funcall *numeric-binary* #'eql x y))))
          (not (null (eql x y))))) objects)) objects))`);
  if (vm.vm['test-numeric-primary-packet']) {
    for (let operation = 0; operation < 2; operation++) {
      assert.equal(vm.vm['test-numeric-primary-packet'](operation), 1);
    }
  }
} finally { vm.close(); }
console.log('Saved numeric callbacks: all target numeric kinds, values, effects, replacement, errors and recovery pass.');
