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
  for (const safety of [0, 3]) {
    for (const initial of ['', ':initial-element element']) {
      check(`(let ((f (miricl-compiler:compile-native '(lambda (n element)
                    (declare (type (integer 0 536870910) n) (inline make-array)
                             (optimize (safety ${safety})))
                    (make-array n ${initial})))))
        (every (lambda (n)
          (let* ((element (list :shared)) (a (funcall f n element)) (b (funcall f n element)))
            (and (typep a 'simple-vector) (= (array-rank a) 1) (= (length a) n)
                 (eq (array-element-type a) t) (not (array-has-fill-pointer-p a))
                 (not (eq a b))
                 ${initial ? '(every (lambda (x) (eq x element)) a)' : 't'})))
          '(0 1 8 64)))`);
    }
    check(`(let ((f (miricl-compiler:compile-native '(lambda (n)
                   (declare (fixnum n) (inline make-string) (optimize (safety ${safety})))
                   (make-string n)))))
      (every (lambda (n)
        (let ((a (funcall f n)) (b (funcall f n)))
          (and (typep a 'simple-string) (= (length a) n) (not (eq a b))
               (string= a (make-string n))))) '(0 1 8 64)))`);
  }
  // Unknown dimensionality and other keyword layouts retain general MAKE-ARRAY.
  check(`(let ((f (miricl-compiler:compile-native '(lambda (dimensions element)
                (declare (inline make-array)) (make-array dimensions :initial-element element)))))
    (and (equal (array-dimensions (funcall f '(2 3) :value)) '(2 3))
         (equal (array-dimensions (funcall f nil :value)) nil)
         (eq (row-major-aref (funcall f nil :value) 0) :value)))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (n)
                (declare (fixnum n) (inline make-array))
                (make-array n :element-type 'bit :initial-element 1 :fill-pointer 1)))))
    (let ((v (funcall f 3)))
      (and (= (length v) 1) (= (array-dimension v 0) 3) (= (bit v 2) 1))))`);
  check(`(let ((events nil)
        (f (miricl-compiler:compile-native '(lambda (size initial)
                (declare (inline make-array))
                (make-array (the (integer 0 536870910) (funcall size))
                            :initial-element (funcall initial))))))
    (let ((v (funcall f (lambda () (push :size events) (values 3 :extra))
                        (lambda () (push :initial events) (values :value :extra)))))
      (and (equal (reverse events) '(:size :initial)) (equalp v #(:value :value :value)))))`);
  check(`(let ((f (miricl-compiler:compile-native '(lambda (n)
                (declare (fixnum n) (inline make-array)) (make-array n :initial-element :value)))))
    (and (handler-case (progn (funcall f -1) nil) (type-error () t))
         (handler-case (progn (funcall f 536870911) nil) (type-error () t))
         (equalp (funcall f 2) #(:value :value))
         (= 1 (length (multiple-value-list (funcall f 2))))))`);
  for (const args of ['', 'n :initial-element']) {
    check(`(let ((f (miricl-compiler:compile-native '(lambda (n)
                  (declare (fixnum n) (inline make-array)) (make-array ${args})))))
      (handler-case (progn (funcall f 0) nil) (program-error () t)))`);
  }
  if (!sourceCompiler) {
    check(`(let ((ordinary (miricl-compiler:compile-native '(lambda (n)
                   (declare (fixnum n)) (make-array n :initial-element nil))))
          (captured (miricl-compiler:compile-native '(lambda (n)
                   (declare (fixnum n) (inline make-array)) (make-array n :initial-element nil))))
          (dynamic (miricl-compiler:compile-native '(lambda (n)
                   (declare (fixnum n) (notinline make-array)) (make-array n :initial-element nil))))
          (original (symbol-function 'make-array)))
      (unwind-protect
        (progn (setf (symbol-function 'make-array)
                 (lambda (&rest arguments) (declare (ignore arguments)) (values :replacement :extra)))
          (and (equal (multiple-value-list (funcall ordinary 2)) '(:replacement :extra))
               (equal (multiple-value-list (funcall dynamic 2)) '(:replacement :extra))
               (equalp (funcall captured 2) #(nil nil))))
        (setf (symbol-function 'make-array) original)))`);
  }
} finally { vm.close(); }
console.log('Vector allocation: dimensions, initialization/sharing, fresh arrays, effects/value counts, general layouts, errors/recovery and malformed calls pass.');
