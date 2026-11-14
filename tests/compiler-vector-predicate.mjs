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
    check(`(progn
      (defparameter *inline-vector-p* (miricl-compiler:compile-native
        '(lambda (v) (declare (inline vectorp) (optimize (safety ${safety}))) (vectorp v))))
      (defparameter *vector-length-branch* (miricl-compiler:compile-native
        '(lambda (v) (declare (inline vectorp length) (optimize (safety ${safety})))
          (if (vectorp v) (length v) :not-vector))))
      (every (lambda (v)
        (and (eql (not (null (vectorp v))) (not (null (funcall *inline-vector-p* v))))
             (= 1 (length (multiple-value-list (funcall *inline-vector-p* v))))
             (eql (if (vectorp v) (length v) :not-vector) (funcall *vector-length-branch* v))))
        (list nil 42 :value '(1 2) "abc" #(1 2 3) #*101
          (make-array nil :initial-element 42) (make-array '(2 3))
          (make-array 4 :fill-pointer 1 :adjustable t)
          (make-array 3 :displaced-to #(0 1 2 3 4) :displaced-index-offset 1))))`);
  }
  // Rank/type is invariant, while a vector's active length stays observable
  // after callbacks. A predicate proof must not hoist a mutable fill pointer.
  check(`(progn
    (defparameter *vector-callback-lengths* (miricl-compiler:compile-native
      '(lambda (v callback)
         (declare (inline vectorp length = 1+) (optimize (safety 0) (debug 0)))
         (block done (let ((result nil) (index 0))
           (declare (type (integer 0 3) index))
           (tagbody next
             (if (= index 3) (return-from done (reverse result)))
             (if (vectorp v) (setq result (cons (length v) result)))
             (funcall callback v)
             (setq index (1+ index))
             (go next)))))))
    (let ((v (make-array 3 :fill-pointer 3 :adjustable t)))
      (equal '(3 2 1) (funcall *vector-callback-lengths* v
        (lambda (v) (setf (fill-pointer v) (1- (fill-pointer v))))))))`);
  check(`(let ((events nil)
         (f (miricl-compiler:compile-native '(lambda (callback value)
               (declare (inline vectorp)) (vectorp (funcall callback value))))))
    (and (funcall f (lambda (x) (push :operand events) (values x :extra)) #(1))
         (equal events '(:operand))))`);
  if (!sourceCompiler) {
    // Replacing dependencies of a live source compiler can invalidate the
    // compiler itself. Run this contract check only with the sealed image.
    check(`(progn
      (defparameter *ordinary-vector-p-primary* (miricl-compiler:compile-native
        '(lambda (v) (values (vectorp v)))))
      (defparameter *ordinary-vector-p-full* (miricl-compiler:compile-native
        '(lambda (v) (vectorp v))))
      (let ((original (symbol-function 'vectorp)))
        (unwind-protect
          (progn (setf (symbol-function 'vectorp)
                   (lambda (v) (declare (ignore v)) (values :replacement :extra)))
            (and (equal (multiple-value-list (funcall *ordinary-vector-p-primary* nil)) '(:replacement))
                 (equal (multiple-value-list (funcall *ordinary-vector-p-full* nil)) '(:replacement :extra))
                 (funcall *inline-vector-p* #(1))
                 (not (funcall *inline-vector-p* nil))))
          (setf (symbol-function 'vectorp) original))))`);
  }
} finally {vm.close();}
console.log(`Vector predicate: representation kinds, value count, operand effects and mutable lengths pass${sourceCompiler ? ' (source compiler; sealed redefinition check deferred)' : ', including redefinition'}.`);
