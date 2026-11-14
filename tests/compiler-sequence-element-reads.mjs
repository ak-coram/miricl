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
    check(`(let ((f (miricl-compiler:compile-native
        '(lambda (a i) (declare (inline sbit) (optimize (safety ${safety}))) (sbit a i))))
        (bits (make-array 67 :element-type 'bit :initial-element 0)))
      (dolist (i '(0 31 32 63 64 66)) (setf (sbit bits i) 1))
      (and (loop for i below 67 always
              (equal (multiple-value-list (funcall f bits i)) (list (sbit bits i))))
           (every (lambda (i) (handler-case (progn (funcall f bits i) nil) (type-error () t)))
                  (list -1 67 (ash 1 100) 1/2 :bad))
           (handler-case (progn (funcall f #(1 0) 0) nil) (type-error () t))
           (handler-case (progn (funcall f (make-array '(2 2) :element-type 'bit) 0) nil)
              (program-error () t))
           (handler-case (progn (funcall f (make-array 4 :element-type 'bit :adjustable t) 0) nil)
              (type-error () t))
           (handler-case (progn (funcall f (make-array 2 :element-type 'bit :displaced-to bits) 0) nil)
              (type-error () t))
           (= 1 (funcall f bits 32))))`);
    check(`(let ((f (miricl-compiler:compile-native
        '(lambda (a i) (declare (inline row-major-aref) (optimize (safety ${safety})))
           (row-major-aref a i)))))
      (and (every (lambda (a) (loop for i below (array-total-size a) always
                   (equal (multiple-value-list (funcall f a i)) (list (row-major-aref a i)))))
            (list #(a b c) "aλ🙂" #*1001 (make-array nil :initial-element :scalar)
                  (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
           (every (lambda (pair) (handler-case (progn (funcall f (first pair) (second pair)) nil)
                                    (type-error () t)))
              (list (list :bad 0) (list #(1) -1) (list #(1) 1) (list #(1) (ash 1 100))
                    (list #(1) 1/2) (list #(1) :bad) (list (make-array 0 :element-type nil) 0)))
           (= 7 (funcall f #(7) 0))))`);
  }
  check(`(let* ((backing (vector :a :b :c :d))
       (a (make-array 2 :displaced-to backing :displaced-index-offset 1 :fill-pointer 0))
       (f (miricl-compiler:compile-native
           '(lambda (a callback) (declare (inline row-major-aref))
              (let ((first (row-major-aref a 1)))
                (funcall callback a) (list first (row-major-aref a 1)))))))
    (equal '(:c :changed) (funcall f a (lambda (a) (setf (fill-pointer a) 1 (aref backing 2) :changed)))))`);
  check(`(let* ((a #*0) (f (miricl-compiler:compile-native
        '(lambda (a callback) (declare (inline sbit zerop))
           (let ((first (zerop (sbit a 0))))
             (funcall callback a) (list first (zerop (sbit a 0))))))))
    (equal '(t nil) (funcall f a (lambda (a) (setf (sbit a 0) 1)))))`);
  check(`(let ((f (miricl-compiler:compile-native
         '(lambda (a) (declare (inline sbit)) (sbit a 1 1)))))
    (= 1 (funcall f (make-array '(2 2) :element-type 'bit :initial-contents '((0 0) (0 1))))))`);
  for (const operator of ['sbit', 'row-major-aref']) {
    check(`(let ((events nil)
       (f (miricl-compiler:compile-native '(lambda (callback)
            (declare (inline ${operator}))
            (${operator} (funcall callback :array #*01) (funcall callback :index 1)))))
       (discard (miricl-compiler:compile-native '(lambda (a i)
            (declare (inline ${operator})) (${operator} a i) :done))))
      (and (= 1 (funcall f (lambda (event value) (push event events) (values value :extra))))
           (equal (reverse events) '(:array :index))
           (handler-case (progn (funcall discard #*01 2) nil) (type-error () t))
           (eq :done (funcall discard #*01 0))))`);
    // Replacing array primitives can change the live source compiler itself.
    // Exercise definition capture with the sealed compiler instead.
    if (!sourceCompiler) {
      check(`(progn
        (defparameter *element-ordinary* (miricl-compiler:compile-native '(lambda (a i) (${operator} a i))))
        (defparameter *element-notinline* (miricl-compiler:compile-native
          '(lambda (a i) (declare (notinline ${operator})) (${operator} a i))))
        (defparameter *element-captured* (miricl-compiler:compile-native
          '(lambda (a i) (declare (inline ${operator})) (${operator} a i))))
        (let ((original (symbol-function '${operator})))
          (unwind-protect
            (progn (setf (symbol-function '${operator})
                    (lambda (a i) (declare (ignore a i)) (values :replacement :extra)))
              (and (equal (multiple-value-list (funcall *element-ordinary* #*01 1)) '(:replacement :extra))
                   (equal (multiple-value-list (funcall *element-notinline* #*01 1)) '(:replacement :extra))
                   (equal (multiple-value-list (funcall *element-captured* #*01 1)) '(1))
                   (let ((fresh (miricl-compiler:compile-native
                            '(lambda (a i) (declare (inline ${operator})) (${operator} a i)))))
                     (equal (multiple-value-list (funcall fresh #*01 1)) '(:replacement :extra)))))
            (setf (symbol-function '${operator}) original))))`);
    }
  }
} finally {vm.close();}
console.log(`Sequence element reads: checked array contracts, live mutation, values and effects pass (${sourceCompiler ? 'source' : 'sealed'} compiler).`);
