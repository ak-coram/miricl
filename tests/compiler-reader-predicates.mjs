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
  for (const operator of ['stringp', 'integerp']) {
    for (const safety of [0, 3]) {
      check(`(progn
        (defparameter *reader-predicate* (miricl-compiler:compile-native
          '(lambda (v) (declare (inline ${operator}) (optimize (safety ${safety}))) (${operator} v))))
        (every (lambda (v)
          (and (eql (not (null (${operator} v))) (not (null (funcall *reader-predicate* v))))
               (= 1 (length (multiple-value-list (funcall *reader-predicate* v))))))
          (list nil t :value '(1 2) 0 -1 1073741824 (ash 1 100) (- (ash 1 100))
                3/2 1.25d0 #c(1 2) #\\a "" "aλ🙂" #(1 2 3) #*101
                (make-array 0 :element-type nil)
                (make-array 3 :element-type 'character :initial-element #\\x :fill-pointer 1 :adjustable t)
                (make-array 3 :element-type 'character :displaced-to "abλcd" :displaced-index-offset 1)
                (make-array '(2 3) :element-type 'character :initial-element #\\x))))`);
    }
    check(`(let ((events nil)
           (f (miricl-compiler:compile-native '(lambda (callback value)
                 (declare (inline ${operator})) (${operator} (funcall callback value)))))
           (discarded (miricl-compiler:compile-native '(lambda (callback value)
                 (declare (inline ${operator})) (${operator} (funcall callback value)) :done))))
      (and (funcall f (lambda (x) (push :tested events) (values x :extra)) ${operator === 'stringp' ? '"value"' : '(ash 1 100)'})
           (eq :done (funcall discarded (lambda (x) (push :discarded events) (values x :extra)) nil))
           (equal (reverse events) '(:tested :discarded))))`);
  }
  check(`(let ((f (miricl-compiler:compile-native
       '(lambda (value) (declare (inline integerp +))
          (if (integerp value) (+ value 1) :not-integer)))))
    (and (= (1+ (ash 1 100)) (funcall f (ash 1 100)))
         (= (- 1 (ash 1 100)) (funcall f (- (ash 1 100))))
         (eq :not-integer (funcall f 3/2)) (eq :not-integer (funcall f "value"))))`);
  check(`(let ((f (miricl-compiler:compile-native
       '(lambda (value callback)
          (declare (inline stringp integerp < = + 1+ length) (optimize (safety 0) (debug 0)))
          (let ((i 0) (sum 0) (lengths nil))
            (declare (type (integer 0 4) i) (type fixnum sum))
            (tagbody next
              (if (= i 4) (go done))
              (if (integerp value) (setq sum (+ sum 1)))
              (if (stringp value) (progn (setq sum (+ sum 2)) (setq lengths (cons (length value) lengths))))
              (funcall callback value)
              (setq i (1+ i)) (go next)
             done)
            (list sum (reverse lengths)))))))
    (and (equal '(4 nil) (funcall f (ash 1 100) #'identity))
         (equal '(0 nil) (funcall f :value #'identity))
         (let ((text (make-array 4 :element-type 'character :initial-element #\\x :fill-pointer 4)))
           (equal '(8 (4 3 2 1))
             (funcall f text (lambda (s) (setf (fill-pointer s) (1- (fill-pointer s)))))))))`);
  if (!sourceCompiler) {
    // Replacement tests need a sealed compiler; the source compiler itself
    // uses these predicates while analyzing and emitting test functions.
    for (const operator of ['stringp', 'integerp']) {
      check(`(progn
        (defparameter *predicate-ordinary-primary* (miricl-compiler:compile-native
          '(lambda (v) (values (${operator} v)))))
        (defparameter *predicate-ordinary-full* (miricl-compiler:compile-native
          '(lambda (v) (${operator} v))))
        (defparameter *predicate-notinline* (miricl-compiler:compile-native
          '(lambda (v) (declare (notinline ${operator})) (${operator} v))))
        (defparameter *predicate-captured* (miricl-compiler:compile-native
          '(lambda (v) (declare (inline ${operator})) (${operator} v))))
        (let ((original (symbol-function '${operator})))
          (unwind-protect
            (progn (setf (symbol-function '${operator})
                     (lambda (v) (declare (ignore v)) (values :replacement :extra)))
              (and (equal (multiple-value-list (funcall *predicate-ordinary-primary* nil)) '(:replacement))
                   (equal (multiple-value-list (funcall *predicate-ordinary-full* nil)) '(:replacement :extra))
                   (equal (multiple-value-list (funcall *predicate-notinline* nil)) '(:replacement :extra))
                   (funcall *predicate-captured* ${operator === 'stringp' ? '"value"' : '(ash 1 100)'})
                   (not (funcall *predicate-captured* nil))))
            (setf (symbol-function '${operator}) original))))`);
    }
  }
} finally {vm.close();}
console.log('Reader predicates: target integers/strings, checked branches, effects, multiple values, loop invariants and mutable lengths pass.');
