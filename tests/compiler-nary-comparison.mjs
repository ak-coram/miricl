import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const sourceCompiler = process.argv.includes('--source');
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = form => assert.equal(vm.evaluate(form).output, 'T\n', form);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (sourceCompiler) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  for (const operator of ['<', '<=', '>', '>=']) {
    check(`(progn
      (defparameter *nary-comparison* (miricl-compiler:compile-native
        '(lambda (a b c d)
           (declare (type (signed-byte 64) a b c d)
                    (optimize (safety 0)) (inline ${operator}))
           (${operator} a b c d))))
      (every (lambda (arguments)
               (equal (multiple-value-list (apply *nary-comparison* arguments))
                      (list (not (null (apply #'${operator} arguments))))))
             (list '(0 1 2 3) '(3 2 1 0) '(1 1 1 1) '(0 2 1 3)
                   (list (- (ash 1 63)) -1 0 (1- (ash 1 63))))))`);
  }
  for (const count of [8, 9]) {
    const parameters = Array.from({length: count}, (_, i) => `v${i}`).join(' ');
    const values = Array.from({length: count}, (_, i) => String(i)).join(' ');
    check(`(let ((f (miricl-compiler:compile-native
        '(lambda (${parameters})
           (declare (type fixnum ${parameters}) (optimize (safety 0)) (inline <=))
           (<= ${parameters})))))
      (equal (multiple-value-list (funcall f ${values})) '(t)))`);
  }
  check(`(progn
    (defparameter *nary-effects* (miricl-compiler:compile-native
      '(lambda (callback)
         (declare (optimize (safety 0)) (inline <=))
         (<= (the fixnum (funcall callback :first 5))
             (the fixnum (funcall callback :second 1))
             (the fixnum (funcall callback :third 9))))))
    (let ((events nil))
      (and (null (funcall *nary-effects*
                   (lambda (event value) (push event events) (values value :extra))))
           (equal (reverse events) '(:first :second :third)))))`);
  // A loop-invariant chain can share a raw condition; variable chains must
  // retain every operand and each iteration's integer value.
  check(`(let ((f (miricl-compiler:compile-native
       '(lambda (low high)
          (declare (type fixnum low high) (optimize (safety 0)) (inline <= < +))
          (let ((i 0) (sum 0))
            (declare (type fixnum i sum))
            (tagbody next
              (if (< i 10) nil (go done))
              (if (<= low 5 high) (setq sum (+ sum i)))
              (if (<= 2 i 7) (setq sum (+ sum 1)))
              (setq i (+ i 1)) (go next)
             done)
            sum)))))
    (and (= 51 (funcall f 0 9)) (= 6 (funcall f 6 9))))`);
  // Unknown numeric representations retain full CL comparison semantics,
  // including arbitrary precision, rational/float contagion and conditions.
  check(`(let ((f (miricl-compiler:compile-native
       '(lambda (a b c) (declare (optimize (safety 0)) (inline <=)) (<= a b c)))))
    (and (funcall f 1 3/2 2.0d0)
         (funcall f 0 (ash 1 100) (ash 1 101))
         (handler-case (progn (funcall f 3 1 :bad) nil) (type-error () t))
         (funcall f 1 2 3)))`);
  // Ordinary definitions and NOTINLINE keep their redefinition contract.
  // A source compiler itself calls comparisons, so this runs only with the
  // sealed native compiler and all test functions compiled before replacement.
  if (!sourceCompiler) {
    check(`(progn
      (defparameter *nary-ordinary* (miricl-compiler:compile-native
        '(lambda (a b c) (declare (type fixnum a b c) (optimize (safety 0))) (<= a b c))))
      (defparameter *nary-notinline* (miricl-compiler:compile-native
        '(lambda (a b c) (declare (type fixnum a b c) (optimize (safety 0)) (notinline <=)) (<= a b c))))
      (let ((original (symbol-function '<=)))
        (unwind-protect
          (progn (setf (symbol-function '<=) (lambda (&rest args)
                    (declare (ignore args)) (values :replacement :extra)))
            (and (equal (multiple-value-list (funcall *nary-ordinary* 1 2 3)) '(:replacement :extra))
                 (equal (multiple-value-list (funcall *nary-notinline* 1 2 3)) '(:replacement :extra))))
          (setf (symbol-function '<=) original))))`);
  }
} finally { vm.close(); }
console.log('N-ary comparison: scalar endpoints, ordering, multiple values, effects, loop hoists and generic fallback pass.');
