// Fixed-arity callback dispatch must use the resolved function object's identity.
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
  check(`(progn
    (defparameter *predicate-primary* (miricl-compiler:compile-native
      '(lambda (function value) (values (funcall function value)))))
    (defparameter *predicate-full* (miricl-compiler:compile-native
      '(lambda (function value) (funcall function value))))
    (defparameter *predicate-effects* (miricl-compiler:compile-native
      '(lambda (callback) (values (funcall (funcall callback :function) (funcall callback :argument))))))
    t)`);
  for (const operator of ['oddp', 'evenp']) {
    check(`(every (lambda (integer)
      (equal (multiple-value-list (funcall *predicate-primary* #'${operator} integer))
             (list (${operator} integer))))
      '(0 1 2 -1 -2 1073741823 -1073741824 9223372036854775807
        -9223372036854775808 1267650600228229401496703205376
        1267650600228229401496703205377 -1267650600228229401496703205377))`);
    check(`(and (every (lambda (value)
      (handler-case (progn (funcall *predicate-primary* #'${operator} value) nil)
        (type-error () t))) '(nil :bad "7" #\\7 1/2 2.0 #c(1 2)))
      (equal (multiple-value-list (funcall *predicate-primary* #'${operator} 3))
             (list (${operator} 3))))`);
    // Compile before replacing ODDP/EVENP, which the live source compiler may
    // itself call. During replacement this invokes only already compiled code.
    check(`(let ((original (symbol-function '${operator})))
      (unwind-protect
        (progn
          (setf (symbol-function '${operator})
            (lambda (&rest arguments) (values (car arguments) :extra)))
          (and
            (equal (multiple-value-list (funcall *predicate-primary* original 3))
                   (list ${operator === 'oddp' ? 't' : 'nil'}))
            (equal (multiple-value-list (funcall *predicate-primary* (symbol-function '${operator}) :replacement))
                   '(:replacement))
            (equal (multiple-value-list (funcall *predicate-full* (symbol-function '${operator}) :replacement))
                   '(:replacement :extra))))
        (setf (symbol-function '${operator}) original)))`);
  }
  check(`(let ((events nil))
    (and (equal (multiple-value-list
      (funcall *predicate-effects* (lambda (event)
        (push event events) (values (if (eq event :function) #'oddp 3) :extra)))) '(t))
      (equal (reverse events) '(:function :argument))))`);
  check(`(and (equal (multiple-value-list
       (funcall *predicate-primary* (lambda (x) (declare (ignore x)) (values)) :ignored)) '(nil))
     (equal (multiple-value-list
       (funcall *predicate-primary* (lambda (&rest xs) (values xs :extra)) 9)) '((9)))
     (equal (multiple-value-list
       (funcall *predicate-full* (lambda (&rest xs) (values xs :extra)) 9)) '((9) :extra)))`);
} finally {vm.close();}
console.log(`Resolved predicate callbacks: signed/bignum parity, type errors/recovery, saved identity, replacement, effects and value counts pass (${sourceCompiler ? 'source' : 'sealed'} compiler).`);
