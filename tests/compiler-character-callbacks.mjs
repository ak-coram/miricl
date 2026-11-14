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
    (defparameter *character-primary* (miricl-compiler:compile-native
      '(lambda (function value) (values (funcall function value)))))
    (defparameter *character-full* (miricl-compiler:compile-native
      '(lambda (function value) (funcall function value))))
    (defparameter *character-effects* (miricl-compiler:compile-native
      '(lambda (callback) (values (funcall (funcall callback :function) (funcall callback :argument))))))
    (defparameter *character-discard* (miricl-compiler:compile-native
      '(lambda (function value) (funcall function value) :done))) t)`);
  for (const operator of ['char-upcase', 'char-downcase', 'miricl::unicode-char-upcase', 'miricl::unicode-char-downcase', 'alphanumericp']) {
    check(`(every (lambda (code)
      (let ((character (code-char code)))
        (and (equal (multiple-value-list (funcall *character-primary* #'${operator} character))
                    (list (${operator} character)))
             (equal (multiple-value-list (funcall *character-full* #'${operator} character))
                    (list (${operator} character))))))
      '(0 9 32 48 65 97 181 255 955 962 931 7838 8490 65535 1114111))`);
    check(`(and (every (lambda (value)
      (and (handler-case (progn (funcall *character-primary* #'${operator} value) nil) (type-error () t))
           (handler-case (progn (funcall *character-discard* #'${operator} value) nil) (type-error () t))))
       '(nil :bad "a" 65 1/2 2.0 #c(1 2) 1267650600228229401496703205376))
      (equal (multiple-value-list (funcall *character-primary* #'${operator} #\\a))
             (list (${operator} #\\a))))`);
    check(`(let* ((original (symbol-function '${operator}))
                 (wanted (${operator} #\\a)))
      (unwind-protect
        (progn
          (setf (symbol-function '${operator})
            (lambda (&rest arguments) (values (car arguments) :extra)))
          (and (equal (multiple-value-list (funcall *character-primary* original #\\a)) (list wanted))
               (equal (multiple-value-list (funcall *character-full* original #\\a)) (list wanted))
               (equal (multiple-value-list (funcall *character-primary* (symbol-function '${operator}) #\\a)) '(#\\a))
               (equal (multiple-value-list (funcall *character-full* (symbol-function '${operator}) #\\a)) '(#\\a :extra))))
        (setf (symbol-function '${operator}) original)))`);
  }
  check(`(let ((events nil))
    (and (equal (multiple-value-list (funcall *character-effects*
          (lambda (event) (push event events)
            (if (eq event :function) (values #'char-upcase :extra) (values #\\a :extra))))) '(#\\A))
         (equal (reverse events) '(:function :argument))))`);
  check(`(let* ((saved nil) (function (lambda (&rest arguments) (push arguments saved) (values (car arguments) :extra))))
    (and (char= #\\a (funcall *character-primary* function #\\a))
         (char= #\\b (funcall *character-primary* function #\\b))
         (not (eq (first saved) (second saved))) (equal saved '((#\\b) (#\\a)))))`);
} finally {vm.close();}
console.log(`Character callback dispatch: checked values, capture, multiple values, effects and recovery pass (${sourceCompiler ? 'source' : 'sealed'} compiler).`);
