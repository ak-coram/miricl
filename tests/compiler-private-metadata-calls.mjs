import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

// Diagnostic cold images retain the source compiler and expose the private
// callbacks without making a test hook part of the shipped runtime.
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ??
  'build/kernel-migration/private-metadata-calls/after-cold.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  if (process.argv.includes('--source')) {
    vm.evaluate('(setq miricl-compiler::*compiler-state* :interpreted)');
    vm.evaluate(await fs.readFile('wasm-compiler.lisp', 'utf8'));
    vm.evaluate('(in-package :cl-user)');
  }
  assert.equal(typeof vm.vm['test-private-metadata-install'], 'function', 'Use a diagnostic cold image');
  vm.vm['test-private-metadata-install']();
  assert.equal(vm.vm['test-private-metadata-values'](), 1, 'primary ABI clears transient value packet');
  check(`(progn
    (defparameter *metadata-array* (fdefinition *metadata-array-symbol*))
    (defparameter *metadata-stream* (fdefinition *metadata-stream-symbol*))
    (defparameter *metadata-primary3* (miricl-compiler:compile-native
      '(lambda (function a b c) (list (funcall function a b c)))))
    (defparameter *metadata-values3* (miricl-compiler:compile-native
      '(lambda (function a b c) (funcall function a b c))))
    (defparameter *metadata-primary2* (miricl-compiler:compile-native
      '(lambda (function a b) (list (funcall function a b)))))
    (defparameter *metadata-primary4* (miricl-compiler:compile-native
      '(lambda (function a b c d) (list (funcall function a b c d)))))
    (defparameter *metadata-discard3* (miricl-compiler:compile-native
      '(lambda (function a b c) (funcall function a b c) :done)))
    t)`);
  check(`(and (equal (funcall *metadata-primary3* *metadata-array* 0 'integer nil) '(integer))
    (equal (funcall *metadata-primary3* *metadata-array* 1 'bit nil) '(2))
    (equal (funcall *metadata-primary3* *metadata-array* 3 '(a b c) nil) '(3))
    (equal (multiple-value-list (funcall *metadata-values3* *metadata-array* 3 nil nil)) '(0))
    (let ((cycle (list 1 2))) (setf (cdr (last cycle)) cycle)
      (equal (funcall *metadata-primary3* *metadata-array* 3 cycle nil) '(nil))))`);
  for (const selector of [':bad', '-1', '5', '(ash 1 100)', '1/2']) {
    check(`(and (handler-case (progn
      (funcall *metadata-primary3* *metadata-array* ${selector} 'integer nil) nil) (program-error () t))
      (equal (funcall *metadata-primary3* *metadata-array* 3 nil nil) '(0)))`);
  }
  check(`(and (handler-case (progn
    (funcall *metadata-primary3* *metadata-array* 3 '(a . b) nil) nil) (type-error () t))
    (handler-case (progn
      (funcall *metadata-discard3* *metadata-array* 4 '(array bit (a)) 2) nil) (type-error () t))
    (eq :done (funcall *metadata-discard3* *metadata-array* 3 nil nil)))`);
  check(`(let* ((out (make-string-output-stream)) (alias (make-two-way-stream (make-string-input-stream "") out))
                (broadcast (make-broadcast-stream out out)))
    (and (equal (funcall *metadata-primary3* *metadata-stream* out 0 nil) '(22))
      (eq out (car (funcall *metadata-primary3* *metadata-stream* alias 1 nil)))
      (equal (car (funcall *metadata-primary3* *metadata-stream* broadcast 2 nil)) (list out out))
      (equal (funcall *metadata-primary3* *metadata-stream* out 3 #("aλ🙂b" 1 2 2)) '(nil))
      (string= (get-output-stream-string out) "λ🙂")))`);
  for (const selector of [':bad', '-1', '4', '(ash 1 100)', '1/2']) {
    check(`(let ((out (make-string-output-stream)))
      (and (handler-case (progn (funcall *metadata-primary3* *metadata-stream* out ${selector} nil) nil)
               (type-error () t))
           (equal (funcall *metadata-primary3* *metadata-stream* out 0 nil) '(22))))`);
  }
  for (const packet of ['nil', '#(1 2 3)', '#("a" -1 1 2)', '#("a" 0 2 2)', '#("a" 0 1 0)']) {
    check(`(let ((out (make-string-output-stream)))
      (and (handler-case (progn (funcall *metadata-primary3* *metadata-stream* out 3 ${packet}) nil)
              (error () t))
           (equal (funcall *metadata-primary3* *metadata-stream* out 0 nil) '(22))))`);
  }
  check(`(let ((out (make-string-output-stream)))
    (close out)
    (and (handler-case (progn (funcall *metadata-primary3* *metadata-stream* out 0 nil) nil) (stream-error () t))
      (handler-case (progn (funcall *metadata-primary3* *metadata-stream* :bad 0 nil) nil) (type-error () t))))`);
  for (const callback of ['*metadata-array*', '*metadata-stream*']) {
    check(`(and (handler-case (progn (funcall *metadata-primary2* ${callback} nil nil) nil) (program-error () t))
      (handler-case (progn (funcall *metadata-primary4* ${callback} nil nil nil nil) nil) (program-error () t)))`);
  }
  // Compile before replacing any dependency used by the live source compiler.
  // Saved old objects continue to use their immutable builtin identity.
  check(`(let ((array (fdefinition *metadata-array-symbol*)) (stream (fdefinition *metadata-stream-symbol*)))
    (unwind-protect
      (progn
        (setf (fdefinition *metadata-array-symbol*) (lambda (a b c) (values (list a b c) :extra))
              (fdefinition *metadata-stream-symbol*) (lambda (a b c) (values (list c b a) :extra)))
        (and (equal (funcall *metadata-primary3* (fdefinition *metadata-array-symbol*) :a :b :c) '((:a :b :c)))
          (equal (multiple-value-list (funcall *metadata-values3* (fdefinition *metadata-array-symbol*) :a :b :c))
                 '((:a :b :c) :extra))
          (equal (funcall *metadata-primary3* (fdefinition *metadata-stream-symbol*) :a :b :c) '((:c :b :a)))
          (equal (funcall *metadata-primary3* array 3 '(a b) nil) '(2))
          (let ((out (make-string-output-stream)))
            (equal (funcall *metadata-primary3* stream out 0 nil) '(22)))))
      (setf (fdefinition *metadata-array-symbol*) array (fdefinition *metadata-stream-symbol*) stream)))`);
  check(`(let ((kept (list :first :second)))
    (equal (multiple-value-list (multiple-value-prog1 (values-list kept)
      (funcall *metadata-primary3* *metadata-array* 3 '(a b) nil))) kept))`);
} finally { vm.close(); }
console.log('Private metadata calls: checked selectors, packets, values, saved identity, replacement, arity and recovery pass.');
