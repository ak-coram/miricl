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
  // Get the actual private primitive from its owning library closure; its
  // uninterned symbol identity cannot be reconstructed by INTERN or by name.
  check(`(progn
    (defun fixed-primitive-symbol (name form)
      (cond ((symbolp form) (if (and (null (symbol-package form)) (string= name (symbol-name form))) form))
            ((consp form) (or (fixed-primitive-symbol name (car form))
                              (fixed-primitive-symbol name (cdr form))))))
    (defparameter *fixed-list-symbol*
      (fixed-primitive-symbol "%%LIST-LENGTH" (function-lambda-expression #'list-length)))
    (defparameter *fixed-list-function* (fdefinition *fixed-list-symbol*))
    (defparameter *fixed-primary1* (miricl-compiler:compile-native
      '(lambda (function x) (list (funcall function x)))))
    (defparameter *fixed-primary2* (miricl-compiler:compile-native
      '(lambda (function a b) (list (funcall function a b)))))
    (defparameter *fixed-primary3* (miricl-compiler:compile-native
      '(lambda (function a b c) (list (funcall function a b c)))))
    (defparameter *fixed-values1* (miricl-compiler:compile-native
      '(lambda (function x) (funcall function x))))
    (and *fixed-list-symbol* (functionp *fixed-list-function*)))`);
  check(`(and (equal (funcall *fixed-primary1* *fixed-list-function* nil) '(0))
              (equal (funcall *fixed-primary1* *fixed-list-function* '(a b c)) '(3))
              (equal (multiple-value-list (funcall *fixed-values1* *fixed-list-function* '(a b))) '(2))
              (let ((cycle (list 1 2))) (setf (cdr (last cycle)) cycle)
                (equal (funcall *fixed-primary1* *fixed-list-function* cycle) '(nil))))`);
  for (const datum of [':bad', "'(1 . 2)"]) {
    check(`(and (handler-case (progn (funcall *fixed-primary1* *fixed-list-function* ${datum}) nil)
                   (type-error () t))
                (equal (funcall *fixed-primary1* *fixed-list-function* '(a)) '(1)))`);
  }
  check(`(and (handler-case (progn (funcall *fixed-primary2* *fixed-list-function* nil nil) nil)
                   (program-error () t))
              (handler-case (progn (funcall *fixed-primary3* *fixed-list-function* nil nil nil) nil)
                   (program-error () t))
              (equal (funcall *fixed-primary1* *fixed-list-function* nil) '(0)))`);
  check(`(let ((events nil)
       (f (miricl-compiler:compile-native '(lambda (function callback)
          (list (funcall function (funcall callback)))))))
     (and (equal (funcall f *fixed-list-function*
                   (lambda () (push :argument events) (values '(a b) :discarded))) '(2))
          (equal events '(:argument))))`);
  check(`(let ((events nil)
       (f (miricl-compiler:compile-native '(lambda (callback)
          (list (funcall (funcall callback :function)
                         (funcall callback :a) (funcall callback :b)
                         (funcall callback :c)))))))
     (and (equal (funcall f (lambda (step)
                    (push step events)
                    (if (eq step :function) (lambda (a b c) (values (list a b c) :extra)) step)))
                 '((:a :b :c)))
          (equal (reverse events) '(:function :a :b :c))))`);
  check(`(let ((f (miricl-compiler:compile-native
                   '(lambda (function x) (funcall function x) :done))))
    (and (handler-case (progn (funcall f *fixed-list-function* '(a . b)) nil) (type-error () t))
         (eq :done (funcall f *fixed-list-function* '(a b)))))`);
  // The source compiler may itself use this private primitive. Isolate its
  // redefinition after compilation and restore it before compiling anything.
  check(`(let ((saved (fdefinition *fixed-list-symbol*)))
     (unwind-protect
       (progn (setf (fdefinition *fixed-list-symbol*)
                (lambda (x) (declare (ignore x)) (values :replacement :extra)))
         (and (equal (funcall *fixed-primary1* (fdefinition *fixed-list-symbol*) nil) '(:replacement))
              (equal (multiple-value-list (funcall *fixed-values1* (fdefinition *fixed-list-symbol*) nil))
                     '(:replacement :extra))
              (equal (funcall *fixed-primary1* saved '(a b c)) '(3))))
       (setf (fdefinition *fixed-list-symbol*) saved)))`);
  check(`(let* ((f (lambda (&rest args) (values args :extra)))
                 (a (car (funcall *fixed-primary2* f :a :b)))
                 (b (car (funcall *fixed-primary2* f :a :b))))
     (and (equal a '(:a :b)) (equal b '(:a :b)) (not (eq a b))
          (equal (funcall *fixed-primary3* f :a :b :c) '((:a :b :c)))))`);
  check(`(let ((kept (list :kept :second)))
     (equal (multiple-value-list
       (multiple-value-prog1 (values-list kept)
         (funcall *fixed-primary1* *fixed-list-function* '(a b c)))) kept))`);
  if (vm.vm['test-fixed-primitives-install']) {
    vm.vm['test-fixed-primitives-install']();
    assert.equal(vm.vm['test-fixed-primary-values'](), 1, 'raw primary hash lookup preserves pending value packet');
    check(`(and (equal (funcall *fixed-primary1* *fixed-odd* (ash 1 100)) '(nil))
                (equal (funcall *fixed-primary1* *fixed-odd* (1+ (ash 1 100))) '(t))
                (handler-case (progn (funcall *fixed-primary1* *fixed-odd* 1/2) nil) (type-error () t)))`);
    check(`(let* ((plist (list :a 1 :b 2)) (tail (cdddr plist)))
       (and (eq (car (funcall *fixed-primary2* *fixed-cell* plist :b)) tail)
            (equal (funcall *fixed-primary2* *fixed-eq-list* '(a b) '(a b)) '(t))
            (equal (funcall *fixed-primary2* *fixed-eq-list* '(a) '(b)) '(nil))
            (handler-case (progn (funcall *fixed-primary2* *fixed-cell* '(:a . :bad) :b) nil)
              (type-error () t))))`);
    check(`(let ((table (make-hash-table)))
       (setf (gethash :key table) :value)
       (and (eq (cdr (car (funcall *fixed-primary2* *fixed-hash-cell* :key table))) :value)
            (equal (funcall *fixed-primary2* *fixed-hash-cell* :absent table) '(nil))
            (handler-case (progn (funcall *fixed-primary2* *fixed-hash-cell* :key :bad) nil)
              (type-error () t))))`);
    check(`(let ((p (find-package :cl)))
       (let* ((entry (car (funcall *fixed-primary2* *fixed-package-present* "CAR" p)))
              (symbol (car (funcall *fixed-primary3* *fixed-package-fields* entry 4 nil))))
         (and (eq symbol 'car)
              (equal (funcall *fixed-primary2* *fixed-package-inherited* "MISSING-FIXED" p) '(nil))
              (handler-case (progn (funcall *fixed-primary3* *fixed-package-fields* p -1 nil) nil)
                (package-error () t))
              (handler-case (progn (funcall *fixed-primary3* *fixed-package-fields* p (ash 1 100) nil) nil)
                (package-error () t))
              (handler-case (progn (funcall *fixed-primary3* *fixed-package-fields* p 1 -1) nil)
                (package-error () t))
              (eq (car (funcall *fixed-primary3* *fixed-package-fields* entry 4 nil)) 'car))))`);
  }
} finally { vm.close(); }
console.log('Fixed private primitives: checked calls, values, effects, identity, replacement, arity and recovery pass.');
