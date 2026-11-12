import fs from 'node:fs';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';
const vm = await createCompilerTestVM(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
check(`(progn
  (defmacro compile-regression (x) (list '+ x 1))
  (defparameter *compiled-regression* (compile nil '(lambda (x) (compile-regression x))))
  (defmacro compile-regression (x) (list '+ x 10))
  (and (compiled-function-p *compiled-regression*) (= (funcall *compiled-regression* 2) 3)
    (= (compile-regression 2) 12)))`);
check(`(let ((calls 0)) (declare (special calls))
  (let ((function (compile nil '(lambda () (load-time-value (incf calls))))))
    (and (compiled-function-p function) (= calls 1) (= (funcall function) 1)
      (= (funcall function) 1) (= calls 1))))`);
check(`(let* ((literal (list :literal)) (other (list :literal))
             (function (compile nil (list 'lambda nil (list 'values (list 'quote literal) (list 'quote other))))))
  (multiple-value-bind (a b) (funcall function) (and (eq a literal) (eq b other) (not (eq a b)))))`);
check(`(let* ((x 10) (original (lambda (n) (incf x n))) (compiled (compile nil original)))
  (and (compiled-function-p compiled) (= (funcall compiled 3) 13) (= (funcall original 2) 15) (= x 15)))`);
check(`(let ((function (compile nil '(lambda (x &optional (y (+ x 2)))
  (let ((nested (lambda () y))) (values (compiled-function-p nested) (funcall nested)))))))
  (equal (multiple-value-list (funcall function 3)) '(t 5)))`);
check(`(let ((function (compile nil '(lambda ()
  (macrolet ((local (x) (list '+ x 7))) (let ((x 1)) (local x)))))))
  (and (compiled-function-p function) (= (funcall function) 8)))`);
check(`(let ((function (compile nil '(lambda ()
  (symbol-macrolet ((x (car y))) (let ((y (list 1))) (setq x 3) y))))))
  (and (compiled-function-p function) (equal (funcall function) '(3))))`);
check(`(let ((function (compile nil '(lambda (x)
  (flet ((compile-regression (x) (+ x 2))) (compile-regression x))))))
  (and (compiled-function-p function) (= (funcall function 3) 5)))`);
check(`(let ((function (compile nil '(lambda (x)
  (labels ((factorial (x) (if (= x 0) 1 (* x (factorial (1- x)))))) (factorial x))))))
  (and (compiled-function-p function) (= (funcall function 5) 120)))`);
check(`(let ((function (compile nil '(lambda (x y) (values \`(a ,x ,@y . end) \`#(,x ,@y))))))
  (equalp (multiple-value-list (funcall function 2 '(3 4))) '((a 2 3 4 . end) #(2 3 4))))`);
check(`(let ((function (compile nil '(lambda () (values)))))
  (and (compiled-function-p function) (null (multiple-value-list (funcall function)))))`);
check(`(let ((function (compile nil '(lambda ()
  (flet ((local () 17)) (values (compiled-function-p #'local) (local)))))))
  (equal (multiple-value-list (funcall function)) '(t 17)))`);
check(`(let ((function (compile nil '(lambda ()
  (handler-case (error "Expected") (error (condition) (declare (ignore condition)) :caught))))))
  (and (compiled-function-p function) (eq (funcall function) :caught)))`);
check(`(let ((function (compile nil '(lambda ()
  (handler-bind ((error (lambda (condition) (declare (ignore condition)) (invoke-restart 'use-value 17))))
    (restart-case (error "Expected") (use-value (value) :report "Use a value." value)))))))
  (and (compiled-function-p function) (= (funcall function) 17)))`);
check(`(let ((function (compile nil '(lambda ()
  (macrolet ((answer () 42))
    (define-condition compiled-lexical-condition (error)
      ((value :initform (answer) :reader compiled-lexical-condition-value)
       (default :initarg :default))
      (:default-initargs :default (answer))
      (:report (lambda (condition stream) (declare (ignore condition)) (princ (answer) stream))))
    (let ((condition (make-condition 'compiled-lexical-condition)))
      (list (compiled-lexical-condition-value condition)
            (slot-value condition 'default) (format nil "~A" condition))))))))
  (equal (funcall function) '(42 42 "42")))`);
console.log('Compilation: frozen macro expansions, single load-time evaluation, literals, closures, lexical scopes, defaults, recursion, and multiple values pass.');
