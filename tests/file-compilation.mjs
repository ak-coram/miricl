import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const binary = fs.readFileSync(process.argv[2] ?? 'miricl.wasm');
const vm = await Miricl.instantiate(binary);
vm.mountFile('portable.lisp', `
  (defpackage :compiled-package (:use :cl))
  (in-package :compiled-package)
  (eval-when (:compile-toplevel) (defmacro increment (x) (list '+ x 1)))
  (defparameter *literal* '#1=("é𝄞" . #1#))
  (defparameter *long-literal* '#.(let ((items (loop for i below 10000 collect i)))
    (setf (cdr (last items)) items) items))
  (defparameter *deep-literal* '#.(loop with x = nil repeat 600 do (setq x (list x)) finally (return x)))
  (defparameter *package-literal* #.*package*)
  (defparameter *encoding-probe* #.(map 'string #'code-char '(0 233 20013 119070)))
  (defstruct persisted-structure (value 42))
  (defun persisted-character () #\\))
  (defun report-persisted-condition (condition stream)
    (write-string (persisted-message condition) stream))
  (define-condition persisted-condition (condition)
    ((message :initarg :message :reader persisted-message))
    (:report report-persisted-condition))
  (define-condition persisted-subcondition (persisted-condition) ())
  (eval-when (:compile-toplevel)
    (assert (subtypep 'persisted-subcondition 'persisted-condition)))
  (defparameter *loads* 0)
  (defun aux-captures (x &aux (before (lambda () x)) (x (1+ x))
                            (between (lambda () x)) (x (1+ x)))
    (list (funcall before) (funcall between) x))
  (eval-when (:compile-toplevel)
    (proclaim '(special compile-only-special)))
  (defun compile-only-reader () compile-only-special)
  (defun compile-only-parameter (compile-only-special)
    "Keep the documentation before the preserved declaration."
    (list (compile-only-reader)
      (let ((compile-only-special (+ compile-only-special 1))) (compile-only-reader))
      (let* ((compile-only-special (+ compile-only-special 2))) (compile-only-reader))))
  (defun persisted (x &key (y 2)) (values (increment x) y *literal*))
  (defun loaded-once () (load-time-value (incf *loads*)))
  (defun loaded-nil () (load-time-value nil))
  (defun bounded-loop (n)
    (let ((sum 0)) (loop for index below n do (incf sum index) finally (return (values index sum)))))
  (defun package-contents (package)
    (with-package-iterator (next package :external)
      (loop for more = (nth-value 0 (next)) while more count more)))
  ;; UIOP uses (CHECK-TYPE SYMBOL SYMBOL). Alpha conversion must rename
  ;; the place while retaining atomic and compound type syntax literally.
  (defun checked-symbol (symbol)
    (values (check-type symbol symbol) symbol))
  (defun checked-member (member symbol)
    (declare (ignore symbol))
    (check-type member (member symbol))
    member)
  (defun repaired-symbol (symbol)
    (handler-bind ((type-error (lambda (condition)
                    (unless (and (eq (type-error-datum condition) (car symbol))
                                 (eq (type-error-expected-type condition) 'symbol))
                      (error "Incorrect CHECK-TYPE condition."))
                    (store-value :repaired condition))))
      (list (check-type (car symbol) symbol) symbol)))
  (macrolet ((constant-macro () '42)) (defparameter *constant-expansion* (constant-macro)))
  (defun checked-pathname (value &key required)
    (macrolet ((check (condition) \`(unless ,condition (error "Expected a pathname."))))
      (etypecase value
        (pathname (check (pathnamep value)))
        (null (check (not required))))
      value))
  (defun collected-values ()
    (macrolet ((bump (x) \`(1+ ,x)))
      (values (loop for x in '(1 2 3) collect (bump x))
              (loop for x below (bump 2) sum (bump x))
              (let ((x (list 1 2)))
                (psetf (car x) (bump (cadr x)) (cadr x) (bump (car x))) x))))
`);
const check = (machine, source) => assert.equal(machine.evaluate(source, {eof: true}).output, 'T\n', source);
check(vm, `(let ((result (multiple-value-list (compile-file "portable.lisp" :verbose nil :print nil))))
  (and (= (length result) 3) (pathnamep (first result)) (null (second result)) (null (third result))))`);
check(vm, `(not (fboundp 'compiled-package::persisted))`);
check(vm, `(not (fboundp 'compiled-package::report-persisted-condition))`);
const exported = vm.evaluate(`(with-open-file (stream "portable.wasm" :element-type '(unsigned-byte 8))
  (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream) bytes))`, {eof: true}).output;
assert.ok(exported.startsWith('#('));
const bytes = Uint8Array.from(exported.match(/\d+/g).map(Number));
assert.equal(WebAssembly.validate(bytes), true);
const module = await WebAssembly.compile(bytes);
assert.equal(WebAssembly.Module.customSections(module, 'miricl.fasl').length, 1);
assert.deepEqual(WebAssembly.Module.imports(module), []);
const fresh = await Miricl.instantiate(binary);
fresh.mountFile('portable.wasm', bytes);
check(fresh, `(let ((*read-eval* nil) (*read-base* 16) (*readtable* (copy-readtable nil)))
  (set-macro-character (code-char 40) (lambda (stream character) (declare (ignore stream character)) (error "User reader called.")))
  (load "portable.wasm" :verbose nil :print nil))`);
check(fresh, `(equal (compiled-package::aux-captures 40) '(40 41 42))`);
check(fresh, `(and (eq (compiled-package::persisted-character) (code-char 41))
  (eq (compiled-package::persisted-character) (peek-char t (make-string-input-stream " )"))))`);
check(fresh, `(string= (format nil "~A"
  (make-condition 'compiled-package::persisted-subcondition :message "loaded report")) "loaded report")`);
check(fresh, `(and (compiled-function-p #'compiled-package::persisted)
  (not (fboundp 'compiled-package::increment))
  (multiple-value-bind (x y object) (compiled-package::persisted 3 :y 5)
    (and (= x 4) (= y 5) (eq object (cdr object)) (equal (car object) "é𝄞")))
  (= (compiled-package::loaded-once) 1) (= (compiled-package::loaded-once) 1)
  (= compiled-package::*loads* 1) (null (compiled-package::loaded-nil))
  (eq compiled-package::*package-literal* (find-package :compiled-package))
  (equal (map 'list #'char-code compiled-package::*encoding-probe*) '(0 233 20013 119070))
  (= (car (nthcdr 9999 compiled-package::*long-literal*)) 9999)
  (eq compiled-package::*long-literal* (nthcdr 10000 compiled-package::*long-literal*))
  (= (compiled-package::persisted-structure-value (compiled-package::make-persisted-structure)) 42)
  (null (loop with x = compiled-package::*deep-literal* repeat 600 do (setq x (car x)) finally (return x)))
  (= compiled-package::*constant-expansion* 42)
  (equal (multiple-value-list (compiled-package::collected-values)) '((2 3 4) 6 (3 2)))
  (equal (compiled-package::checked-pathname #p"file") #p"file")
  (null (compiled-package::checked-pathname nil))
  (handler-case (progn (compiled-package::checked-pathname nil :required t) nil)
    (error () t))
  (= (compiled-package::package-contents :keyword)
     (let ((count 0)) (do-external-symbols (symbol :keyword count) (incf count))))
  (compiled-function-p #'compiled-package::bounded-loop)
  (equal (multiple-value-list (compiled-package::bounded-loop 5)) '(4 10)))`);
check(fresh, `(and
  (equal (compiled-package::compile-only-parameter 40) '(40 41 42))
  (compiled-function-p #'compiled-package::checked-symbol)
  (equal (multiple-value-list (compiled-package::checked-symbol 'ok)) '(nil ok))
  (eq (compiled-package::checked-member 'symbol :ignored) 'symbol)
  (handler-case (progn (compiled-package::checked-symbol 42) nil)
    (type-error (condition)
      (and (= (type-error-datum condition) 42)
           (eq (type-error-expected-type condition) 'symbol))))
  (handler-case (progn (compiled-package::checked-member :bad :ignored) nil)
    (type-error (condition)
      (equal (type-error-expected-type condition) '(member symbol))))
  (equal (compiled-package::repaired-symbol (list 42)) '(nil (:repaired))))`);
vm.evaluate('(defvar *file-events* nil)', {eof: true});
vm.mountFile('phases.lisp', `
  (macrolet ((phase (situation &body body) (list* 'eval-when (list situation) body)))
    (phase :compile-toplevel (push :compile *file-events*))
    (phase :load-toplevel (push :load *file-events*)))
  (locally (declare (special *file-events*))
    (eval-when (:compile-toplevel) (push :declared *file-events*)))
`);
check(vm, `(progn (compile-file "phases.lisp" :verbose nil :print nil)
  (equal *file-events* '(:declared :compile)))`);
check(vm, `(progn (setq *file-events* nil) (load "phases.wasm" :verbose nil :print nil)
  (equal *file-events* '(:load)))`);
vm.evaluate('(make-package "LITERAL-PACKAGE" :use nil)');
vm.mountFile('package-literal.lisp', '(defparameter *foreign-package* #.(find-package "LITERAL-PACKAGE"))');
vm.evaluate('(compile-file "package-literal.lisp" :verbose nil :print nil)');
const packageBytes = vm.evaluate(`(with-open-file (s "package-literal.wasm" :element-type '(unsigned-byte 8))
  (let ((bytes (make-array (file-length s) :element-type '(unsigned-byte 8))))
    (read-sequence bytes s) bytes))`).output;
fresh.mountFile('package-literal.wasm', Uint8Array.from(packageBytes.match(/\d+/g).map(Number)));
check(fresh, `(handler-case (load "package-literal.wasm")
  (package-error (condition) (equal (package-error-package condition) "LITERAL-PACKAGE")))`);
check(fresh, `(progn (make-package "LITERAL-PACKAGE" :use nil)
  (load "package-literal.wasm") (eq *foreign-package* (find-package "LITERAL-PACKAGE")))`);
console.log('File compilation: portable WASM artifacts load in a fresh VM, preserve cyclic Unicode literals, freeze macros, and evaluate load-time values once.');
