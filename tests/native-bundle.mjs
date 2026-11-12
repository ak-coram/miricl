import assert from 'node:assert/strict';
import fs from 'node:fs';
import {Miricl} from '../scripts/miricl.mjs';

const image = fs.readFileSync(process.argv[2] ?? 'miricl.wasm');
const builder = await Miricl.instantiate(image);
const check = (vm, source) => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
let fresh;
try {
  builder.mountFile('/native-source.lisp', `
    (defpackage :native-bundle-test (:use :cl))
    (in-package :native-bundle-test)
    (defmacro deeply-expanded (x)
      (dotimes (i 2000 x) (setq x (list 'identity x))))
    (defun deep-from-file (x) (deeply-expanded x))
    (defmacro deeply-bound (x)
      (dotimes (i 1000 x)
        (setq x (list 'let* '((a 17) (b a))
                  (list 'locally '(declare (special *deep-special*))
                    (list 'if 'b x :absent))))))
    (defun bindings-from-file (x) (deeply-bound x))
    (defparameter *initializations* 0)
    (defparameter *cycle* (progn (incf *initializations*) '#1=(:cycle . #1#)))
    (defun values-and-capture (x)
      (let ((next (lambda () (incf x))))
        (values (funcall next) (funcall next))))
    ;; Application dependencies can request DEBUG 3. Native bundle emission
    ;; must continue to accept that policy during the HIR debugger migration.
    (defun debug-values (x)
      (declare (optimize (debug 3)))
      (let ((next (lambda () (incf x))))
        (values (funcall next) (funcall next))))
    (defun constant-factory-a () (lambda () 17))
    (defun constant-factory-b () (lambda () 29))
    (defun nested-static (x) (lambda (y) (+ x y (funcall (lambda () 17)))))
    (defun capture-offset-a (x) (lambda (y) (+ x y 17)))
    (defun capture-offset-b (x) (lambda (y) (+ x y 29)))
    (defun first-constant () 17)
    (defun second-constant () 29)
    ${Array.from({length: 140}, (_, i) => `(defun folded-${i} (x) (lambda (y) (+ x y ${i})))`).join('\n')}
    ;; One literal context produces distinct bodies on both sides of the
    ;; one-byte function-index boundary. Reordering must relocate every closure.
    (defparameter *indexed-functions*
      (vector ${Array.from({length: 140}, (_, i) => `(lambda (x) (+ x ${i}))`).join(' ')}))
    (defun escape (x)
      (block done (mapcar (lambda (n) (when (= n x) (return-from done n))) '(1 2 3)) nil))
    (defclass point () ((x :initarg :x :reader point-x)))
    (defgeneric twice (point))
    (defmethod twice ((point point)) (* 2 (point-x point)))
    (incf *initializations*)`);
  builder.evaluate('(compile-file "/native-source.lisp") (load "/native-source.wasm")', {eof: true});
  check(builder, '(= native-bundle-test::*initializations* 2)');
  builder.evaluate('(miricl-compiler:compile-native-bundle "/native-source.wasm" "/native.wasm")', {eof: true});
  check(builder, '(= native-bundle-test::*initializations* 2)');
  const bytes = await builder.readFile('/native.wasm');
  assert.ok(WebAssembly.validate(bytes));
  fresh = await Miricl.instantiate(image);
  fresh.mountFile('/native.wasm', bytes);
  fresh.evaluate('(load "/native.wasm")', {eof: true});
  check(fresh, `(and (eq miricl-compiler::*compiler-state* :cold)
    (= native-bundle-test::*initializations* 2)
    (= (native-bundle-test::deep-from-file 42) 42)
    (= (native-bundle-test::bindings-from-file 42) 42)
    (null (function-lambda-expression #'native-bundle-test::values-and-capture))
    (eq (nth-value 2 (function-lambda-expression #'native-bundle-test::values-and-capture))
        'native-bundle-test::values-and-capture)
    (progn
      (setf (symbol-function 'native-bundle-test::alias) #'native-bundle-test::first-constant)
      (eq (nth-value 2 (function-lambda-expression #'native-bundle-test::alias))
          'native-bundle-test::first-constant))
    (plusp (miricl-compiler::compiler-primitive 26 #'native-bundle-test::values-and-capture))
    (equal (multiple-value-list (native-bundle-test::values-and-capture 40)) '(41 42))
    (equal (multiple-value-list (native-bundle-test::debug-values 40)) '(41 42))
    (= (funcall (native-bundle-test::constant-factory-a)) 17)
    (= (funcall (native-bundle-test::constant-factory-b)) 29)
    (let ((f (native-bundle-test::nested-static 20))) (and (= (funcall f 2) 39) (= (funcall f 3) 40)))
    (= (funcall (native-bundle-test::capture-offset-a 1) 2) 20)
    (= (funcall (native-bundle-test::capture-offset-b 10) 20) 59)
    (not (eq #'native-bundle-test::first-constant #'native-bundle-test::second-constant))
    (= (native-bundle-test::first-constant) 17)
    (= (native-bundle-test::second-constant) 29)
    (loop for i below 140 always
      (= (funcall (funcall (fdefinition (find-symbol (format nil "FOLDED-~D" i) :native-bundle-test)) i) 3)
         (+ (* 2 i) 3)))
    (loop for i below 140 always
      (= (funcall (aref native-bundle-test::*indexed-functions* i) 11) (+ i 11)))
    (= (native-bundle-test::escape 2) 2)
    (null (native-bundle-test::escape 4))
    (= (native-bundle-test::twice (make-instance 'native-bundle-test::point :x 21)) 42)
    (eq native-bundle-test::*cycle* (cdr native-bundle-test::*cycle*)))`);
  builder.mountFile('/ordered.lisp', `
    (defpackage :native-order-test (:use :cl))
    (in-package :native-order-test)
    (defvar *initializations* 0)
    (defun count-up (n acc)
      (if (zerop n) acc (count-up (1- n) (1+ acc))))
    (defparameter *count* (load-time-value (count-up 100000 0)))
    (defclass token () ((value :initarg :value :reader token-value)))
    (defparameter *token* (make-instance 'token :value *count*))
    (incf *initializations*)
    (defun state () (values (token-value *token*) *initializations*))`);
  builder.evaluate('(compile-file "/ordered.lisp") (values)', {eof: true});
  const ordered = await Miricl.instantiate(image);
  try {
    ordered.mountFile('/ordered.wasm', await builder.readFile('/ordered.wasm'));
    // No application packages or definitions exist before this call. Decoding
    // the LTV needs the preceding function already installed as native code.
    ordered.evaluate(`(miricl-compiler:compile-native-bundle "/ordered.wasm" "/ordered-native.wasm" :load t)
      (values)`, {eof: true});
    check(ordered, `(equal (multiple-value-list (native-order-test::state)) '(100000 1))`);
    fresh.mountFile('/ordered-native.wasm', await ordered.readFile('/ordered-native.wasm'));
    fresh.evaluate('(load "/ordered-native.wasm") (values)', {eof: true});
    check(fresh, `(equal (multiple-value-list (native-order-test::state)) '(100000 1))`);
  } finally {ordered.close();}
  console.log('Native bundle: fresh load without compiler startup, initialization order, native closures, multiple values, nonlocal exits, methods and cyclic constants pass.');
} finally {builder.close(); fresh?.close();}
