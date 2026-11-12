import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';
const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const builder = await createCompilerTestVM(image);
let fresh;
try {
  assert.equal((await builder.evaluateAsync(`
    (defparameter *ltv-compile-count* 0)
    (set 'ltv-lexical 42)
    (defmacro native-ltv-macro () '(load-time-value (vector (incf *ltv-compile-count*))))
    (let* ((f (miricl-compiler:compile-native '(lambda () (native-ltv-macro))))
           (a (funcall f)) (b (funcall f))
           (global (miricl-compiler:compile-native '(lambda (ltv-lexical) (load-time-value ltv-lexical))))
           (primary (miricl-compiler:compile-native '(lambda () (load-time-value (values (vector 42) :ignored)))))
           (distinct (miricl-compiler:compile-native
             '(lambda () (vector (load-time-value (vector 1) nil) (load-time-value (vector 1) nil))))))
      (and (= *ltv-compile-count* 1) (eq a b) (= (aref a 0) 1)
           (progn (setf (aref a 0) 7) (= (aref (funcall f) 0) 7))
           (= (funcall global 0) 42)
           (= (length (multiple-value-list (funcall primary))) 1)
           (let ((result (funcall distinct))) (not (eq (aref result 0) (aref result 1))))))`, {eof: true})).output,
    '*LTV-COMPILE-COUNT*\n42\nNATIVE-LTV-MACRO\nT\n');
  builder.mountFile('/recipes.lisp', `
    (defpackage :native-ltv-test (:use :cl))
    (in-package :native-ltv-test)
    ;; No MAKE-LOAD-FORM method exists for this instance or its function slot.
    (defstruct action callback)
    (defparameter *creations* 0)
    (defparameter *ordinary* '(one two))
    (defparameter *alias* (load-time-value *ordinary*))
    (defparameter *actions* (load-time-value
      (let* ((offset 7) (a (make-action :callback (lambda (x) (+ x offset)))))
        (incf *creations*) (values (vector a a) :ignored))))
    (defparameter *callback* (load-time-value
      (let ((offset 9)) (lambda (x) (+ x offset)))))
    (defparameter *table* (load-time-value
      (let ((table (make-hash-table :test 'eq)))
        (incf *creations*)
        (setf (gethash :self table) table
              (gethash :callback table) (lambda (x) (+ x 11))) table)))
    (defparameter *nested* (load-time-value
      (let ((inner (load-time-value (lambda (x) (+ x 13)))))
        (vector (lambda (x) (funcall inner x)) inner))))`);
  await builder.evaluateAsync('(compile-file "/recipes.lisp") (values)', {eof: true});
  const portable = await builder.readFile('/recipes.wasm');
  // The builder has never loaded the application's package or structures.
  const nativeBuilder = await createCompilerTestVM(image);
  let native;
  try {
    nativeBuilder.mountFile('/recipes.wasm', portable);
    await nativeBuilder.evaluateAsync(`
      (miricl-compiler:compile-native-bundle "/recipes.wasm" "/native.wasm" :load t)
      (values)`, {eof: true});
    native = await nativeBuilder.readFile('/native.wasm');
  } finally {nativeBuilder.close();}
  fresh = await createCompilerTestVM(image);
  const initialState = (await fresh.evaluateAsync('miricl-compiler::*compiler-state*', {eof: true})).output.trim();
  fresh.mountFile('/native.wasm', native);
  await fresh.evaluateAsync('(load "/native.wasm") (values)', {eof: true});
  assert.equal((await fresh.evaluateAsync(`
    (and (eq miricl-compiler::*compiler-state* ${initialState})
      (= native-ltv-test::*creations* 2)
      (eq native-ltv-test::*ordinary* native-ltv-test::*alias*)
      (eq (aref native-ltv-test::*actions* 0) (aref native-ltv-test::*actions* 1))
      (let ((callback (native-ltv-test::action-callback (aref native-ltv-test::*actions* 0))))
        (and (= (funcall callback 35) 42) (plusp (miricl-compiler::compiler-primitive 26 callback))))
      (= (funcall native-ltv-test::*callback* 33) 42)
      (plusp (miricl-compiler::compiler-primitive 26 native-ltv-test::*callback*))
      (eq (gethash :self native-ltv-test::*table*) native-ltv-test::*table*)
      (= (funcall (gethash :callback native-ltv-test::*table*) 31) 42)
      (plusp (miricl-compiler::compiler-primitive 26 (gethash :callback native-ltv-test::*table*)))
      (= (funcall (aref native-ltv-test::*nested* 0) 29) 42)
      (every (lambda (fn) (plusp (miricl-compiler::compiler-primitive 26 fn))) native-ltv-test::*nested*))`, {eof: true})).output, 'T\n');
  console.log('Native LTV recipes: opaque structures, native captured functions, shared vectors, cyclic hash tables, ordinary literal aliases, initialization order and compiler-free fresh loads pass.');
} finally {builder.close(); fresh?.close();}
