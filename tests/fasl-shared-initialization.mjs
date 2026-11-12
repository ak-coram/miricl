import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';
const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const definitions = `
  (defpackage :fasl-shared-init (:use :cl))
  (in-package :fasl-shared-init)
  (defvar *creations* 0)
  (defvar *boolean-cell* (list :uninitialized))
  (defclass token () ((value :initarg :value) (unbound)))
  (defmethod make-load-form ((object token) &optional env)
    (multiple-value-bind (creation initialization)
        (make-load-form-saving-slots object :environment env)
      (values (list 'progn '(incf *creations*) creation) initialization)))`;
const vm = await createCompilerTestVM(image);
let fresh;
try {
  await vm.evaluateAsync(definitions + ' (values)', {eof: true});
  vm.mountFile('/graph.lisp', `(in-package :fasl-shared-init)
    (define-symbol-macro boolean-cell (the boolean (car (load-time-value *boolean-cell*))))
    (setq boolean-cell nil)
    (setf boolean-cell t)
    (defun read-boolean-cell () boolean-cell)
    (defparameter *one* (quote #.(make-instance 'token :value 42)))
    (defparameter *shared* (vector (quote #1=#.(make-instance 'token :value 17)) (quote #1#)))
    (defparameter *cycle* (quote #.(let* ((array (make-array 1)) (object (make-instance 'token :value array)))
      (setf (aref array 0) object) array)))`);
  await vm.evaluateAsync(`(compile-file "/graph.lisp")
    (progn (miricl-compiler:shake-file "/graph.wasm" "/compacted.wasm" :strip nil) (values))`, {eof: true});
  assert.equal(vm.evaluate('(eq (car *boolean-cell*) :uninitialized)').output, 'T\n');
  const compacted = await vm.readFile('/compacted.wasm');
  fresh = await createCompilerTestVM(image);
  await fresh.evaluateAsync(definitions + ' (values)', {eof: true});
  fresh.mountFile('/compacted.wasm', compacted);
  for (let count = 1; count <= 2; count++) {
    await fresh.evaluateAsync('(load "/compacted.wasm") (values)', {eof: true});
    assert.equal((await fresh.evaluateAsync(`
      (and (= *creations* ${count * 3}) (= (slot-value *one* 'value) 42)
        (eq (car *boolean-cell*) t) (eq (read-boolean-cell) t)
        (progn (setf boolean-cell nil) (null (car *boolean-cell*)))
        (null (read-boolean-cell))
        (not (slot-boundp *one* 'unbound))
        (eq (aref *shared* 0) (aref *shared* 1))
        (= (slot-value (aref *shared* 0) 'value) 17)
        (eq (slot-value (aref *cycle* 0) 'value) *cycle*))`, {eof: true})).output, 'T\n');
  }
  console.log('Compacted FASL initialization: shared root/initializer code, one creation per object, unbound slots, aliases, cyclic arrays and repeated fresh loads pass.');
} finally {vm.close(); fresh?.close();}
