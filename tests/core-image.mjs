import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {runInNewContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';

const runtime = await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm');
const source = await Miricl.instantiate(runtime);
let saved;
try {
  source.mountFile('/persistent.bin', Uint8Array.of(0, 128, 255));
  await source.evaluateAsync(`(defpackage :core-test (:use :cl) (:local-nicknames (:kernel :miricl-core)))
    (in-package :core-test) (values)`, {eof: true});
  await source.evaluateAsync(`(progn
    (defstruct example value)
    (define-condition saved-error (error)
      ((value :initform 42 :reader saved-error-value)))
    (defparameter *example* (make-example :value (ash 1 80)))
    (defparameter *total* 0)
    (defparameter *ring* (list *example*))
    (setf (cdr *ring*) *ring*)
    (defparameter *table* (make-hash-table :test 'eq))
    (setf (gethash *table* *table*) *ring*)
    (defparameter *values* (list -536870912 536870911 -9223372036854775808
                                9223372036854775807 3/5 -0.0d0))
    (defparameter *deep* nil)
    (dotimes (index 100000) (push index *deep*))
    (defparameter *counter* (funcall
      (miricl-compiler:compile-native '(lambda (count) (lambda () (incf count)))) 40))
    (js:define-import restart-event ("fixture" "restart") (:boolean) :void)
    (defun restart ()
      (incf *total*) (restart-event t) (setf *readtable* (copy-readtable nil)))
    (setf *readtable* (copy-readtable nil))
    (set-macro-character #\\( (lambda (stream char) (declare (ignore stream char)) :different))
    (kernel:save-image #P"/test-image.wasm" :toplevel 'restart)
    (values))`, {eof: true});
  saved = await source.readFile('/test-image.wasm');
} finally {source.close();}

const restarts = [];
const options = {imports: {fixture: {restart(value) {restarts.push(value);}}}};
const restored = await Miricl.instantiate(saved, options);
try {
  assert.deepEqual(restarts, [true]);
  assert.deepEqual(await restored.readFile('/persistent.bin'), Uint8Array.of(0, 128, 255));
  const check = await restored.evaluateAsync(`(and
    (eq *package* (find-package :core-test))
    (eq (find-package :kernel) (find-package :miricl-core))
    (= (example-value *example*) (ash 1 80))
    (= (handler-case (error 'saved-error)
         (saved-error (condition) (saved-error-value condition))) 42)
    (eq (car *ring*) *example*) (eq (cdr *ring*) *ring*)
    (eq (gethash *table* *table*) *ring*)
    (equal *values* '(-536870912 536870911 -9223372036854775808 9223372036854775807 3/5 -0.0d0))
    (minusp (float-sign (sixth *values*)))
    (= (length *deep*) 100000) (= *total* 1) (= (funcall *counter*) 41))`, {eof: true});
  assert.equal(check.output, 'T\n');
  const second = await Miricl.instantiate(restored.saveImage(), options);
  try {
    assert.deepEqual(restarts, [true]);
    assert.equal(second.evaluate('(funcall *counter*)').output, '42\n');
  } finally {second.close();}
  const module = runInNewContext('new WebAssembly.Module(saved)', {saved});
  const compiled = await Miricl.instantiate(module, options);
  try {assert.equal(compiled.evaluate('(funcall *counter*)').output, '41\n');}
  finally {compiled.close();}
  assert.deepEqual(restarts, [true, true]);
  assert.throws(() => {
    restored.evaluate('(defparameter *foreign* (js:global "Math"))');
    restored.saveImage();
  }, /host object/);
} finally {restored.close();}

await assert.rejects(Miricl.instantiate(saved.subarray(0, -1)), /Truncated Miricl image/);
console.log('Miricl images restore packages, cyclic state, native closures and the restart function.');
