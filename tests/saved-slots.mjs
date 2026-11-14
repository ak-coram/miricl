import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const definitions = `(defpackage :saved-slot-test (:use :cl)) (in-package :saved-slot-test)
  (defvar *initforms* 0) (defvar *creations* 0) (defvar *restorations* 0)
  (defclass plain () ((value :initarg :value) (next) (unbound)
    (default :initform (incf *initforms*)) (shared :allocation :class :initform 99)))
  (defmethod make-load-form ((object plain) &optional environment)
    (make-load-form-saving-slots object :environment environment))
  (defclass explicit-shared (plain) ((shared :allocation :class :initform 88)))
  (defmethod make-load-form ((object explicit-shared) &optional environment)
    (make-load-form-saving-slots object :slot-names '(shared value unbound) :environment environment))
  (defclass wrapped (plain) ())
  (defmethod make-load-form ((object wrapped) &optional environment)
    (multiple-value-bind (creation initialization) (make-load-form-saving-slots object :environment environment)
      (values (list 'progn '(incf *creations*) creation)
        (list 'progn '(incf *restorations*) initialization))))
  (defclass empty () ())
  (defmethod make-load-form ((object empty) &optional environment)
    (make-load-form-saving-slots object :environment environment))
  (defclass wide () (${Array.from({length: 40}, (_, i) => `(slot-${i})`).join(' ')}))
  (defmethod make-load-form ((object wide) &optional environment)
    (make-load-form-saving-slots object :environment environment))
  (defstruct structure value next)
  (defmethod make-load-form ((object structure) &optional environment)
    (make-load-form-saving-slots object :environment environment))
  (values)`;
const source = `(in-package :saved-slot-test)
  (defparameter *graph* (quote #.(let* ((a (make-instance 'plain :value nil))
      (b (make-instance 'plain :value (vector a)))
      (array (vector a b a)) (wrapped (make-instance 'wrapped :value 42))
      (shared (make-instance 'explicit-shared :value 17))
      (structure (make-structure :value array)) (wide (allocate-instance (find-class 'wide))))
    (setf (slot-value a 'next) b (slot-value b 'next) a
      (slot-value a 'value) array (slot-value a 'shared) 7
      (slot-value wrapped 'next) wrapped (slot-value shared 'shared) 42
      (structure-next structure) structure (slot-value wide 'slot-39) nil)
    (vector array wrapped shared (make-instance 'empty) structure wide))))`;
const valid = `(and (= *initforms* 0) (= *creations* 1) (= *restorations* 1)
  (let* ((array (aref *graph* 0)) (a (aref array 0)) (b (aref array 1))
         (wrapped (aref *graph* 1)) (shared (aref *graph* 2)) (structure (aref *graph* 4)))
    (and (eq a (aref array 2)) (eq (slot-value a 'value) array)
      (eq (aref (slot-value b 'value) 0) a)
      (eq (slot-value a 'next) b) (eq (slot-value b 'next) a)
      (not (slot-boundp a 'unbound)) (= (slot-value a 'shared) 99)
      (eq (slot-value wrapped 'next) wrapped) (= (slot-value wrapped 'value) 42)
      (= (slot-value shared 'shared) 42) (= (slot-value shared 'value) 17)
      (not (slot-boundp shared 'unbound)) (not (slot-boundp shared 'default))
      (typep (aref *graph* 3) 'empty)
      (slot-boundp (aref *graph* 5) 'slot-39) (null (slot-value (aref *graph* 5) 'slot-39))
      (not (slot-boundp (aref *graph* 5) 'slot-0))
      (eq (structure-value structure) array) (eq (structure-next structure) structure))))`;
const builder = await createCompilerTestVM(image);
try {
  await builder.evaluateAsync(definitions, {eof: true});
  // Calling the standard helper directly must also return executable forms,
  // without relying on COMPILE-FILE to recognize the dense representation.
  assert.equal((await builder.evaluateAsync(`(let ((original (make-instance 'plain :value nil)))
    (multiple-value-bind (creation initialization)
        (make-load-form-saving-slots original :slot-names '(value unbound))
      (let ((copy (eval creation)))
        (eval (sublis (list (cons original copy)) initialization :test #'eq))
        (and (not (eq copy original)) (slot-boundp copy 'value) (null (slot-value copy 'value))
          (not (slot-boundp copy 'unbound)) (not (slot-boundp copy 'default))))))`, {eof: true})).output, 'T\n');
  builder.mountFile('/graph.lisp', source);
  await builder.evaluateAsync(`(compile-file "/graph.lisp" :verbose nil)
    (miricl-compiler:shake-file "/graph.wasm" "/compact.wasm" :strip nil)
    (load "/compact.wasm")
    (miricl-compiler:compile-native-bundle "/compact.wasm" "/native.wasm") (values)`, {eof: true});
  const records = (await builder.evaluateAsync(`(let* ((unit (first (miricl-compiler::read-units "/graph.wasm")))
      (records (miricl-compiler::ir-unit-records unit)))
    (and (find 11 records :key #'first) (find 9 records :key #'first) t))`, {eof: true})).output;
  assert.equal(records, 'T\n', 'Dense instances and custom MLF forms must coexist');
  for (const file of ['graph', 'compact', 'native']) {
    const bytes = await builder.readFile(`/${file}.wasm`);
    const module = new WebAssembly.Module(bytes);
    const units = WebAssembly.Module.customSections(module, 'miricl.fasl');
    assert.ok(units.some(unit => new DataView(unit).getUint32(4, true) & 2), 'Dense record feature bit');
    const fresh = await createCompilerTestVM(image);
    try {
      await fresh.evaluateAsync(definitions, {eof: true}); fresh.mountFile('/graph.wasm', bytes);
      await fresh.evaluateAsync('(load "/graph.wasm") (values)', {eof: true});
      assert.equal((await fresh.evaluateAsync(valid, {eof: true})).output, 'T\n', file);
    } finally {fresh.close();}
  }
  console.log('Saved slots: executable helper forms, dense/custom coexistence, cycles, aliases, structures, selected/class/unbound slots and no initforms pass.');
} finally {builder.close();}
