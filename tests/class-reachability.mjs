import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const binary = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const builder = await createCompilerTestVM(binary);
const fresh = await createCompilerTestVM(binary);
const check = (vm, form) => assert.equal(vm.evaluate(form, {eof: true}).output, 'T\n', form);
try {
  builder.mountFile('/types.lisp', `
    (defpackage :class-reachability-test (:use :cl))
    (in-package :class-reachability-test)
    (eval-when (:compile-toplevel) (defparameter *events* 0))
    (defparameter *events* 0)
    (defstruct parent (value 17) (seal 5 :read-only t))
    (defstruct discarded (value (error "Unused constructor default ran.")))
    (defclass discarded-class () ((value :initform (error "Unused instance default ran.") :accessor discarded-class-value)))
    (defclass shared-effect () ((value :allocation :class :initform (incf *events*))))
    (defclass live-class () ((value :initarg :value :accessor live-value :reader read-value :writer write-value)))
    (defclass classifier-only () ())
    (defgeneric eql-event (object))
    (defmethod eql-event ((object (eql (progn (incf *events*) :event)))) object)
    (defun parent-printer (object stream) (format stream "parent/~D" (parent-value object)))
    (defun discarded-printer (object stream) (declare (ignore object)) (write-string "discarded" stream))
    (defmethod print-object ((object parent) stream) (parent-printer object stream))
    (defmethod print-object ((object discarded) stream) (discarded-printer object stream))
    (defmethod native-runtime-extra ((object integer)) (+ object 1))
    (defmethod no-arguments () 42)
  `);
  builder.mountFile('/application.lisp', `
    (in-package :class-reachability-test)
    (defstruct (child (:include parent) (:conc-name child-) (:constructor build-child)) flag)
    (defstruct (readonly-child (:include parent (value 17 :read-only t)) (:conc-name locked-)))
    (defstruct (unnamed (:type vector)) item)
    (defstruct (coordinates (:type list) (:conc-name nil) (:constructor point (horizontal vertical))
                            (:predicate nil) (:copier nil)) horizontal vertical)
    (defun classify-only (object)
      (and (typep object 'classifier-only nil) (typecase object (classifier-only t) (t nil))))
    (defun start ()
      (let ((child (build-child)) (object (make-instance 'live-class :value 23)) (point (point 5 7)))
        (setf (child-value child) 19)
        (write-value 29 object)
        (list (child-value child) (parent-value child) (princ-to-string child)
          (live-value object) (read-value object) (horizontal point) (vertical point) *events*
          (classify-only nil))))
    (defun dynamic-type-query (type) (typep nil type))
    (defun escaped-factory () #'make-instance)
    (defun coerced-factory () (coerce 'make-instance 'function))
    (defun unknown-callback (callback) (funcall callback))
    (defun dynamic-instance (type) (make-instance type))
    (defun dynamic-array (type) (make-array 1 :element-type type))
    (defun reader-object () (read-from-string "#S(class-reachability-test::discarded :value 7)"))
  `);
  for (const file of ['types', 'application']) {
    check(builder, `(pathnamep (compile-file "/${file}.lisp"))`);
    check(builder, `(load "/${file}.wasm")`);
  }
  // Application analysis never runs its initializers in the optimizer image.
  check(builder, `(progn (setq class-reachability-test::*events* 0)
    (with-open-file (out "/all.wasm" :direction :output :element-type '(unsigned-byte 8))
      (dolist (file '("/types.wasm" "/application.wasm"))
        (with-open-file (in file :element-type '(unsigned-byte 8))
          (unless (equal file "/types.wasm") (file-position in 8))
          (loop for byte = (read-byte in nil nil) while byte do (write-byte byte out)))))
    (miricl-compiler:shake-file "/all.wasm" "/small.wasm" :entry-points '("CLASS-REACHABILITY-TEST:START")
      :report "/report.txt")
    (zerop class-reachability-test::*events*))`);
  const small = await builder.readFile('/small.wasm');
  fresh.mountFile('/small.wasm', small);
  check(fresh, '(load "/small.wasm")');
  check(fresh, `(and
    (equal (class-reachability-test::start) '(19 19 "parent/19" 29 29 5 7 2 nil))
    (class-reachability-test::classify-only (make-instance 'class-reachability-test::classifier-only))
    (null (find-class 'class-reachability-test::discarded nil))
    (null (find-class 'class-reachability-test::discarded-class nil))
    (not (fboundp 'class-reachability-test::make-discarded))
    (not (fboundp 'class-reachability-test::discarded-value))
    (not (fboundp 'class-reachability-test::discarded-printer))
    (= (class-reachability-test::native-runtime-extra 41) 42)
    (eq (class-reachability-test::eql-event :event) :event)
    (= (class-reachability-test::no-arguments) 42))`);
  const report = new TextDecoder().decode(await builder.readFile('/report.txt'));
  assert.match(report, /DROP :STRUCTURE \("DISCARDED"/);
  assert.match(report, /DROP :CLASS \("DISCARDED-CLASS"/);
  assert.match(report, /KEEP :CLASS \("SHARED-EFFECT"/);
  assert.match(report, /KEEP :STRUCTURE \("CHILD"/);
  assert.match(report, /DROP :FUNCTION \("DISCARDED-PRINTER"/);

  // A generated inherited accessor can itself be the public entry point.
  check(builder, `(pathnamep (miricl-compiler:shake-file "/all.wasm" "/accessor.wasm"
    :entry-points '("CLASS-REACHABILITY-TEST:CHILD-VALUE")))`);
  for (const name of ['(setf class-reachability-test::child-seal)',
    '(setf class-reachability-test::locked-value)', 'class-reachability-test::unnamed-p'])
    check(builder, `(handler-case
      (progn (miricl-compiler:shake-file "/all.wasm" "/invalid-entry.wasm" :entry-points '(${name})) nil)
      (error () t))`);
  check(builder, `(pathnamep (miricl-compiler:shake-file "/all.wasm" "/reflection.wasm"
    :entry-points '("CLASS-REACHABILITY-TEST:DYNAMIC-TYPE-QUERY") :report "/reflection.txt"))`);
  assert.match(new TextDecoder().decode(await builder.readFile('/reflection.txt')), /:REFLECTION \("TYPEP"\)/);
  for (const entry of ['ESCAPED-FACTORY', 'COERCED-FACTORY', 'UNKNOWN-CALLBACK', 'DYNAMIC-INSTANCE', 'DYNAMIC-ARRAY', 'READER-OBJECT']) {
    check(builder, `(pathnamep (miricl-compiler:shake-file "/all.wasm" "/open-types.wasm"
      :entry-points '("CLASS-REACHABILITY-TEST:${entry}") :report "/open-types.txt"))`);
    const report = new TextDecoder().decode(await builder.readFile('/open-types.txt'));
    assert.match(report, /KEEP :CLASS \("DISCARDED-CLASS"/, entry);
    assert.match(report, /KEEP :STRUCTURE \("DISCARDED"/, entry);
    if (entry === 'READER-OBJECT') {
      const reader = await createCompilerTestVM(binary);
      try {
        reader.mountFile('/reader.wasm', await builder.readFile('/open-types.wasm'));
        check(reader, '(load "/reader.wasm")');
        check(reader, '(= (class-reachability-test::discarded-value (class-reachability-test::reader-object)) 7)');
      } finally {reader.close();}
    }
  }
  check(builder, `(and
    (fourth (miricl-compiler::definition-info
      '(defclass custom () () (:metaclass custom-metaclass)) 1))
    (fourth (miricl-compiler::definition-info
      '(defmethod custom ((object (eql (print :effect)))) object) 2)))`);
  console.log(`Generic class reachability: ${(await builder.readFile('/all.wasm')).length} -> ${small.length} bytes; inherited/BOA constructors, typed structures, readers/writers, runtime methods and shared-slot effects pass.`);
} finally {builder.close(); fresh.close();}
