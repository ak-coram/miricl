import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
  check(`(progn
    (defclass clos-initarg-base () ((a :initarg :a :initarg :alias) (shared :allocation :class :initform 9)))
    (defclass clos-initarg-child (clos-initarg-base) ())
    (defmethod initialize-instance :after ((object clos-initarg-base) &key accepted)
      (when accepted (setf (slot-value object 'a) accepted)))
    (defmethod reinitialize-instance :after ((object clos-initarg-child) &key changed)
      (when changed (setf (slot-value object 'a) changed)))
    (let ((object (make-instance 'clos-initarg-child :accepted 7)))
      (and (= (slot-value object 'a) 7)
           (handler-case (progn (make-instance 'clos-initarg-child :changed 8) nil) (program-error () t))
           (eq object (reinitialize-instance object :changed 8)) (= (slot-value object 'a) 8)
           (handler-case (progn (reinitialize-instance object :accepted 9) nil) (program-error () t))
           (eq object (reinitialize-instance object :alias nil :a 4)) (null (slot-value object 'a)))))`);
  check(`(let ((object (make-instance 'clos-initarg-child :unknown 3 :allow-other-keys t)))
    (and (not (slot-boundp object 'a))
         (handler-case (progn (make-instance 'clos-initarg-child :unknown 3 :allow-other-keys nil :allow-other-keys t) nil)
           (program-error () t))
         (eq object (reinitialize-instance object :unknown 3 :allow-other-keys t :allow-other-keys nil))))`);
  check(`(progn
    (defclass clos-initarg-open () ())
    (defmethod initialize-instance :after ((object clos-initarg-open) &key &allow-other-keys) nil)
    (and (typep (make-instance 'clos-initarg-open :anything t) 'clos-initarg-open)
         (handler-case (progn (reinitialize-instance (make-instance 'clos-initarg-open) :anything t) nil)
           (program-error () t))))`);
  check(`(progn
    (defclass clos-initarg-old () ((kept :initarg :kept)))
    (defclass clos-initarg-new () ((kept) (new :initarg :new)))
    (defmethod update-instance-for-different-class :after
      ((previous clos-initarg-old) (current clos-initarg-new) &key update)
      (when update (setf (slot-value current 'new) update)))
    (let* ((object (make-instance 'clos-initarg-old :kept 7))
           (result (multiple-value-list (change-class object 'clos-initarg-new :update 8))))
      (and (equal result (list object)) (= (slot-value object 'kept) 7)
           (= (slot-value object 'new) 8))))`);
  check(`(progn
    (defclass clos-initarg-redefinition () ())
    (defmethod initialize-instance :after ((object clos-initarg-redefinition) &key first) nil)
    (make-instance 'clos-initarg-redefinition :first t)
    (defmethod initialize-instance :after ((object clos-initarg-redefinition) &key second) nil)
    (and (typep (make-instance 'clos-initarg-redefinition :second t) 'clos-initarg-redefinition)
         (handler-case (progn (make-instance 'clos-initarg-redefinition :first t) nil) (program-error () t))))`);
  // The MOP reader is always called; same-identity mutable results are observed.
  check(`(progn
    (defclass clos-initarg-mutable () ())
    (defmethod initialize-instance :after ((object clos-initarg-mutable) &key &allow-other-keys) nil)
    (let* ((method (find-method #'initialize-instance '(:after) (list (find-class 'clos-initarg-mutable))))
           (saved (fdefinition 'mop:method-lambda-list)) (profile (list 'object '&key 'old)))
      (and (unwind-protect
             (progn
               (setf (fdefinition 'mop:method-lambda-list)
                 (lambda (candidate) (if (eq candidate method) profile (funcall saved candidate))))
               (make-instance 'clos-initarg-mutable :old 1)
               (setf (third profile) 'new)
               (and (typep (make-instance 'clos-initarg-mutable :new 2) 'clos-initarg-mutable)
                    (handler-case (progn (make-instance 'clos-initarg-mutable :old 1) nil) (program-error () t))
                    (progn (setf (cdddr profile) profile)
                      (handler-case (progn (make-instance 'clos-initarg-mutable :new 2) nil) (type-error () t)))))
             (setf (fdefinition 'mop:method-lambda-list) saved))
           (typep (make-instance 'clos-initarg-mutable :recovered 3) 'clos-initarg-mutable))))`);
  check(`(let* ((class (find-class 'clos-initarg-child))
                (left (allocate-instance class)) (right (allocate-instance class)))
    (and (not (eq left right)) (eq (class-of left) class) (not (slot-boundp left 'a))
         (= (length (multiple-value-list (allocate-instance class))) 1)
         (progn (setf (slot-value left 'shared) 17) (= (slot-value right 'shared) 17))))`);
  check(`(let ((object (make-instance 'clos-initarg-child :a 1)))
    (and (handler-case (progn (reinitialize-instance object :a) nil) (program-error () t))
         (handler-case (progn (allocate-instance (find-class 'integer)) nil) (error () t))
         (eq object (reinitialize-instance object :a 8)) (= (slot-value object 'a) 8)))`);
  console.log('CLOS initarg kernels: context validity, applicability, mutable metadata, redefinition, allocation, values and rejection recovery pass.');
} finally { vm.close(); }
