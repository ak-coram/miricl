import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
  check(`(progn
    (defparameter *clos-method-events* nil)
    (defclass clos-method-base ()
      ((a :initarg :a :initarg :alias :initform (progn (push :a *clos-method-events*) 10))
       (b :initarg :b :initform (progn (push :b *clos-method-events*) 20))
       (shared :allocation :class :initarg :shared :initform 30)
       (unbound))
      (:default-initargs :a (progn (push :default-a *clos-method-events*) (values 11 :ignored))
                        :b (progn (push :default-b *clos-method-events*) 22)))
    (defclass clos-method-child (clos-method-base) () (:default-initargs :b 44))
    t)`);
  check(`(let* ((*clos-method-events* nil) (object (make-instance 'clos-method-child :a nil :a :later)))
    (and (null (slot-value object 'a)) (= (slot-value object 'b) 44)
         (= (slot-value object 'shared) 30) (not (slot-boundp object 'unbound))
         (null *clos-method-events*)))`);
  check(`(let* ((*clos-method-events* nil) (object (make-instance 'clos-method-base :alias 5)))
    (and (= (slot-value object 'a) 5) (= (slot-value object 'b) 22)
         (equal *clos-method-events* '(:default-b :default-a))))`);
  check(`(let ((object (allocate-instance (find-class 'clos-method-base))) (*clos-method-events* nil))
    (and (eq object (shared-initialize object '(a))) (= (slot-value object 'a) 10)
         (not (slot-boundp object 'b)) (equal *clos-method-events* '(:a))
         (equal (multiple-value-list (shared-initialize object t :b nil)) (list object))
         (null (slot-value object 'b)) (equal *clos-method-events* '(:a))))`);
  check(`(let ((object (make-instance 'clos-method-base :a 1 :b 2)))
    (and (eq object (reinitialize-instance object :alias nil :a :later))
         (null (slot-value object 'a)) (= (slot-value object 'b) 2)
         (eq object (shared-initialize object nil :a 8)) (= (slot-value object 'a) 8)))`);
  check(`(let ((left (make-instance 'clos-method-base)) (right (make-instance 'clos-method-base)))
    (and (eq left (shared-initialize left nil :shared 99))
         (= (slot-value right 'shared) 99)))`);
  check(`(progn
    (defclass clos-method-profile () ((a :initarg :a :initform 1)))
    (defmethod initialize-instance :after ((object clos-method-profile) &key accepted)
      (when accepted (setf (slot-value object 'a) accepted)))
    (and (= (slot-value (make-instance 'clos-method-profile :accepted 7) 'a) 7)
         (handler-case (progn (make-instance 'clos-method-profile :unknown t) nil) (program-error () t))
         (= (slot-value (make-instance 'clos-method-profile :unknown t :allow-other-keys t) 'a) 1)))`);
  check(`(progn
    (defclass clos-method-redefined () ((x :initarg :x :initform 1)))
    (defparameter *clos-method-old* (make-instance 'clos-method-redefined :x 9))
    (slot-value *clos-method-old* 'x)
    (defclass clos-method-redefined () ((y :initform 2) (x :initarg :x :initform 3)))
    (and (= (slot-value *clos-method-old* 'x) 9) (= (slot-value *clos-method-old* 'y) 2)
         (= (slot-value (make-instance 'clos-method-redefined) 'x) 3)))`);
  check(`(progn
    (defclass clos-method-custom-meta (standard-class) ())
    (defmethod mop:validate-superclass ((class clos-method-custom-meta) (super standard-class)) t)
    (defclass clos-method-custom () ((x :initarg :x) (y :initarg :y)) (:metaclass clos-method-custom-meta))
    (defparameter *clos-method-getters* 0)
    (defmethod mop:class-slots :around ((class clos-method-custom-meta))
      (incf *clos-method-getters*) (call-next-method))
    (let ((object (make-instance 'clos-method-custom :x 1 :y 2)))
      (setf *clos-method-getters* 0)
      (and (= (slot-value object 'x) 1) (> *clos-method-getters* 0))))`);
  check(`(progn
    (defclass clos-method-functionp-contract () ((x :initarg :x :initform 23)) (:default-initargs :x 17))
    (make-instance 'clos-method-functionp-contract)
    (let ((saved (fdefinition 'functionp)))
      (and (unwind-protect
             (progn
               (setf (fdefinition 'functionp) (lambda (object) (declare (ignore object)) nil))
               (let ((a (make-instance 'clos-method-functionp-contract))
                     (b (allocate-instance (find-class 'clos-method-functionp-contract))))
                 (shared-initialize b t)
                 (and (= (slot-value a 'x) 17) (= (slot-value b 'x) 23))))
             (setf (fdefinition 'functionp) saved))
           (functionp #'cons))))`);
  // Invalid public packets reject without corrupting subsequent object use.
  check(`(let ((object (make-instance 'clos-method-base)) (cycle (list 'a)))
    (setf (cdr cycle) cycle)
    (and (handler-case (progn (shared-initialize object cycle) nil) (type-error () t))
         (eq object (shared-initialize object nil :a 8)) (= (slot-value object 'a) 8)))`);
  check(`(let ((object (make-instance 'clos-method-base)))
    (and (handler-case (progn (shared-initialize object '(a . bad)) nil) (type-error () t))
         (handler-case (progn (shared-initialize object t :a) nil) (program-error () t))
         (eq object (shared-initialize object t :a nil)) (null (slot-value object 'a))))`);
  console.log('CLOS method kernels: initarg precedence, defaults, slot selection, shared slots, values, redefinition, custom metadata and malformed-input recovery pass.');
} finally { vm.close(); }
