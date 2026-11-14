// Core MOP behavior, without any McCLIM compatibility code.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = (name, source) => {
  assert.equal(vm.evaluate(source, {eof: true}).output.trim(), 'T', name);
  console.log(`PASS ${name}`);
};
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  check('native initialization keeps class dispatch, leftmost initargs and caller ownership', `(progn
    (defclass native-init-metaclass (standard-class) ())
    (defvar *native-init-calls* 0)
    (defmethod make-instance :around ((class native-init-metaclass) &rest initargs)
      (incf *native-init-calls*) (call-next-method))
    (defclass native-init-object () ((value :initarg :value :accessor native-init-value) (other :initarg :other))
      (:metaclass native-init-metaclass) (:default-initargs :other 19))
    (let* ((args (list :value 42 :value 17)) (copy (copy-list args))
           (object (apply #'make-instance 'native-init-object args)))
      (and (= *native-init-calls* 1) (equal args copy)
        (= (native-init-value object) 42) (= (slot-value object 'other) 19)
        (= (setf (native-init-value object) 43) 43) (= (slot-value object 'value) 43)
        (compiled-function-p (mop:method-function (first (mop:generic-function-methods #'native-init-value)))))))`);
  check('default initargs use class metadata, precedence and finalization', `(progn
    (defvar *default-effects* 0)
    (defclass default-parent () ((a :initarg :a) (b :initarg :b))
      (:default-initargs :a (incf *default-effects*) :b 19))
    (defclass default-child (default-parent) () (:default-initargs :a 42))
    (let* ((class (find-class 'default-child)) (defaults (mop:class-default-initargs class))
           (object (make-instance class)))
      (and (= *default-effects* 0) (= (slot-value object 'a) 42) (= (slot-value object 'b) 19)
        (= (length defaults) 2)
        (eq defaults (mop:class-default-initargs class))
        (= (length (mop:class-direct-default-initargs class)) 1)
        (= (funcall (third (assoc :a defaults))) 42)
        (progn
          (defclass default-parent () ((a :initarg :a) (b :initarg :b)))
          (and (handler-case (progn (mop:class-default-initargs class) nil) (error () t))
               (progn (mop:finalize-inheritance class)
                 (null (assoc :b (mop:class-default-initargs class)))))))))`);
  check('computed defaults are shared with MAKE-INSTANCE', `(progn
    (defclass defaults-metaclass (standard-class) ())
    (defmethod mop:compute-default-initargs ((class defaults-metaclass))
      (cons (list :extra 23 (lambda () 23)) (call-next-method)))
    (defclass computed-defaults () ((value :initarg :extra)) (:metaclass defaults-metaclass))
    (= (slot-value (make-instance 'computed-defaults) 'value) 23))`);
  check('class prototypes are uninitialized and belong to their class', `(progn
    (defvar *prototype-effects* 0)
    (defclass prototype-class () ((value :initform (incf *prototype-effects*))))
    (let* ((class (find-class 'prototype-class)) (prototype (mop:class-prototype class)))
      (and (eq (class-of prototype) class) (eq prototype (mop:class-prototype class))
           (not (slot-boundp prototype 'value)) (zerop *prototype-effects*))))`);
  check('custom slot metadata and raw access bypass slot specializations', `(progn
    (defclass extra-direct-slot (mop:standard-direct-slot-definition)
      ((annotation :initarg :annotation :reader slot-annotation)))
    (defclass extra-metaclass (standard-class) ())
    (defmethod mop:direct-slot-definition-class ((class extra-metaclass) &rest initargs)
      (declare (ignore initargs)) (find-class 'extra-direct-slot))
    (defclass boxed-slot () ((value :type integer :annotation :retained)) (:metaclass extra-metaclass))
    (defvar *slot-protocol-calls* 0)
    (defmethod mop:slot-value-using-class :around ((class extra-metaclass) object slot)
      (incf *slot-protocol-calls*) (call-next-method))
    (defmethod (setf mop:slot-value-using-class) :around (value (class extra-metaclass) object slot)
      (incf *slot-protocol-calls*) (call-next-method))
    (let* ((object (make-instance 'boxed-slot)) (class (class-of object))
           (location (mop:slot-definition-location (first (mop:class-slots class))))
           (box (list 42)))
      (setq *slot-protocol-calls* 0)
      (setf (mop:standard-instance-access object location) box)
      (and (eq (mop:standard-instance-access object location) box)
           (zerop *slot-protocol-calls*)
           (eq (slot-annotation (first (mop:class-direct-slots class))) :retained))))`);
  check('raw locations remain consistent when a metaclass reorders slots', `(progn
    (defclass reordered-metaclass (standard-class) ())
    (defmethod mop:compute-slots ((class reordered-metaclass)) (reverse (call-next-method)))
    (defclass reordered-object () ((left :initform 17) (right :initform 42))
      (:metaclass reordered-metaclass))
    (let ((object (make-instance 'reordered-object)))
      (every (lambda (slot)
               (eql (mop:standard-instance-access object (mop:slot-definition-location slot))
                    (slot-value object (mop:slot-definition-name slot))))
             (mop:class-slots (class-of object)))))`);
  check('raw access observes customized location readers', `(progn
    (defclass relocated-slot (mop:standard-effective-slot-definition) ())
    (defclass relocated-metaclass (standard-class) ())
    (defmethod mop:effective-slot-definition-class ((class relocated-metaclass) &rest initargs)
      (declare (ignore initargs)) (find-class 'relocated-slot))
    (defclass relocated-object () ((left :initform 17) (right :initform 42))
      (:metaclass relocated-metaclass))
    (let ((object (make-instance 'relocated-object)) (method nil))
      (unwind-protect
          (progn
            (setq method (defmethod mop:slot-definition-location ((slot relocated-slot))
              (- 1 (call-next-method))))
            (every (lambda (slot)
                     (eql (mop:standard-instance-access object (mop:slot-definition-location slot))
                          (slot-value object (mop:slot-definition-name slot))))
                   (mop:class-slots (class-of object))))
        (when method (remove-method #'mop:slot-definition-location method)))))`);
  check('allocation rejects a callback-corrupted layout and recovers', `(progn
    (defclass unstable-slot (mop:standard-effective-slot-definition) ())
    (defclass unstable-metaclass (standard-class) ())
    (defparameter *unstable-layout* nil) (defparameter *corrupt-layout* nil)
    (defmethod mop:effective-slot-definition-class ((class unstable-metaclass) &rest initargs)
      (declare (ignore initargs)) (find-class 'unstable-slot))
    (defmethod mop:class-slots :around ((class unstable-metaclass))
      (let ((slots (call-next-method)))
        (if *corrupt-layout* (setq *unstable-layout* (copy-list slots)) slots)))
    (defmethod mop:slot-definition-name :around ((slot unstable-slot))
      (when (and *corrupt-layout* *unstable-layout*)
        (setq *corrupt-layout* nil)
        (setf (cdr *unstable-layout*) *unstable-layout*))
      (call-next-method))
    (defclass unstable-object () ((value :initform 42)) (:metaclass unstable-metaclass))
    (mop:finalize-inheritance (find-class 'unstable-object))
    (setq *corrupt-layout* t)
    (and (handler-case (progn (allocate-instance (find-class 'unstable-object)) nil) (error () t))
         (= (slot-value (make-instance 'unstable-object) 'value) 42)))`);
  check('raw access preserves funcallable instances and bypasses declared slot types', `(progn
    (defclass raw-callable () ((value :initform 17 :type integer))
      (:metaclass mop:funcallable-standard-class))
    (let* ((object (make-instance 'raw-callable))
           (location (mop:slot-definition-location (first (mop:class-slots (class-of object)))))
           (box (list 42)))
      (mop:set-funcallable-instance-function object (lambda (value) (+ value 7)))
      (setf (mop:funcallable-standard-instance-access object location) box)
      (and (= (funcall object 35) 42)
           (eq (mop:funcallable-standard-instance-access object location) box))))`);
  check('generic metadata remains mutable after its storage is borrowed', `(let*
      ((get (symbol-function (miricl-compiler::compiler-primitive 10 1 "%GET")))
       (put (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%PUT")))
       (gf #'native-init-value)
       (info (funcall (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%GENERIC-INFO")) gf)))
    (funcall get gf :name)
    (funcall put gf :metadata-extra :replacement)
    (let* ((slots (funcall (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%RAW-SLOTS")) info))
           (name-cell (loop for tail on slots by #'cddr when (eq (car tail) :name) return tail))
           (extra-cell (loop for tail on slots by #'cddr when (eq (car tail) :metadata-extra) return tail)))
      (unwind-protect
          (progn
            (rotatef (car name-cell) (car extra-cell))
            (and (eq (funcall get gf :name) :replacement)
                 (progn (rotatef (car name-cell) (car extra-cell))
                   (eq (funcall get gf :name) 'native-init-value))))
        (setf (car name-cell) :name (car extra-cell) :metadata-extra))))`);
  check('inherited accessors follow actual layouts, allocation and redefinition', `(progn
    (defclass indexed-base () ((value :initarg :value :accessor indexed-value)))
    (defclass indexed-child (indexed-base) ((pad :initarg :pad)))
    (defclass indexed-shared (indexed-base) ((value :allocation :class :initform 29)))
    (let ((base (make-instance 'indexed-base :value 17))
          (child (make-instance 'indexed-child :value 42 :pad 99))
          (shared (make-instance 'indexed-shared)))
      (and (= (indexed-value base) 17) (= (indexed-value child) 42)
           (= (indexed-value shared) 29)
           (= (setf (indexed-value child) 43) 43) (= (slot-value child 'pad) 99)
           (progn (defclass indexed-base () ((new :initform 5) (value :initarg :value :accessor indexed-value)))
             (and (= (indexed-value base) 17) (= (indexed-value child) 43)
                  (= (indexed-value shared) 29) (= (slot-value base 'new) 5))))))`);
  check('accessor caches preserve object-specific and value-specific MOP methods', `(progn
    (defclass indexed-hook-object () ((value :initarg :value :accessor indexed-hook-value)))
    (defparameter *indexed-a* (make-instance 'indexed-hook-object :value 42))
    (defparameter *indexed-b* (make-instance 'indexed-hook-object :value 17))
    (indexed-hook-value *indexed-a*) (setf (indexed-hook-value *indexed-b*) 18)
    (let ((calls 0) (reader nil) (writer nil))
      (unwind-protect
          (progn
            (setq reader (defmethod mop:slot-value-using-class :around
              ((class standard-class) (object (eql *indexed-a*)) slot)
              (incf calls) (call-next-method)))
            (setq writer (defmethod (setf mop:slot-value-using-class) :around
              ((value integer) (class standard-class) object slot)
              (incf calls) (call-next-method)))
            (and (= (indexed-hook-value *indexed-b*) 18) (= calls 0)
                 (= (indexed-hook-value *indexed-a*) 42) (= calls 1)
                 (= (setf (indexed-hook-value *indexed-b*) 19) 19) (= calls 2)))
        (when reader (remove-method #'mop:slot-value-using-class reader))
        (when writer (remove-method (fdefinition '(setf mop:slot-value-using-class)) writer)))))`);
  check('unbound accessors return only the primary hook value', `(progn
    (defclass indexed-unbound () ((empty :reader indexed-empty) (many :reader indexed-many)))
    (defmethod slot-unbound ((class standard-class) (object indexed-unbound) name)
      (if (eq name 'empty) (values) (values 1 2 3)))
    (let ((object (make-instance 'indexed-unbound)))
      (and (equal (multiple-value-list (indexed-empty object)) '(nil))
           (equal (multiple-value-list (indexed-many object)) '(1))
           (equal (multiple-value-list (slot-value object 'empty)) '(nil))
           (equal (multiple-value-list (slot-value object 'many)) '(1)))))`);
  check('slot type predicates run once when they redefine the class', `(progn
    (defparameter *indexed-type-calls* 0) (defparameter *indexed-redefine* nil)
    (defun indexed-type-p (value)
      (incf *indexed-type-calls*)
      (when *indexed-redefine*
        (setq *indexed-redefine* nil)
        (defclass indexed-typed () ((pad :initform 9)
          (value :initarg :value :type (satisfies indexed-type-p) :accessor indexed-typed-value))))
      (integerp value))
    (defclass indexed-typed ()
      ((value :initarg :value :type (satisfies indexed-type-p) :accessor indexed-typed-value)))
    (let ((object (make-instance 'indexed-typed :value 17)))
      (setf (indexed-typed-value object) 18)
      (setq *indexed-type-calls* 0 *indexed-redefine* t)
      (and (= (setf (indexed-typed-value object) 42) 42)
           (= *indexed-type-calls* 1) (= (indexed-typed-value object) 42))))`);
  check('ordinary slot reads preserve receiver-specific methods and mutable cells', `(progn
    (defclass cached-read-object () ((value :initarg :value) (empty)))
    (defparameter *cached-read-a* (make-instance 'cached-read-object :value 17))
    (defparameter *cached-read-b* (make-instance 'cached-read-object :value 42))
    (dotimes (i 3) (slot-value *cached-read-a* 'value) (slot-value *cached-read-b* 'value))
    (let ((method nil))
      (unwind-protect
          (progn
            (setq method (defmethod mop:slot-value-using-class :around
              ((class standard-class) (object (eql *cached-read-b*)) slot)
              (+ 100 (call-next-method))))
            (and (= (slot-value *cached-read-a* 'value) 17)
                 (= (slot-value *cached-read-b* 'value) 142)
                 (= (setf (slot-value *cached-read-a* 'value) 19) 19)
                 (= (slot-value *cached-read-a* 'value) 19)))
        (when method (remove-method #'mop:slot-value-using-class method)))))`);
  check('ordinary slot caches follow class redefinition', `(progn
    (defclass cached-read-object () ((pad :initform 9) (value :initarg :value) (empty)))
    (and (= (slot-value *cached-read-a* 'value) 19)
         (= (slot-value *cached-read-b* 'value) 42)
         (= (slot-value *cached-read-a* 'pad) 9)))`);
  check('read probing does not require an unused type getter', `(let
      ((original (fdefinition 'mop:slot-definition-type)))
    (unwind-protect
        (progn (fmakunbound 'mop:slot-definition-type)
          (= (slot-value *cached-read-a* 'value) 19))
      (setf (fdefinition 'mop:slot-definition-type) original)))`);
  check('metadata method combinations invalidate direct slot assumptions', `(progn
    (defmethod slot-missing ((class standard-class) (object cached-read-object) name operation &optional value)
      (declare (ignore name operation value)) :metadata-missing)
    (define-method-combination cached-name-constant () ((methods () :required t))
      (declare (ignore methods)) (list 'quote :renamed))
    (let* ((get (symbol-function (miricl-compiler::compiler-primitive 10 1 "%GET")))
           (original (funcall get #'mop:slot-definition-name :method-combination-name)))
      (unwind-protect
          (progn
            (ensure-generic-function 'mop:slot-definition-name :method-combination '(cached-name-constant))
            (eq (slot-value *cached-read-a* 'value) :metadata-missing))
        (ensure-generic-function 'mop:slot-definition-name :method-combination original))))`);
  check('cold slot probing does not repeat custom metadata getter effects', `(let
      ((calls 0) (method nil))
    (unwind-protect
        (progn
          (setq method (defmethod mop:slot-definition-name :around
            ((slot mop:standard-effective-slot-definition))
            (incf calls) (call-next-method)))
          (let ((value (slot-value *cached-read-a* 'value)) (first-calls calls))
            (setq calls 0)
            (and (= value 19) (plusp first-calls)
                 (= (slot-value *cached-read-a* 'value) 19) (= first-calls calls))))
      (when method (remove-method #'mop:slot-definition-name method))))`);
  // A populated cache must preserve each class/name pair, including collisions.
  // Expectations come from the fixture's slots, not the cache's capacity.
  const cacheSlots = Array.from({length: 128}, (_, index) => [`cache-slot-${index}`, index]);
  const cacheClasses = Array.from({length: 12}, (_, index) => `cache-key-class-${index}`);
  for (const name of cacheClasses)
    vm.evaluate(`(defclass ${name} () (${cacheSlots.map(([slot, value]) =>
      `(${slot} :initform ${value})`).join(' ')}))
      (defparameter *${name}* (make-instance '${name})) (values)`);
  for (let round = 0; round < 2; round++)
    for (const name of cacheClasses)
      check('ordinary slot reads preserve distinct keys: ' + name,
        `(and ${cacheSlots.map(([slot, value]) => `(= (slot-value *${name}* '${slot}) ${value})`).join(' ')})`);
  const restored = await Miricl.instantiate(vm.saveImage());
  try {
    assert.equal(restored.evaluate(`(and (= (slot-value *cached-read-a* 'value) 19)
      (= (slot-value *cached-read-b* 'value) 42))`).output.trim(), 'T',
      'Slot caches rebuild after image restore');
  } finally {restored.close();}
  check('specialized lambda-list extraction preserves defaults and opaque specializers', `(let
      ((parameters '(x (y (eql :key)) &optional (z 42 supplied) &rest tail &key ((:size size) 17))))
    (and (equal (mop:extract-lambda-list parameters)
                '(x y &optional (z 42 supplied) &rest tail &key ((:size size) 17)))
         (equal (mop:extract-specializer-names parameters) '(t (eql :key)))
         (eq (first (mop:extract-specializer-names '((x unknown-class)))) 'unknown-class)))`);
  for (const source of [
    "(mop:extract-lambda-list '#1=(x . #1#))",
    "(mop:extract-lambda-list '((x integer extra)))",
    "(mop:extract-lambda-list '(x x))",
    "(mop:extract-lambda-list '(x &rest x))",
    "(mop:extract-lambda-list '(&rest &key x))",
    "(mop:extract-lambda-list '(&rest (x)))",
    "(mop:extract-lambda-list '(&key (x nil t)))",
    "(mop:extract-lambda-list '(&optional #1=(x . #1#)))",
    "(mop:extract-specializer-names '(x . y))",
    "(mop:ensure-class 'bad-defaults :direct-default-initargs '#1=((:x 1 identity) . #1#))",
    "(mop:ensure-class 'bad-defaults :direct-default-initargs '((:x 1 not-a-function)))",
    "(mop:standard-instance-access (make-instance 'prototype-class) -1)",
    "(mop:standard-instance-access (make-instance 'prototype-class) 999)",
    "(funcall (symbol-function (miricl-compiler::compiler-primitive 10 1 \"%%MAKE-GENERIC\")) nil)",
  ]) check('reject malformed input: ' + source, `(handler-case (progn ${source} nil) (error () t))`);
  check('MOP remains usable after rejection', `(= (slot-value (make-instance 'computed-defaults) 'value) 23)`);
} finally {vm.close();}
