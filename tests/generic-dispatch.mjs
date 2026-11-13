import assert from 'node:assert/strict';
import fs from 'node:fs';
import {createCompilerTestVM} from './compiler-test-vm.mjs';
const vm = await createCompilerTestVM(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  vm.evaluate(`(miricl-compiler::ensure-compiler)
    (setq miricl-compiler:*compile-evaluations* nil) (values)`);
  check(`(every (lambda (name) (compiled-function-p (symbol-function
    (miricl-compiler::compiler-primitive 10 1 name))))
    '("%EFFECTIVE-CALL" "%AROUND-PLAN" "%CALL-NEXT" "%INVOKE-EFFECTIVE"))`);
  check("(typep #'make-instance 'standard-generic-function)");
  // A linked compiler can be ready in WASI even though the host cannot
  // instantiate new modules. Generated dispatch plans must still work there.
  const nativeServices = vm.vm['native-services'];
  const nativeService = nativeServices.get(0);
  nativeServices.set(0, null);
  try {
    check(`(progn
      (defgeneric loader-free-dispatch (object &key))
      (defmethod loader-free-dispatch ((object t) &key (value 17)) (+ object value))
      (defmethod loader-free-dispatch :around ((object integer) &key value)
        (declare (ignore value)) (call-next-method))
      (= (loader-free-dispatch 25 :value 17) 42))`);
  } finally { nativeServices.set(0, nativeService); }
  vm.evaluate(`(defclass native-accessor-object () ((value :initarg :value :accessor native-accessor-value)))
    (defparameter *native-accessor-object* (make-instance 'native-accessor-object :value 42)) (values)`);
  check(`(let ((compiler (fdefinition 'miricl-compiler:compile-native))
              (put (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%PUT"))))
    (unwind-protect
        (progn
          (funcall put #'native-accessor-value :cache nil)
          (funcall put (fdefinition '(setf native-accessor-value)) :cache nil)
          (setf (fdefinition 'miricl-compiler:compile-native)
            (lambda (&rest arguments) (declare (ignore arguments)) (error "Accessor plan invoked the compiler.")))
          (and (= (native-accessor-value *native-accessor-object*) 42)
               (= (setf (native-accessor-value *native-accessor-object*) 43) 43)
               (= (native-accessor-value *native-accessor-object*) 43)))
      (setf (fdefinition 'miricl-compiler:compile-native) compiler)))`);
  vm.evaluate(`
    (defgeneric eql-dispatch (x))
    (defmethod eql-dispatch ((x t)) (values :other x))
    (defmethod eql-dispatch ((x (eql 7))) (values :integer x))
    (defmethod eql-dispatch ((x (eql 7f0))) (values :single x))
    (defmethod eql-dispatch ((x (eql nil))) (values :nil x))
    (defun check-eql-dispatch ()
      (and (loop for i below 1000 always
        (equal (multiple-value-list (eql-dispatch i)) (list (if (= i 7) :integer :other) i)))
        (equal (multiple-value-list (eql-dispatch 7f0)) '(:single 7f0))
        (equal (multiple-value-list (eql-dispatch 7d0)) '(:other 7d0))
        (equal (multiple-value-list (eql-dispatch nil)) '(:nil nil))))`);
  check('(check-eql-dispatch)');
  check(`(let ((methods (compute-applicable-methods #'eql-dispatch '(7))))
    (and (= (length methods) 2)
      (progn (setf (car methods) nil (cdr methods) nil)
        (loop repeat 3 always
          (and (= (length (compute-applicable-methods #'eql-dispatch '(7))) 2)
               (= (length (compute-applicable-methods #'eql-dispatch '(7f0))) 2)
               (= (length (compute-applicable-methods #'eql-dispatch '(7d0))) 1))))))`);
  vm.evaluate(`
    (defclass applicability-left () ())
    (defclass applicability-right () ())
    (defclass applicability-child (applicability-left) ())
    (defgeneric applicability-choice (object))
    (defmethod applicability-choice ((object applicability-left)) :left)
    (defmethod applicability-choice ((object applicability-right)) :right)
    (defparameter *applicability-object* (make-instance 'applicability-child))`);
  check(`(and (eq (applicability-choice *applicability-object*) :left)
    (progn (compute-applicable-methods #'applicability-choice (list *applicability-object*))
      (defclass applicability-child (applicability-right) ())
      (eq (applicability-choice *applicability-object*) :right)))`);
  vm.evaluate(`
    (defgeneric context-dispatch (x))
    (defmethod context-dispatch ((x t)) (values :base x))
    (defmethod context-dispatch ((x integer))
      (let ((next (next-method-p)))
        (values (lambda (&rest args) (apply #'call-next-method args)) next)))
    (defun check-method-context ()
      (multiple-value-bind (first present) (context-dispatch 7)
        (let ((second (context-dispatch 19)))
          (and present
            (equal (multiple-value-list (funcall first)) '(:base 7))
            (equal (multiple-value-list (funcall second)) '(:base 19))
            (equal (multiple-value-list (funcall first 11)) '(:base 11))))))`);
  check('(check-method-context)');
  check(`(let* ((getter (miricl-compiler:compile-native
                    '(lambda (gf) (list (miricl-mop:generic-function-name gf)))))
               (put (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%PUT")))
               (info (funcall (symbol-function
                       (miricl-compiler::compiler-primitive 10 1 "%%GENERIC-INFO")) #'context-dispatch)))
    (and (equal (funcall getter #'context-dispatch) '(context-dispatch))
      (progn
        (funcall put #'context-dispatch :metadata-extra :replacement)
        (let* ((slots (funcall (symbol-function
                        (miricl-compiler::compiler-primitive 10 1 "%%RAW-SLOTS")) info))
               (name (loop for tail on slots by #'cddr when (eq (car tail) :name) return tail))
               (extra (loop for tail on slots by #'cddr when (eq (car tail) :metadata-extra) return tail)))
          (unwind-protect
              (progn (rotatef (car name) (car extra))
                     (equal (funcall getter #'context-dispatch) '(:replacement)))
            (setf (car name) :name (car extra) :metadata-extra))))))`);
  check(`(let* ((planner (symbol-function
                   (miricl-compiler::compiler-primitive 10 1 "%%PROPERTY-EFFECTIVE")))
               (method (find-method #'context-dispatch nil (list (find-class 'integer))))
               (next (list method)))
    (setf (cdr next) next)
    (and (handler-case
             (progn (funcall planner nil (list 'call-method method next) #'context-dispatch '(7)) nil)
           (type-error () t))
         (functionp (context-dispatch 7))))`);
  vm.evaluate(`
    (defgeneric keyword-context-dispatch (x &key))
    (defmethod keyword-context-dispatch ((x t) &key (value 17)) (values x value))
    (defmethod keyword-context-dispatch ((x integer) &key value)
      (declare (ignore value))
      (values (lambda (&rest args) (apply #'call-next-method args)) (next-method-p)))
    (defparameter *keyword-next-first* (keyword-context-dispatch 31 :value 7))
    (defparameter *keyword-next-second* (keyword-context-dispatch 47 :value 11))
    (defgeneric optional-context-dispatch (x &optional y))
    (defmethod optional-context-dispatch ((x t) &optional (y 17 supplied) &aux (sum (+ x y)))
      (values sum supplied))
    (defmethod optional-context-dispatch ((x integer) &optional y)
      (declare (ignore y)) (call-next-method))`);
  check(`(and
    (equal (multiple-value-list (funcall *keyword-next-first*)) '(31 7))
    (equal (multiple-value-list (funcall *keyword-next-second*)) '(47 11))
    (equal (multiple-value-list (funcall *keyword-next-first* 53 :value 13)) '(53 13))
    (equal (multiple-value-list (optional-context-dispatch 25)) '(42 nil))
    (equal (multiple-value-list (optional-context-dispatch 25 4)) '(29 t)))`);
  // Presentation systems customize dispatch on a generic-function subclass.
  // Its dynamic protocol must run on every call while unrelated generics
  // retain cached plans, including the native slot metadata readers.
  vm.evaluate(`
    (defclass selective-dispatch-gf (standard-generic-function) ()
      (:metaclass miricl-mop:funcallable-standard-class))
    (defgeneric selective-dispatch (value) (:generic-function-class selective-dispatch-gf))
    (defmethod selective-dispatch ((value t)) :base)
    (defmethod selective-dispatch ((value integer)) :integer)
    (defparameter *selective-base-only* nil)
    (defparameter *selective-protocol-calls* 0)
    (defparameter *selective-cam-method*
      (defmethod compute-applicable-methods :around ((gf selective-dispatch-gf) arguments)
        (incf *selective-protocol-calls*)
        (let ((methods (call-next-method)))
          (if *selective-base-only* (last methods) methods))))`);
  check(`(and (eq (selective-dispatch 1) :integer)
    (progn (setq *selective-base-only* t) (eq (selective-dispatch 1) :base))
    (progn (setq *selective-base-only* nil) (eq (selective-dispatch 1) :integer))
    (>= *selective-protocol-calls* 3))`);
  check(`(progn (check-eql-dispatch)
    (let* ((get (symbol-function (miricl-compiler::compiler-primitive 10 1 "%GET")))
           (cache (funcall get #'eql-dispatch :cache)))
      (and cache (check-eql-dispatch) (eq cache (funcall get #'eql-dispatch :cache)))))`);
  vm.evaluate(`(remove-method #'compute-applicable-methods *selective-cam-method*)
    (defparameter *selective-cem-method*
      (defmethod miricl-mop:compute-effective-method :around
          ((gf selective-dispatch-gf) combination methods)
        (incf *selective-protocol-calls*)
        (call-next-method gf combination (if *selective-base-only* (last methods) methods))))`);
  check(`(and (eq (selective-dispatch 1) :integer)
    (progn (setq *selective-base-only* t) (eq (selective-dispatch 1) :base))
    (progn (setq *selective-base-only* nil) (eq (selective-dispatch 1) :integer))
    (check-eql-dispatch))`);
  vm.evaluate('(remove-method #\'miricl-mop:compute-effective-method *selective-cem-method*) (values)');
  check('(eq (selective-dispatch 1) :integer)');
  vm.evaluate(`
    (defstruct dispatch-parent (value 1 :type integer))
    (defstruct (dispatch-child (:include dispatch-parent (value 1 :type (unsigned-byte 8)))) extra)
    (defparameter *dispatch-child* (make-dispatch-child :value 17))
    (defclass slot-probe () ((value :initarg :value)))
    (defparameter *slot-probe* (make-instance 'slot-probe :value 19))
    (defvar *slot-name-calls* 0)`);
  check(`(and (= (dispatch-parent-value *dispatch-child*) 17)
    (= (setf (dispatch-parent-value *dispatch-child*) 23) 23)
    (= (dispatch-child-value *dispatch-child*) 23)
    (handler-case (progn (setf (dispatch-parent-value *dispatch-child*) 256) nil) (type-error () t))
    (= (slot-value *slot-probe* 'value) 19))`);
  vm.evaluate(`(defparameter *slot-name-method*
    (defmethod miricl-mop:slot-definition-name :around ((slot miricl-mop:standard-effective-slot-definition))
      (incf *slot-name-calls*) (call-next-method)))
    (setq *slot-name-calls* 0) (values)`);
  check(`(and (= (slot-value *slot-probe* 'value) 19) (plusp *slot-name-calls*))`);
  vm.evaluate('(remove-method #\'miricl-mop:slot-definition-name *slot-name-method*) (values)');
  check("(= (slot-value *slot-probe* 'value) 19)");
  // Around-method continuations retain the original arguments and primary
  // plan after the generic call returns, including across later cache hits.
  vm.evaluate(`
    (defvar *around-events* nil)
    (defgeneric deferred-around (x))
    (defmethod deferred-around ((x t)) (values :base x))
    (defmethod deferred-around :before ((x t)) (push (list :before x) *around-events*))
    (defmethod deferred-around :after ((x t)) (push (list :after x) *around-events*) (values))
    (defmethod deferred-around :around ((x number)) (call-next-method))
    (defmethod deferred-around :around ((x integer))
      (lambda (&rest args) (apply #'call-next-method args)))`);
  check(`(let ((first (deferred-around 17)) (second (deferred-around 29)))
    (setq *around-events* nil)
    (and (equal (multiple-value-list (funcall first)) '(:base 17))
         (equal (multiple-value-list (funcall second)) '(:base 29))
         (equal (multiple-value-list (funcall first 19)) '(:base 19))
         (equal (reverse *around-events*)
           '((:before 17) (:after 17) (:before 29) (:after 29) (:before 19) (:after 19)))
         (handler-case (progn (funcall first :different-methods) nil) (error () t))))`);
  check(`(let ((get (symbol-function (miricl-compiler::compiler-primitive 10 1 "%GET"))))
    (and (not miricl-compiler:*compile-evaluations*)
      (some (lambda (entry) (plusp (miricl-compiler::compiler-primitive 26 (third entry))))
        (funcall get #'deferred-around :cache))))`);
  vm.evaluate(`(setf (symbol-function 'check-method-context)
    (miricl-compiler:compile-native (function-lambda-expression #'check-method-context)))`);
  check('(check-method-context)');
  // Distinct arguments outside the EQL specializers share a dispatch class.
  // Bound retained cache state independently of the number of objects passed.
  check(`(<= (length (funcall (symbol-function
    (miricl-compiler::compiler-primitive 10 1 "%GET")) #'eql-dispatch :cache)) 5)`);
  vm.evaluate('(defmethod eql-dispatch ((x (eql 19))) :new)');
  check('(and (eq (eql-dispatch 19) :new) (eq (eql-dispatch 20) :other))');
  vm.evaluate("(remove-method #'eql-dispatch (find-method #'eql-dispatch nil '((eql 19))))");
  check('(check-eql-dispatch)');
  vm.evaluate("(setf (symbol-function 'check-eql-dispatch) (miricl-compiler:compile-native (function-lambda-expression #'check-eql-dispatch)))");
  check('(check-eql-dispatch)');
  // Saving a cached plan and escaped continuations must preserve their
  // separate argument contexts, including native callers after restoration.
  vm.evaluate(`(defparameter *saved-next-first* (context-dispatch 31))
    (defparameter *saved-next-second* (context-dispatch 47)) (values)`);
  const restored = await createCompilerTestVM(vm.saveImage());
  try {
    assert.equal(restored.evaluate(`(and (check-eql-dispatch) (check-method-context)
      (equal (multiple-value-list (funcall *saved-next-first*)) '(:base 31))
      (equal (multiple-value-list (funcall *saved-next-second*)) '(:base 47))
      (equal (multiple-value-list (funcall *keyword-next-first*)) '(31 7))
      (equal (multiple-value-list (funcall *keyword-next-second* 53 :value 13)) '(53 13))
      (equal (multiple-value-list (funcall *saved-next-first* 53)) '(:base 53)))`).output, 'T\n');
  } finally {restored.close();}
  console.log('Generic dispatch: native method plans, bounded EQL cache, types, multiple values, method edits, escaped primary/around contexts and native callers pass.');
} finally {vm.close();}
