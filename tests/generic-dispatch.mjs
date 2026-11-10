import assert from 'node:assert/strict';
import fs from 'node:fs';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  vm.evaluate(`(miricl-compiler::ensure-compiler)
    (setq miricl-compiler:*compile-evaluations* nil) (values)`);
  check(`(every (lambda (name) (compiled-function-p (symbol-function
    (miricl-compiler::compiler-primitive 10 1 name))))
    '("%EFFECTIVE-CALL" "%AROUND-PLAN" "%CALL-NEXT" "%INVOKE-EFFECTIVE"))`);
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
  const restored = await Miricl.instantiate(vm.saveImage());
  try {
    assert.equal(restored.evaluate(`(and (check-eql-dispatch) (check-method-context)
      (equal (multiple-value-list (funcall *saved-next-first*)) '(:base 31))
      (equal (multiple-value-list (funcall *saved-next-second*)) '(:base 47))
      (equal (multiple-value-list (funcall *saved-next-first* 53)) '(:base 53)))`).output, 'T\n');
  } finally {restored.close();}
  console.log('Generic dispatch: native method plans, bounded EQL cache, types, multiple values, method edits, escaped primary/around contexts and native callers pass.');
} finally {vm.close();}
