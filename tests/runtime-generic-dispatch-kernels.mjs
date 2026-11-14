import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  const check = source => assert.equal(vm.evaluate(`(not (null ${source}))`, {eof: true}).output, 'T\n', source);
  check(`(progn
    (defgeneric dispatch-port-zero ())
    (defmethod dispatch-port-zero () (values :zero :extra))
    (defgeneric dispatch-port-four (a b c d))
    (defmethod dispatch-port-four ((a integer) (b symbol) (c cons) (d character))
      (values a b c d))
    (and (equal (multiple-value-list (dispatch-port-zero)) '(:zero :extra))
      (equal (multiple-value-list (dispatch-port-four 7 :b '(c) #\\d)) '(7 :b (c) #\\d))))`);
  check(`(progn (defgeneric dispatch-port-eql (object))
    (defmethod dispatch-port-eql ((object t)) (values :general object))
    (defmethod dispatch-port-eql ((object (eql 7))) (values :integer object))
    (defmethod dispatch-port-eql ((object (eql 7.0))) (values :single object))
    (defmethod dispatch-port-eql ((object (eql nil))) (values))
    (defun dispatch-port-check ()
      (and (equal (multiple-value-list (dispatch-port-eql 7)) '(:integer 7))
        (equal (multiple-value-list (dispatch-port-eql 7.0)) '(:single 7.0))
        (equal (multiple-value-list (dispatch-port-eql 7.0d0)) '(:general 7.0d0))
        (null (multiple-value-list (dispatch-port-eql nil)))))
    (loop repeat 5 always (dispatch-port-check)))`);
  check(`(progn (defmethod dispatch-port-eql ((object (eql 7))) (values :changed object))
    (loop repeat 5 always (equal (multiple-value-list (dispatch-port-eql 7)) '(:changed 7))))`);
  check(`(progn
    (remove-method #'dispatch-port-eql
      (find-method #'dispatch-port-eql nil (list (miricl-mop:intern-eql-specializer 7))))
    (equal (multiple-value-list (dispatch-port-eql 7)) '(:general 7)))`);
  check(`(progn (defclass dispatch-port-a () ()) (defclass dispatch-port-b () ())
    (defparameter *dispatch-port-object* (make-instance 'dispatch-port-a))
    (defgeneric dispatch-port-class (object))
    (defmethod dispatch-port-class ((object dispatch-port-a)) :a)
    (defmethod dispatch-port-class ((object dispatch-port-b)) :b)
    (and (loop repeat 5 always (eq (dispatch-port-class *dispatch-port-object*) :a))
      (progn (change-class *dispatch-port-object* 'dispatch-port-b)
        (loop repeat 5 always (eq (dispatch-port-class *dispatch-port-object*) :b)))))`);
  check(`(progn (defgeneric dispatch-port-optional (a &optional b))
    (defmethod dispatch-port-optional ((a integer) &optional (b 9)) (values a b))
    (and (equal (multiple-value-list (dispatch-port-optional 3)) '(3 9))
      (equal (multiple-value-list (dispatch-port-optional 3 4)) '(3 4))
      (handler-case (progn (dispatch-port-four 1 :b '(c)) nil) (program-error () t))
      (handler-case (progn (dispatch-port-four 1 :b '(c) #\\d :extra) nil) (program-error () t))
      (equal (multiple-value-list (dispatch-port-zero)) '(:zero :extra))))`);
  check(`(progn (defparameter *dispatch-port-order* nil)
    (defgeneric dispatch-port-next (x))
    (defmethod dispatch-port-next ((x t)) (push :base *dispatch-port-order*) (values :base x))
    (defmethod dispatch-port-next :before ((x integer)) (push :before *dispatch-port-order*))
    (defmethod dispatch-port-next :after ((x integer)) (push :after *dispatch-port-order*))
    (defmethod dispatch-port-next :around ((x integer))
      (push :around-in *dispatch-port-order*)
      (multiple-value-prog1 (call-next-method) (push :around-out *dispatch-port-order*)))
    (and (equal (multiple-value-list (dispatch-port-next 9)) '(:base 9))
      (equal (reverse *dispatch-port-order*) '(:around-in :before :base :after :around-out))))`);
  check(`(equal (multiple-value-list (multiple-value-prog1 (values :first :second :third)
    (dispatch-port-eql 7) (dispatch-port-four 1 :b '(c) #\\d))) '(:first :second :third))`);
  // Replacing an applicable dispatch protocol method must retire cached proof.
  check(`(progn (defclass dispatch-port-generic (standard-generic-function) ())
    (defgeneric dispatch-port-custom (x) (:generic-function-class dispatch-port-generic))
    (defmethod dispatch-port-custom ((x integer)) :ordinary)
    (defparameter *dispatch-port-protocol-calls* 0)
    (dispatch-port-custom 1)
    (defmethod compute-applicable-methods :around ((gf dispatch-port-generic) arguments)
      (incf *dispatch-port-protocol-calls*) (call-next-method))
    (dispatch-port-custom 1)
    (> *dispatch-port-protocol-calls* 0))`);
  if (typeof vm.vm['test-generic-dispatch-boundary'] === 'function') {
    for (let mode = 0; mode < 10; mode++)
      assert.equal(vm.vm['test-generic-dispatch-boundary'](mode), 1, `boundary ${mode}`);
    for (let mode = 0; mode < 7; mode++)
      assert.equal(vm.vm['test-generic-dispatch-packets'](mode), 1, `packet ${mode}`);
    check(`(equal (multiple-value-list (dispatch-port-zero)) '(:zero :extra))`);
  }
  console.log('Generic dispatch kernels: EQL identity/types, arities, method/class/protocol invalidation, combination order, values and recovery pass.');
} finally { vm.close(); }
