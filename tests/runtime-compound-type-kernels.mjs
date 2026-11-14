import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
// Predicate/control regressions replace compiler dependencies such as CAR.
// Interpret the harness while exercising the image's sealed runtime workers.
vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
const baseline = process.argv.includes('--baseline');
const context = createContext({vm, source: ''});
const evaluate = new Script('vm.evaluate(source).output');
const check = source => {
  context.source = source;
  assert.equal(evaluate.runInContext(context, {timeout: 3000}), 'T\n', source);
};
try {
  // Public type truth tables belong to the upstream ANSI TYPEP/SUBTYPEP/EQL
  // tests. Keep target representation, callback and checked-boundary regressions.
  check(`(let ((value (ash 1 100))) (typep value (list 'member 1 (ash 1 100))))`);
  check(`(let ((literal (list :literal))) (setf (cdr literal) literal)
    (and (typep literal (list 'eql literal)) (typep literal (list 'member :other literal))
         (not (typep (list :literal) (list 'eql literal)))))`);
  check(`(progn (defparameter *compound-port-calls* nil)
    (defun compound-port-yes (x) (push x *compound-port-calls*) (values :yes :discard))
    (defun compound-port-no (x) (push x *compound-port-calls*) (values nil :discard))
    (defun compound-port-zero (x) (declare (ignore x)) (values)) t)`);
  check(`(equal (multiple-value-list (typep 3 '(satisfies compound-port-zero))) '(nil))`);
  for (const [type, expected, calls] of [
    ['(and (satisfies compound-port-yes) (satisfies compound-port-yes))', 't', '(3 3)'],
    ['(and (satisfies compound-port-no) (satisfies compound-port-yes))', 'nil', '(3)'],
    ['(or (satisfies compound-port-yes) (satisfies compound-port-no))', 't', '(3)'],
    ['(or (satisfies compound-port-no) (satisfies compound-port-yes))', 't', '(3 3)'],
    ['(not (satisfies compound-port-yes))', 'nil', '(3)'],
    ['(satisfies compound-port-no)', 'nil', '(3)'],
  ]) check(`(progn (setq *compound-port-calls* nil)
    (and (equal (multiple-value-list (typep 3 '${type})) '(${expected}))
         (equal *compound-port-calls* '${calls})))`);
  check(`(let ((original (fdefinition 'compound-port-yes)) (observed :unset))
    (unwind-protect (progn (setf (fdefinition 'compound-port-yes) #'compound-port-no)
                          (setq observed (typep 3 '(satisfies compound-port-yes))))
      (setf (fdefinition 'compound-port-yes) original)) (null observed))`);
  check(`(let ((original (fdefinition 'integerp)) (observed nil))
    (unwind-protect
      (progn (setf (fdefinition 'integerp) (lambda (x) (declare (ignore x)) nil))
        (setq observed (list (typep 3 '(satisfies integerp)) (typep 3 'integer))))
      (setf (fdefinition 'integerp) original))
    (equal observed '(nil t)))`);
  check(`(progn (defun compound-port-throw (x) (throw 'compound-port-escape x))
    (= 42 (catch 'compound-port-escape (typep 42 '(and (satisfies compound-port-throw) integer)))))`);
  check(`(progn (defparameter *compound-port-control* nil) (defparameter *compound-port-cursor* nil)
    (defun compound-port-change-tail (x) (declare (ignore x))
      (setf (cdr *compound-port-cursor*) (list 'nil)) t)
    (let* ((*compound-port-control* (list 'and '(satisfies compound-port-change-tail) 't))
           (*compound-port-cursor* (cdr *compound-port-control*)))
      (not (typep 3 *compound-port-control*))))`);
  check(`(progn (deftype compound-port-local (value) (list 'or 'string (list 'eql value)))
    (and (typep 3 '(and integer (compound-port-local 3)))
         (not (typep 4 '(and integer (compound-port-local 3))))))`);
  check(`(progn (deftype compound-port-context (&environment e)
      (if (nth-value 1 (macroexpand '(compound-port-macro) e)) '(eql 3) '(eql 4)))
    (defmacro compound-port-context-check (&environment e)
      (list 'quote (typep 3 '(and integer compound-port-context) e)))
    (and (macrolet ((compound-port-macro () t)) (compound-port-context-check))
         (not (typep 3 '(and integer compound-port-context)))))`);
  check(`(progn (deftype compound-port-opaque-data (datum) (declare (ignore datum)) 'integer)
    (let ((datum (list :data))) (setf (cdr datum) datum)
      (typep 3 (list 'and (list 'compound-port-opaque-data datum)))))`);
  for (const type of ['and', 'or', 'not', 'member', 'eql', 'satisfies', '(not)', '(not integer string)',
    '(eql)', '(eql 1 2)', '(satisfies)', '(satisfies 3)', '(satisfies (lambda (x) x))',
    '(member . 3)', '(and integer . 3)', '(or nil . 3)', 'values', '(values integer)', '(function (t) t)']) {
    if (baseline && ['and', 'or'].includes(type)) continue;
    check(`(handler-case (progn (typep 3 '${type}) nil) (error () t))`);
    check(`(typep 42 '(and integer (not string)))`);
  }
  if (!baseline) {
    for (const name of ['and', 'or', 'not']) check(`(let ((type (list '${name} nil)))
      (setf (second type) type) (handler-case (progn (typep 3 type) nil)
        (simple-type-error (condition) (eq type (type-error-datum condition)))))`);
    check(`(let ((type (list 'member 3))) (setf (cddr type) (cdr type))
      (handler-case (progn (typep 3 type) nil) (simple-type-error () t)))`);
    check(`(progn (defun compound-port-cycle-root (x) (declare (ignore x))
      (setf (cdr *compound-port-control*) *compound-port-control*) t)
      (let* ((*compound-port-control* (list 'and '(satisfies compound-port-cycle-root) 'integer))
             (*compound-port-cursor* (cdr *compound-port-control*)))
        (handler-case (progn (typep 3 *compound-port-control*) nil) (simple-type-error () t))))`);
    check(`(progn (defun compound-port-cycle-detached (x) (declare (ignore x))
      (setf (cdr *compound-port-control*) nil (cdr *compound-port-cursor*) *compound-port-cursor*) t)
      (let* ((*compound-port-control* (list 'and '(satisfies compound-port-cycle-detached) 'integer))
             (*compound-port-cursor* (cdr *compound-port-control*)))
        (handler-case (progn (typep 3 *compound-port-control*) nil) (simple-type-error () t))))`);
    // No later control access follows a short circuit, even after mutation.
    check(`(let* ((*compound-port-control* (list 'or '(satisfies compound-port-cycle-root) 'nil)))
      (typep 3 *compound-port-control*))`);
    check(`(typep 42 '(and integer (not string)))`);
    check(`(let ((original (fdefinition 'error)) (type (list 'not)) (cycle (list 'and nil))
                 (observed nil) (cycle-observed nil) (calls 0))
      (setf (second cycle) cycle)
      (unwind-protect
        (progn (setf (fdefinition 'error)
          (lambda (&rest arguments) (declare (ignore arguments)) (incf calls) nil))
          (setq observed (handler-case (progn (typep 3 type) nil)
            (simple-type-error (condition) (eq type (type-error-datum condition)))))
          (setq cycle-observed (handler-case (progn (typep 3 cycle) nil)
            (simple-type-error (condition) (eq cycle (type-error-datum condition))))))
        (setf (fdefinition 'error) original))
      (and observed cycle-observed (zerop calls) (typep 42 '(and integer (not string)))))`);
  }
  for (const builtin of ['eq', 'eql', 'car', 'cdr', 'consp', 'symbolp', '=']) check(`
    (let ((original (fdefinition '${builtin})) (observed nil))
      (unwind-protect
        (progn (setf (fdefinition '${builtin})
          (lambda (&rest arguments) (declare (ignore arguments)) (error "Public primitive used internally.")))
          (setq observed (list (typep 3 '(and integer (not string)))
            (typep 3 '(member 1 2 3)) (typep 3 '(eql 3)))))
        (setf (fdefinition '${builtin}) original))
      (equal observed '(t t t)))`);
  check(`(equal (multiple-value-list (multiple-value-prog1 (values :first :second :third)
    (typep 3 '(satisfies compound-port-yes)))) '(:first :second :third))`);
  console.log('Compound TYPEP migration: target literals, callbacks, mutable controls, environments, values and recovery pass.');
} finally { vm.close(); }
