import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const baseline = process.argv.includes('--baseline');
try {
  const check = source => assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source);
  for (const [a, b, yes] of [
    ['(integer 1 3)', '(integer 0 4)', true], ['(integer 0 4)', '(integer 1 3)', false],
    ['(integer (0) (0))', 'nil', true], ['(unsigned-byte 0)', '(integer 0 0)', true],
    ['(signed-byte 1)', '(integer -1 0)', true], ['(unsigned-byte 63)', 'fixnum', true],
    ['(unsigned-byte 64)', 'fixnum', false], ['(signed-byte 64)', 'fixnum', true],
    ['(signed-byte 65)', 'fixnum', false], ['(integer 9223372036854775808 *)', 'bignum', true],
    ['(integer * -9223372036854775809)', 'bignum', true], ['(integer * -9223372036854775808)', 'bignum', false],
    ['(integer 9223372036854775807 *)', 'bignum', false],
    ['(unsigned-byte 100000000000000000000000000000000000000)', '(signed-byte 100000000000000000000000000000000000001)', true],
    ['(unsigned-byte 100000000000000000000000000000000000000)', '(signed-byte 100000000000000000000000000000000000000)', false],
    ['(and integer (real (0) (3)))', '(integer 1 2)', true],
    ['(and (integer 5 9) (integer 1 3) (satisfies integer-port-opaque))', 'nil', true],
    ['(and (and integer) (and (integer 1 5) (integer 2 4)))', '(integer 2 4)', true],
  ]) check(`(equal (multiple-value-list (subtypep '${a} '${b})) '(${yes ? 't' : 'nil'} t))`);
  check(`(and (subtypep (find-class 'integer) 'integer)
    (subtypep '(integer 9223372036854775808 *) (find-class 'integer)))`);
  check(`(progn (defparameter *integer-port-expansions* 0)
    (deftype integer-port-small () (incf *integer-port-expansions*) '(integer 2 3))
    (and (subtypep '(and integer integer-port-small) 'integer) (= *integer-port-expansions* 1)))`);
  check(`(progn (defparameter *integer-port-predicate-calls* 0)
    (defun integer-port-opaque (object) (declare (ignore object))
      (incf *integer-port-predicate-calls*) (error "Predicate must not run."))
    (equal (multiple-value-list (subtypep '(and integer (satisfies integer-port-opaque)) 'fixnum)) '(nil nil))
    (zerop *integer-port-predicate-calls*))`);
  check(`(equal (multiple-value-list (multiple-value-prog1 (values :first :second :third)
    (subtypep '(unsigned-byte 64) 'integer))) '(:first :second :third))`);
  for (const form of ['(signed-byte 0)', '(unsigned-byte -1)', '(mod 0)', '(unsigned-byte 1/2)'])
    check(`(handler-case (progn (subtypep '${form} 'integer) nil) (error () t))`);
  if (!baseline) {
    check(`(let ((form (list 'and 'integer nil))) (setf (third form) form)
      (handler-case (progn (subtypep form 'integer) nil)
        (simple-type-error (condition) (eq form (type-error-datum condition)))))`);
    check(`(let ((form (list 'and 'integer))) (setf (cdr (cdr form)) (cdr form))
      (handler-case (progn (subtypep form 'integer) nil) (simple-type-error () t)))`);
    check(`(let ((bound (list 1))) (setf (cdr bound) bound)
      (handler-case (progn (subtypep (list 'integer bound '*) 'integer) nil)
        (simple-type-error (condition) (eq bound (type-error-datum condition)))))`);
    check(`(let ((datum (list :literal))) (setf (cdr datum) datum)
      (equal (multiple-value-list (subtypep (list 'eql datum) (list 'member datum))) '(t t)))`);
    check(`(progn (deftype integer-port-bad-control ()
      (let ((form (list 'and 'integer nil))) (setf (third form) form) form))
      (handler-case (progn (subtypep 'integer-port-bad-control 'integer) nil)
        (simple-type-error () t)))`);
    // A DEFTYPE expansion can invalidate the spine checked at entry.
    check(`(progn (defparameter *integer-port-mutable-tail* nil)
      (deftype integer-port-mutate-spine ()
        (setf (cdr *integer-port-mutable-tail*) *integer-port-mutable-tail*) 'integer)
      (let ((form (list 'and 'integer 'integer-port-mutate-spine 'integer)))
        (setq *integer-port-mutable-tail* (cddr form))
        (and (handler-case (progn (subtypep form 'integer) nil)
               (type-error () t))
             (subtypep '(integer 2 3) 'integer))))`);
    // Internal malformed-control diagnostics must retain canonical ERROR.
    // Capture the condition before asserting, so the assertion macro cannot
    // move the signaling operation outside HANDLER-CASE.
    check(`(let ((saved (symbol-function 'error)) (calls 0) (bound (list 1)) (caught nil))
      (setf (cdr bound) bound)
      (unwind-protect
        (progn (setf (symbol-function 'error)
                 (lambda (&rest arguments) (declare (ignore arguments)) (incf calls) :returned))
          (setq caught (handler-case (progn (subtypep (list 'integer bound '*) 'integer) nil)
                         (simple-type-error (condition) condition))))
        (setf (symbol-function 'error) saved))
      (and (typep caught 'simple-type-error) (eq bound (type-error-datum caught))
           (= calls 0) (subtypep '(integer 2 3) 'integer)))`);
  }
  check(`(and (subtypep '(unsigned-byte 1) 'bit)
    (equal (upgraded-array-element-type '(unsigned-byte 1)) 'bit)
    (subtypep '(rational (1) (2)) 'ratio))`);
  console.log('Integer type kernels: target widths, symbolic endpoints, intersections, classes, expansion, values, malformed control and recovery pass.');
} finally { vm.close(); }
