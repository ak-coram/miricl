import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const baseline = process.argv.includes('--baseline');
try {
  const check = source => assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source);
  for (const [a, b, yes] of [
    ['(rational 0 1)', '(real -1 2)', true], ['(rational 0 1)', '(rational (0) 1)', false],
    ['(rational (0) 1)', '(rational 0 1)', true], ['(rational (0) (1))', 'ratio', true],
    ['(rational 0 (1))', 'ratio', false], ['(rational (0) 1)', 'ratio', false],
    ['(rational 1 1)', 'integer', true], ['(rational 1/2 1/2)', 'integer', false],
    ['(rational 1 1)', '(mod 2)', true], ['(rational 2 2)', '(mod 2)', false],
    ['(rational 9223372036854775807 9223372036854775807)', 'fixnum', true],
    ['(rational (0) 0)', 'nil', true], ['(real 2 1)', 'nil', true],
    ['(real (0) (1))', 'rational', false], ['(single-float 0 1)', 'float', true],
    ['float', 'single-float', false], ['short-float', 'single-float', true],
    ['long-float', 'double-float', true], ['(single-float 1 1)', 'double-float', false],
    ['(single-float 1 1)', '(single-float (1) 2)', false],
    ['(single-float (1) 2)', '(single-float 1 2)', true],
    ['(single-float 0 0)', '(single-float -0.0 -0.0)', true],
    ['(single-float (0) (0))', 'nil', true], ['(double-float (0) (0))', 'nil', true],
    ['(rational 0 0)', 'complex', false], ['(rational 16777217 16777217)', '(real * 16777216.0)', false],
    ['(single-float 16777217 16777218)', '(single-float 16777218 16777218)', true],
    ['(single-float 1/3 1/3)', 'nil', true], ['(double-float 1/3 1/3)', 'nil', true],
    ['(single-float (3.4028235e38) *)', 'nil', true],
    ['(single-float * (-3.4028235e38))', 'nil', true],
    ['(double-float (1.7976931348623157d308) *)', 'nil', true],
    ['(double-float * (-1.7976931348623157d308))', 'nil', true],
  ]) check(`(equal (multiple-value-list (subtypep '${a} '${b})) '(${yes ? 't' : 'nil'} t))`);
  check(`(subtypep (list 'single-float (1+ (ash 1 1024)) '*) nil)`);
  check(`(and (subtypep (find-class 'single-float) 'float)
    (subtypep '(rational 1 1) (find-class 'integer)))`);
  check(`(progn (defparameter *real-port-expansions* 0)
    (deftype real-port-small () (incf *real-port-expansions*) '(real 1 2))
    (and (subtypep 'real-port-small 'real) (= *real-port-expansions* 1)))`);
  check(`(progn (defparameter *real-port-predicate-calls* 0)
    (defun real-port-opaque (object) (declare (ignore object))
      (incf *real-port-predicate-calls*) (error "Predicate must not run."))
    (equal (multiple-value-list (subtypep '(and real (satisfies real-port-opaque)) 'rational)) '(nil nil))
    (zerop *real-port-predicate-calls*))`);
  check(`(equal (multiple-value-list (multiple-value-prog1 (values :first :second :third)
    (subtypep '(real 0 1) 'real))) '(:first :second :third))`);
  for (const form of ['(real 1 2 3)', '(rational (1 2) 3)', '(float () 1)', '(single-float bad *)', '(double-float ((1)) 2)', '(real (1 . 2) *)'])
    check(`(handler-case (progn (subtypep '${form} 'real) nil) (error () t))`);
  if (!baseline) {
    check(`(let ((bound (list 1))) (setf (cdr bound) bound)
      (handler-case (progn (subtypep (list 'real bound '*) 'real) nil)
        (simple-type-error (condition) (eq bound (type-error-datum condition)))))`);
    check(`(let ((form (list 'and 'real))) (setf (cdr (cdr form)) (cdr form))
      (handler-case (progn (subtypep form 'real) nil) (simple-type-error () t)))`);
    check(`(progn (deftype real-port-bad-control ()
      (let ((form (list 'or 'real nil))) (setf (third form) form) form))
      (handler-case (progn (subtypep 'real-port-bad-control 'real) nil) (simple-type-error () t)))`);
  }
  check(`(equal (multiple-value-list (subtypep '(real 1 2) 'real)) '(t t))`);
  console.log('Real type kernels: exact intervals, float adjacency, signed zero, classes, expansion, values, malformed control and recovery pass.');
} finally { vm.close(); }
