import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const baseline = process.argv.includes('--baseline');
try {
  const check = source => assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source);
  for (const [a, b, answer] of [
    ['(array t (17))', '(array t (*))', true],
    ['(array t (*))', '(array t (17))', false],
    ['(array nil (*))', '(array nil (0))', false],
    ['(array * 256)', 'nil', true],
    ['(array t (536870910 2))', 'nil', true],
    ['(array t (536870910 2 0))', 'nil', false],
    ['(array t (536870910 2 *))', 'nil', false],
    ['(and (array * (* 2)) (array * (1 *)))', '(array * (1 2))', true],
    ['(array * (1 2))', '(and (array * (* 2)) (array * (1 *)))', true],
    ['(and (array bit) (array character))', 'nil', true],
    ['(and (array * 1) (array * 2))', 'nil', true],
    ['(and (array t (1)) (array t (2)))', 'nil', true],
    ['(and (array t (536870910 *)) (array t (* 2)))', 'nil', true],
    ['(and (array t (536870910 2 *)) (array t (* * 0)))', 'nil', false],
    ['(array t (*))', 'sequence', true],
    ['(array t (* *))', 'sequence', false],
    ['(array character)', '(array t)', false],
    ['(simple-array bit (*))', '(array bit (*))', true],
    ['(array bit (*))', '(simple-array bit (*))', false],
  ]) check(`(equal (multiple-value-list (subtypep '${a} '${b})) '(${answer ? 't' : 'nil'} t))`);
  check(`(let ((class (find-class 'array)))
    (and (subtypep class '(array *)) (subtypep '(array *) class)))`);
  check(`(progn (setf (find-class 'array-type-port-alias) (find-class 'array))
    (and (subtypep 'array-type-port-alias '(array * *))
         (subtypep '(array) 'array-type-port-alias)))`);
  check(`(progn (defparameter *array-type-port-expansions* 0)
    (deftype array-type-port-bit () (incf *array-type-port-expansions*) 'bit)
    (and (subtypep '(and (array array-type-port-bit (1 *)) (array array-type-port-bit (* 2)))
                   '(array array-type-port-bit (1 2)))
         (= *array-type-port-expansions* 3)))`);
  check(`(progn (defparameter *array-type-port-predicate-calls* 0)
    (defun array-type-port-forbidden (object) (declare (ignore object))
      (incf *array-type-port-predicate-calls*) (error "Predicate must not run."))
    (and (subtypep '(array (satisfies array-type-port-forbidden)) '(array t))
         (zerop *array-type-port-predicate-calls*)))`);
  check(`(equal (multiple-value-list (multiple-value-prog1 (values :kept :second)
    (subtypep '(array bit (3)) '(array bit (*))))) '(:kept :second))`);
  if (!baseline) {
    check(`(let ((form (list 'and '(array bit) nil)))
      (setf (third form) form)
      (handler-case (progn (subtypep form 'array) nil)
        (simple-type-error (condition) (eq form (type-error-datum condition)))))`);
    check(`(let ((dimensions (list 3)))
      (setf (cdr dimensions) dimensions)
      (handler-case (progn (subtypep (list 'array 'bit dimensions) 'array) nil)
        (simple-type-error (condition) (eq dimensions (type-error-datum condition)))))`);
    check(`(let ((form (list 'array 't)))
      (setf (cdr (cdr form)) (cdr form))
      (handler-case (progn (subtypep form 'array) nil)
        (simple-type-error (condition) (eq form (type-error-datum condition)))))`);
    check(`(let ((form '(array t (1 . bad))))
      (handler-case (progn (subtypep form 'array) nil) (type-error () t)))`);
    check(`(let ((datum (list :literal))) (setf (cdr datum) datum)
      (equal (multiple-value-list (subtypep (list 'eql datum) (list 'member datum))) '(t t)))`);
    check(`(progn (deftype array-type-port-opaque (datum) (declare (ignore datum)) 'bit)
      (let ((datum (list :literal))) (setf (cdr datum) datum)
        (subtypep (list 'array (list 'array-type-port-opaque datum)) '(array bit))))`);
    for (const form of ['(array t -1)', '(array t (nil))', '(simple-vector 1 2)'])
      check(`(handler-case (progn (subtypep '${form} 'array) nil) (error () t))`);
  }
  check(`(equal (multiple-value-list (subtypep '(array bit (2)) '(array bit (*)))) '(t t))`);
  console.log('Array type kernels: target kinds, shapes, intersections, classes, environments, values, malformed control and recovery pass.');
} finally { vm.close(); }
