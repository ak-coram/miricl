import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
  check(`(every (lambda (pair) (= (rationalize (car pair)) (cadr pair)))
    '((0.1 1/10) (0.1d0 1/10) (-0.2d0 -1/5) (1.25d0 5/4) (0.0d0 0) (-0.0d0 0)))`);
  check(`(every (lambda (x) (let ((r (rationalize x)))
      (and (rationalp r) (= (float r x) x))))
    (list least-positive-single-float least-positive-double-float
      least-positive-normalized-single-float least-positive-normalized-double-float
      most-positive-single-float most-negative-single-float
      most-positive-double-float most-negative-double-float))`);
  check(`(let ((value (/ 7 11))) (eq value (rationalize value)))`);
  check(`(equal (multiple-value-list (rationalize (values 0.1d0 :discarded))) '(1/10))`);
  check(`(equal (multiple-value-list (multiple-value-prog1 (values :kept :second)
            (rationalize 0.1d0))) '(:kept :second))`);
  for (const source of ['(rationalize)', '(rationalize 1 2)'])
    check(`(handler-case (progn ${source} nil) (program-error () t))`);
  for (const source of ['(rationalize nil)', '(rationalize #c(1 2))', '(rationalize "0.1")'])
    check(`(handler-case (progn ${source} nil) (type-error () t))`);
  // The runtime owns canonical arithmetic; replacing public bindings must
  // affect user calls while leaving this internal numeric algorithm intact.
  for (const name of ['+', '-', '*', '/', 'truncate', 'numerator', 'denominator'])
    check(`(let ((saved (symbol-function '${name})))
      (unwind-protect
        (progn (setf (symbol-function '${name})
                      (lambda (&rest args) (declare (ignore args)) (values :replacement :extra)))
          (and (eq (${name} ${['numerator','denominator'].includes(name)?'2':'2 1'}) :replacement) (= (rationalize 0.1d0) 1/10)))
        (setf (symbol-function '${name}) saved)))`);
  check('(= (rationalize -0.125d0) -1/8)');
  console.log('RATIONALIZE: exact values, float round trips, values, canonical arithmetic, rejection and recovery pass.');
} finally { vm.close(); }
