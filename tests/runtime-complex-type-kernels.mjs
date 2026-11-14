import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
// Install and exercise contextual macros in evaluation order. Later primitive
// rebinding checks also replace dependencies of the evaluation compiler.
// Interpret this harness while exercising the image's sealed runtime workers.
vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
const baseline = process.argv.includes('--baseline');
try {
  const check = source => assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source);
  for (const [type, upgraded] of [
    ['nil', 'nil'], ['integer', 'rational'], ['ratio', 'rational'], ['fixnum', 'rational'],
    ['bignum', 'rational'], ['bit', 'rational'], ['signed-byte', 'rational'], ['unsigned-byte', 'rational'],
    ['short-float', 'single-float'], ['single-float', 'single-float'],
    ['double-float', 'double-float'], ['long-float', 'double-float'], ['float', 'float'], ['real', 'real'],
    ['(integer 2 1)', 'nil'], ['(integer 1 3)', 'rational'], ['(rational (1) 1)', 'nil'],
    ['(single-float 1/3 1/3)', 'nil'], ['(double-float 1/3 1/3)', 'nil'],
    ['(float 16777217 16777217)', 'double-float'], ['(real 1/3 1/3)', 'rational'],
    ['(eql 1)', 'rational'], ['(eql 1.0)', 'single-float'], ['(eql 1.0d0)', 'double-float'],
    ['(member)', 'nil'], ['(member 1 2/3)', 'rational'], ['(member 1.0 1.0d0)', 'float'],
    ['(member 1 1.0)', 'real'], ['(or)', 'nil'], ['(or integer single-float)', 'real'],
  ]) check(`(eq (upgraded-complex-part-type '${type}) '${upgraded})`);
  for (const [a, b, yes] of [
    ['(complex nil)', 'symbol', true], ['(complex integer)', '(complex rational)', true],
    ['(complex short-float)', '(complex single-float)', true],
    ['(complex single-float)', '(complex float)', true],
    ['(complex double-float)', '(complex single-float)', false],
    ['(complex single-float)', '(complex (real 0 1))', false],
    ['(complex (single-float 0 1))', '(complex (real 0 2))', true],
    ['(complex float)', '(complex real)', true], ['complex', 'complex', true],
    ['complex', 'number', true], ['complex', 't', true], ['complex', 'atom', true],
    ['complex', 'integer', false], ['complex', 'symbol', false],
    ['integer', 'complex', false], ['symbol', 'complex', false],
    ['(real 2 1)', 'complex', true], ['(real 0 1)', 'complex', false],
  ]) check(`(equal (multiple-value-list (subtypep '${a} '${b})) '(${yes ? 't' : 'nil'} t))`);
  check(`(and (eq (upgraded-complex-part-type (find-class 'long-float)) 'double-float)
    (subtypep (find-class 'complex) (find-class 'number)))`);
  check(`(and (typep #c(1 1) '(complex integer)) (typep #c(1.0 1.0) '(complex single-float))
    (typep #c(1.0d0 1.0d0) '(complex double-float)) (not (typep #c(1.0 1.0) '(complex double-float))))`);
  check(`(progn (defparameter *complex-port-expansions* 0)
    (deftype complex-port-local () (incf *complex-port-expansions*) '(integer 0 3))
    (and (eq (upgraded-complex-part-type 'complex-port-local) 'rational)
         (= *complex-port-expansions* 1)))`);
  check(`(progn (deftype complex-port-context (&environment e)
    (if (nth-value 1 (macroexpand '(complex-port-local-macro) e)) 'single-float 'rational))
    (defmacro complex-port-context-check (&environment e)
      (list 'quote (upgraded-complex-part-type 'complex-port-context e)))
    (and (eq (macrolet ((complex-port-local-macro () t)) (complex-port-context-check)) 'single-float)
         (eq (upgraded-complex-part-type 'complex-port-context) 'rational)))`);
  check(`(progn (defparameter *complex-port-predicate-calls* 0)
    (defun complex-port-opaque (x) (declare (ignore x))
      (incf *complex-port-predicate-calls*) (error "Predicate must not run."))
    (handler-case (upgraded-complex-part-type '(satisfies complex-port-opaque)) (error () nil))
    (zerop *complex-port-predicate-calls*))`);
  check(`(equal (multiple-value-list (multiple-value-prog1 (values :first :second :third)
    (upgraded-complex-part-type '(member 1 2)))) '(:first :second :third))`);
  check(`(equal (multiple-value-list (upgraded-complex-part-type 'integer)) '(rational))`);
  for (const type of ['(complex real integer)', '(complex . real)', '(complex (integer (1 2) 3))'])
    check(`(handler-case (progn (subtypep '${type} 'complex) nil) (error () t))`);
  for (const type of ['(eql :bad)', '(member 1 :bad)', '(or integer symbol)'])
    check(`(handler-case (progn (upgraded-complex-part-type '${type}) nil) (error () t))`);
  check(`(progn (deftype complex-port-opaque-data (datum) (declare (ignore datum)) 'integer)
    (let ((datum (list :literal))) (setf (cdr datum) datum)
      (eq (upgraded-complex-part-type (list 'complex-port-opaque-data datum)) 'rational)))`);
  if (!baseline) {
    check(`(let ((type (list 'complex nil))) (setf (second type) type)
      (handler-case (progn (subtypep type 'complex) nil)
        (simple-type-error (condition) (eq type (type-error-datum condition)))))`);
    check(`(let ((type (list 'complex nil))) (setf (second type) type)
      (handler-case (progn (typep #c(1 1) type) nil)
        (simple-type-error (condition) (eq type (type-error-datum condition)))))`);
    check(`(let ((type (list 'complex (list 'or nil)))) (setf (second (second type)) type)
      (handler-case (progn (upgraded-complex-part-type type) nil) (simple-type-error () t)))`);
    check(`(progn (defparameter *complex-port-bad-control* nil)
      (deftype complex-port-bad-control () *complex-port-bad-control*)
      (let ((type (list 'complex nil))) (setf (second type) type *complex-port-bad-control* type)
        (handler-case (progn (subtypep 'complex-port-bad-control 'complex) nil)
          (simple-type-error (condition) (eq type (type-error-datum condition))))))`);
    check(`(let ((literal (list :literal))) (setf (cdr literal) literal)
      (and (handler-case (progn (upgraded-complex-part-type (list 'eql literal)) nil) (error () t))
           (handler-case (progn (upgraded-complex-part-type (list 'member literal)) nil) (error () t))))`);
    check(`(let ((original (fdefinition 'error)) (type (list 'complex 'real 'integer))
                 (cycle (list 'complex nil)) (condition nil) (cycle-condition nil) (calls 0))
      (setf (second cycle) cycle)
      (unwind-protect
        (progn (setf (fdefinition 'error)
                 (lambda (&rest arguments) (declare (ignore arguments)) (incf calls) :returned))
          (setq condition (handler-case (progn (subtypep type 'complex) nil)
                            (simple-type-error (c) c))
                cycle-condition (handler-case (progn (typep #c(1 1) cycle) nil)
                                  (simple-type-error (c) c))))
        (setf (fdefinition 'error) original))
      (and (zerop calls) condition cycle-condition
           (eq type (type-error-datum condition))
           (eq cycle (type-error-datum cycle-condition))
           (subtypep '(complex integer) '(complex rational))))`);
  }
  check(`(equal (multiple-value-list (subtypep '(complex integer) '(complex rational))) '(t t))`);
  for (const builtin of ['eq', 'car', 'cdr', 'consp', '=', 'logand', 'svref', '1+', 'realp', 'rational', 'float']) {
    check(`(let ((original (fdefinition '${builtin})) (observed nil))
      (unwind-protect
        (progn (setf (fdefinition '${builtin})
                 (lambda (&rest arguments) (declare (ignore arguments)) (error "Replaced public primitive was called.")))
          (setq observed (list (upgraded-complex-part-type '(float 16777217 16777217))
            (subtypep '(complex integer) '(complex rational))
            (subtypep '(complex single-float) '(complex (real 0 1))))))
        (setf (fdefinition '${builtin}) original))
      (equal observed '(double-float t nil)))`);
  }
  console.log('Complex type kernels: target formats, original part containment, emptiness, environments, values, malformed controls and recovery pass.');
} finally { vm.close(); }
