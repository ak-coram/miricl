import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const baseline = process.argv.includes('--baseline');
try {
  const check = source => assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source);
  for (const [type, upgraded] of [
    ['nil', 'nil'], ['character', 'character'], ['base-char', 'character'],
    ['standard-char', 'character'], ['extended-char', 'nil'], ['bit', 'bit'],
    ['(integer 0 1)', 'bit'], ['(integer (0) 1)', 'bit'], ['(integer 0 (1))', 'bit'],
    ['(integer (0) (1))', 'nil'], ['(integer 2 1)', 'nil'],
    ['(integer -1 1)', 't'], ['(integer 0 2)', 't'],
    ['(unsigned-byte 0)', 'bit'], ['(unsigned-byte 1)', 'bit'], ['(unsigned-byte 2)', 't'],
    ['(unsigned-byte 10000000000000000000000000000)', 't'], ['(signed-byte 1)', 't'],
    ['(mod 1)', 'bit'], ['(mod 2)', 'bit'], ['(mod 3)', 't'],
    ['(member)', 'nil'], ['(member 0 1)', 'bit'], ['(member #\\a #\\b)', 'character'],
    ['(member 0 #\\a)', 't'], ['(eql 1)', 'bit'], ['(or)', 'nil'], ['(not t)', 'nil'],
  ]) {
    check(`(eq (upgraded-array-element-type '${type}) '${upgraded})`);
    check(`(eq (array-element-type (make-array 0 :element-type '${type})) '${upgraded})`);
    check(`(subtypep '(array ${type}) '(array ${upgraded}))`);
  }
  check(`(progn (defparameter *array-upgrade-expansions* 0)
    (deftype array-upgrade-local-bit () (incf *array-upgrade-expansions*) 'bit)
    (and (eq (upgraded-array-element-type 'array-upgrade-local-bit) 'bit)
         (= *array-upgrade-expansions* 1)))`);
  check(`(progn (defparameter *array-upgrade-predicates* 0)
    (defun array-upgrade-forbidden (x) (declare (ignore x))
      (incf *array-upgrade-predicates*) (error "Must not execute predicate."))
    (and (eq (upgraded-array-element-type '(satisfies array-upgrade-forbidden)) t)
         (= *array-upgrade-predicates* 0)))`);
  check(`(let ((literal (list :literal))) (setf (cdr literal) literal)
    (and (eq (upgraded-array-element-type (list 'eql literal)) t)
         (eq (upgraded-array-element-type (list 'member literal)) t)))`);
  check(`(progn (deftype array-upgrade-opaque (literal) (declare (ignore literal)) 'bit)
    (let ((literal (list :literal))) (setf (cdr literal) literal)
      (eq (upgraded-array-element-type (list 'array-upgrade-opaque literal)) 'bit)))`);
  if (!baseline) {
    for (const type of ['(eql)', '(eql 0 1)', '(not)', '(not t t)', '(integer 0 1 extra)', '(unsigned-byte 1 extra)', '(member 0 . bad)']) {
      check(`(handler-case (progn (upgraded-array-element-type '${type}) nil) (error () t))`);
      check(`(eq (upgraded-array-element-type '(integer 0 1)) 'bit)`);
    }
    check(`(let ((type (list 'member 0))) (setf (cdr (cdr type)) (cdr type))
      (handler-case (progn (upgraded-array-element-type type) nil)
        (type-error (condition) (eq (type-error-datum condition) type))))`);
    check(`(let ((type (list 'or nil))) (setf (second type) type)
      (handler-case (progn (make-array 0 :element-type type) nil)
        (type-error (condition) (eq (type-error-datum condition) type))))`);
    check(`(progn (defparameter *array-upgrade-cycle* nil)
      (deftype array-upgrade-cycle () *array-upgrade-cycle*)
      (let ((type (list 'or nil))) (setf (second type) type *array-upgrade-cycle* type)
        (handler-case (progn (upgraded-array-element-type 'array-upgrade-cycle) nil)
          (type-error (condition) (eq (type-error-datum condition) type)))))`);
  }
  check(`(equal (multiple-value-list (multiple-value-prog1 (values :first :second)
    (upgraded-array-element-type '(member 0 1)))) '(:first :second))`);
  console.log('Array upgrading: target specialization, construction, subtype, expansion, opaque data, rejection and recovery pass.');
} finally { vm.close(); }
