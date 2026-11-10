#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const context = createContext({vm, source: ''});
const evaluate = new Script('vm.evaluate(source).output');
const check = source => {
  context.source = source;
  assert.equal(evaluate.runInContext(context, {timeout: 2000}), 'T\n', source);
};
check(`(let ((bound 7)) (deftype captured () (list 'integer 0 bound))
  (and (typep 7 'captured) (not (typep 8 'captured))))`);
check(`(progn (deftype optional (&optional (n)) (list 'integer 0 n))
  (and (typep 100 'optional) (typep 3 '(optional 3)) (not (typep 4 '(optional 3)))))`);
check(`(progn (deftype keywords (&key low high) (list 'member low high))
  (and (typep '* 'keywords) (not (typep nil 'keywords)) (typep nil '(keywords :high nil))))`);
check(`(progn (deftype nested ((a &optional b)) (list 'member a b))
  (and (typep 1 '(nested (1))) (typep '* '(nested (1))) (typep nil '(nested (1 nil)))))`);
check(`(progn (deftype documented () (declare (optimize (safety 3))) "doc" 'integer)
  (and (string= (documentation 'documented 'type) "doc")
    (string= (setf (documentation 'documented 'type) "new") "new")
    (string= (documentation 'documented 'type) "new")))`);
check(`(progn (deftype first-type () 'integer) (deftype alias-type () 'first-type)
  (and (typep 1 'alias-type) (progn (deftype first-type () 'string) (typep "x" 'alias-type))))`);
check(`(progn (deftype char-type () 'character) (deftype double-type () 'double-float)
  (and (eq (array-element-type (make-array 3 :element-type 'char-type)) 'character)
    (typep (coerce 1/3 'double-type) 'double-float)))`);
check(`(progn (deftype bounded (&optional (n 7)) (list 'integer 0 n))
  (equal (multiple-value-list (subtypep '(bounded 9) '(integer 0 9))) '(t t)))`);
check(`(progn
  (deftype contextual (&environment e)
    (if (nth-value 1 (macroexpand '(local-type-macro) e)) 'integer 'null))
  (defmacro check-context (&environment e)
    (list 'quote (and (typep 1 'contextual e) (typep nil 'contextual nil) (not (typep nil 'contextual e)))))
  (macrolet ((local-type-macro () t)) (check-context)))`);
check(`(progn (deftype contextual (&environment e) (declare (ignore e)) (throw 'done :done))
  (defmacro throw-context (&environment e) (list 'quote (catch 'done (typep 1 'contextual e))))
  (and (eq :done (macrolet ((local-type-macro () t)) (throw-context)))
    (progn (deftype contextual (&environment e) (list 'eql (not (null e)))) (null (the contextual nil)))))`);
check(`(handler-case (typep 1 '(bounded 2 3)) (program-error () t))`);
// Use mathematical interval bounds, independent of host Lisp fixnum widths.
const integers = [
  ['bit', 0n, 1n], ['fixnum', -(1n << 63n), (1n << 63n) - 1n],
  ['integer', null, null], ['(and fixnum unsigned-byte)', 0n, (1n << 63n) - 1n],
  ['(and t rational integer)', null, null],
  ['(and (integer -5 20) (integer 0 30))', 0n, 20n],
  ['(and integer (rational (0) (3)))', 1n, 2n],
  ['(and (mod 20) (and (integer 3 *) (integer * 8)))', 3n, 8n],
  ['(and integer (real 1/3 2/3))', 1n, 0n],
  ...[8n, 16n, 32n, 64n].flatMap(bits => [
    [`(unsigned-byte ${bits})`, 0n, (1n << bits) - 1n],
    [`(signed-byte ${bits})`, -(1n << (bits - 1n)), (1n << (bits - 1n)) - 1n],
  ]),
];
for (const [a, low, high] of integers) {
  for (const [b, toLow, toHigh] of integers) {
    const empty = low !== null && high !== null && low > high;
    const contained = empty || ((toLow === null || (low !== null && low >= toLow))
      && (toHigh === null || (high !== null && high <= toHigh)));
    check(`(equal (multiple-value-list (subtypep '${a} '${b})) '(${contained ? 't' : 'nil'} t))`);
  }
}
check(`(and (equal (multiple-value-list (subtypep 'integer '(and))) '(t t))
  (equal (multiple-value-list (subtypep '(and) 'integer)) '(nil t))
  (equal (multiple-value-list (subtypep '(and float) 'integer)) '(nil nil))
  (equal (multiple-value-list (subtypep '(and integer float) nil)) '(t t))
  (equal (multiple-value-list (subtypep '(and integer (integer 10 0) (satisfies never-call)) nil)) '(t t)))`);
check(`(let ((calls 0))
  (defun never-call (x) (declare (ignore x)) (incf calls) t)
  (and (equal (multiple-value-list
          (subtypep '(and fixnum (satisfies never-call)) '(unsigned-byte 8))) '(nil nil))
       (equal (multiple-value-list
          (subtypep 'fixnum '(and integer (satisfies never-call)))) '(nil nil))
       (zerop calls)))`);
check(`(progn
  (deftype positive-fixnum () '(and fixnum unsigned-byte))
  (deftype contextual-integer (&environment e)
    (if (nth-value 1 (macroexpand '(intersection-context) e)) '(integer 0 1) 'integer))
  (defmacro intersection-proof (&environment e)
    (list 'quote (and
      (equal (multiple-value-list (subtypep '(and contextual-integer positive-fixnum) 'bit e)) '(t t))
      (equal (multiple-value-list (subtypep 'bit '(and contextual-integer positive-fixnum) e)) '(t t)))))
  (macrolet ((intersection-context () t)) (intersection-proof)))`);
check(`(and
  (equal (multiple-value-list (subtypep
    '(and (unsigned-byte 100000000000000000000) (signed-byte 100000000000000000000))
    '(unsigned-byte 99999999999999999999))) '(t t))
  (equal (multiple-value-list (subtypep
    '(unsigned-byte 100000000000000000000)
    '(and (unsigned-byte 100000000000000000000) fixnum))) '(nil t)))`);
check('(= 42 (+ 20 22))');
console.log(`Guest derived types: expansion, lexical environments, ${integers.length ** 2} integer intersection proofs, uncertainty, and recovery pass.`);
