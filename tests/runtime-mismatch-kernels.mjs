import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  for (const [source, expected] of [
    ["(mismatch '(1 2) '(1 2))", 'NIL'],
    ["(mismatch #(1 2 3) #(1 9 3))", '1'],
    ["(mismatch '(1 2 3) #(1 9 3) :from-end t)", '2'],
    ["(mismatch '(1 2 3 4) '(3 4) :from-end t)", '2'],
    ["(mismatch '(1 2) '(1 2 3))", '2'],
    ["(mismatch nil '(1 2) :from-end t)", '0'],
    ["(mismatch '(1 2 3) '(2 3 4) :key #'oddp :test-not #'eq)", 'NIL'],
    ["(mismatch '(1 2) '(2 3) :test #'<)", 'NIL'],
    ['(mismatch "λ🙂" "λx" :key #\'char-code)', '1'],
    ['(mismatch "Ab" "aB" :test #\'char-equal)', 'NIL'],
    ['(mismatch "Ab" "aX" :test #\'char-equal :from-end t)', '2'],
    ['(mismatch "ab" "ab" :test-not #\'char=)', '0'],
    ["(multiple-value-list (mismatch '(1) '(2) :test (lambda (a b) (declare (ignore a b)) (values t :extra))))", '(NIL)'],
    ["(handler-case (mismatch nil '(1 . :bad)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (let ((x (list 1 2))) (setf (cddr x) x) (mismatch '(1) x)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (mismatch nil nil :start1 -1) (type-error () :caught))", ':CAUGHT'],
    ["(mismatch '(1 9) '(1 2))", '1'],
  ]) assert.equal(vm.evaluate(source).output.trim(), expected, source);
} finally {vm.close();}
console.log('MISMATCH kernels: alignment, reverse indices, keys, values, character paths and recovery pass.');
