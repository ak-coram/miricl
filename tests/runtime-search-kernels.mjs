import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  for (const [source, expected] of [
    ["(search '(1 2) '(0 1 2 1 2 3))", '1'],
    ["(search #(1 2) #(0 1 2 1 2 3) :from-end t)", '3'],
    ["(search '(1 9) #(0 1 2 1 2 3))", 'NIL'],
    ["(search '(0 1) '(2 4 6 1 3 5) :key #'oddp)", '2'],
    ["(search '(1 2) '(2 3 1 2 3) :test #'<)", '0'],
    ["(search '(1 2) '(2 3 1 2 3) :test #'< :from-end t)", '3'],
    ["(search '(1 2) '(1 2 3 4) :test-not #'eql)", '1'],
    ["(search nil '(1 2 3) :start2 1 :end2 2)", '1'],
    ["(search nil '(1 2 3) :start2 1 :end2 2 :from-end t)", '2'],
    ['(search "λ🙂" "abλ🙂xy" :key #\'char-code)', '2'],
    ['(search "Ab" "xABab" :test #\'char-equal :from-end t)', '3'],
    ["(multiple-value-list (search '(1) '(2) :test (lambda (a b) (declare (ignore a b)) (values t :extra))))", '(0)'],
    ["(handler-case (search '(1) '(1 . :bad)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (let ((x (list 1 2))) (setf (cddr x) x) (search '(1) x)) (type-error () :caught))", ':CAUGHT'],
    ["(handler-case (search nil nil :start1 -1) (type-error () :caught))", ':CAUGHT'],
    ["(search '(2 3) '(1 2 3 4))", '1'],
  ]) assert.equal(vm.evaluate(source).output.trim(), expected, source);
} finally {vm.close();}
console.log('SEARCH kernels: regions, callbacks, character fast paths, errors and recovery pass.');
