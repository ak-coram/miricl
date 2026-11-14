import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  for (const [form, expected] of [
    ['(search "ababac" "ababababababababababababac")', '20'],
    ['(search "aba" "ababa" :from-end t)', '2'],
    ['(search "AbA" "ababA" :test #\'char-equal :from-end t)', '2'],
    ['(search "A" "baBA" :test #\'char-equal :from-end t)', '3'],
    ['(search "λ🙂" "λ🙂xλ🙂" :from-end t)', '3'],
    ['(search "S" "xſ" :test #\'char-equal)', 'NIL'],
    ['(mismatch "S" "ſ" :test #\'char-equal)', '0'],
    ['(mismatch "xabc" "abc" :from-end t)', '1'],
    ['(mismatch "abc" "xbc" :from-end t)', '1'],
    ['(mismatch "abc" "abc" :test-not #\'char=)', '0'],
    ['(mismatch "" "abc" :from-end t)', '0'],
    ['(count #\\a "abacada" :from-end t)', '4'],
    ['(count #\\a "AbAcA" :test #\'char-equal)', '3'],
    ['(position #\\a "AbAcA" :test #\'char-equal :from-end t)', '4'],
    ['(count 10 "abc")', '0'],
    ['(count 10 "abc" :test-not #\'eql)', '3'],
    ['(position 10 "abc" :test-not #\'eql :from-end t)', '2'],
    ['(multiple-value-list (search "a" "a"))', '(0)'],
    ['(multiple-value-list (mismatch "a" "b"))', '(0)'],
    ['(let ((s (make-array 6 :element-type \'character :initial-contents "abaxxx" :fill-pointer 3))) (search "ba" s :from-end t))', '1'],
    ['(let* ((s (copy-seq "xxababayy")) (d (make-array 5 :element-type \'character :displaced-to s :displaced-index-offset 2))) (search "aba" d :from-end t))', '2'],
  ]) assert.equal(vm.evaluate(form).output.trim(), expected, form);
  for (const form of ['(search "" "" :start2 -1)', '(mismatch "" "" :end1 1)',
    '(position #\\a "a" :start 2)', '(count #\\a "a" :end 2)']) {
    assert.equal(vm.evaluate(`(handler-case (progn ${form} :missed) (type-error () :caught))`).output.trim(), ':CAUGHT', form);
    assert.equal(vm.evaluate('(search "aba" "ababa" :from-end t)').output.trim(), '2');
  }
} finally {vm.close();}
console.log('Canonical character traversal: KMP overlaps, Unicode fold, reverse regions, displaced/fill-pointer strings, value counts, rejection and recovery pass.');
