#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl-interpreter.wasm'));
for (const [source, expected] of [
  ['#*010101', '#*010101'], ['#*', '#*'], ['#0*', '#*'], ['#8*101', '#*10111111'],
  ['#8*100', '#*10000000'], ['#0A(1 2)', '#0A(1 2)'],
  ['#2A((1 2) (3 4))', '#2A((1 2) (3 4))'],
  ['#3A(() ())', '#3A(() ())'], ['#2A(#(1 2) #(3 4))', '#2A((1 2) (3 4))'],
  ['(array-dimensions #3A(() ()))', '(2 0 0)'],
  ["(let ((a '#1=#0A#1#)) (eq a (aref a)))", 'T'],
  ["(let ((a '#1=#2A((#1#)))) (eq a (aref a 0 0)))", 'T'],
  ["(let ((a '#(#1=#2A((1)) #1#))) (eq (svref a 0) (svref a 1)))", 'T'],
  ["'(#-miricl #999A rubbish 7)", '(7)'],
  ["'(#-miricl #999*|bad bits ()| 7)", '(7)'],
  ["(let ((a (make-array 5 :element-type 'character :initial-contents \"aλ🙂ZQ\" :fill-pointer 3))) (list (char a 4) (elt a 2) (string= a \"aλ🙂\")))", '(#\\Q #\\🙂 T)'],
]) {
  assert.equal(vm.evaluate(source, {eof: true}).output, `${expected}\n`, source);
}
for (const source of ['#*012', '#2*101', '#1*', '#2*', '#3*', '#*|01|', '#*0\\1', '#A(1)', '#256A()', '#2A rubbish',
  '#2A((1 2) (3))', '#2A((1) (2 3))', '#2A((1) 2)', '#2A((1) (2 . 3))', '#2A((1) . 2)', '#3A(((1)) ((2 3)))']) {
  assert.throws(() => vm.evaluate(source, {eof: true}), {code: 15}, source);
  assert.equal(vm.evaluate('(aref #2A((1 2) (3 4)) 1 1)').output, '4\n');
}
assert.equal(vm.evaluate('#2A(').incomplete, true);
for (const rank of [254, 255]) {
  const array = `#${rank}A${'('.repeat(rank)}17${')'.repeat(rank)}`;
  assert.equal(vm.evaluate(`(let ((a '${array})) (list (array-rank a) (array-total-size a) (row-major-aref a 0)))`).output,
    `(${rank} 1 17)\n`, `maximum supported array rank ${rank}`);
  assert.equal(vm.evaluate(`(array-rank (read-from-string "${array}"))`).output, `${rank}\n`);
}
const bits = '01101001'.repeat(1250);
assert.equal(vm.evaluate(`#*${bits}`).output, `#*${bits}\n`);
vm.mountFile('bits.lisp', `#*${bits} #8*101`);
assert.equal(vm.evaluate(`(with-open-file (stream "bits.lisp")
  (list (equal (read stream) #*${bits}) (equal (read stream) #*10111111)))`).output, '(T T)\n');
assert.equal(vm.evaluate(`(read (make-concatenated-stream
  (make-string-input-stream "#*01") (make-string-input-stream "101")))`).output, '#*01101\n');
console.log('Guest arrays: bit prefixes, ranks, nested vector contents, zero dimensions, sharing, cycles, suppression, and recovery pass.');
