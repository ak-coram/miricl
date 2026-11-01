#!/usr/bin/env node
// Read and inspect native vectors, including graphs that transport cannot encode.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'interpreter.wasm'));
const fixtures = [
  ['#()', '#()'],
  ['#0()', '#()'],
  ['#(1 nil "λ🙂" #(2 3))', '#(1 NIL "λ🙂" #(2 3))'],
  ['#5(1 2)', '#(1 2 2 2 2)'],
  ['#3(nil)', '#(NIL NIL NIL)'],
  ['(let ((v #4((1)))) (eq (svref v 0) (svref v 3)))', 'T'],
  ['`#(1 ,(+ 1 1) ,@(list 3 4))', '#(1 2 3 4)'],
  ['`#(1 ,@(list 2 3) ,.(list 4 5))', '#(1 2 3 4 5)'],
  ['(let ((x 7)) (equalp `#(1 `#(2 ,x) ,x) (vector 1 (list \'quasiquote (vector 2 (list \'unquote \'x))) 7)))', 'T'],
  ["(let ((v '#1=#(#1#))) (eq v (svref v 0)))", 'T'],
  ["(let ((v '#1=#((a . #1#)))) (eq v (cdr (svref v 0))))", 'T'],
  ["(let ((v '#1=(#(#1#)))) (eq v (svref (car v) 0)))", 'T'],
  ["(let ((v '#(#1=(1) #1#))) (eq (svref v 0) (svref v 1)))", 'T'],
  ["(let ((v '#1=#(#2=#(#1#) #2#))) (list (eq (svref v 0) (svref v 1)) (eq v (svref (svref v 0) 0))))", '(T T)'],
  ['(read-from-string "#(1 2)")', '#(1 2)\n6'],
  ["'(#-miricl #999() 1)", '(1)'],
  ['(let ((f #\'vector)) (apply f (list 1 2 3)))', '#(1 2 3)'],
];
for (const [source, expected] of fixtures) {
  assert.equal(vm.evaluate(source, {eof: true}).output, `${expected}\n`, source);
}
for (const source of ['#(1 . 2)', '#2(1 2 3)', '#1()', '#2()', '#0(1)', "'#(#1=1 #1=2)", '#1=#(#2#)']) {
  assert.throws(() => vm.evaluate(source, {eof: true}), {code: 15}, source);
  assert.equal(vm.evaluate('(svref #(1 2 3) 2)').output, '3\n');
}
assert.equal(vm.evaluate('#(1 2').incomplete, true);
assert.equal(vm.evaluate('#10(').incomplete, true);
assert.throws(() => vm.evaluate('#1#'), {code: 15});
const elements = Array.from({length: 10000}, (_, i) => i).join(' ');
assert.equal(vm.evaluate(`(let ((v #(${elements}))) (list (length v) (svref v 9999)))`).output, '(10000 9999)\n');
assert.equal(vm.evaluate(`(let ((v '#1=#(${elements} #1#))) (eq v (svref v 10000)))`).output, 'T\n');
console.log('Guest vectors: literals, size prefixes, Unicode, quasiquote, sharing, mixed cycles, large inputs, and recovery pass.');
