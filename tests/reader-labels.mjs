#!/usr/bin/env node
// Exercise the guest text reader, including cycles that host transport cannot encode.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const binary = process.argv[2] ?? 'interpreter.wasm';
const vm = await Miricl.instantiate(fs.readFileSync(binary));
const fixtures = [
  ["((lambda (#1=#:foo) #1#) 17)", '17'],
  ["((lambda (&rest #1=#:foo) #1#) 1 2 3)", '(1 2 3)'],
  ["(let ((x '(#1=(1 2) #1#))) (eq (car x) (cadr x)))", 'T'],
  ["(let ((x '(#0=nil #0#))) (list x (eq (car x) (cadr x))))", '((NIL NIL) T)'],
  ["(let ((x '(#123456789012345678901234567890=(1 2) #123456789012345678901234567890#))) (eq (car x) (cadr x)))", 'T'],
  ["(let ((x '#1=(1 2 . #1#))) (eq x (cddr x)))", 'T'],
  ["(let ((x '#1=(#1#))) (eq x (car x)))", 'T'],
  ["(let ((x '#1=(#2=(2 . #2#) . #1#))) (list (eq x (cdr x)) (eq (car x) (cdar x))))", '(T T)'],
  ["(let ((x '#1=(#2=#1# #2#))) (list (eq x (car x)) (eq x (cadr x))))", '(T T)'],
  ["(let ((x '(#1=(#2=#1#) #2#))) (list (eq (car x) (cadr x)) (eq (car x) (caar x))))", '(T T)'],
  ["(let ((x '#1=#2=(1 . #1#))) (eq x (cdr x)))", 'T'],
  ["(let ((x '(#1=(1 2) #.(read-from-string \"#1=3\") #1#))) (list (eq (car x) (caddr x)) (cadr x)))", '(T 3)'],
  ["(let ((x '(#1=(1) #-miricl #1=(99) #1#))) (eq (car x) (cadr x)))", 'T'],
  ["(let ((x '(#-miricl #999# 1))) x)", '(1)'],
  ['#1=17 #1=18', '17\n18'],
];
for (const [source, expected] of fixtures) {
  assert.equal(vm.evaluate(source, {eof: true}).output, `${expected}\n`, source);
}
for (const source of ['#1#', "'(#1=1 #1=2)", '#1=#1#', '#1=#2=#1#', '#=17', '##']) {
  assert.throws(() => vm.evaluate(source, {eof: true}), {code: 15}, source);
  assert.equal(vm.evaluate('(+ 1 2)').output, '3\n');
}
vm.evaluate('#1=17');
assert.throws(() => vm.evaluate('#1#'), {code: 15});
assert.equal(vm.evaluate('#1=').incomplete, true);
assert.equal(vm.evaluate('(let ((x \'#1=(1 . #1#))) (eq x (cdr x)))').output, 'T\n');
const longCycle = Array.from({length: 10000}, (_, i) => i).join(' ');
assert.equal(vm.evaluate(`(let ((x '#1=(${longCycle} . #1#))) (eq x (nthcdr 10000 x)))`).output, 'T\n');
console.log('Guest reader labels: sharing, cycles, scope, large labels, nested reads, suppression, and recovery pass.');
