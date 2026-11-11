import assert from 'node:assert/strict';
import {examples, config} from '../coalton/examples.mjs';
import {coaltonInput} from '../scripts/coalton.mjs';

assert.ok(examples.length >= 6);
assert.equal(new Set(examples.map(example => example.id)).size, examples.length);
for (const example of examples) {
  assert.ok(config.modes.includes(example.mode), example.id);
  assert.ok(example.code.trim() && example.expected && example.note && example.source, example.id);
  assert.ok(!example.code.includes('...'), example.id);
}
assert.ok(examples.some(example => example.code.includes('define-type')));
assert.ok(examples.some(example => example.code.includes('rec %')));
assert.ok(examples.some(example => example.code.includes('lisp (->')));
assert.throws(() => coaltonInput('42', 'invalid'), /mode/u);
assert.ok(coaltonInput('(+ 20 22)', 'expression').includes('(coalton:coalton\n(+ 20 22)\n)'));
const repl = coaltonInput('(in-package :example)', 'lisp', {persistentPackage: true});
assert.ok(repl.includes('cl:unwind-protect'));
assert.ok(repl.includes('miricl::%coalton-repl-package cl:*package*'));
assert.ok(!coaltonInput('(+ 20 22)', 'expression', {persistentPackage: true}).includes('cl:unwind-protect'));
console.log(`Coalton REPL: ${examples.length} self-contained documentation examples, source links, input modes and persistent Lisp wrapper passed.`);
