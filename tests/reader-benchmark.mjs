#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const host = args.includes('--sbcl-source');
const suppressed = args.includes('--suppressed');
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
const iterations = 20;
const samples = 5;
const quote = text => `"${text.replaceAll('\\', '\\\\').replaceAll('"', '\\"')}"`;
const tokens = [
  ['symbols', 'READER-BENCH-SYMBOL'], ['integers', '123'],
  ['strings', '"aλ🙂"'], ['characters', '#\\a'],
];
if (suppressed) tokens.push(['uninterpreted', 'NO-SUCH-PACKAGE:::BAD']);
const workloads = tokens.flatMap(([name, token]) => [1000, 10000].map(count =>
  [`${name}-${count}`, `#(${`${token} `.repeat(count)})`, count]));
const result = text => suppressed
  ? `(let ((*read-suppress* t)) (nth-value 1 (funcall *reader-function* ${text})))`
  : `(length (funcall *reader-function* ${text}))`;

if (host) {
  console.log('(defparameter *reader-function* (symbol-function \'read-from-string))');
  for (const [name, source, count] of workloads) {
    const expected = suppressed ? [...source].length : count;
    console.log(`(let ((text ${quote(source)}))
      (assert (= ${expected} ${result('text')}))
      (format t "${name}: ~,3F ms~%"
        (loop repeat ${samples} minimize
          (let ((start (get-internal-real-time)))
            (dotimes (i ${iterations}) (assert (= ${expected} ${result('text')})))
            (* 1000.0d0 (/ (- (get-internal-real-time) start)
                          (* ${iterations} internal-time-units-per-second)))))))`);
  }
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  const results = {};
  vm.evaluate("(defparameter *reader-text* nil) (defparameter *reader-function* #'read-from-string)");
  for (const [name, source, count] of workloads) {
    vm.evaluate(`(setq *reader-text* ${quote(source)})`);
    const expected = suppressed ? [...source].length : count;
    const run = () => assert.equal(vm.evaluate(result('*reader-text*')).output, `${expected}\n`);
    for (let i = 0; i < iterations; i++) run();
    const batch = () => assert.equal(vm.evaluate(
      `(dotimes (i ${iterations}) (assert (= ${expected} ${result('*reader-text*')})))`).output, 'NIL\n');
    batch();
    const times = [];
    for (let i = 0; i < samples; i++) { const start = performance.now(); batch(); times.push((performance.now() - start) / iterations); }
    results[name] = {characters: [...source].length, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, suppressed, results}, null, 2));
}
