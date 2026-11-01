#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const workloads = [
  ['zero-start', '(loop for i below 20000 collect i)', '(20000 0 19999)'],
  ['explicit-start', '(loop for i from 100000 to 119999 collect i)', '(20000 100000 119999)'],
  ['constant', '(loop for i below 20000 collect 17)', '(20000 17 17)'],
];
const source = form => `(defun loop-range-run ()
  (let ((result ${form})) (list (length result) (car result) (car (last result)))))`;
if (args.includes('--sbcl-source')) {
  console.log('(declaim (optimize (speed 3) (safety 1)))');
  for (const [name, form, expected] of workloads) console.log(`${source(form)}
    (assert (equal (loop-range-run) '${expected}))
    (format t "${name} (20000 elements): ~,3F ms~%"
      (loop repeat 5 minimize (let ((start (get-internal-real-time)))
        (dotimes (sample 100) (assert (equal (loop-range-run) '${expected})))
        (* 10.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))`);
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  const milliseconds = {};
  for (const [name, form, expected] of workloads) {
    vm.evaluate(source(form));
    const run = () => assert.equal(vm.evaluate('(loop-range-run)').output, `${expected}\n`);
    for (let i = 0; i < 5; i++) run();
    const times = [];
    for (let i = 0; i < 5; i++) {const start = performance.now(); run(); times.push(performance.now() - start);}
    milliseconds[name] = Math.min(...times);
  }
  console.log(JSON.stringify({binary, elements: 20000, milliseconds}, null, 2));
}
