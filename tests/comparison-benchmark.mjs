#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const fixtures = '(defparameter *comparison-operation* nil) (defparameter *comparison-result* nil)';
const workloads = [
  ['integer-order', 100000, '<', '32768 32769', 'T'],
  ['integer-equality', 100000, '=', '32768 32768', 'T'],
  ['mixed-equality', 100000, '=', '32768 32768.0', 'T'],
  ['float-order', 100000, '<', '0.1 0.2d0', 'T'],
  ['large-integer-order', 10000, '<', `${2n ** 100n} ${2n ** 100n + 1n}`, 'T'],
  ['exact-mixed-order', 10000, '<', `${2n ** 53n} ${2n ** 53n + 1n}.0d0`, 'NIL'],
  ['ratio-order', 10000, '<', '1/10 0.1', 'T'],
];
const body = (count, values) => `(progn (dotimes (i ${count})
  (setq *comparison-result* (funcall *comparison-operation* ${values}))) (not (null *comparison-result*)))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, count, operator, values, expected] of workloads) {
    console.log(`(setq *comparison-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(count, values)}))))
        (assert (eql ${expected} (funcall fn)))
        (format t "${name} (${count} calls): ~,3F ms~%"
          (loop repeat 3 minimize
            (let ((start (get-internal-real-time)))
              (assert (eql ${expected} (funcall fn)))
              (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, count, operator, values, expected] of workloads) {
    vm.evaluate(`(setq *comparison-operation* #'${operator}) (defun comparison-run () ${body(count, values)})`);
    const run = () => assert.equal(vm.evaluate('(comparison-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {calls: count, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
