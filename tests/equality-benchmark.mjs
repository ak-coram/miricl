#!/usr/bin/env node
// Use runtime-selected functions and retained results for both engines.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const fixtures = `(defparameter *equality-operation* nil) (defparameter *equality-result* nil)
  (defparameter *equality-left* (make-list 128 :initial-element 1))
  (defparameter *equality-right* (make-list 128 :initial-element 1))
  (defparameter *equality-floats* (make-list 128 :initial-element 1.0))
  (defparameter *equality-text* "${'aBλ🙂'.repeat(8192)}")
  (defparameter *equality-folded* (string-upcase *equality-text*))`;
const workloads = [
  ['equal-list', 1000, 'equal', '*equality-left* *equality-right*', 'T'],
  ['equalp-list', 1000, 'equalp', '*equality-left* *equality-floats*', 'T'],
  ['equalp-short', 100000, 'equalp', "'(1) '(1.0)", 'T'],
  ['equal-early-mismatch', 1000, 'equal', '(cons 2 *equality-left*) (cons 3 *equality-right*)', 'NIL'],
  ['equalp-string', 100, 'equalp', '*equality-text* *equality-folded*', 'T'],
];
const body = (count, values) => `(progn (dotimes (i ${count})
  (setq *equality-result* (funcall *equality-operation* ${values}))) (not (null *equality-result*)))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, count, operator, values, expected] of workloads) {
    console.log(`(setq *equality-operation* #'${operator})
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
    vm.evaluate(`(setq *equality-operation* #'${operator}) (defun equality-run () ${body(count, values)})`);
    const run = () => assert.equal(vm.evaluate('(equality-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {calls: count, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
