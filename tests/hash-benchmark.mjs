#!/usr/bin/env node
// Measure removing the current MAPHASH entry, excluding table population.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const host = args.includes('--sbcl-source');
const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
const source = `(defparameter *bench-table* nil)
  (defun hash-benchmark-setup (test)
    (setq *bench-table* (make-hash-table :test test))
    (dotimes (i 1000) (setf (gethash i *bench-table*) i)))
  (defun hash-benchmark-run ()
    (let ((count 0))
      (maphash (lambda (key value) (declare (ignore value))
                 (incf count) (remhash key *bench-table*)) *bench-table*)
      (list (hash-table-count *bench-table*) count)))`;
const tests = ['eq', 'eql', 'equal', 'equalp'];
if (host) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${source}`);
  for (const test of tests) {
    console.log(`(hash-benchmark-setup '${test})
      (assert (equal '(0 1000) (hash-benchmark-run)))
      (format t "${test}: ~,3F ms~%"
        (loop repeat 3 minimize
          (progn (hash-benchmark-setup '${test})
            (let ((start (get-internal-real-time)))
              (assert (equal '(0 1000) (hash-benchmark-run)))
              (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(source);
  const results = {};
  for (const test of tests) {
    const run = () => assert.equal(vm.evaluate('(hash-benchmark-run)').output, '(0 1000)\n');
    vm.evaluate(`(hash-benchmark-setup '${test})`); run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      vm.evaluate(`(hash-benchmark-setup '${test})`);
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[test] = Math.min(...times);
  }
  console.log(JSON.stringify({binary, entries: 1000, milliseconds: results}, null, 2));
}
