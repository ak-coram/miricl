#!/usr/bin/env node
// Population and lookup timings, with matching compiled host workloads.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const fixtures = `(defparameter *indexed-table* nil)
  (defparameter *indexed-lookup* #'gethash)
  (defparameter *identity-keys* (loop for i below 4096 collect (list i)))
  (defun indexed-identity-populate ()
    (setq *indexed-table* (make-hash-table :test 'eq))
    (let ((i 0))
      (dolist (key *identity-keys*) (setf (gethash key *indexed-table*) (incf i))))
    (hash-table-count *indexed-table*))
  (defun indexed-populate (characters)
    (setq *indexed-table* (make-hash-table :test (if characters 'equal 'eql)))
    (dotimes (i 4096)
      (setf (gethash (if characters (code-char i) i) *indexed-table*) (1+ i)))
    (hash-table-count *indexed-table*))`;
const expectedSum = Array.from({length: 10000}, (_, i) => (i & 4095) + 1).reduce((a, b) => a + b, 0);
const workloads = [
  ['integer-population', '(values)', '(indexed-populate nil)', 4096],
  ['character-population', '(values)', '(indexed-populate t)', 4096],
  ['identity-population', '(values)', '(indexed-identity-populate)', 4096],
  ['identity-lookups', '(indexed-identity-populate)', `(let ((sum 0))
    (dolist (key *identity-keys*) (incf sum (funcall *indexed-lookup* key *indexed-table* 0))) sum)`, 8390656],
  ['present-lookups', '(indexed-populate nil)', `(let ((sum 0))
    (dotimes (i 10000) (incf sum (funcall *indexed-lookup* (logand i 4095) *indexed-table* 0))) sum)`, expectedSum],
  ['absent-lookups', '(indexed-populate nil)', `(let ((sum 0))
    (dotimes (i 10000) (incf sum (funcall *indexed-lookup* (+ 4096 (logand i 4095)) *indexed-table* 0))) sum)`, 0],
];
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, setup, source, expected] of workloads) {
    console.log(`(let ((fn (compile nil '(lambda () ${source}))))
      ${setup} (assert (= ${expected} (funcall fn)))
      (format t "${name}: ~,3F ms~%"
        (loop repeat 3 minimize (progn ${setup}
          (let ((start (get-internal-real-time))) (assert (= ${expected} (funcall fn)))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))))`);
  }
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl-interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, setup, source, expected] of workloads) {
    vm.evaluate(`(defun indexed-run () ${source})`);
    const run = () => assert.equal(vm.evaluate('(indexed-run)').output, `${expected}\n`);
    vm.evaluate(setup); run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      vm.evaluate(setup);
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, entries: 4096, lookups: 10000, results}, null, 2));
}
