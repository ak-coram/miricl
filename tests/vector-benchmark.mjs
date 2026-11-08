#!/usr/bin/env node
// Measure native vector access/traversal and compare the same calls with SBCL.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const host = args.includes('--sbcl-source');
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
const fixture = `(defparameter *bench-vector* #(${Array.from({length: 4096}, (_, i) => i).join(' ')}))
  (defparameter *bench-target* (copy-seq *bench-vector*))
  (defparameter *bench-result* nil) (defparameter *bench-operation* nil)
  (defun interpreted-access-wrapper (array &rest indices)
    (unless (arrayp array) (error 'type-error :datum array :expected-type 'array))
    (unless (= (length indices) 1) (error 'program-error))
    (elt array (car indices)))`;
const workloads = [
  ['svref', 'svref', '*bench-vector* 17', '*bench-result*', 17, 100000],
  ['aref', 'aref', '*bench-vector* 17', '*bench-result*', 17, 100000],
  ['interpreted-access-wrapper', 'interpreted-access-wrapper', '*bench-vector* 17', '*bench-result*', 17, 100000],
  ['row-major-aref', 'row-major-aref', '*bench-vector* 17', '*bench-result*', 17, 100000],
  ['array-dimension', 'array-dimension', '*bench-vector* 0', '*bench-result*', 4096, 100000],
  ['fill-vector', 'fill', '*bench-target* 7', '(length *bench-result*)', 4096, 1000],
  ['replace-vector', 'replace', '*bench-target* *bench-vector* :start1 1', '(length *bench-result*)', 4096, 1000],
  ['map-vector', 'map', "'vector #'1+ *bench-vector*", '(length *bench-result*)', 4096, 100],
  ['map-into-vector', 'map-into', "*bench-target* #'1+ *bench-vector*", '(length *bench-result*)', 4096, 100],
  ['copy-vector', 'copy-seq', '*bench-vector*', '(length *bench-result*)', 4096, 1000],
  ['reduce-vector', 'reduce', "#'+ *bench-vector*", '*bench-result*', 8386560, 100],
  ['find-vector', 'find', '4095 *bench-vector*', '*bench-result*', 4095, 100],
  ['count-vector', 'count', '4095 *bench-vector*', '*bench-result*', 1, 100],
];
const body = (values, result, count) => `(progn
  (dotimes (i ${count}) (setq *bench-result* (funcall *bench-operation* ${values}))) ${result})`;
if (host) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixture}`);
  for (const [name, operator, values, result, expected, count] of workloads) {
    console.log(`(setq *bench-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values, result, count)}))))
        (assert (= ${expected} (funcall fn)))
        (format t "${name} (${count} calls): ~,3F ms~%"
          (loop repeat 3 minimize
            (let ((start (get-internal-real-time)))
              (assert (= ${expected} (funcall fn)))
              (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixture);
  const results = {};
  for (const [name, operator, values, result, expected, count] of workloads) {
    vm.evaluate(`(setq *bench-operation* #'${operator}) (defun vector-benchmark-run () ${body(values, result, count)})`);
    const run = () => assert.equal(vm.evaluate('(vector-benchmark-run)').output, `${expected}\n`, name);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {iterations: count, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
