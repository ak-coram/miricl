#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
const entries = 1000, traversals = 10;
const source = `(defparameter *iteration-table* (make-hash-table))
  (dotimes (i ${entries}) (setf (gethash i *iteration-table*) i))`;
const workloads = [
  ['maphash', `(maphash (lambda (key value) (incf count) (incf sum (+ key value))) *iteration-table*)`],
  ['iterator', `(with-hash-table-iterator (next *iteration-table*)
    (loop (multiple-value-bind (more key value) (next)
      (unless more (return)) (incf count) (incf sum (+ key value)))))`],
];
const body = traversal => `(let ((count 0) (sum 0))
  (dotimes (i ${traversals}) ${traversal})
  (and (= count ${entries * traversals}) (= sum ${entries * (entries - 1) * traversals})))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${source}`);
  for (const [name, traversal] of workloads) console.log(`
    (let ((fn (compile nil '(lambda () ${body(traversal)}))))
      (assert (funcall fn))
      (format t "${name}: ~,3F ms~%"
        (loop repeat 3 minimize (let ((start (get-internal-real-time)))
          (dotimes (sample 100) (assert (funcall fn)))
          (* 10.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(source); const milliseconds = {};
  for (const [name, traversal] of workloads) {
    vm.evaluate(`(defun iteration-run () ${body(traversal)})`);
    const run = () => assert.equal(vm.evaluate('(iteration-run)').output, 'T\n');
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, entries, traversals, milliseconds}, null, 2));
}
