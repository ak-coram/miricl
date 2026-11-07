#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl-interpreter.wasm';
const calls = 10000;
const fixtures = '(defparameter *constructed-table* nil)';
const workloads = [
  ['default', '(make-hash-table)'],
  ['keywords', "(make-hash-table :test #'equal :size 1024 :rehash-size 1.5 :rehash-threshold .75)"],
];
const body = form => `(progn (dotimes (i ${calls}) (setq *constructed-table* ${form}))
  (and (hash-table-p *constructed-table*) (= (hash-table-count *constructed-table*) 0)
       (progn (setf (gethash 1 *constructed-table*) 17) (= (gethash 1 *constructed-table*) 17))))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, form] of workloads) console.log(`
    (let ((fn (compile nil '(lambda () ${body(form)}))))
      (assert (funcall fn))
      (format t "${name} (${calls} tables): ~,3F ms~%"
        (loop repeat 3 minimize (let ((start (get-internal-real-time)))
          (assert (funcall fn))
          (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures); const milliseconds = {};
  for (const [name, form] of workloads) {
    vm.evaluate(`(defun construction-run () ${body(form)})`);
    const run = () => assert.equal(vm.evaluate('(construction-run)').output, 'T\n');
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
