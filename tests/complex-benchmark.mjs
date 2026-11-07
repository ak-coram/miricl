#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl-interpreter.wasm';
const calls = 10000;
const fixture = `(defparameter *complex-real* 1.25)
  (defparameter *complex-imaginary* 2.5)
  (defparameter *complex-result* nil)`;
const workloads = [
  ['construct', '(complex *complex-real* *complex-imaginary*)',
    '(and (= *complex-result* #c(1.25 2.5)) (typep *complex-result* \'(complex single-float)))'],
  ['coerce-double', '(coerce *complex-real* \'(complex double-float))',
    '(and (= *complex-result* #c(1.25d0 0.0d0)) (typep *complex-result* \'(complex double-float)))'],
  ['upgrade-integer', '(upgraded-complex-part-type \'integer)', '(eq *complex-result* \'rational)'],
].filter(([name]) => !args.includes('--construct-only') || name === 'construct');
const body = form => `(dotimes (i ${calls}) (setq *complex-result* ${form}))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixture}`);
  for (const [name, form, check] of workloads) console.log(`
    (let ((fn (compile nil '(lambda () ${body(form)}))))
      (funcall fn) (assert ${check})
      (format t "${name} (${calls} calls): ~,3F ms~%"
        (loop repeat 3 minimize (let ((start (get-internal-real-time)))
          (dotimes (sample 50) (funcall fn))
          (* 20.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixture);
  const milliseconds = {};
  for (const [name, form, check] of workloads) {
    vm.evaluate(`(defun complex-benchmark-run () ${body(form)})`);
    const run = () => {
      vm.evaluate('(complex-benchmark-run)');
      assert.equal(vm.evaluate(check).output, 'T\n', name);
    };
    for (let warmup = 0; warmup < 5; warmup++) run();
    const samples = [];
    for (let sample = 0; sample < 3; sample++) {
      const start = performance.now(); run(); samples.push(performance.now() - start);
    }
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
