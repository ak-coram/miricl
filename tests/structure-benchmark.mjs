#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
const calls = 5000;
const ordinaryOnly = args.includes('--ordinary-only');
const fixture = `(defstruct (bench-record (:constructor bench-record (x y z))
               (:constructor make-bench-record)) x y z)
  (defparameter *structure-value* 17)
  (defparameter *structure-object* (make-bench-record :x 17))
  (defparameter *structure-result* nil)
  ${ordinaryOnly ? '' : `(defstruct (bench-list (:type list) :named) x y z)
    (defstruct (bench-vector (:type vector) :named) x y z)
    (defparameter *structure-list* (make-bench-list :x 17))
    (defparameter *structure-vector* (make-bench-vector :x 17))`}`;
const workloads = [
  ['read', '(bench-record-x *structure-object*)', '(= *structure-result* 17)'],
  ['write', '(setf (bench-record-x *structure-object*) *structure-value*)', '(= (bench-record-x *structure-object*) 17)'],
  ['keyword', '(make-bench-record :x *structure-value* :y 2 :z 3)', '(= (bench-record-x *structure-result*) 17)'],
  ['boa', '(bench-record *structure-value* 2 3)', '(= (bench-record-x *structure-result*) 17)'],
  ...ordinaryOnly ? [] : [
    ['list-read', '(bench-list-x *structure-list*)', '(= *structure-result* 17)'],
    ['vector-read', '(bench-vector-x *structure-vector*)', '(= *structure-result* 17)'],
    ['list-keyword', '(make-bench-list :x *structure-value* :y 2 :z 3)', '(= (bench-list-x *structure-result*) 17)'],
    ['vector-keyword', '(make-bench-vector :x *structure-value* :y 2 :z 3)', '(= (bench-vector-x *structure-result*) 17)'],
  ],
];
const body = form => `(dotimes (i ${calls}) (setq *structure-result* ${form}))`;
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
    vm.evaluate(`(defun structure-benchmark-run () ${body(form)})`);
    const run = () => {
      vm.evaluate('(structure-benchmark-run)');
      assert.equal(vm.evaluate(check).output, 'T\n', name);
    };
    for (let warmup = 0; warmup < 3; warmup++) run();
    const samples = [];
    for (let sample = 0; sample < 3; sample++) {
      const start = performance.now(); run(); samples.push(performance.now() - start);
    }
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
