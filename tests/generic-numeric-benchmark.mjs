#!/usr/bin/env node
// Dynamic calls keep the WASM and compiled SBCL workloads comparable.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const host = args.includes('--sbcl-source');
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl-interpreter.wasm';
const fixtures = `(defparameter *bench-operation* nil)
  (defparameter *bench-result* nil)
  (defun bench-ordinary (x) (+ x 1))
  (defgeneric bench-generic (x))
  (defmethod bench-generic ((x integer)) (+ x 1))
  (defgeneric bench-keywords (x &key))
  (defmethod bench-keywords ((x integer) &key (a 0)) (+ x a))
  (defmethod bench-keywords ((x number) &key b) b)
  (defclass bench-accessed () ((x :initform 17 :accessor bench-accessor)))
  (defparameter *bench-object* (make-instance 'bench-accessed))`;
const workloads = [
  ['ordinary', 'bench-ordinary', '1', 2, 5000],
  ['generic', 'bench-generic', '1', 2, 1000],
  ['generic-keyword', 'bench-keywords', '1 :a 2', 3, 1000],
  ['generic-keyword-union', 'bench-keywords', '1 :a 2 :b 3', 3, 1000],
  ['accessor', 'bench-accessor', '*bench-object*', 17, 1000],
  ['integer-power', 'expt', '2 10', 1024, 10000],
  ['integral-float-power', 'expt', '2d0 10d0', 1024, 10000],
  ['zero-power', 'expt', '2d0 0d0', 1, 10000],
];
const body = (values, expected, count) => `(progn
  (dotimes (i ${count}) (setq *bench-result* (funcall *bench-operation* ${values})))
  (= *bench-result* ${expected}))`;

if (host) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, values, expected, count] of workloads) {
    console.log(`(setq *bench-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values, expected, count)}))))
        (assert (funcall fn))
        (format t "${name} (${count} calls): ~,3F ms~%"
          (loop repeat 3 minimize
            (let ((start (get-internal-real-time)))
              (dotimes (batch 1000) (assert (funcall fn)))
              ;; Report one batch; repeating resolves the host timer tick.
              (* 1.0d0 (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second))))))`);
  }
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, operator, values, expected, count] of workloads) {
    vm.evaluate(`(setq *bench-operation* #'${operator})
      (defun generic-numeric-benchmark-run () ${body(values, expected, count)})`);
    const run = () => assert.equal(vm.evaluate('(generic-numeric-benchmark-run)').output, 'T\n');
    try { run(); } catch (error) {
      results[name] = {unavailable: error.message};
      continue;
    }
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {iterations: count, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
