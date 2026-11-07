#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const fixtures = `(defparameter *set-a* (loop for i below 4096 collect i))
  (defparameter *set-b* (loop for i from 2048 below 6144 collect i))
  (defparameter *set-c* (reverse *set-a*))
  (defparameter *set-keys* (mapcar #'list *set-b*))
  (defparameter *set-operation* nil)`;
const workloads = [
  ['intersection', 'intersection', '(length (funcall *set-operation* *set-a* *set-b*))', 2048],
  ['difference', 'set-difference', '(length (funcall *set-operation* *set-a* *set-b*))', 2048],
  ['union', 'union', '(length (funcall *set-operation* *set-a* *set-b*))', 6144],
  ['subset', 'subsetp', '(if (funcall *set-operation* *set-a* *set-c*) 1 0)', 1],
  ['keyed-intersection', 'intersection', '(length (funcall *set-operation* *set-keys* *set-keys* :key (function car)))', 4096],
];
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, source, expected] of workloads) {
    console.log(`(setq *set-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${source}))))
        (assert (= ${expected} (funcall fn)))
        (format t "${name}: ~,3F ms~%"
          (loop repeat 3 minimize
            (let ((start (get-internal-real-time)))
              (assert (= ${expected} (funcall fn)))
              (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl-interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, operator, source, expected] of workloads) {
    vm.evaluate(`(setq *set-operation* #'${operator}) (defun set-run () ${source})`);
    const run = () => assert.equal(vm.evaluate('(set-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, entriesPerList: 4096, results}, null, 2));
}
