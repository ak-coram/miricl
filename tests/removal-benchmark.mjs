#!/usr/bin/env node
// Identical native sequence filtering workloads for WASM and compiled SBCL.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const count = 100;
const fixtures = `(defparameter *filter-ascii* "${'abcd'.repeat(8192)}")
  (defparameter *filter-case* "${'aAbB'.repeat(8192)}")
  (defparameter *filter-list* (loop for i below 10000
    for value = 0 then (if (= value 3) 0 (1+ value)) collect value))
  (defparameter *filter-operation* nil) (defparameter *filter-result* nil)`;
const workloads = [
  ['count-eql', 'count', '#\\a *filter-ascii*', 8192, true],
  ['count-fold', 'count', "#\\a *filter-case* :test #'char-equal", 16384, true],
  ['string-eql', 'remove', '#\\a *filter-ascii*', 24576],
  ['string-fold', 'remove', "#\\a *filter-case* :test #'char-equal", 16384],
  ['list-eql', 'remove', '1 *filter-list*', 7500],
  ['list-predicate', 'remove-if', "#'oddp *filter-list*", 5000],
  ['delete-list-with-copy', 'delete', '1 (copy-list *filter-list*)', 7500],
];
const body = (values, scalar) => `(progn (dotimes (i ${count})
  (setq *filter-result* (funcall *filter-operation* ${values})))
  ${scalar ? '*filter-result*' : '(length *filter-result*)'})`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, values, expected, scalar] of workloads) {
    console.log(`(setq *filter-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values, scalar)}))))
        (assert (= ${expected} (funcall fn)))
        (format t "${name} (${count} calls): ~,3F ms~%"
          (loop repeat 3 minimize
            (let ((start (get-internal-real-time)))
              (assert (= ${expected} (funcall fn)))
              (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, operator, values, expected, scalar] of workloads) {
    vm.evaluate(`(setq *filter-operation* #'${operator}) (defun filter-run () ${body(values, scalar)})`);
    const run = () => assert.equal(vm.evaluate('(filter-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = Math.min(...times);
  }
  console.log(JSON.stringify({binary, calls: count, milliseconds: results}, null, 2));
}
