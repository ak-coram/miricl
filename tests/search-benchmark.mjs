#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const fixtures = `(defparameter *search-operation* nil) (defparameter *search-result* nil)
  (defparameter *search-text* "${'abcd'.repeat(8192)}")`;
const workloads = [
  ['short-position', 200000, 'position', '#\\a "abc"', '0'],
  ['short-bounded-fold', 200000, 'position', '#\\a "Abca" :start 1 :end nil :from-end t :test (function char-equal)', '3'],
  ['short-predicate', 200000, 'find-if', "(function oddp) '(2 4 5)", '5'],
  ['long-absent', 2000, 'position', '#\\x *search-text*', 'NIL'],
  ['long-reverse', 2000, 'position', '#\\a *search-text* :from-end t', '32764'],
];
const body = (count, values) => `(progn (dotimes (i ${count})
  (setq *search-result* (funcall *search-operation* ${values}))) *search-result*)`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, count, operator, values, expected] of workloads) {
    console.log(`(setq *search-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(count, values)}))))
        (assert (eql ${expected} (funcall fn)))
        (format t "${name} (${count} calls): ~,3F ms~%"
          (loop repeat 3 minimize
            (let ((start (get-internal-real-time)))
              (assert (eql ${expected} (funcall fn)))
              (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, count, operator, values, expected] of workloads) {
    vm.evaluate(`(setq *search-operation* #'${operator}) (defun search-run () ${body(count, values)})`);
    const run = () => assert.equal(vm.evaluate('(search-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {calls: count, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
