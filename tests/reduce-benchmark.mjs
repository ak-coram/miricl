#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const fixtures = `(defparameter *reduction-list* (make-list 4096 :initial-element 1))
  (defparameter *reduction-text* (make-string 4096 :initial-element #\\a))
  (defparameter *accessor-list* '(1 2 3 4 5 6 7 8 9 10))
  (defparameter *reduction-operation* nil) (defparameter *reduction-result* nil)`;
const workloads = [
  ['accessor-cadr', 100000, 'cadr', '*accessor-list*', '*reduction-result*', '2'],
  ['accessor-caddr', 100000, 'caddr', '*accessor-list*', '*reduction-result*', '3'],
  ['accessor-cddddr', 100000, 'cddddr', '*accessor-list*', '(length *reduction-result*)', '6'],
  ['list-sum', 100, 'reduce', "'+ *reduction-list*", '*reduction-result*', '4096'],
  ['list-right-sum', 100, 'reduce', "'+ *reduction-list* :from-end t", '*reduction-result*', '4096'],
  ['list-key-sum', 100, 'reduce', "'+ *reduction-list* :key '1+", '*reduction-result*', '8192'],
  ['string-key-sum', 100, 'reduce', "'+ *reduction-text* :key 'char-code", '*reduction-result*', '397312'],
  ['list-right-cons', 100, 'reduce', "'cons *reduction-list* :from-end t :initial-value nil", '(length *reduction-result*)', '4096'],
].filter(row => !args.includes('--accessors-only') || row[2] !== 'reduce');
const body = (count, values, result) => `(progn (dotimes (i ${count})
  (setq *reduction-result* (funcall *reduction-operation* ${values}))) ${result})`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, count, operator, values, result, expected] of workloads) {
    console.log(`(setq *reduction-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(count, values, result)}))))
        (assert (eql ${expected} (funcall fn)))
        (format t "${name} (${count} calls): ~,3F ms~%"
          (loop repeat 3 minimize
            (let ((start (get-internal-real-time)))
              (assert (eql ${expected} (funcall fn)))
              (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, count, operator, values, result, expected] of workloads) {
    vm.evaluate(`(setq *reduction-operation* #'${operator})
      (defun reduction-run () ${body(count, values, result)})`);
    const run = () => assert.equal(vm.evaluate('(reduction-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {calls: count, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
