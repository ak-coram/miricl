#!/usr/bin/env node
// Identical workloads, including dynamic function lookup, for WASM and SBCL.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const calls = 100;
const fixtures = `(defparameter *substitution-string* "${'aAbB'.repeat(8192)}")
  (defparameter *substitution-list* (loop for i below 10000
    for value = 0 then (if (= value 3) 0 (1+ value)) collect value))
  (defparameter *substitution-operation* nil) (defparameter *substitution-result* nil)`;
const workloads = [
  ['string-eql', 'substitute', '#\\z #\\a *substitution-string*', '(count #\\z *substitution-result*)', 8192],
  ['string-fold', 'substitute', "#\\z #\\a *substitution-string* :test #'char-equal", '(count #\\z *substitution-result*)', 16384],
  ['string-destructive-with-copy', 'nsubstitute', '#\\z #\\a (copy-seq *substitution-string*)', '(count #\\z *substitution-result*)', 8192],
  ['list-eql', 'substitute', '9 1 *substitution-list*', '(count 9 *substitution-result*)', 2500],
  ['list-predicate', 'substitute-if', "9 #'oddp *substitution-list*", '(count 9 *substitution-result*)', 5000],
  ['list-reverse-count', 'substitute', '9 1 *substitution-list* :from-end t :count 1', '(count 9 *substitution-result*)', 1],
];
const body = values => `(progn (dotimes (i ${calls})
  (setq *substitution-result* (funcall *substitution-operation* ${values})))
  (length *substitution-result*))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, values, check, expected] of workloads) {
    console.log(`(setq *substitution-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values)}))))
        (funcall fn) (assert (= ${expected} ${check}))
        (format t "${name} (${calls} calls): ~,3F ms~%"
          (loop repeat 3 minimize (let ((start (get-internal-real-time)))
            (funcall fn)
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args[0] ?? 'miricl-interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const milliseconds = {};
  for (const [name, operator, values, check, expected] of workloads) {
    vm.evaluate(`(setq *substitution-operation* #'${operator}) (defun substitution-run () ${body(values)})`);
    const run = () => vm.evaluate('(substitution-run)');
    run();
    assert.equal(vm.evaluate(check).output, `${expected}\n`, name);
    const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    assert.equal(vm.evaluate(check).output, `${expected}\n`, name);
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
