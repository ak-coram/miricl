#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const calls = 100;
const fixtures = `(defparameter *tree-source* (loop for i below 10000 collect i))
  (defparameter *tree-copy* (copy-tree *tree-source*))
  (defparameter *tree-operation* nil) (defparameter *tree-result* nil)`;
const workloads = [
  ['sublis-unchanged', 'sublis', "'((10001 . :absent)) *tree-source*", '(eq *tree-result* *tree-source*)', 'T'],
  ['sublis-last', 'sublis', "'((9999 . 99999)) *tree-source*", '(car (last *tree-result*))', '99999'],
  ['subst-last', 'subst', '99999 9999 *tree-source*', '(car (last *tree-result*))', '99999'],
  ['nsubst-with-copy', 'nsubst', '99999 9999 (copy-tree *tree-source*)', '(car (last *tree-result*))', '99999'],
  ['sublis-number-key', 'sublis', "'((t . 9)) *tree-source* :key #'numberp", '(car (last *tree-result*))', '9'],
  ['tree-equal', 'tree-equal', '*tree-source* *tree-copy*', '*tree-result*', 'T'],
];
const body = (values, result) => `(progn (dotimes (i ${calls})
  (setq *tree-result* (funcall *tree-operation* ${values}))) ${result})`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, values, result, expected] of workloads) {
    console.log(`(setq *tree-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values, result)}))))
        (assert (eql ${expected} (funcall fn)))
        (format t "${name} (${calls} calls): ~,3F ms~%"
          (loop repeat 3 minimize (let ((start (get-internal-real-time)))
            (assert (eql ${expected} (funcall fn)))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args[0] ?? 'interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const milliseconds = {};
  for (const [name, operator, values, result, expected] of workloads) {
    vm.evaluate(`(setq *tree-operation* #'${operator}) (defun tree-run () ${body(values, result)})`);
    const run = () => assert.equal(vm.evaluate('(tree-run)').output, `${expected}\n`, name);
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
