#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const calls = 100;
const shuffled = Array.from({length: 10000}, (_, i) => (i * 7919) % 10000);
const fixtures = `(defparameter *ordering-vector* #(${shuffled.join(' ')}))
  (defparameter *ordering-list* (coerce *ordering-vector* 'list))
  (defparameter *ordering-keyed* (mapcar (lambda (x) (cons x nil)) *ordering-list*))
  (defparameter *ordering-evens* (loop for i below 10000 by 2 collect i))
  (defparameter *ordering-odds* (loop for i from 1 below 10000 by 2 collect i))
  (defparameter *ordering-function* nil) (defparameter *ordering-result* nil)`;
const workloads = [
  ['sort-vector', 'sort', "(copy-seq *ordering-vector*) #'<", '(aref *ordering-result* 9999)'],
  ['stable-sort-list', 'stable-sort', "(copy-list *ordering-list*) #'<", '(car (last *ordering-result*))'],
  ['stable-sort-keyed', 'stable-sort', "(copy-list *ordering-keyed*) #'< :key #'car", '(caar (last *ordering-result*))'],
  ['merge-list', 'merge', "'list (copy-list *ordering-evens*) (copy-list *ordering-odds*) #'<", '(car (last *ordering-result*))'],
  ['merge-vector', 'merge', "'vector *ordering-evens* *ordering-odds* #'<", '(aref *ordering-result* 9999)'],
];
const body = (values, result) => `(progn (dotimes (i ${calls})
  (setq *ordering-result* (funcall *ordering-function* ${values}))) ${result})`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, values, result] of workloads) {
    console.log(`(setq *ordering-function* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values, result)}))))
        (assert (= 9999 (funcall fn)))
        (format t "${name} (${calls} calls): ~,3F ms~%"
          (loop repeat 3 minimize (let ((start (get-internal-real-time)))
            (assert (= 9999 (funcall fn)))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args[0] ?? 'interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const milliseconds = {};
  for (const [name, operator, values, result] of workloads) {
    vm.evaluate(`(setq *ordering-function* #'${operator}) (defun ordering-run () ${body(values, result)})`);
    const run = () => assert.equal(vm.evaluate('(ordering-run)').output, '9999\n', name);
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
