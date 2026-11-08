#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
const calls = 1000;
const fixture = `(defstruct printer-bench x y)
  (defparameter *printer-structure* (make-printer-bench :x 17 :y '(1 2 3)))
  (defparameter *printer-data* (loop for i below 50 collect i))
  (defparameter *printer-number* 1234.125d0)
  (defparameter *printer-result* nil)`;
const workloads = [
  ['structure', '(write-to-string *printer-structure* :pretty nil)', '(string= *printer-result* "#S(PRINTER-BENCH :X 17 :Y (1 2 3))")'],
  ['list', '(write-to-string *printer-data* :pretty nil)', '(string= *printer-result* (format nil "(~{~D~^ ~})" *printer-data*))'],
  ['pretty-fill', '(let ((*print-pretty* t) (*print-right-margin* 24)) (with-output-to-string (s) (pprint-fill s *printer-data*)))', '(and (find #\\Newline *printer-result*) (equal (read-from-string *printer-result*) *printer-data*))'],
  ['pretty-linear', '(let ((*print-pretty* t) (*print-right-margin* 24)) (with-output-to-string (s) (pprint-linear s *printer-data*)))', '(and (find #\\Newline *printer-result*) (equal (read-from-string *printer-result*) *printer-data*))'],
  ['format-fixed', '(format nil "~12,3F" *printer-number*)', '(string= *printer-result* "    1234.125")'],
  ['format-exponential', '(format nil "~12,3E" *printer-number*)', '(string= *printer-result* "    1.234d+3")'],
];
const body = form => `(dotimes (i ${calls}) (setq *printer-result* ${form}))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixture}`);
  for (const [name, form, check] of workloads) console.log(`
    (let ((fn (compile nil '(lambda () ${body(form)}))))
      (funcall fn) (assert ${check})
      (format t "${name} (${calls} calls): ~,3F ms~%"
        (loop repeat 3 minimize (let ((start (get-internal-real-time)))
          (dotimes (sample 20) (funcall fn))
          (* 50.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixture);
  const milliseconds = {};
  for (const [name, form, check] of workloads) {
    vm.evaluate(`(defun printer-benchmark-run () ${body(form)})`);
    const run = () => {
      vm.evaluate('(printer-benchmark-run)');
      assert.equal(vm.evaluate(check).output, 'T\n', name);
    };
    run(); const times = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); times.push(performance.now() - start);}
    milliseconds[name] = Math.min(...times);
  }
  console.log(JSON.stringify({binary, binaryBytes: fs.statSync(binary).size, calls, milliseconds}, null, 2));
}
