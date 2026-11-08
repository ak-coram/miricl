#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
const calls = 10000;
const formatter = !args.includes('--format-only');
const fixture = `(defparameter *format-result* nil)
  (defparameter *format-stream* (make-string-output-stream))
  ${formatter ? '(defparameter *format-function* (formatter "~D/~D"))' : ''}`;
const workloads = [
  ['format-string', '(format nil "~D/~D" 17 23)', '"17/23"'],
  ['format-iteration', '(format nil "~{~D~^,~}" \'(1 2 3 4 5))', '"1,2,3,4,5"'],
  ...(formatter ? [
    ['formatter-control', '(format nil *format-function* 17 23)', '"17/23"'],
    ['formatter-tail', '(let ((tail (funcall *format-function* *format-stream* 17 23 31))) (get-output-stream-string *format-stream*) tail)', "'(31)"],
    ['formatter-create', '(functionp (formatter "~D/~D"))', 't'],
  ] : []),
];
const body = form => `(dotimes (i ${calls}) (setq *format-result* ${form}))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixture}`);
  for (const [name, form, expected] of workloads) console.log(`
    (let ((fn (compile nil '(lambda () ${body(form)}))))
      (funcall fn) (assert (equal *format-result* ${expected}))
      (format t "${name} (${calls} calls): ~,3F ms~%"
        (loop repeat 3 minimize (let ((start (get-internal-real-time)))
          (dotimes (sample 100) (funcall fn))
          (* 10.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixture);
  const milliseconds = {};
  for (const [name, form, expected] of workloads) {
    vm.evaluate(`(defun format-benchmark-run () ${body(form)})`);
    const run = () => {
      vm.evaluate('(format-benchmark-run)');
      assert.equal(vm.evaluate(`(equal *format-result* ${expected})`).output, 'T\n', name);
    };
    run(); const times = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); times.push(performance.now() - start);}
    milliseconds[name] = Math.min(...times);
  }
  let retainedFormatterBytes;
  if (args.includes('--retain-formatters')) {
    assert.equal(typeof global.gc, 'function', 'Use --expose-gc for retained formatters.');
    vm.evaluate(`(defparameter *retained-formatters* (make-array ${calls}))`);
    global.gc(); const before = process.memoryUsage().heapUsed;
    vm.evaluate(`(dotimes (i ${calls}) (setf (aref *retained-formatters* i) (formatter "~D/~D")))`);
    global.gc(); retainedFormatterBytes = process.memoryUsage().heapUsed - before;
    assert.equal(vm.evaluate('(equal (format nil (aref *retained-formatters* 9999) 17 23) "17/23")').output, 'T\n');
  }
  console.log(JSON.stringify({binary, calls, milliseconds, retainedFormatterBytes}, null, 2));
}
