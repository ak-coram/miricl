#!/usr/bin/env node
// Backquote construction and repeated macro expansion, with compiled host forms.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const fixtures = `(defparameter *macro-operation* nil)
  (defparameter *macro-result* nil)
  (defun construct-template (value &rest forms)
    \`(let ((result ,value)) ,@forms result))
  (defun benchmark-classify (x) (case x ((#\\0 a) 1) ((#\\1 b) 2) (otherwise 3)))
  (defun benchmark-type-classify (x) (typecase x (character 1) (symbol 2) (t 3)))
  (defmacro expanding-wrapper (form &rest cases)
    \`(let () (handler-case ,form ,@cases)))`;
const workloads = [
  ['template-construction', `(setq *macro-operation* #'construct-template)`, `(progn
    (dotimes (i 10000) (setq *macro-result* (funcall *macro-operation* i '(incf result))))
    (length *macro-result*))`, 4],
  ['repeated-expansion', '(values)', `(let ((sum 0))
    (dotimes (i 10000) (incf sum (expanding-wrapper (1+ i) (arithmetic-error () 0)))) sum)`, 50005000],
  ['loop-expansion', '(values)', `(let ((sum 0))
    (dotimes (j 1000) (incf sum (loop for i from 0 below 10 sum i))) sum)`, 45000, 1000],
  ['case-selection', "(setq *macro-operation* #'benchmark-classify)", `(let ((sum 0))
    (dotimes (i 10000) (incf sum (funcall *macro-operation* (if (oddp i) #\\0 'b)))) sum)`, 15000],
  ['typecase-selection', "(setq *macro-operation* #'benchmark-type-classify)", `(let ((sum 0))
    (dotimes (i 100000) (incf sum (funcall *macro-operation* (if (oddp i) #\\0 'b)))) sum)`, 150000, 100000],
];
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, setup, source, expected] of workloads) {
    console.log(`${setup}
      (let ((fn (compile nil '(lambda () ${source}))))
        (assert (= ${expected} (funcall fn)))
        (format t "${name}: ~,3F ms~%"
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
  for (const [name, setup, source, expected, iterations = 10000] of workloads) {
    vm.evaluate(`${setup} (defun macro-run () ${source})`);
    const run = () => assert.equal(vm.evaluate('(macro-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {iterations, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
