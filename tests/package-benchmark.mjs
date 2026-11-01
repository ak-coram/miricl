#!/usr/bin/env node
// Exclude symbol population; compare the same target-package membership counts.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const fixtures = `(defparameter *enumeration-package* (make-package "ENUMERATION-BENCH" :use nil))
  (dotimes (i 10000) (intern (format nil "S~D" i) *enumeration-package*))`;
const workloads = [
  ['apropos-empty', '(length (apropos-list "" *enumeration-package*))'],
  ['apropos-case-folded', '(length (apropos-list "s" *enumeration-package*))'],
  ['apropos-substring', '(length (apropos-list "99" *enumeration-package*))', 280],
  ['present-symbols', '(let ((n 0)) (do-symbols (s *enumeration-package* n) (incf n)))'],
  ['package-iterator', `(let ((n 0))
    (with-package-iterator (next *enumeration-package* :internal)
      (loop (unless (next) (return)) (incf n))) n)`],
  ['all-symbols', `(let ((n 0)) (do-all-symbols (s n)
    (when (eq (symbol-package s) *enumeration-package*) (incf n))))`],
];
const calls = 10;
const body = source => `(let ((result 0)) (dotimes (i ${calls}) (setq result ${source})) result)`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, source, expected = 10000] of workloads) {
    console.log(`(let ((fn (compile nil '(lambda () ${body(source)}))))
      (assert (= ${expected} (funcall fn)))
      (format t "${name} (${calls} calls): ~,3F ms~%"
        (loop repeat 3 minimize
          (let ((start (get-internal-real-time))) (assert (= ${expected} (funcall fn)))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, source, expected = 10000] of workloads) {
    vm.evaluate(`(defun enumeration-run () ${body(source)})`);
    const run = () => assert.equal(vm.evaluate('(enumeration-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {calls, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
