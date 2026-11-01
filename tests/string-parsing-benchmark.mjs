#!/usr/bin/env node
// Identical dynamically selected operations in WASM and compiled SBCL;
// --sbcl-source emits the host script without launching a subprocess.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
const fixtures = `(defparameter *parsing-operation* nil) (defparameter *parsing-result* nil)
  (defparameter *trim-ascii* "${'ab'.repeat(16384)}middle${'ba'.repeat(16384)}")
  (defparameter *trim-unicode* "${'λ🙂'.repeat(16384)}middle${'🙂λ'.repeat(16384)}")
  (defparameter *parse-long* "${'1234567890'.repeat(100)}")
  (defparameter *parse-displaced* (make-array 3 :element-type 'character
    :displaced-to "x123y" :displaced-index-offset 1))`;
const workloads = [
  ['short-trim', 100000, 'string-trim', '"ab" "abcaba"', '(equal *parsing-result* "c")'],
  ['ascii-trim', 100, 'string-trim', '"ab" *trim-ascii*', '(equal *parsing-result* "middle")'],
  ['unicode-trim-list', 100, 'string-trim', "'(#\\λ #\\🙂) *trim-unicode*", '(equal *parsing-result* "middle")'],
  ['short-parse', 100000, 'parse-integer', '"-12345678"', '(= *parsing-result* -12345678)'],
  ['displaced-parse', 100000, 'parse-integer', '*parse-displaced*', '(= *parsing-result* 123)'],
  ['long-parse', 100, 'parse-integer', '*parse-long*', '(= (mod *parsing-result* 100000) 67890)'],
];
const body = (calls, values, check) => `(progn (dotimes (i ${calls})
  (setq *parsing-result* (funcall *parsing-operation* ${values}))) ${check})`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, calls, operator, values, check] of workloads) {
    console.log(`(setq *parsing-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(calls, values, check)}))))
        (assert (funcall fn))
        (format t "${name} (${calls} calls): ~,3F ms~%"
          (loop repeat 3 minimize (let ((start (get-internal-real-time)))
            (assert (funcall fn))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, calls, operator, values, check] of workloads) {
    vm.evaluate(`(setq *parsing-operation* #'${operator}) (defun parsing-run () ${body(calls, values, check)})`);
    const run = () => assert.equal(vm.evaluate('(parsing-run)').output, 'T\n', name);
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    results[name] = {calls, milliseconds: Math.min(...samples)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
