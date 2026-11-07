#!/usr/bin/env node
// The RATIONAL.2 ANSI workload: 80,004 float coercions and round trips.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const conversion = args.includes('--rationalize') ? 'rationalize' : 'rational';
const source = `(defun numeric-benchmark-run ()
  (loop for type in '(short-float single-float double-float long-float)
    collect (loop for i from -10000 to 10000
      for x = (coerce i type) for r = (${conversion} x) count (not (eql r i)))))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${source}
    (assert (equal '(0 0 0 0) (numeric-benchmark-run)))
    (format t "float-roundtrip (80004 calls): ~,3F ms~%"
      (loop repeat 3 minimize
        (let ((start (get-internal-real-time)))
          (assert (equal '(0 0 0 0) (numeric-benchmark-run)))
          (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))`);
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl-interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(source);
  const run = () => assert.equal(vm.evaluate('(numeric-benchmark-run)').output, '(0 0 0 0)\n');
  run();
  const times = [];
  for (let i = 0; i < 3; i++) {
    const start = performance.now(); run(); times.push(performance.now() - start);
  }
  console.log(JSON.stringify({binary, calls: 80004, milliseconds: Math.min(...times)}, null, 2));
}
