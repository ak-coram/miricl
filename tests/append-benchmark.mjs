#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const source = `(defun append-benchmark-run ()
  (loop for n below 2048 count
    (not (equal (apply #'append (loop for i from 1 to n collect '(a)))
                (make-list n :initial-element 'a)))))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${source}
    (assert (zerop (append-benchmark-run)))
    (format t "append-6: ~,3F ms~%"
      (loop repeat 3 minimize (let ((start (get-internal-real-time)))
        (assert (zerop (append-benchmark-run)))
        (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))`);
} else {
  const binary = args[0] ?? 'miricl.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(source);
  const run = () => assert.equal(vm.evaluate('(append-benchmark-run)').output, '0\n');
  run(); const samples = [];
  for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
  console.log(JSON.stringify({binary, milliseconds: Math.min(...samples)}, null, 2));
}
