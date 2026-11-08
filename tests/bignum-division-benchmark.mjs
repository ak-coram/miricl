#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const calls = 1000;
let seed = 0x12345678;
const integer = () => {
  let value = 1n;
  for (let i = 0; i < 7; i++) {
    seed ^= seed << 13; seed ^= seed >>> 17; seed ^= seed << 5;
    value = (value << 32n) | BigInt(seed >>> 0);
  }
  return (value & ((1n << 200n) - 1n)) | (1n << 199n);
};
const a = Array.from({length: 32}, integer);
const b = Array.from({length: 32}, integer);
const fixtures = `(defparameter *division-a* #(${a.join(' ')}))
  (defparameter *division-b* #(${b.join(' ')}))
  (defparameter *division-operation* nil) (defparameter *division-result* nil)`;
const workloads = [
  ['gcd-200-bit', 'gcd', '(funcall *division-operation* a b)'],
  ['rational-200-bit', '/', '(funcall *division-operation* a b)'],
  ['oneplus-rational-200-bit', '/', '(1+ (funcall *division-operation* a b))'],
];
const body = expression => `(progn (dotimes (i ${calls})
  (let ((a (aref *division-a* (logand i 31))) (b (aref *division-b* (logand i 31))))
    (setq *division-result* ${expression}))) (rationalp *division-result*))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, expression] of workloads) {
    console.log(`(setq *division-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(expression)}))))
        (assert (funcall fn))
        (format t "${name} (${calls} calls): ~,3F ms~%"
          (loop repeat 3 minimize (let ((start (get-internal-real-time)))
            (assert (funcall fn))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args[0] ?? 'miricl.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const milliseconds = {};
  for (const [name, operator, expression] of workloads) {
    vm.evaluate(`(setq *division-operation* #'${operator}) (defun division-run () ${body(expression)})`);
    const run = () => assert.equal(vm.evaluate('(division-run)').output, 'T\n', name);
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
