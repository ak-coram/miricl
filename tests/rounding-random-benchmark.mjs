#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const calls = 100000;
const fixtures = `(defparameter *arithmetic-operation* nil)
  (defparameter *arithmetic-result* nil)
  (defparameter *arithmetic-state* (make-random-state t))
  (defparameter *arithmetic-big* (1- (expt 2 100)))`;
const workloads = [
  ['identity-control', 'identity', '17', '(= *arithmetic-result* 17)'],
  ['integer-floor', 'floor', '1000000000 97', '(= *arithmetic-result* 10309278)'],
  ['integer-mod', 'mod', '1000000000 97', '(= *arithmetic-result* 34)'],
  ['bignum-truncate', 'truncate', '*arithmetic-big* 97', '(= *arithmetic-result* 13068562888950818572130960880)'],
  ['double-round', 'round', '12345.25d0 3d0', '(= *arithmetic-result* 4115)'],
  ['integer-lcm', 'lcm', '1000000000 97', '(= *arithmetic-result* 97000000000)'],
  ['integer-isqrt', 'isqrt', '1000000000', '(= *arithmetic-result* 31622)'],
  ['bignum-isqrt', 'isqrt', '*arithmetic-big*', '(= *arithmetic-result* 1125899906842623)'],
  ['double-sqrt', 'sqrt', '152399025d0', '(= *arithmetic-result* 12345d0)'],
  ['complex-sqrt', 'sqrt', '#c(-3d0 4d0)', '(= *arithmetic-result* #c(1d0 2d0))'],
  ['complex-signum', 'signum', '#c(3d0 4d0)', '(< (abs (- *arithmetic-result* #c(.6d0 .8d0))) 1d-15)'],
  ['random-integer', 'random', '1000000000 *arithmetic-state*', '(<= 0 *arithmetic-result* 999999999)'],
  ['random-bignum', 'random', '*arithmetic-big* *arithmetic-state*', '(< -1 *arithmetic-result* *arithmetic-big*)'],
  ['random-double', 'random', '1d0 *arithmetic-state*', '(<= 0d0 *arithmetic-result* 1d0)'],
];
const body = (values, check) => `(progn (dotimes (i ${calls})
  (setq *arithmetic-result* (funcall *arithmetic-operation* ${values}))) ${check})`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, values, check] of workloads) {
    console.log(`(setq *arithmetic-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values, check)}))))
        (assert (funcall fn))
        (format t "${name} (${calls} calls): ~,3F ms~%"
          (loop repeat 3 minimize (let ((start (get-internal-real-time)))
            (assert (funcall fn))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args[0] ?? 'miricl-interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const milliseconds = {};
  for (const [name, operator, values, check] of workloads) {
    vm.evaluate(`(setq *arithmetic-operation* #'${operator}) (defun arithmetic-run () ${body(values, check)})`);
    const run = () => assert.equal(vm.evaluate('(arithmetic-run)').output, 'T\n', name);
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
