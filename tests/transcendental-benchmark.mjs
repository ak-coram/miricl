#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
const calls = 10000;
const fixture = `(defparameter *math-real* 1.25d0)
  (defparameter *math-small* 0.25d0)
  (defparameter *math-complex* #c(1.25d0 2.5d0))
  (defparameter *math-large* 1d300)
  (defparameter *math-result* nil)`;
let workloads = [
  ['log-real', '(log *math-real*)', '(and (typep *math-result* \'double-float) (< (abs (- *math-result* 0.22314355131420976d0)) 1d-15))'],
  ['log-complex', '(log *math-complex*)', '(and (typep *math-result* \'(complex double-float)) (< (abs (- *math-result* #c(1.02786250753126d0 1.1071487177940904d0))) 1d-14))'],
  ['phase-complex', '(phase *math-complex*)', '(< (abs (- *math-result* 1.1071487177940904d0)) 1d-15)'],
  ['atan-real', '(atan *math-real*)', '(< (abs (- *math-result* 0.8960553845713439d0)) 1d-15)'],
  ['atan-complex', '(atan *math-complex*)', '(< (abs (- *math-result* #c(1.3949391589068967d0 0.3218222093361104d0))) 1d-14)'],
];
if (args.includes('--elementary')) {
  const values = [
    ['sin', '0.9489846193555862d0', '#c(5.819448397636909d0 1.9077647699359064d0)'],
    ['cos', '0.3153223623952687d0', '#c(1.9336480056192062d0 -5.741550996463005d0)'],
    ['tan', '3.0095696738628313d0', '#c(0.008152593477633623d0 1.0108216673671506d0)'],
    ['exp', '3.4903429574618414d0', '#c(-2.796265976439757d0 2.088873033410333d0)'],
    ['sinh', '1.6019190803008256d0', '#c(-1.2833672438058223d0 1.1301690867416598d0)'],
    ['cosh', '1.8884238771610158d0', '#c(-1.5128987326339347d0 0.9587039466686733d0)'],
    ['tanh', '0.8482836399575129d0', '#c(0.9429940867424084d0 -0.14945939818643733d0)'],
  ];
  workloads = values.flatMap(([operator, real, complex]) => [
    [`${operator}-real`, `(${operator} *math-real*)`, `(< (abs (- *math-result* ${real})) 1d-14)`],
    [`${operator}-complex`, `(${operator} *math-complex*)`, `(< (abs (- *math-result* ${complex})) 1d-13)`],
  ]);
  workloads.push(['sin-large', '(sin *math-large*)', '(< (abs (- *math-result* -0.8178819121159085d0)) 1d-14)']);
}
if (args.includes('--inverse')) {
  const values = [
    ['asin', '*math-small*', '0.25268025514207865d0', '#c(0.4394795763507196d0 1.740545149840038d0)'],
    ['acos', '*math-small*', '1.318116071652818d0', '#c(1.131316750444177d0 -1.740545149840038d0)'],
    ['asinh', '*math-real*', '1.0475930126492587d0', '#c(1.7023497340243106d0 1.0800415375009798d0)'],
    ['acosh', '*math-real*', '0.6931471805599453d0', '#c(1.740545149840038d0 1.131316750444177d0)'],
    ['atanh', '*math-small*', '0.25541281188299536d0', '#c(0.14584412860614157d0 1.2542231021472243d0)'],
  ];
  workloads = values.flatMap(([operator, argument, real, complex]) => [
    [`${operator}-real`, `(${operator} ${argument})`, `(< (abs (- *math-result* ${real})) 1d-14)`],
    [`${operator}-complex`, `(${operator} *math-complex*)`, `(< (abs (- *math-result* ${complex})) 1d-13)`],
  ]);
}
const body = form => `(dotimes (i ${calls}) (setq *math-result* ${form}))`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixture}`);
  for (const [name, form, check] of workloads) console.log(`
    (let ((fn (compile nil '(lambda () ${body(form)}))))
      (funcall fn) (assert ${check})
      (format t "${name} (${calls} calls): ~,3F ms~%"
        (loop repeat 3 minimize (let ((start (get-internal-real-time)))
          (dotimes (sample 50) (funcall fn))
          (* 20.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixture);
  const milliseconds = {};
  for (const [name, form, check] of workloads) {
    vm.evaluate(`(defun math-benchmark-run () ${body(form)})`);
    const run = () => {
      vm.evaluate('(math-benchmark-run)');
      assert.equal(vm.evaluate(check).output, 'T\n', name);
    };
    for (let warmup = 0; warmup < 5; warmup++) run();
    const samples = [];
    for (let sample = 0; sample < 3; sample++) {
      const start = performance.now(); run(); samples.push(performance.now() - start);
    }
    milliseconds[name] = Math.min(...samples);
  }
  console.log(JSON.stringify({binary, calls, milliseconds}, null, 2));
}
