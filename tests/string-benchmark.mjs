#!/usr/bin/env node
// The same Lisp workloads run in WASM and compiled SBCL. Emit the host script
// with --sbcl-source; run it separately so no child process is required here.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const host = args.includes('--sbcl-source');
const binary = args.find(arg => !arg.startsWith('--')) ?? 'interpreter.wasm';
const long = 'a'.repeat(32768);
const fixtures = `(defparameter *bench-short-a* "AbCdEfGh")
  (defparameter *bench-short-b* "aBcDeFgH")
  (defparameter *bench-long-a* "${long}")
  (defparameter *bench-long-copy* (copy-seq *bench-long-a*))
  (defparameter *bench-long-b* "${long.slice(0, -1)}b")
  (defparameter *bench-unicode-a* "${'ÉΛЖ𐐀'.repeat(4096)}")
  (defparameter *bench-unicode-b* "${'éλж𐐨'.repeat(4096)}")`;
const workloads = [
  ['loop', 'nil', 'NIL'],
  ['short-fold', '(string-equal *bench-short-a* *bench-short-b*)', 'T'],
  ['short-bounds', '(string-equal *bench-short-a* *bench-short-b* :start1 2 :end1 6 :start2 2 :end2 6)', 'T'],
  ['long-identical', '(string= *bench-long-a* *bench-long-a*)', 'T'],
  ['long-equal', '(string= *bench-long-a* *bench-long-copy*)', 'T'],
  ['long-order', '(string< *bench-long-a* *bench-long-b*)', '32767'],
  ['unicode-fold', '(string-equal *bench-unicode-a* *bench-unicode-b*)', 'T'],
];
const iterations = name => name.startsWith('long-') || name === 'unicode-fold' ? 2000 : 200000;
const body = (name, form) => `(let ((result nil)) (dotimes (i ${iterations(name)} result) (setq result ${form})))`;

if (host) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, form, expected] of workloads) {
    console.log(`(let ((fn (compile nil '(lambda () ${body(name, form)}))))
      (assert (eql (funcall fn) ${expected}))
      (format t "${name} (${iterations(name)} calls): ~,3F ms~%"
        (loop repeat 3 minimize
          (let ((start (get-internal-real-time)))
            (assert (eql (funcall fn) ${expected}))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures, {eof: true});
  const results = {};
  for (const [name, form, expected] of workloads) {
    const operator = form.match(/^\(([^ ]+)/)?.[1];
    if (operator && vm.evaluate(`(fboundp '${operator})`).output === 'NIL\n') {
      results[name] = {unavailable: operator};
      continue;
    }
    vm.evaluate(`(defun string-benchmark-run () ${body(name, form)})`);
    const run = () => assert.equal(vm.evaluate('(string-benchmark-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now();
      run();
      times.push(performance.now() - start);
    }
    results[name] = {iterations: iterations(name), milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
