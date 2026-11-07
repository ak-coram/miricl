#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const fixtures = `(defparameter *compare-string* "${'aBλ🙂'.repeat(8192)}")
  (defparameter *compare-copy* (copy-seq *compare-string*))
  (defparameter *compare-fold* (string-upcase *compare-string*))
  (defparameter *compare-list* (loop for i below 10000 collect i))
  (defparameter *compare-list-copy* (copy-list *compare-list*))
  (defparameter *compare-repetitive* "${'a'.repeat(16384)}b")
  (defparameter *compare-pattern* "${'a'.repeat(127)}b")
  (defparameter *compare-operation* nil) (defparameter *compare-result* nil)`;
const workloads = [
  ['string-eql', 'mismatch', '*compare-string* *compare-copy*', 100, 'NIL'],
  ['string-fold', 'mismatch', "*compare-string* *compare-fold* :test #'char-equal", 100, 'NIL'],
  ['list-eql', 'mismatch', '*compare-list* *compare-list-copy*', 100, 'NIL'],
  ['list-reverse', 'mismatch', '*compare-list* *compare-list-copy* :from-end t', 100, 'NIL'],
  ['list-key', 'mismatch', "*compare-list* *compare-list-copy* :key #'oddp", 100, 'NIL'],
  ['short-call', 'mismatch', "'(1 2 3) '(1 2 4)", 10000, '2'],
  ['search-repetitive', 'search', '*compare-pattern* *compare-repetitive*', 20, '16257'],
  ['search-absent', 'search', '"zz" *compare-string*', 100, 'NIL'],
  ['search-reverse', 'search', '"λ🙂" *compare-string* :from-end t', 1000, '32766'],
];
const body = (values, calls) => `(progn (dotimes (i ${calls})
  (setq *compare-result* (funcall *compare-operation* ${values}))) *compare-result*)`;
if (args.includes('--sbcl-source')) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, values, calls, expected] of workloads) {
    console.log(`(setq *compare-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values, calls)}))))
        (assert (eql ${expected} (funcall fn)))
        (format t "${name} (${calls} calls): ~,3F ms~%"
          (loop repeat 3 minimize (let ((start (get-internal-real-time)))
            (assert (eql ${expected} (funcall fn)))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args[0] ?? 'miricl-interpreter.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, operator, values, calls, expected] of workloads) {
    vm.evaluate(`(setq *compare-operation* #'${operator}) (defun compare-run () ${body(values, calls)})`);
    const run = () => assert.equal(vm.evaluate('(compare-run)').output, `${expected}\n`, name);
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    results[name] = {calls, milliseconds: Math.min(...samples)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
