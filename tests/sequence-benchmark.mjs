#!/usr/bin/env node
// Compare copying/construction with compiled SBCL using the same Lisp forms.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args = process.argv.slice(2);
const host = args.includes('--sbcl-source');
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
const fixtures = `(defparameter *bench-text* "${'abcd'.repeat(8192)}")
  (defparameter *bench-unicode* "${'ÉλЖ🙂'.repeat(4096)}")
  (defparameter *bench-result* nil) (defparameter *bench-operation* nil)`;
const workloads = [
  ['copy-ascii', 'copy-seq', '*bench-text*', 32768],
  ['copy-unicode', 'copy-seq', '*bench-unicode*', 16384],
  ['substring', 'subseq', '*bench-text* 100 20100', 20000],
  ['concatenate', 'concatenate', "'string *bench-text* *bench-text*", 65536],
  ['make-string', 'make-string', '32768 :initial-element #\\λ', 32768],
  ['short-character-string', 'string', '#\\a', 1, 100000],
  ['short-concatenate', 'concatenate', "'string \"a\" \"b\"", 2, 100000],
  ['short-substring', 'subseq', '"abc" 1', 2, 100000],
  ['short-copy', 'copy-seq', '"abc"', 3, 100000],
];
// A dynamically bound function prevents the host compiler from replacing
// LENGTH(COPY-SEQ(x)) with LENGTH(x) or discarding an unused allocation.
const body = (values, count) => `(progn
  (dotimes (i ${count}) (setq *bench-result* (funcall *bench-operation* ${values})))
  (length *bench-result*))`;

if (host) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for (const [name, operator, values, expected, count = 1000] of workloads) {
    console.log(`(setq *bench-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values, count)}))))
        (assert (= ${expected} (funcall fn)))
        (format t "${name} (${count} calls): ~,3F ms~%"
          (loop repeat 3 minimize
            (let ((start (get-internal-real-time)))
              (assert (= ${expected} (funcall fn)))
              (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixtures);
  const results = {};
  for (const [name, operator, values, expected, count = 1000] of workloads) {
    if (vm.evaluate(`(fboundp '${operator})`).output === 'NIL\n') {
      results[name] = {unavailable: operator};
      continue;
    }
    vm.evaluate(`(setq *bench-operation* #'${operator}) (defun sequence-benchmark-run () ${body(values, count)})`);
    const run = () => assert.equal(vm.evaluate('(sequence-benchmark-run)').output, `${expected}\n`);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {iterations: count, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
