#!/usr/bin/env node
// Native array construction and the reader workload unlocked by MAKE-ARRAY.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const host = args.includes('--sbcl-source');
const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl-interpreter.wasm';
const only = args.find(arg => arg.startsWith('--only='))?.slice(7);
const fixture = `(defparameter *bench-operation* nil) (defparameter *bench-result* nil)
(defparameter *bench-bit-a* (make-array 65536 :element-type 'bit :initial-element 1))
(defparameter *bench-bit-b* (make-array 65536 :element-type 'bit :initial-element 0))
(defparameter *bench-bit-base-a* (make-array 65568 :element-type 'bit :initial-element 1))
(defparameter *bench-bit-base-b* (make-array 65568 :element-type 'bit :initial-element 0))
(defparameter *bench-bit-base-result* (make-array 65568 :element-type 'bit :initial-element 0))
(defparameter *bench-bit-view-a* (make-array 65536 :element-type 'bit :displaced-to *bench-bit-base-a* :displaced-index-offset 1))
(defparameter *bench-bit-view-b* (make-array 65536 :element-type 'bit :displaced-to *bench-bit-base-b* :displaced-index-offset 7))
(defparameter *bench-bit-view-result* (make-array 65536 :element-type 'bit :displaced-to *bench-bit-base-result* :displaced-index-offset 31))
(defun array-vector-stack (n)
  (let ((a (make-array 0 :adjustable t :fill-pointer 0)))
    (dotimes (i n) (vector-push-extend i a))
    (dotimes (i n) (vector-pop a))
    a))
(defun array-symbol-reader (n)
  (let* ((p (make-package "ARRAY-SYMBOL-READER" :use '(:cl))) (*package* p))
    (unwind-protect
      (loop for i below n for c = (code-char i)
        for str = (and c (make-array 2 :element-type 'character :initial-contents (list #\\\\ c)))
        for sym = (and c (read-from-string str))
        unless (or (not c) (and (symbolp sym) (eql sym (find-symbol (string c)))
                               (equal (symbol-name sym) (string c))))
        collect (list i c str sym))
      (delete-package p))))`;
const workloads = [
  ['small-character-array', 'make-array', "2 :element-type 'character :initial-contents '(#\\a #\\b)", '(array-total-size *bench-result*)', 2, 100000],
  ['general-vector', 'make-array', '256 :initial-element 7', '(array-total-size *bench-result*)', 256, 10000],
  ['matrix', 'make-array', "'(8 8) :initial-element 7", '(array-total-size *bench-result*)', 64, 10000],
  ['bit-vector', 'make-array', "1024 :element-type 'bit :initial-element 0", '(array-total-size *bench-result*)', 1024, 10000],
  ['bit-xor-simple', 'bit-xor', '*bench-bit-a* *bench-bit-b*', '(array-total-size *bench-result*)', 65536, 1000],
  ['bit-xor-unaligned', 'bit-xor', '*bench-bit-view-a* *bench-bit-view-b* *bench-bit-view-result*', '(array-total-size *bench-result*)', 65536, 1000],
  ['replace-bit-unaligned', 'replace', '*bench-bit-view-result* *bench-bit-view-a*', '(bit *bench-result* 65535)', 1, 100],
  ['vector-stack-growth', 'array-vector-stack', '100000', '(fill-pointer *bench-result*)', 0, 1],
  ['symbol-reader', 'array-symbol-reader', '65536', '(if (null *bench-result*) 0 -1)', 0, 1],
].filter(([name]) => !only || name.includes(only));
const body = (values, result, count) => `(progn
  (dotimes (i ${count}) (setq *bench-result* (funcall *bench-operation* ${values}))) ${result})`;
if (host) {
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixture}`);
  for (const [name, operator, values, result, expected, count] of workloads) {
    console.log(`(setq *bench-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(values, result, count)}))))
        (assert (= ${expected} (funcall fn)))
        (format t "${name} (${count} calls): ~,3F ms~%"
          (loop repeat 3 minimize
            (let ((start (get-internal-real-time)))
              (assert (= ${expected} (funcall fn)))
              (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  vm.evaluate(fixture);
  const results = {};
  for (const [name, operator, values, result, expected, count] of workloads) {
    vm.evaluate(`(setq *bench-operation* #'${operator}) (defun array-benchmark-run () ${body(values, result, count)})`);
    const run = () => assert.equal(vm.evaluate('(array-benchmark-run)').output, `${expected}\n`, name);
    run();
    const times = [];
    for (let i = 0; i < 3; i++) {
      const start = performance.now(); run(); times.push(performance.now() - start);
    }
    results[name] = {iterations: count, milliseconds: Math.min(...times)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
