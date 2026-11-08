#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const args = process.argv.slice(2);
const workloads = [
  ['wide-byte', '(signed-byte 65536)', '(loop repeat 5000 always (typep -17 type))', 'T'],
  ['fixnum', 'fixnum', '(let ((n 0)) (dotimes (i 10000) (when (typep i type) (incf n))) n)', '10000'],
  ['integer-subtype', '(integer 0 20)', "(loop repeat 5000 always (subtypep '(integer 0 10) type))", 'T'],
];
if (args.includes('--subtypes')) workloads.push(
  ['float-subtype', '(single-float 0.0 20.0)', "(loop repeat 5000 always (subtypep '(single-float 0.0 10.0) type))", 'T'],
  ['cons-subtype', '(cons number atom)', "(loop repeat 5000 always (subtypep '(cons integer symbol) type))", 'T'],
  ['member-subtype', '(integer 0 20)', "(loop repeat 5000 always (subtypep '(member 1 2 3) type))", 'T']);
if (args.includes('--arrays')) workloads.push(
  ['array-rank', '(array t 2)', "(loop repeat 5000 always (subtypep '(array t (17 23)) type))", 'T'],
  ['array-shape', '(simple-array bit (* 23))', "(loop repeat 5000 always (subtypep '(simple-array bit (17 23)) type))", 'T'],
  ['array-negative', '(array character (*))', "(loop repeat 5000 always (not (subtypep '(array t (17)) type)))", 'T'],
  ['array-intersection', '(array bit (17 23))', "(loop repeat 5000 always (subtypep '(and (array bit (17 *)) (array bit (* 23))) type))", 'T']);
if (args.includes('--arrays') || args.includes('--array-upgrades')) workloads.push(
  ['array-upgrade', '(integer 0 1)', "(loop repeat 5000 always (eq (upgraded-array-element-type type) 'bit))", 'T']);
if (args.includes('--complex')) workloads.push(
  ['complex-subtype', '(complex real)', "(loop repeat 5000 always (subtypep '(complex (integer 0 10)) type))", 'T'],
  ['complex-negative', '(complex float)', "(loop repeat 5000 always (not (subtypep '(complex rational) type)))", 'T']);
if (args.includes('--complex') || args.includes('--complex-upgrades')) workloads.push(
  ['complex-upgrade', '(double-float 0d0 20d0)', "(loop repeat 5000 always (eq (upgraded-complex-part-type type) 'double-float))", 'T']);
if (args.includes('--sbcl-source')) {
  for (const [name, type, body, expected] of workloads) {
    console.log(`(let ((fn (compile nil '(lambda (type) ${body}))))
      (assert (equal (funcall fn '${type}) ${expected}))
      (format t "${name}: ~,3F ms~%" (loop repeat 3 minimize
        (let ((start (get-internal-real-time))) (dotimes (i 10) (funcall fn '${type}))
          (* 100.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
} else {
  const binary = args.find(arg => !arg.startsWith('--')) ?? 'miricl.wasm';
  const vm = await Miricl.instantiate(fs.readFileSync(binary));
  const results = {};
  for (const [name, type, body, expected] of workloads) {
    vm.evaluate(`(defun type-benchmark-run (type) ${body})`);
    const run = () => assert.equal(vm.evaluate(`(type-benchmark-run '${type})`).output, `${expected}\n`, name);
    run(); const samples = [];
    for (let i = 0; i < 3; i++) {const start = performance.now(); run(); samples.push(performance.now() - start);}
    results[name] = {milliseconds: Math.min(...samples)};
  }
  console.log(JSON.stringify({binary, results}, null, 2));
}
