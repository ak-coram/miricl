#!/usr/bin/env node
// Measure the real upstream RT registration/comparison workload in WASM.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const binary = process.argv[2] ?? 'miricl.wasm';
const count = Number(process.argv[3] ?? 120);
assert.ok(Number.isSafeInteger(count) && count > 0, 'Test count must be a positive integer.');
const started = performance.now();
const vm = await Miricl.instantiate(fs.readFileSync(binary));
for (const file of ['rt-package.lsp', 'rt.lsp']) {
  vm.mountFile(file, fs.readFileSync(new URL(`./ansi-test/${file}`, import.meta.url), 'utf8'));
}
vm.evaluate(`(load "rt-package.lsp") (load "rt.lsp")
  (defpackage :cl-test (:use :cl :rt)) (in-package :cl-test) (defvar archive nil)`);
const startupMs = performance.now() - started;
let phase = performance.now();
for (let i = 0; i < count; i++) {
  vm.evaluate(`(progn (deftest engine-${i} (+ 20 22) 42)
    (push (rt::get-entry 'engine-${i}) archive) (rt:rem-all-tests))`);
}
vm.evaluate('(progn (setq archive (nreverse archive)) (values))');
const registrationMs = performance.now() - phase;
phase = performance.now();
let passed = 0;
for (let i = 0; i < count; i++) {
  const result = vm.evaluate(`(progn (rt:rem-all-tests)
    (let ((entry (pop archive))) (rt::add-entry entry)
      (if (rt:do-test (rt::name entry)) :pass :fail)))`);
  if (result.output.endsWith(':PASS\n')) passed++;
}
const testMs = performance.now() - phase;
assert.equal(passed, count);
console.log(JSON.stringify({binary, tests: count, passed, startupMs, registrationMs, testMs}, null, 2));
