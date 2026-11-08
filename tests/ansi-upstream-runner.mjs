import fs from 'node:fs';
import assert from 'node:assert/strict';
import {runUpstream} from './ansi-upstream.mjs';

const binary = process.argv[2] ?? 'miricl.wasm';
const sources = ['rt-package.lsp', 'rt.lsp'].map(file => [file,
  fs.readFileSync(new URL(`./ansi-test/${file}`, import.meta.url), 'utf8')]);
sources.push(['sandbox/fixture.txt', 'fixture\n'], ['init.lsp', `
  (load "rt-package.lsp") (load "rt.lsp")
  (defpackage :cl-test (:use :cl :rt))
  (load "fixture.lsp")
`], ['fixture.lsp', `
  (in-package :cl-test)
  (deftest forward-helper (later-helper) 42)
  (deftest wrong-value (+ 1 1) 3)
  (deftest undefined (missing-function) t)
  (deftest hangs (loop do (values)) t)
  (deftest after-hang (later-helper) 42)
  (deftest sandbox-fixture (with-open-file (s "fixture.txt") (read-line s)) "fixture" nil)
  (deftest get-universal-time.3 (progn (sleep .2) 42) 42)
  (defun later-helper () 42)
`]);
const updates = [];
const report = await runUpstream({binary, sources, timeout: 150, expectedTests: 7,
  onProgress: current => updates.push({...current.progress})});
assert.equal(report.error, null);
assert.deepEqual(report.tests.map(test => test.status), ['pass', 'fail', 'fail', 'timeout', 'pass', 'pass', 'pass']);
assert.ok(report.tests.every(test => test.file === 'fixture.lsp'));
assert.ok(report.tests.every(test => Number.isFinite(test.elapsedMs) && test.elapsedMs >= 0));
assert.equal(report.summary.complete, true);
assert.equal(report.summary.passed, 4);
assert.equal(report.summary.failed, 2);
assert.equal(report.summary.timedOut, 1);
assert.equal(report.recoveries, 0);
assert.ok(report.tests[3].interrupted);
assert.match(report.tests[2].output, /UNDEFINED-FUNCTION/);
assert.equal(updates.at(-1).testsProcessed, 7);
assert.equal(updates.at(-1).passed, 4);
assert.equal(report.tests.at(-1).timeout, 7000);
assert.equal(updates.at(-1).phase, 'finished');
const missing = await runUpstream({binary, sources, expectedTests: 8});
assert.equal(missing.summary.complete, false);
assert.match(missing.error, /registered 7 tests; expected 8/);
const broken = await runUpstream({binary, sources: [...sources.filter(([file]) => file !== 'init.lsp'),
  ['init.lsp', '(load "missing.lsp")']]});
assert.equal(broken.summary.complete, false);
assert.equal(broken.loadErrors.length, 1);
assert.match(broken.error, /initialization failed/);
console.log('Upstream ANSI runner: normal loading, guest inventory, forward definitions, failure details, individual timeouts, sandbox defaults, and incomplete-run accounting passed.');
