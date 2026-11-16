import fs from 'node:fs';
import assert from 'node:assert/strict';
import {runUpstream} from './ansi-upstream.mjs';
import {correctUpstreamTestInput} from './ansi-full.mjs';

const binary = process.argv[2] ?? 'miricl-interpreter.wasm';
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
const report = await runUpstream({binary, sources, timeout: 150,
  onProgress: current => updates.push({...current.progress})});
assert.equal(report.error, null);
assert.deepEqual(report.tests.map(test => test.status), ['pass', 'fail', 'fail', 'timeout', 'pass', 'pass', 'pass']);
assert.ok(report.tests.every(test => test.file === 'fixture.lsp'));
assert.ok(report.tests.every(test => Number.isFinite(test.elapsedMs) && test.elapsedMs >= 0));
assert.equal(report.summary.complete, true);
assert.equal(report.summary.expected, report.tests.length, 'Discover the total from upstream registration.');
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
const expanded = await runUpstream({binary, sources: sources.map(([file, source]) => [file,
  file === 'fixture.lsp' ? source + '\n(deftest newly-added-test (later-helper) 42)\n' : source]), timeout: 150});
assert.equal(expanded.error, null);
assert.equal(expanded.summary.complete, true);
assert.equal(expanded.summary.expected, report.summary.expected + 1);
assert.equal(expanded.summary.total, expanded.summary.expected);
assert.equal(expanded.tests.at(-1).status, 'pass');
const missing = await runUpstream({binary, sources, expectedTests: 8});
assert.equal(missing.summary.complete, false);
assert.match(missing.error, /registered 7 tests; expected 8/);
const broken = await runUpstream({binary, sources: [...sources.filter(([file]) => file !== 'init.lsp'),
  ['init.lsp', '(load "missing.lsp")']]});
assert.equal(broken.summary.complete, false);
assert.equal(broken.loadErrors.length, 1);
assert.match(broken.error, /initialization failed/);
console.log('Upstream ANSI runner: normal loading, guest inventory, forward definitions, failure details, individual timeouts, sandbox defaults, and incomplete-run accounting passed.');

// Force both RANDOM endpoints so a rare invalid printer-test input is tested
// deterministically. One sample per selected upstream assertion is sufficient
// here; the ordinary ANSI suite retains its complete sampling loops.
const printerFile = 'printer/print-floats.lsp';
const originalPrinter = fs.readFileSync(new URL(`./ansi-test/${printerFile}`, import.meta.url), 'utf8');
const corrections = [];
const correctedPrinter = correctUpstreamTestInput(printerFile, originalPrinter, value => corrections.push(value));
assert.ok(corrections.length, 'Report the upstream printer input correction.');
const endpoints = [
  {source: originalPrinter, upper: false, status: 'fail'},
  {source: correctedPrinter, upper: false, status: 'pass'},
  {source: correctedPrinter, upper: true, status: 'pass'},
];
let originalNames;
for (const {source, upper, status} of endpoints) {
  const fixture = source.replace(/\brepeat\s+\d+/gi, 'repeat 1');
  const printerReport = await runUpstream({binary,
    sources: [...sources.filter(([file]) => ['rt-package.lsp', 'rt.lsp'].includes(file)),
      ['float-input.lsp', fixture], ['init.lsp', `
        (load "rt-package.lsp") (load "rt.lsp")
        (defpackage :cl-test (:use :cl :rt))
        (in-package :cl-test)
        (with-open-file (input "float-input.lsp")
          (loop for form = (read input nil :eof) until (eq form :eof) do
            (when (or (eq (car form) 'defparameter)
                      (and (eq (car form) 'deftest)
                        (let* ((name (symbol-name (second form)))
                               (dot (position #\\. name :from-end t)))
                          (and dot (member (subseq name (1+ dot)) '("2" "4") :test #'equal)))))
              (eval form))))
        (setf (symbol-function 'random)
          (lambda (limit &optional state)
            (declare (ignore state))
            ${upper ? '(1- limit)' : '(declare (ignore limit)) 0'}))
      `]],
  });
  assert.equal(printerReport.error, null);
  assert.ok(printerReport.tests.length, 'Discover the upstream floating-point printer assertions.');
  const names = printerReport.tests.map(test => test.name);
  originalNames ??= names;
  assert.deepEqual(names, originalNames, 'Input correction preserves every selected upstream assertion.');
  assert.ok(printerReport.tests.every(test => test.status === status), JSON.stringify(printerReport.tests));
}
console.log('Upstream ANSI runner: floating-point printer input correction preserves assertions and checks both random endpoints.');
