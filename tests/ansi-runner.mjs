#!/usr/bin/env node
// Exercise accounting, forward definitions, reader/load failures, and
// recovery after a nonterminating test using the real WASM interpreter.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {runFull} from './ansi-full.mjs';
import {formatProgress} from './ansi-progress.mjs';
import {Miricl} from '../scripts/miricl.mjs';

const binary = process.argv[2] ?? 'miricl.wasm';
const record = (source, tests = [], packageName = 'CL-TEST') => ({
  file: 'runner-fixture.lsp', package: packageName, source,
  tests: tests.map(name => `${packageName}::${name}`),
});
const generated = (source, tests) => ({...record(source, tests),
  testKeys: Object.fromEntries(tests.map(name => [`CL-TEST::${name}`, '(I42;.((I42;.N).N))']))});
const records = [
  record('(defpackage :cl-test (:use :cl :rt))', [], 'CL-USER'),
  record('(deftest forward-helper (later-helper) 42)', ['FORWARD-HELPER']),
  record('(deftest wrong-value (+ 1 1) 3)', ['WRONG-VALUE']),
  record('(deftest undefined (missing-function) t)', ['UNDEFINED']),
  record('(deftest unreadable #q() t)', ['UNREADABLE']),
  record('(missing-setup-function)'),
  record('(deftest missing-package 1 1)', ['MISSING-PACKAGE'], 'NO-SUCH-PACKAGE'),
  record(`(progn
    (deftest hangs (loop do (values)) t)
    (deftest after-hang (later-helper) 42))`, ['HANGS', 'AFTER-HANG']),
  record('(defun later-helper () 42)'),
  record('(deftest final-pass (later-helper) 42)', ['FINAL-PASS']),
  {...record('(deftest unqualified 42 42)'), tests: ['UNQUALIFIED']},
  record('(deftest guest-only 42 42)'),
  generated('(deftest guest-alias 42 42)', ['HOST-ALIAS']),
  generated('(deftest guest-different 43 42)', ['HOST-DIFFERENT']),
  generated('(progn (deftest guest-a 42 42) (deftest guest-b 42 42))', ['HOST-AMBIGUOUS']),
  generated('(deftest guest-exact 42 42)', ['GUEST-EXACT', 'HOST-REUSE']),
];
const sources = ['rt-package.lsp', 'rt.lsp'].map(file => [file,
  fs.readFileSync(new URL(`./ansi-test/${file}`, import.meta.url), 'utf8')]);
const updates = [];
await assert.rejects(runFull({binary, records, sources, heapLimitMb: 0}), /Heap limit/);
await assert.rejects(runFull({binary, records, sources, heapLimitMb: 16.5}), /Heap limit/);
await assert.rejects(runFull({binary, records, sources, seed: [1, 2, -1, 4]}), /Seed/);
const report = await runFull({binary, records, sources, timeout: 150, heapLimitMb: 128,
  onProgress: current => updates.push({...current.progress})});
assert.equal(report.error, null);
assert.equal(report.heapLimitMb, 128);
assert.equal(report.imageSizes[0].file, binary);
assert.equal(report.imageSizes[0].rawBytes, fs.statSync(binary).size);
assert.ok(report.imageSizes[0].gzipBytes > 0 && report.imageSizes[0].brotliBytes > 0);
assert.ok(report.tests.every(test => Number.isFinite(test.elapsedMs) && test.elapsedMs >= 0));
assert.deepEqual(report.tests.map(test => test.status), [
  'pass', 'fail', 'fail', 'blocked', 'blocked', 'timeout', 'pass', 'pass', 'pass', 'pass',
  'pass', 'blocked', 'fail', 'blocked', 'pass', 'pass', 'pass', 'blocked',
]);
assert.deepEqual(report.summary, {
  passed: 9, failed: 3, blocked: 5, timedOut: 1, total: 18, expected: 18,
  loadErrors: 3, extraTests: 4, referenceTests: 14, notPassed: 9, complete: true,
});
const undefinedFailure = report.tests.find(test => test.name.endsWith('UNDEFINED'));
assert.match(undefinedFailure.output, /UNDEFINED-FUNCTION/);
assert.match(undefinedFailure.output, /Undefined function: MISSING-FUNCTION\./);
assert.doesNotMatch(undefinedFailure.output, /error during printing/);
assert.ok(report.loadErrors.some(error => error.source.includes('#q')));
assert.ok(updates.some(progress => progress.phase === 'load' && progress.loadedForms > 0));
assert.ok(updates.some(progress => progress.phase === 'test' && progress.testsProcessed > 0));
assert.equal(report.recoveries, 0, 'VM interruption must preserve state without replaying the suite');
assert.ok(report.tests.find(test => test.status === 'timeout').interrupted);
assert.equal(report.progress.phase, 'finished');
assert.equal(report.progress.loadedForms, records.length);
assert.equal(report.progress.testsProcessed, 18);
assert.deepEqual(report.inventoryAliases, [{file: 'runner-fixture.lsp', name: 'CL-TEST::HOST-ALIAS', guestName: 'GUEST-ALIAS'}]);
assert.equal(report.tests.find(test => test.name.endsWith('HOST-ALIAS')).guestName, 'GUEST-ALIAS');
assert.match(formatProgress(report.progress), /9.*3.*5.*1.*elapsed/);
assert.match(formatProgress(updates.find(progress => progress.phase === 'load')),
  /loading 0\/16 \(0%\) forms/);
assert.match(formatProgress({...report.progress, phase: 'recover', replayedOperations: 0, totalReplayOperations: 20}),
  /recovering 0\/\d+ \(0%\) setup operations/);
console.log('ANSI runner: failure accounting, load recovery, forward definitions, and timeout recovery passed.');

const fixtureReport = await runFull({binary, sources: [...sources, ['sandbox/fixture.txt', 'fixture\n']],
  testDirectory: '/sandbox/', records: [
    record('(defpackage :cl-test (:use :cl :rt))', [], 'CL-USER'),
    record('(deftest sandbox-fixture (with-open-file (s "fixture.txt") (read-line s)) "fixture" nil)', ['SANDBOX-FIXTURE']),
  ]});
assert.equal(fixtureReport.summary.passed, 1);
assert.equal(fixtureReport.summary.failed, 0);
console.log('ANSI runner: filesystem fixtures run under the upstream sandbox defaults.');

const seed = [1, 2, 3, 4];
const seededVM = await Miricl.instantiate(fs.readFileSync(binary));
seededVM.vm['seed-random'](...seed);
const expectedRandom = seededVM.evaluate('(loop repeat 20 collect (random 100000))').output.trim();
const seedReport = await runFull({binary, sources, seed, records: [
  record('(defpackage :cl-test (:use :cl :rt))', [], 'CL-USER'),
  record(`(deftest seeded-state
    (loop for expected in '${expectedRandom} always (= expected (random 100000))) t)`, ['SEEDED-STATE']),
]});
assert.deepEqual(seedReport.seed, seed);
assert.equal(seedReport.summary.passed, 1);
console.log('ANSI runner: recorded seeds reproduce the guest random state.');
