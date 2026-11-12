#!/usr/bin/env node
// Both ANSI modes are blocking. Reject conformance failures, incomplete reports
// and compiler tests that passed without native compilation.
import fs from 'node:fs';
import assert from 'node:assert/strict';
assert.ok(process.argv.length > 2, 'Provide at least one ANSI report filename.');
for (const file of process.argv.slice(2)) {
  const report = JSON.parse(fs.readFileSync(file, 'utf8'));
  const s = report.summary;
  const summary = `### Full ANSI suite (${report.execution})\n\n${s.passed} passed, ${s.failed} failed, ${s.blocked} blocked, ${s.timedOut} timed out; ${s.total}/${s.expected} tests accounted for, ${s.loadErrors} load errors.\n`;
  console.log(summary);
  if (process.env.GITHUB_STEP_SUMMARY) fs.appendFileSync(process.env.GITHUB_STEP_SUMMARY, summary);
  assert.ok(['interpreter', 'compiler'].includes(report.execution), 'Missing or invalid execution mode.');
  assert.equal(report.error, null, report.error);
  assert.equal(s.complete, true, 'Full ANSI suite did not finish');
  assert.ok(s.total > 0);
  assert.equal(s.total, s.expected);
  assert.equal(report.tests.length, s.total, 'Report entries do not match the summary.');
  for (const key of ['failed', 'blocked', 'timedOut', 'loadErrors']) {
    assert.equal(s[key], 0, `ANSI ${report.execution}: ${key} must be zero.`);
  }
  assert.equal(s.passed, s.total, 'Every ANSI test must pass.');
  assert.equal(report.loadErrors.length, 0, 'ANSI source loading must succeed.');
  assert.ok(report.tests.every(test => test.status === 'pass' && test.passed === true),
    'Summary hides non-passing test entries.');
  if (report.execution === 'compiler') {
    assert.equal(report.compilerBootstrap?.mode, 'self-compiled', 'The ANSI compiler was not bootstrapped with itself');
    assert.equal(report.compilerBootstrap.origin, 'image', 'ANSI compiler tests must use the precompiled image');
    assert.ok(report.tests.filter(test => test.passed).every(test => test.compilation?.compiled > 0),
      'A compiler test passed without native compilation');
  }
}
