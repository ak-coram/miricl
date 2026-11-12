// Both ANSI modes must pass every test. Reject incomplete or inconsistent
// reports and compiler successes that never executed native code.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {spawnSync} from 'node:child_process';
const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'miricl-ansi-report-'));
try {
  const file = path.join(directory, 'report.json');
  const base = {
    execution: 'interpreter', error: null, loadErrors: [],
    summary: {complete: true, total: 2, expected: 2, passed: 2, failed: 0,
      blocked: 0, timedOut: 0, loadErrors: 0},
    tests: Array.from({length: 2}, () => ({status: 'pass', passed: true, compilation: {compiled: 1}})),
  };
  const check = (report, expected, message) => {
    fs.writeFileSync(file, JSON.stringify(report));
    const result = spawnSync(process.execPath, ['scripts/check-ansi-results.mjs', file], {encoding: 'utf8'});
    assert.ifError(result.error);
    assert.equal(result.status === 0, expected, `${message}\n${result.stderr}`);
  };
  for (const execution of ['interpreter', 'compiler']) {
    const report = {...base, execution, ...(execution === 'compiler' ? {
      compilerBootstrap: {mode: 'self-compiled', origin: 'image', functions: 152},
    } : {})};
    check(report, true, `${execution}: a complete passing run must succeed`);
    check({...report, error: 'load failed'}, false, `${execution}: runner errors must fail CI`);
    check({...report, summary: {...base.summary, complete: false}}, false,
      `${execution}: an incomplete run must fail CI`);
    check({...report, summary: {...base.summary, total: 1}}, false,
      `${execution}: missing tests must fail CI`);
    for (const key of ['failed', 'blocked', 'timedOut', 'loadErrors']) {
      check({...report, summary: {...base.summary, [key]: 1}}, false,
        `${execution}: ${key} must fail CI`);
    }
    check({...report, tests: [base.tests[0]]}, false,
      `${execution}: missing report entries must fail CI`);
    check({...report, tests: [base.tests[0], {status: 'fail', passed: false}]}, false,
      `${execution}: a green summary must not hide a failure`);
    check({...report, loadErrors: [{error: 'load failed'}]}, false,
      `${execution}: a green summary must not hide a load error`);
    check({...report, summary: {...base.summary, passed: 1}}, false,
      `${execution}: passing counts must match the total`);
  }
  const compiler = {...base, execution: 'compiler',
    compilerBootstrap: {mode: 'self-compiled', origin: 'image', functions: 152}};
  check({...compiler, compilerBootstrap: {...compiler.compilerBootstrap, origin: 'first-use'}}, false,
    'Compiler conformance must use the precompiled image');
  check({...compiler, tests: base.tests.map(test => ({...test, compilation: {compiled: 0}}))}, false,
    'Passing without native compilation must fail CI');
  check({...base, execution: undefined}, false, 'Missing execution mode must fail CI');
  console.log('ANSI report gate: both modes require all tests to pass, complete accounting, successful loading and native compiler execution.');
} finally {
  fs.rmSync(directory, {recursive: true, force: true});
}
