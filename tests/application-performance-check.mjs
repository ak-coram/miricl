// Exercise local regression detection and the boundaries that make timings comparable.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {spawnSync} from 'node:child_process';
import {compareApplicationPerformance as compare} from '../scripts/check-application-performance.mjs';
import {renderApplicationPerformance} from '../scripts/build-application-performance-html.mjs';

const environment = {browser: 'Chrome/140.0.1', javascript: '14.0.1', node: 'v26.0.0',
  platform: 'linux', arch: 'x64', cpuModel: 'Local CPU', cpuCount: 4, totalMemoryBytes: 16000000000, osRelease: '6.1'};
const report = (time = 1000) => ({passed: true, commit: String(time), environment: {...environment},
  applications: [{id: 'algebra', passed: true,
    startup: {coldSamplesMs: [time, time + 10, time - 10], warmSamplesMs: [time, time, time]},
    examples: [{id: 'factor', passed: true, firstSamplesMs: [time, time, time], warmSamplesMs: [time, time, time]}]}]});
const baseline = report();
const check = (current, reference = baseline, options) => compare(current, [{label: 'reference', report: reference}], options);
assert.equal(check(report(1100)).passed, true, 'Ordinary variance must pass.');
for (const [phase, mutate] of [
  ['cold startup', app => {app.startup.coldSamplesMs = [9000, 8000, 8500];}],
  ['cached startup', app => {app.startup.warmSamplesMs = [9000, 8000, 8500];}],
  ['first example', app => {app.examples[0].firstSamplesMs = [9000, 8000, 8500];}],
  ['warm example', app => {app.examples[0].warmSamplesMs = [9000, 8000, 8500];}],
]) {
  const current = report(); mutate(current.applications[0]);
  const result = check(current);
  assert.equal(result.passed, false, `${phase} regression must fail even when other timings are stable.`);
  assert.ok(result.regressions.some(item => item.currentMedianMs === 8500));
}
const spike = report(); spike.applications[0].startup.coldSamplesMs = [1000, 1000, 9000];
assert.equal(check(spike).passed, true, 'One noisy sample must not dominate the repeated-sample median.');
assert.equal(check(report(200), report(100)).passed, true, 'Large percentages on tiny absolute differences must pass.');
assert.equal(check(report(1300), baseline, {relativeTolerance: 0.5, absoluteToleranceMs: 10}).passed, true,
  'An absolute increase alone must not fail below the relative tolerance.');
assert.equal(check(report(1300), baseline, {relativeTolerance: 0.1, absoluteToleranceMs: 10}).passed, false,
  'Configured tolerances must control the gate.');
const gradual = compare(report(1800), [{label: 'candidate', report: report(1700)}, {label: 'reference', report: baseline}]);
assert.equal(gradual.passed, false, 'Every explicitly supplied reference must be checked independently.');
assert.ok(gradual.regressions.every(item => item.baseline === 'reference'));
for (const field of Object.keys(environment)) {
  const different = report(); different.environment[field] = typeof environment[field] === 'number' ? environment[field] + 1 : `${environment[field]} changed`;
  const result = check(report(9000), different);
  assert.equal(result.status, 'uncompared', `${field} mismatch must not be reported as a passing comparison.`);
  assert.ok(result.warnings.some(message => message.includes('NO COMPARISON') && message.includes(field)));
}
const incomplete = report(); incomplete.applications[0].examples = [];
assert.equal(check(incomplete).passed, false, 'Missing example coverage must fail.');
const partial = report(); partial.applications[0].examples[0].firstSamplesMs = [1000];
assert.equal(check(partial).passed, false, 'A first-use measurement cannot silently lose repetitions.');
assert.equal(check(report(), partial).passed, false, 'An incomplete reference cannot silently disable the fixed gate.');
const failed = report(); failed.applications[0].examples[0].passed = false;
assert.equal(check(failed).passed, false, 'A green top-level flag cannot hide a failed example.');
const failedReference = report(); failedReference.passed = false;
delete failedReference.environment;
assert.equal(check(report(), failedReference).passed, false, 'A failed reference cannot silently disable regression checks.');
const invalid = report(); invalid.applications[0].startup.coldSamplesMs = [-1, NaN, Infinity];
assert.equal(check(invalid).passed, false, 'Invalid timings must not supply performance evidence.');
const stateful = report(); stateful.applications[0].examples[0].warmUnsupported = 'Definition changes the logical world.';
delete stateful.applications[0].examples[0].warmSamplesMs;
const statefulResult = check(stateful, stateful);
assert.equal(statefulResult.passed, true, 'Explicitly non-repeatable operations retain their first-use gate.');
assert.ok(statefulResult.comparisons.some(item => item.metric.endsWith('/first')));
assert.ok(statefulResult.warnings.some(message => message.includes('Definition changes')));
assert.equal(check(stateful).passed, false, 'A previously measured warm operation cannot disappear silently.');
const added = report(); added.applications[0].examples.push({...added.applications[0].examples[0], id: 'new operation'});
assert.equal(check(added).passed, true, 'Additional example coverage need not match an exact inventory.');
assert.ok(check(added).uncompared.some(item => item.metric.includes('new operation')),
  'New examples need explicit uncompared evidence until a reference measures them.');
const originalWorkload = report();
originalWorkload.applications[0].examples[0].workloadSha256 = 'a'.repeat(64);
const changedWorkload = structuredClone(originalWorkload);
Object.assign(changedWorkload.applications[0].examples[0], {workloadSha256: 'b'.repeat(64),
  firstSamplesMs: [9000, 9000, 9000], warmSamplesMs: [9000, 9000, 9000]});
const changedResult = check(changedWorkload, originalWorkload);
assert.equal(changedResult.passed, true, 'Different source must not be compared as the same workload.');
assert.ok(changedResult.uncompared.some(item => item.reason === 'changed-workload'));
assert.ok(changedResult.comparisons.every(item => item.metric.includes('/startup/')));
assert.ok(check(report(), originalWorkload).uncompared.some(item => item.reason === 'missing-workload-identity'),
  'One missing fingerprint must not let different expressions silently share timings.');
const changedStateful = structuredClone(changedWorkload);
changedStateful.applications[0].examples[0].warmUnsupported = 'New source changes state.';
delete changedStateful.applications[0].examples[0].warmSamplesMs;
assert.equal(check(changedStateful, originalWorkload).passed, true,
  'A changed workload cannot inherit a warm comparison from the former expression.');
const missingWorkload = structuredClone(originalWorkload);
missingWorkload.applications[0].examples = [];
assert.equal(check(missingWorkload, originalWorkload).passed, false,
  'An absent workload is still missing coverage, even when the reference has a fingerprint.');
const missingEnvironment = report(); delete missingEnvironment.environment.cpuModel;
assert.equal(check(missingEnvironment).passed, false, 'New reports require hardware metadata.');
assert.equal(check(report(), missingEnvironment).status, 'uncompared', 'Old reports missing metadata are explicitly uncomparable.');
assert.throws(() => check(report(), baseline, {minimumSamples: 0}), /positive integer/);
assert.equal(compare(report(), []).status, 'uncompared');
assert.deepEqual(baseline, report(), 'Comparisons must never mutate or promote a reference report.');
const renderedFailure = renderApplicationPerformance(report(), 'measurements.json', check(report(3000)));
assert.match(renderedFailure, /Behavior checks: <strong>Passed/);
assert.match(renderedFailure, /Performance comparison: <strong class="error">Failed/,
  'A behaviorally correct but slow release must visibly fail its performance comparison.');
assert.match(renderedFailure, /algebra\/startup\/cold/, 'Failed measurements must be named in the report.');
assert.match(renderApplicationPerformance(report()), /Performance comparison: <strong>Not requested/);
assert.match(renderApplicationPerformance(report(), 'measurements.json', check(report(9000), {...baseline, environment: {...environment, browser: 'different'}})),
  /Performance comparison: <strong>Uncompared/);
const customThresholds = renderApplicationPerformance(report(), 'measurements.json',
  check(report(), baseline, {relativeTolerance: 1.25, absoluteToleranceMs: 50}));
assert.match(customThresholds, /125% and 50 ms/, 'The renderer must show the actual configured gate tolerances.');
const maliciousGate = {status: 'failed', errors: ['<script>bad()</script>'], regressions: []};
assert.ok(!renderApplicationPerformance(report(), 'measurements.json', maliciousGate).includes('<script>bad()'));
const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'miricl-performance-gate-'));
try {
  const current = path.join(directory, 'current.json'), reference = path.join(directory, 'reference.json');
  const output = path.join(directory, 'comparison.json'), before = JSON.stringify(baseline);
  fs.writeFileSync(reference, before);
  const run = extra => {
    const child = spawnSync(process.execPath, ['scripts/check-application-performance.mjs', current,
      '--reference', reference, ...extra], {encoding: 'utf8'});
    assert.ifError(child.error); return child;
  };
  fs.writeFileSync(current, JSON.stringify(report(9000)));
  assert.equal(run(['--output', output]).status, 1, 'CLI regressions must return a failing exit status.');
  assert.equal(JSON.parse(fs.readFileSync(output, 'utf8')).passed, false, 'Failure evidence must still be written.');
  fs.writeFileSync(current, JSON.stringify(report()));
  assert.equal(run(['--output', output]).status, 0, 'CLI comparable passing measurements must succeed.');
  const secondReference = path.join(directory, 'second-reference.json');
  fs.writeFileSync(secondReference, JSON.stringify(report(100)));
  assert.equal(run(['--reference', secondReference, '--output', output]).status, 1,
    'The CLI must compare every explicitly supplied reference.');
  const comparisons = JSON.parse(fs.readFileSync(output, 'utf8')).comparisons;
  for (const file of [reference, secondReference])
    assert.ok(comparisons.some(item => item.baseline === file), 'Comparison labels must identify their reference file.');
  assert.equal(run(['--output', reference]).status, 1, 'The CLI must reject overwriting its fixed reference.');
  assert.equal(fs.readFileSync(reference, 'utf8'), before, 'The reference must survive every CLI comparison unchanged.');
} finally {fs.rmSync(directory, {recursive: true, force: true});}
console.log('Application performance gate: startup and first/warm medians, fixed references, noise tolerances, coverage and environment boundaries passed.');
