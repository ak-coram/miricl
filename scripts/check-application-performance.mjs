// Compare local browser measurements against explicitly supplied reference reports.
import fs from 'node:fs/promises';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const environmentFields = ['browser', 'javascript', 'node', 'platform', 'arch', 'cpuModel',
  'cpuCount', 'totalMemoryBytes', 'osRelease'];
const median = samples => {
  const ordered = [...samples].sort((a, b) => a - b), middle = Math.floor(ordered.length / 2);
  return ordered.length % 2 ? ordered[middle] : (ordered[middle - 1] + ordered[middle]) / 2;
};
const validSamples = samples => Array.isArray(samples) && samples.length > 0 &&
  samples.every(value => Number.isFinite(value) && value >= 0);

export function compareApplicationPerformance(current, baselines, options = {}) {
  const {relativeTolerance = 0.5, absoluteToleranceMs = 250, minimumSamples = 3} = options;
  if (!Number.isFinite(relativeTolerance) || relativeTolerance < 0 ||
      !Number.isFinite(absoluteToleranceMs) || absoluteToleranceMs < 0 ||
      !Number.isInteger(minimumSamples) || minimumSamples < 1)
    throw new Error('Expected nonnegative tolerances and a positive integer sample count.');
  const result = {passed: true, status: 'uncompared', commit: current?.commit,
    thresholds: {relativeTolerance, absoluteToleranceMs, minimumSamples},
    comparisons: [], regressions: [], uncompared: [], errors: [], warnings: []};
  function metrics(report, label) {
    const found = new Map(), apps = new Set();
    const problem = message => result.errors.push(`${label}: ${message}`);
    if (!report || report.passed !== true || !Array.isArray(report.applications) || !report.applications.length) {
      problem('expected a complete passing application report.'); return found;
    }
    function add(key, samples, workloadSha256) {
      if (!validSamples(samples) || samples.length < minimumSamples) {
        problem(`${key} requires at least ${minimumSamples} finite nonnegative samples.`);
      } else found.set(key, {medianMs: median(samples), sampleCount: samples.length, workloadSha256});
    }
    for (const app of report.applications) {
      if (!app || typeof app.id !== 'string' || !app.id || apps.has(app.id)) {
        problem('invalid or duplicate application identity.'); continue;
      }
      apps.add(app.id);
      if (app.passed !== true || app.error) problem(`${app.id} failed.`);
      add(`${app.id}/startup/cold`, app.startup?.coldSamplesMs);
      add(`${app.id}/startup/warm`, app.startup?.warmSamplesMs);
      if (!Array.isArray(app.examples) || !app.examples.length) {problem(`${app.id} has no examples.`); continue;}
      const examples = new Set();
      for (const example of app.examples) {
        if (!example || typeof example.id !== 'string' || !example.id || examples.has(example.id)) {
          problem(`${app.id} has an invalid or duplicate example identity.`); continue;
        }
        examples.add(example.id);
        const key = `${app.id}/example/${example.id}`;
        if (example.passed !== true || example.error) problem(`${key} failed.`);
        if (example.workloadSha256 !== undefined && !/^[a-f0-9]{64}$/.test(example.workloadSha256))
          problem(`${key} has an invalid workload fingerprint.`);
        add(`${key}/first`, example.firstSamplesMs, example.workloadSha256);
        if (typeof example.warmUnsupported === 'string' && example.warmUnsupported.trim()) {
          result.warnings.push(`${label}: no warm comparison for ${key}: ${example.warmUnsupported}`);
        } else add(`${key}/warm`, example.warmSamplesMs, example.workloadSha256);
      }
    }
    return found;
  }
  const measured = metrics(current, 'current');
  const missingEnvironment = report => environmentFields.filter(field => {
    const value = report?.environment?.[field];
    return typeof value === 'string' ? !value.trim() : !Number.isFinite(value) || value <= 0;
  });
  const currentMissing = missingEnvironment(current);
  if (currentMissing.length) result.errors.push(`current: missing environment metadata: ${currentMissing.join(', ')}.`);
  const labels = new Set();
  for (const {label, report} of baselines) {
    if (typeof label !== 'string' || !label || labels.has(label)) throw new Error('Expected distinct baseline labels.');
    labels.add(label);
    if (!report || report.passed !== true) {
      result.errors.push(`${label}: reference report did not pass its application checks.`);
      continue;
    }
    const missing = missingEnvironment(report);
    const changed = environmentFields.filter(field => !missing.includes(field) &&
      current?.environment?.[field] !== report?.environment?.[field]);
    if (currentMissing.length || missing.length || changed.length) {
      result.warnings.push(`${label}: NO COMPARISON; ${missing.length ? `missing environment metadata (${missing.join(', ')})` :
        currentMissing.length ? 'current environment metadata is incomplete' : `incompatible environment (${changed.join(', ')})`}.`);
      continue;
    }
    const reference = metrics(report, label);
    for (const [key, before] of reference) {
      const after = measured.get(key);
      const identity = after ?? (key.endsWith('/warm') ? measured.get(key.slice(0, -5) + '/first') : undefined);
      if (identity && (before.workloadSha256 || identity.workloadSha256) &&
          before.workloadSha256 !== identity.workloadSha256) {
        const reason = before.workloadSha256 && identity.workloadSha256 ? 'changed-workload' : 'missing-workload-identity';
        result.uncompared.push({baseline: label, metric: key, reason,
          baselineWorkloadSha256: before.workloadSha256, currentWorkloadSha256: identity.workloadSha256});
        result.warnings.push(`${label}: NO COMPARISON for ${key}; ${reason}.`);
        continue;
      }
      if (!after) {result.errors.push(`${label}: current report is missing required measurement ${key}.`); continue;}
      const increaseMs = after.medianMs - before.medianMs;
      const relativeIncrease = before.medianMs === 0 ? (after.medianMs === 0 ? 0 : null) : increaseMs / before.medianMs;
      const regressed = increaseMs > absoluteToleranceMs && after.medianMs > before.medianMs * (1 + relativeTolerance);
      const comparison = {baseline: label, baselineCommit: report.commit, metric: key,
        baselineMedianMs: before.medianMs, currentMedianMs: after.medianMs,
        baselineSamples: before.sampleCount, currentSamples: after.sampleCount,
        increaseMs, relativeIncrease, regressed};
      result.comparisons.push(comparison);
      if (regressed) result.regressions.push(comparison);
    }
    for (const key of measured.keys()) if (!reference.has(key)) {
      result.uncompared.push({baseline: label, metric: key, reason: 'no-baseline-measurement'});
      result.warnings.push(`${label}: no baseline measurement for ${key}.`);
    }
  }
  if (!baselines.length) result.warnings.push('NO COMPARISON; no reference reports supplied.');
  if (!result.comparisons.length) result.warnings.push('NO COMPARISON; no compatible repeated measurements were available.');
  result.passed = !result.errors.length && !result.regressions.length;
  result.status = result.passed ? (result.comparisons.length ? 'passed' : 'uncompared') : 'failed';
  return result;
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const [input, ...args] = process.argv.slice(2), baselines = [], referenceFiles = [], options = {};
  let output;
  if (!input) throw new Error('Usage: node check-application-performance.mjs CURRENT.json [--reference FILE ...] [--output FILE] [--relative-tolerance 0.5] [--absolute-tolerance-ms 250] [--minimum-samples 3]');
  for (let i = 0; i < args.length; i++) {
    const flag = args[i], value = args[++i];
    if (!value) throw new Error(`Missing value for ${flag}.`);
    if (flag === '--reference') {
      referenceFiles.push(await fs.realpath(value));
      baselines.push({label: value, report: JSON.parse(await fs.readFile(value, 'utf8'))});
    }
    else if (flag === '--output') output = value;
    else if (flag === '--relative-tolerance') options.relativeTolerance = Number(value);
    else if (flag === '--absolute-tolerance-ms') options.absoluteToleranceMs = Number(value);
    else if (flag === '--minimum-samples') options.minimumSamples = Number(value);
    else throw new Error(`Unknown option: ${flag}`);
  }
  if (output && referenceFiles.includes(await fs.realpath(output).catch(() => path.resolve(output))))
    throw new Error('The comparison output must not overwrite a reference report.');
  const result = compareApplicationPerformance(JSON.parse(await fs.readFile(input, 'utf8')), baselines, options);
  if (output) await fs.writeFile(output, JSON.stringify(result, null, 2) + '\n');
  for (const warning of result.warnings) console.warn(`WARNING: ${warning}`);
  for (const error of result.errors) console.error(`ERROR: ${error}`);
  for (const regression of result.regressions) console.error(`REGRESSION vs ${regression.baseline}: ${regression.metric}: ${regression.baselineMedianMs.toFixed(1)} → ${regression.currentMedianMs.toFixed(1)} ms`);
  console.log(`Application performance: ${result.status}; ${result.comparisons.length} comparisons, ${result.regressions.length} regressions.`);
  if (!result.passed) process.exitCode = 1;
}
