#!/usr/bin/env node
// node scripts/build-ansi-html.mjs [ansi-results.json] [ansi-results.html]
import fs from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const statuses = {pass: 'Passed', fail: 'Failed', blocked: 'Blocked', timeout: 'Timed out'};
const escape = value => String(value ?? '').replace(/[&<>"']/g,
  char => ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[char]));
const number = value => Number(value).toLocaleString('en-US');
const pre = (label, value) => value === undefined || value === null || value === '' ? ''
  : `<h4>${label}</h4><pre>${escape(value)}</pre>`;
const counts = tests => Object.fromEntries(Object.keys(statuses).map(status =>
  [status, tests.filter(test => test.status === status).length]));
const tally = totals => Object.entries(totals).filter(([, count]) => count)
  .map(([status, count]) => `<span class="${status}">${number(count)} ${statuses[status].toLowerCase()}</span>`).join(' · ');

function resultDetails(entry) {
  const flags = ['code', 'extra', 'interrupted', 'timedOut', 'crashed', 'phase']
    .filter(key => entry[key] !== undefined)
    .map(key => `${key}: ${entry[key]}`).join(' · ');
  return `${flags ? `<p class="muted">${escape(flags)}</p>` : ''}
    ${pre('Compilation', entry.compilation && JSON.stringify(entry.compilation))}
    ${pre('Error', entry.error)}${pre('Source', entry.source)}${pre('Output', entry.output)}`;
}

export function renderAnsiReport(report, {commit = '', runURL = ''} = {}) {
  if (report.profile !== 'full' || !report.summary) throw new Error('Expected a full ANSI report.');
  const tests = report.tests ?? [];
  if (!Array.isArray(tests)) throw new Error('Report tests must be an array.');
  for (const test of tests) {
    if (!Object.hasOwn(statuses, test.status)) throw new Error(`Unknown test status: ${test.status}`);
  }
  const totals = counts(tests);
  const expected = report.summary.expected ?? report.progress?.totalTests;
  // Checkpoint reports may have only a partial summary. Count their recorded tests.
  const complete = report.summary.complete === true && !report.error
    && tests.length > 0 && tests.length === expected;
  const groups = new Map();
  for (const test of tests) {
    const file = test.file || 'Unknown source';
    const category = file.includes('/') ? file.slice(0, file.indexOf('/')) : 'Top-level tests';
    if (!groups.has(category)) groups.set(category, new Map());
    const files = groups.get(category);
    if (!files.has(file)) files.set(file, []);
    files.get(file).push(test);
  }
  let testId = 0;
  const groupHTML = [...groups].map(([category, files]) => {
    const categoryTests = [...files.values()].flat();
    const fileHTML = [...files].map(([file, entries]) => {
      const testHTML = entries.map(test => `<details class="test" id="test-${++testId}" data-status="${test.status}">
        <summary><span class="badge ${test.status}">${statuses[test.status]}</span> <span class="test-name">${escape(test.name)}</span>${test.extra ? ' <span class="muted">[extra]</span>' : ''}${Number.isFinite(test.elapsedMs) ? ` <span class="muted">[${number(test.elapsedMs)} ms${test.slow ? ', above performance budget' : ''}]</span>` : ''}</summary>
        <div class="test-body">${resultDetails(test) || '<p>No output was recorded.</p>'}</div>
      </details>`).join('\n');
      return `<details class="file" data-file="${escape(file)}">
        <summary><span class="file-name">${escape(file)}</span> <span class="matching"></span><span class="counts">${tally(counts(entries))}</span></summary>
        ${testHTML}
      </details>`;
    }).join('\n');
    return `<details class="group">
      <summary><strong>${escape(category)}</strong> <span class="matching"></span><span class="counts">${tally(counts(categoryTests))}</span></summary>
      ${fileHTML}
    </details>`;
  }).join('\n');
  const loadErrors = report.loadErrors ?? [];
  const mismatches = report.inventoryMismatches ?? [];
  const diagnostics = (label, entries) => !entries.length ? '' : `<details class="diagnostics">
    <summary>${label} (${number(entries.length)})</summary>
    ${entries.map(entry => `<details class="diagnostic"><summary>${escape(entry.file || 'Unknown source')}${entry.tests ? ` · ${escape(entry.tests.join(', '))}` : ''}</summary>
      <div class="test-body">${entry.extra !== undefined ? `<p>${number(entry.extra)} additional guest registrations.</p>` : ''}${resultDetails(entry)}</div>
    </details>`).join('\n')}
  </details>`;
  const metadata = [
    ['Execution', report.execution === 'compiler' ? 'Native compiled test bodies' : 'Interpreter'],
    ...(report.execution === 'compiler' ? [['Compiler bootstrap',
      report.compilerBootstrap?.mode === 'self-compiled'
        ? `Self-compiled frontend and backend (${number(report.compilerBootstrap.functions)} functions)`
        : 'Not recorded']] : []),
    ...(report.compilation ? [['Native compilations', report.compilation.compiled],
      ['Compilation failures', report.compilation.failed]] : []),
    ['Started', report.startedAt ? `${report.startedAt} (UTC)` : 'Not recorded'],
    ['Duration', Number.isFinite(report.totalElapsedMs ?? report.elapsedMs) ? `${((report.totalElapsedMs ?? report.elapsedMs) / 1000).toFixed(1)} seconds` : 'Not recorded'],
    ...(Number.isFinite(report.loadElapsedMs) ? [['Upstream initialization', `${(report.loadElapsedMs / 1000).toFixed(1)} seconds`]] : []),
    [report.runner === 'upstream' ? 'Per-test deadline' : 'Per-operation deadline', report.timeout ? `${number(report.timeout)} ms` : 'Not recorded'],
    ...(report.loadTimeout ? [['Initialization deadline', `${number(report.loadTimeout)} ms`]] : []),
    ['Performance budget', report.performanceBudgetMs ? `${number(report.performanceBudgetMs)} ms` : 'Not recorded'],
    ['Above performance budget', report.summary.slow ?? 0],
    ['Worker heap limit', report.heapLimitMb ? `${number(report.heapLimitMb)} MiB` : 'Not recorded'],
    ...(report.imageSizes?.length ? report.imageSizes.flatMap(image => [
      [`${path.basename(image.file)} raw`, `${number(image.rawBytes)} bytes`],
      [`${path.basename(image.file)} gzip (level 9)`, `${number(image.gzipBytes)} bytes`],
      [`${path.basename(image.file)} Brotli (quality 11)`, `${number(image.brotliBytes)} bytes`],
    ]) : [['Image sizes', 'Not recorded']]),
    ['Inventory', report.inventory ?? 'Not recorded'],
    ['Reference tests', report.summary.referenceTests ?? expected ?? 'Not recorded'],
    ['Additional guest tests', report.summary.extraTests ?? 0],
    ['Worker recoveries', report.recoveries ?? 0],
    ...(commit ? [['Commit', commit]] : []),
  ].map(([key, value]) => `<dt>${escape(key)}</dt><dd>${escape(value)}</dd>`).join('\n');
  // Only allow web URLs in the optional workflow link.
  const workflowLink = /^https?:\/\//.test(runURL) ? `<a href="${escape(runURL)}">Workflow run</a>` : '';
  const substitutions = {
    '@@SUMMARY@@': `<p id="completion" class="${complete ? '' : 'fail'}">${complete ? 'Complete run' : 'Incomplete run'} · ${number(tests.length)} / ${expected === undefined ? 'unknown' : number(expected)} tests accounted for</p>
      <div class="totals">${Object.entries(totals).map(([status, count]) =>
        `<div><strong class="${status}">${number(count)}</strong><span>${statuses[status]}</span></div>`).join('')}
        <div><strong>${number(loadErrors.length)}</strong><span>Load errors</span></div></div>
      <p>${tests.length ? (100 * totals.pass / tests.length).toFixed(1) : '0.0'}% of recorded tests passed.</p>
      ${report.error ? `<div class="run-error">${pre('Run error', report.error)}</div>` : ''}`,
    '@@METADATA@@': `<dl>${metadata}</dl>${workflowLink ? `<p>${workflowLink}</p>` : ''}`,
    '@@DIAGNOSTICS@@': diagnostics('Load errors', loadErrors) + diagnostics('Inventory mismatches', mismatches),
    '@@TESTS@@': groupHTML || '<p>No test results were recorded.</p>',
    '@@TOTAL@@': number(tests.length),
  };
  const template = fs.readFileSync(new URL('./ansi-report.html', import.meta.url), 'utf8');
  return template.replace(/@@[A-Z]+@@/g, marker => {
    if (!Object.hasOwn(substitutions, marker)) throw new Error(`Unknown template marker: ${marker}`);
    return substitutions[marker];
  });
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const input = process.argv[2] ?? process.env.MIRICL_ANSI_REPORT ?? 'ansi-results.json';
  const output = process.argv[3] ?? 'ansi-results.html';
  const report = JSON.parse(fs.readFileSync(input, 'utf8'));
  const runURL = process.env.GITHUB_RUN_ID && process.env.GITHUB_REPOSITORY
    ? `${process.env.GITHUB_SERVER_URL ?? 'https://github.com'}/${process.env.GITHUB_REPOSITORY}/actions/runs/${process.env.GITHUB_RUN_ID}` : '';
  fs.mkdirSync(path.dirname(path.resolve(output)), {recursive: true});
  fs.writeFileSync(output, renderAnsiReport(report, {commit: process.env.GITHUB_SHA, runURL}));
  console.log(`Wrote ${output} (${number(report.tests?.length ?? 0)} test results).`);
}
