#!/usr/bin/env node
// Aggregate compiler diagnostics from one CI run; render offline Plan 9 HTML.
import fs from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import {isDeepStrictEqual} from 'node:util';
import {applicationJob, buildDecisionViews, compilationMode, decisionModes, decisionViewScript, isCompilerCollection,
  latestStageRecords, renderDecisionViews} from './compiler-decision-report.mjs';
import {optimizationRawRules} from './compiler-optimization-catalog.mjs';
const escape = value => String(value ?? '').replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const number = value => value.toLocaleString('en-US');
const safeURL = value => /^https?:\/\//i.test(value ?? '') ? value : '';
const integer = value => Number.isSafeInteger(value) && value >= 0;
const optionalString = value => value == null || typeof value === 'string';
const optionalInteger = value => value == null || integer(value);
const optionalAttempt = value => optionalInteger(value) || value === ''
  || (typeof value === 'string' && /^\d+$/.test(value) && integer(Number(value)));
const optionalEnum = (value, choices) => value == null || choices.includes(value);
const statisticsRecord = value => value?.kind === 'compiler-statistics'
  && value.schemaVersion === 1 && typeof value.id === 'string'
  && typeof value.workload === 'string' && typeof value.phase === 'string'
  && ['complete','partial','failed','cached','skipped'].includes(value.status)
  && optionalString(value.compilationMode) && optionalAttempt(value.attempt)
  && ['commit', 'failure', 'reason'].every(key => optionalString(value[key]))
  && (optionalString(value.runId) || integer(value.runId))
  && (value.job == null || (typeof value.job === 'string' && /^[\w-]*$/.test(value.job)))
  && optionalEnum(value.workloadScope, ['application', 'compiler', 'diagnostic'])
  && optionalEnum(value.collectionKind, ['compiler', 'measurement'])
  && optionalEnum(value.recordType, ['measurement'])
  && (value.timingIncludesCollection == null || typeof value.timingIncludesCollection === 'boolean')
  && (value.elapsedMs == null || (Number.isFinite(value.elapsedMs) && value.elapsedMs >= 0))
  && ['runtimeBytes', 'inputBytes', 'outputBytes'].every(key => optionalInteger(value[key]))
  && Array.isArray(value.counters) && value.counters.every(row => typeof row?.rule === 'string'
    && typeof row.detail === 'string' && integer(row.count));
const ciJob = value => typeof value?.name === 'string' && value.name.length > 0
  && (!value.name.startsWith('Application /') || applicationJob(value) !== undefined)
  && optionalAttempt(value.run_attempt)
  && ['html_url', 'url', 'conclusion', 'status', 'started_at', 'completed_at'].every(key => optionalString(value[key]));

function validateReportMetadata(report, diagnostics) {
  const result = {...report};
  for (const key of ['commit', 'runId', 'runURL', 'generatedAt']) {
    if (optionalString(report[key]) || (key === 'runId' && integer(report[key]))) continue;
    delete result[key];
    diagnostics.push(`Invalid report metadata field ${key}`);
  }
  return result;
}

export function collectCompilerReport(directory, inputContext = {}) {
  const records = [], diagnostics = [];
  const context = validateReportMetadata(inputContext, diagnostics);
  const visit = location => {
    for (const item of fs.readdirSync(location, {withFileTypes:true})) {
      const file = path.join(location, item.name);
      if (item.isDirectory()) visit(file);
      else if (item.isFile() && item.name.endsWith('.json')) {
        let value;
        try {value = JSON.parse(fs.readFileSync(file, 'utf8'));}
        catch (error) {diagnostics.push(`${path.relative(directory, file)}: ${error.message}`); continue;}
        if (value?.kind !== 'compiler-statistics') continue;
        if (!statisticsRecord(value)) {
          diagnostics.push(`${path.relative(directory, file)}: invalid compiler statistics record`); continue;
        }
        if ((context.commit && value.commit && value.commit !== context.commit)
            || (context.runId && value.runId && String(value.runId) !== String(context.runId))) {
          diagnostics.push(`${path.relative(directory, file)}: excluded counters from another commit/run`); continue;
        }
        records.push(value);
      }
    }
  };
  visit(directory);
  return validateCompilerReport({schemaVersion:1, kind:'compiler-statistics-report', generatedAt:new Date().toISOString(),
    ...context, jobs:context.jobs ?? [], records, diagnostics});
}

export function validateCompilerReport(report) {
  if (report?.schemaVersion !== 1 || report.kind !== 'compiler-statistics-report'
      || !Array.isArray(report.records) || !Array.isArray(report.jobs)) throw new Error('Invalid compiler report.');
  const diagnostics = [];
  if (Array.isArray(report.diagnostics)) {
    for (const [index, diagnostic] of report.diagnostics.entries())
      diagnostics.push(typeof diagnostic === 'string' ? diagnostic : `Diagnostic ${index + 1}: invalid report diagnostic`);
  } else if (report.diagnostics != null) diagnostics.push('Invalid report diagnostics');
  const metadata = validateReportMetadata(report, diagnostics);
  const seen = new Map();
  const records = report.records.filter((record, index) => {
    if (!statisticsRecord(record)) {
      diagnostics.push(`Record ${index + 1}: invalid compiler statistics record`);
      return false;
    }
    if (seen.has(record.id)) {
      if (!isDeepStrictEqual(seen.get(record.id), record)) diagnostics.push(`Conflicting record ${record.id}`);
      return false;
    }
    seen.set(record.id, record);
    return true;
  });
  const jobs = report.jobs.filter((job, index) => {
    if (ciJob(job)) return true;
    diagnostics.push(`Job ${index + 1}: invalid CI job record`);
    return false;
  });
  return {...metadata, records, jobs, diagnostics};
}

export function renderCompilerReport(input) {
  const report = validateCompilerReport(input);
  const current = new Set(latestStageRecords(report.records, report.jobs));
  const decisions = buildDecisionViews(report, [...current]);
  const totals = new Map(), workloads = new Map();
  for (const record of report.records) {
    let workload = workloads.get(record.workload);
    if (!workload) workloads.set(record.workload, workload = new Map());
    if (!current.has(record) || record.status === 'cached' || record.status === 'skipped') continue;
    for (const row of record.counters) {
      const key = JSON.stringify([row.rule,row.detail]);
      const total = totals.get(key) ?? {...row,count:0}; total.count += row.count;
      if (!integer(total.count)) throw new Error('Counter total exceeds safe integer range.');
      totals.set(key,total); workload.set(key,(workload.get(key) ?? 0)+row.count);
    }
  }
  const names = [...workloads.keys()].sort();
  const rows = [...totals].sort((a,b) => a[1].rule.localeCompare(b[1].rule) || a[1].detail.localeCompare(b[1].detail));
  const status = record => {
    if (!current.has(record)) return 'Superseded by a newer job attempt';
    if (record.status === 'cached') return 'Cached build';
    if (record.status === 'skipped') return 'Skipped';
    if (record.status !== 'complete') return record.status;
    if (isCompilerCollection(record)) return 'Compiler collection completed';
    return record.recordType === 'measurement' ? 'Artifact measurement' : 'Complete';
  };
  const metric = value => integer(value) ? number(value) : '—';
  const stageRows = report.records.map(record => `<tr data-stage data-workload="${escape(record.workload)}">
    <th>${escape(record.workload)}<small>${escape(record.phase)}</small></th>
    <td>${escape(status(record))}${record.failure ? `<small class="error">${escape(record.failure)}</small>` : ''}${record.reason ? `<small>${escape(record.reason)}</small>` : ''}</td>
    <td>${escape(decisionModes[compilationMode(record)] ?? compilationMode(record))}</td>
    <td>${Number.isFinite(record.elapsedMs) ? `${number(Math.round(record.elapsedMs))} ms${record.timingIncludesCollection ? '<small>collection enabled</small>' : ''}` : '—'}</td>
    <td>${metric(record.runtimeBytes)}</td><td>${metric(record.inputBytes)}</td><td>${metric(record.outputBytes)}</td>
    <td>${escape(record.job || 'not recorded')}${record.attempt ? `<small>attempt ${escape(record.attempt)}</small>` : ''}</td></tr>`).join('\n');
  const jobRows = report.jobs.map(job => `<tr><th>${safeURL(job.html_url ?? job.url)
    ? `<a href="${escape(job.html_url ?? job.url)}">${escape(job.name)}</a>` : escape(job.name)}</th>
    <td class="${['failure','cancelled','timed_out'].includes(job.conclusion) ? 'error' : ''}">${escape(job.conclusion || job.status || 'unknown')}</td>
    <td>${job.started_at && job.completed_at ? `${number(Math.max(0,Math.round((Date.parse(job.completed_at)-Date.parse(job.started_at))/1000)))} s` : '—'}</td></tr>`).join('\n');
  const rawUnit = rule => Object.hasOwn(optimizationRawRules, rule) ? optimizationRawRules[rule].unit
    : rule.endsWith('-bytes') || /^size-(input|output)\//.test(rule) ? 'bytes'
      : rule.startsWith('size-units/') ? 'measured batches' : '';
  const counterRows = rows.map(([key,row]) => `<tr data-rule data-family="${escape(row.rule.split('/')[0])}">
    <th>${escape(row.rule)}<small>${escape([row.detail || 'all', rawUnit(row.rule)].filter(Boolean).join(' · '))}</small></th>
    <td>${number(row.count)}</td>${names.map(name => {
      const value = workloads.get(name).get(key);
      return `<td>${value === undefined ? '—' : number(value)}</td>`;
    }).join('')}</tr>`).join('\n');
  const metadata = [report.commit && `Commit ${report.commit}`, report.runId && `Run ${report.runId}`,
    report.generatedAt && `Generated ${report.generatedAt}`].filter(Boolean).map(escape).join(' · ');
  return `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<link rel="icon" href="data:,"><title>miricl — Compiler and prover statistics</title><style>
:root{color-scheme:light;--paper:#ffffea;--tag:#eaffff;--select:#eeee9e;--border:#8888cc;--ink:#000;--muted:#454557}
*{box-sizing:border-box}[hidden]{display:none!important}body{margin:0;padding:20px;background:var(--border);color:var(--ink);font:14px/1.55 "Lucida Console","DejaVu Sans Mono",monospace}
main{border:1px solid #000;background:var(--paper);min-height:calc(100dvh - 40px)}header,footer,summary,thead,.toolbar{background:var(--tag)}header{display:flex;gap:8px 24px;flex-wrap:wrap;align-items:baseline;padding:10px 16px;border-bottom:1px solid #000}header strong{margin-right:auto}a{color:#000099}nav{display:flex;gap:16px;flex-wrap:wrap}section{padding:12px 16px}h1,h2{font:inherit;font-weight:bold}h1{margin:0}p{margin:8px 0}small,.muted{display:block;color:var(--muted);font-size:12px;overflow-wrap:anywhere}.error{color:#8b2020}details{border-top:3px solid var(--border)}summary{padding:8px 16px;cursor:pointer}summary:hover{background:var(--select)}.table-wrap{overflow-x:auto}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{text-align:right;padding:6px 12px;vertical-align:top;border-bottom:1px solid var(--border)}th:first-child{text-align:left}tbody th{font-weight:normal}td{white-space:nowrap}td small{white-space:normal}tbody tr:hover{background:var(--select)}.toolbar{display:flex;gap:16px;flex-wrap:wrap;padding:8px 16px;border-top:1px solid #000;border-bottom:1px solid #000}label{display:flex;gap:8px;align-items:center}input,select{font:inherit;border:1px solid var(--border);border-radius:0;background:var(--paper);padding:3px 6px;max-width:70vw}footer{padding:8px 16px;border-top:1px solid #000;font-size:12px}::selection{background:var(--select)}:focus-visible{outline:2px solid #000099;outline-offset:2px}@media(max-width:640px){body{padding:6px;font-size:13px}main{min-height:calc(100dvh - 12px)}th,td{padding:6px}header,section{padding:10px}.toolbar{padding:8px}}
.scope-heading{border-top:1px solid var(--border)}.scope-heading h2{margin:0}.group-row{background:#f3f3d5}.group-row th{font-weight:bold}.muted-cell{color:var(--muted)}.counts{margin:0;padding:0;list-style:none;white-space:normal;min-width:12ch}.counts li+li{margin-top:4px}.phase-breakdown,.catalog-category details{border-top-width:1px}.phase-breakdown summary,.catalog-category .catalog-category>summary{padding-left:28px}.catalog-category .decision-breakdown>summary{padding-left:40px}td:has(.counts),th:nth-last-child(-n+2):not(:first-child){max-width:46ch}h3{font:inherit;font-weight:bold;margin:0}
</style></head><body><main>
<header><strong>miricl / Compiler and prover</strong><nav><a href="index.html">REPL</a><a href="image-sizes.html">Image sizes</a><a href="ansi-results.html">ANSI</a>${safeURL(report.runURL) ? `<a href="${escape(report.runURL)}">CI run</a>` : ''}<a href="compiler-statistics.json" download>Raw JSON</a></nav></header>
<section><h1>Compilation across the CI run</h1><p>${number(report.records.length)} stage records · ${number(decisions.applicationCount)} applications · ${number(report.jobs.length)} CI jobs</p><p class="muted">${metadata}</p>
<nav aria-label="Report sections"><a href="#decisions">Decisions</a><a href="#stages" data-open-details>Stages and sizes</a><a href="#raw" data-open-details>Raw counters</a><a href="#jobs" data-open-details>CI jobs</a></nav>
${report.diagnostics?.length ? `<p class="error">Some inputs could not be included. ${report.diagnostics.map(escape).join(' · ')}</p>` : ''}</section>
${renderDecisionViews(decisions, escape)}
<details id="stages"><summary>Compilation stages and raw sizes</summary><div class="table-wrap"><table><thead><tr><th>Workload / stage</th><th>Coverage</th><th>Mode</th><th>Time</th><th>Runtime bytes</th><th>Input bytes</th><th>Output bytes</th><th>CI job</th></tr></thead><tbody>${stageRows || '<tr><td colspan="8">No compiler stage records were received.</td></tr>'}</tbody></table></div></details>
<details id="raw"><summary>Raw compiler and prover counters</summary>
<section><p>Raw totals include the latest recorded attempts across all modes, with any counters recovered from incomplete stages. A dash means that counter is absent. Superseded records are retained in the JSON download.</p>
<p>These events occur during compilation. Proof queries can revisit a fact, planned and emitted changes use different counters, and byte counters describe pass inputs and outputs.</p></section>
<div class="toolbar" id="controls" hidden><label>Rule <input id="search" type="search" placeholder="Search rule or detail"></label><label>Category <select id="family"><option value="">All</option>${[...new Set(rows.map(([,row])=>row.rule.split('/')[0]))].sort().map(f=>`<option>${escape(f)}</option>`).join('')}</select></label><span id="visible"></span></div>
<div class="table-wrap"><table><thead><tr><th>Rule / detail</th><th>Observed total</th>${names.map(name=>`<th>${escape(name)}</th>`).join('')}</tr></thead><tbody id="rules">${counterRows || `<tr><td colspan="${names.length+2}">No instrumented events were observed.</td></tr>`}</tbody></table></div></details>
<details id="jobs"><summary>CI jobs</summary><div class="table-wrap"><table><thead><tr><th>Job</th><th>Result</th><th>Duration</th></tr></thead><tbody>${jobRows || '<tr><td colspan="3">Job results were not supplied.</td></tr>'}</tbody></table></div></details>
</main><script>
${decisionViewScript}
const controls=document.getElementById('controls'),search=document.getElementById('search'),family=document.getElementById('family'),rows=[...document.querySelectorAll('[data-rule]')];
function filter(){const text=search.value.trim().toLowerCase();let count=0;for(const row of rows){row.hidden=!!((family.value&&row.dataset.family!==family.value)||(text&&!row.cells[0].textContent.toLowerCase().includes(text)));if(!row.hidden)count++}document.getElementById('visible').textContent=count+' matching rows'}
controls.hidden=false;search.addEventListener('input',filter);family.addEventListener('change',filter);filter();
</script></body></html>`;
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const [input='build/compiler-statistics', output='build/compiler-statistics.html', jobsFile] = process.argv.slice(2);
  const metadata = jobsFile ? JSON.parse(fs.readFileSync(jobsFile,'utf8')) : {};
  const context = {commit:process.env.GITHUB_SHA ?? '', runId:process.env.GITHUB_RUN_ID ?? '',
    runURL:process.env.GITHUB_RUN_ID && process.env.GITHUB_REPOSITORY ? `${process.env.GITHUB_SERVER_URL ?? 'https://github.com'}/${process.env.GITHUB_REPOSITORY}/actions/runs/${process.env.GITHUB_RUN_ID}` : '', ...metadata};
  const report = fs.statSync(input).isDirectory() ? collectCompilerReport(input,context)
    : validateCompilerReport({...JSON.parse(fs.readFileSync(input,'utf8')),...metadata,generatedAt:new Date().toISOString()});
  fs.mkdirSync(path.dirname(output),{recursive:true});fs.writeFileSync(output,renderCompilerReport(report));
  fs.writeFileSync(path.join(path.dirname(output),'compiler-statistics.json'),JSON.stringify(report,null,2)+'\n');
  console.log(`Wrote ${output}`);
}
