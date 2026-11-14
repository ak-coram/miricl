#!/usr/bin/env node
// Aggregate compiler diagnostics from one CI run; render offline Plan 9 HTML.
import fs from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
const escape = value => String(value ?? '').replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const number = value => value.toLocaleString('en-US');
const safeURL = value => /^https?:\/\//i.test(value ?? '') ? value : '';
const integer = value => Number.isSafeInteger(value) && value >= 0;

export function collectCompilerReport(directory, context = {}) {
  const records = [], diagnostics = [], seen = new Map();
  const visit = location => {
    for (const item of fs.readdirSync(location, {withFileTypes:true})) {
      const file = path.join(location, item.name);
      if (item.isDirectory()) visit(file);
      else if (item.isFile() && item.name.endsWith('.json')) {
        let value;
        try {value = JSON.parse(fs.readFileSync(file, 'utf8'));}
        catch (error) {diagnostics.push(`${path.relative(directory, file)}: ${error.message}`); continue;}
        if (value.kind !== 'compiler-statistics') continue;
        if (value.schemaVersion !== 1 || typeof value.id !== 'string'
            || typeof value.workload !== 'string' || typeof value.phase !== 'string'
            || !['complete','partial','failed','cached','skipped'].includes(value.status)
            || !Array.isArray(value.counters) || value.counters.some(row => typeof row.rule !== 'string'
              || typeof row.detail !== 'string' || !integer(row.count))) {
          diagnostics.push(`${path.relative(directory, file)}: invalid compiler statistics record`); continue;
        }
        if ((context.commit && value.commit && value.commit !== context.commit)
            || (context.runId && value.runId && String(value.runId) !== String(context.runId))) {
          diagnostics.push(`${path.relative(directory, file)}: excluded counters from another commit/run`); continue;
        }
        if (seen.has(value.id)) {
          if (seen.get(value.id) !== JSON.stringify(value)) diagnostics.push(`Conflicting record ${value.id}`);
          continue;
        }
        seen.set(value.id, JSON.stringify(value)); records.push(value);
      }
    }
  };
  visit(directory);
  return {schemaVersion:1, kind:'compiler-statistics-report', generatedAt:new Date().toISOString(),
    ...context, jobs:context.jobs ?? [], records, diagnostics};
}

export function renderCompilerReport(report) {
  if (report.schemaVersion !== 1 || report.kind !== 'compiler-statistics-report'
      || !Array.isArray(report.records) || !Array.isArray(report.jobs)) throw new Error('Invalid compiler report.');
  const totals = new Map(), workloads = new Map();
  for (const record of report.records) {
    let workload = workloads.get(record.workload);
    if (!workload) workloads.set(record.workload, workload = {records:[], counts:new Map()});
    workload.records.push(record);
    if (record.status === 'cached' || record.status === 'skipped') continue;
    for (const row of record.counters) {
      const key = JSON.stringify([row.rule,row.detail]);
      const total = totals.get(key) ?? {...row,count:0}; total.count += row.count;
      if (!integer(total.count)) throw new Error('Counter total exceeds safe integer range.');
      totals.set(key,total); workload.counts.set(key,(workload.counts.get(key) ?? 0)+row.count);
    }
  }
  const names = [...workloads.keys()].sort();
  const complete = workload => {
    const stages = workload.records.filter(record => record.recordType !== 'measurement');
    return stages.length > 0 && stages.every(record => record.status === 'complete');
  };
  const rows = [...totals].sort((a,b) => a[1].rule.localeCompare(b[1].rule) || a[1].detail.localeCompare(b[1].detail));
  const status = record => record.status === 'cached' ? 'Cached: no compilation measured in this run'
    : record.status === 'complete' ? (record.recordType === 'measurement' ? 'Measurement' : 'Collected')
    : `${record.status}: partial or missing coverage`;
  const metric = value => integer(value) ? number(value) : '—';
  const stageRows = report.records.map(record => `<tr data-stage data-workload="${escape(record.workload)}">
    <th>${escape(record.workload)}<small>${escape(record.phase)}</small></th>
    <td>${escape(status(record))}${record.failure ? `<small class="error">${escape(record.failure)}</small>` : ''}</td>
    <td>${Number.isFinite(record.elapsedMs) ? `${number(Math.round(record.elapsedMs))} ms${record.timingIncludesCollection ? '<small>collection enabled</small>' : ''}` : '—'}</td>
    <td>${metric(record.runtimeBytes)}</td><td>${metric(record.inputBytes)}</td><td>${metric(record.outputBytes)}</td>
    <td>${escape(record.job || 'not recorded')}</td></tr>`).join('\n');
  const jobRows = report.jobs.map(job => `<tr><th>${safeURL(job.html_url ?? job.url)
    ? `<a href="${escape(job.html_url ?? job.url)}">${escape(job.name)}</a>` : escape(job.name)}</th>
    <td class="${['failure','cancelled','timed_out'].includes(job.conclusion) ? 'error' : ''}">${escape(job.conclusion || job.status || 'unknown')}</td>
    <td>${job.started_at && job.completed_at ? `${number(Math.max(0,Math.round((Date.parse(job.completed_at)-Date.parse(job.started_at))/1000)))} s` : '—'}</td></tr>`).join('\n');
  const counterRows = rows.map(([key,row]) => `<tr data-rule data-family="${escape(row.rule.split('/')[0])}">
    <th>${escape(row.rule)}<small>${escape(row.detail || 'all')}${row.rule.endsWith('-bytes') ? ' · bytes' : ''}</small></th>
    <td>${number(row.count)}</td>${names.map(name => {
      const workload = workloads.get(name), value = workload.counts.get(key);
      const collected = complete(workload);
      return `<td>${value === undefined ? (collected ? '0' : '—') : number(value)+(collected ? '' : '†')}</td>`;
    }).join('')}</tr>`).join('\n');
  const metadata = [report.commit && `Commit ${report.commit}`, report.runId && `Run ${report.runId}`,
    `Generated ${report.generatedAt}`].filter(Boolean).map(escape).join(' · ');
  return `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<link rel="icon" href="data:,"><title>miricl — Compiler and prover statistics</title><style>
:root{color-scheme:light;--paper:#ffffea;--tag:#eaffff;--select:#eeee9e;--border:#8888cc;--ink:#000;--muted:#454557}
*{box-sizing:border-box}[hidden]{display:none!important}body{margin:0;padding:20px;background:var(--border);color:var(--ink);font:14px/1.55 "Lucida Console","DejaVu Sans Mono",monospace}
main{border:1px solid #000;background:var(--paper);min-height:calc(100dvh - 40px)}header,footer,summary,thead,.toolbar{background:var(--tag)}header{display:flex;gap:8px 24px;flex-wrap:wrap;align-items:baseline;padding:10px 16px;border-bottom:1px solid #000}header strong{margin-right:auto}a{color:#000099}nav{display:flex;gap:16px;flex-wrap:wrap}section{padding:12px 16px}h1,h2{font:inherit;font-weight:bold}h1{margin:0}p{margin:8px 0}small,.muted{display:block;color:var(--muted);font-size:12px;overflow-wrap:anywhere}.error{color:#8b2020}details{border-top:3px solid var(--border)}summary{padding:8px 16px;cursor:pointer}summary:hover{background:var(--select)}.table-wrap{overflow-x:auto}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{text-align:right;padding:6px 12px;vertical-align:top;border-bottom:1px solid var(--border)}th:first-child{text-align:left}tbody th{font-weight:normal}td{white-space:nowrap}td small{white-space:normal}tbody tr:hover{background:var(--select)}.toolbar{display:flex;gap:16px;flex-wrap:wrap;padding:8px 16px;border-top:1px solid #000;border-bottom:1px solid #000}label{display:flex;gap:8px;align-items:center}input,select{font:inherit;border:1px solid var(--border);border-radius:0;background:var(--paper);padding:3px 6px;max-width:70vw}footer{padding:8px 16px;border-top:1px solid #000;font-size:12px}::selection{background:var(--select)}:focus-visible{outline:2px solid #000099;outline-offset:2px}@media(max-width:640px){body{padding:6px;font-size:13px}main{min-height:calc(100dvh - 12px)}th,td{padding:6px}header,section{padding:10px}.toolbar{padding:8px}}
</style></head><body><main>
<header><strong>miricl / Compiler and prover</strong><nav><a href="index.html">REPL</a><a href="image-sizes.html">Image sizes</a><a href="ansi-results.html">ANSI</a>${safeURL(report.runURL) ? `<a href="${escape(report.runURL)}">CI run</a>` : ''}<a href="compiler-statistics.json">JSON</a></nav></header>
<section><h1>Compilation across the CI run</h1><p>${number(report.records.length)} stage records · ${number(names.length)} measured workloads · ${number(report.jobs.length)} CI jobs</p><p class="muted">${metadata}</p>
<p>These are static compilation events, not runtime execution frequencies. Proof queries can revisit the same fact. Planned substitutions and emitted substitutions are separate. Byte counters describe pass input/output, not marginal savings attributable to one rule.</p>
<p>Absent records are missing coverage, not zero usage. Cached builds are excluded from current-run totals; failed stages may contribute partial counters. Collection covers instrumented compiler passes in native application builds and self-compilation; host/browser test status does not imply every test compilation was measured.</p>
<p>Use counts alongside application size and performance measurements. An uncommon rule may still be valuable for typed code or a particular application.</p>
${report.diagnostics?.length ? `<p class="error">Incomplete input: ${report.diagnostics.map(escape).join('; ')}</p>` : ''}</section>
<details open><summary>CI jobs</summary><div class="table-wrap"><table><thead><tr><th>Job</th><th>Result</th><th>Duration</th></tr></thead><tbody>${jobRows || '<tr><td colspan="3">Job results were not supplied.</td></tr>'}</tbody></table></div></details>
<details open><summary>Compilation stages and raw sizes</summary><div class="table-wrap"><table><thead><tr><th>Workload / stage</th><th>Coverage</th><th>Time</th><th>Runtime bytes</th><th>Input bytes</th><th>Output bytes</th><th>CI job</th></tr></thead><tbody>${stageRows || '<tr><td colspan="7">No compiler counters were received.</td></tr>'}</tbody></table></div></details>
<div class="toolbar" id="controls" hidden><label>Rule <input id="search" type="search" placeholder="Search rule or detail"></label><label>Category <select id="family"><option value="">All</option>${[...new Set(rows.map(([,row])=>row.rule.split('/')[0]))].sort().map(f=>`<option>${escape(f)}</option>`).join('')}</select></label><span id="visible"></span></div>
<div class="table-wrap"><table><thead><tr><th>Rule / detail</th><th>Observed total</th>${names.map(name=>`<th>${escape(name)}${complete(workloads.get(name)) ? '' : '<small>partial / cached</small>'}</th>`).join('')}</tr></thead><tbody id="rules">${counterRows || `<tr><td colspan="${names.length+2}">No instrumented events were observed.</td></tr>`}</tbody></table></div>
<footer>Plan 9 paper and tags · Offline report · Counts identify candidates for investigation; they do not justify removing an optimization by themselves.</footer>
</main><script>
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
    : {...JSON.parse(fs.readFileSync(input,'utf8')),...metadata,generatedAt:new Date().toISOString()};
  fs.mkdirSync(path.dirname(output),{recursive:true});fs.writeFileSync(output,renderCompilerReport(report));
  fs.writeFileSync(path.join(path.dirname(output),'compiler-statistics.json'),JSON.stringify(report,null,2)+'\n');
  console.log(`Wrote ${output}`);
}
