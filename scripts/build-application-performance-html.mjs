// Render a local browser application benchmark report without external assets.
import fs from 'node:fs/promises';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const escape = value => String(value ?? '').replace(/[&<>"']/g,
  char => ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[char]));
const milliseconds = value => Number.isFinite(value) && value >= 0
  ? `${value.toLocaleString('en-US', {maximumFractionDigits: 1})} ms` : '—';

export function renderApplicationPerformance(report, jsonName = 'application-performance.json', gate) {
  if (!Array.isArray(report.applications) || !report.applications.length
      || report.applications.some(app => typeof app.id !== 'string' || !Array.isArray(app.examples)))
    throw new Error('Expected browser application measurements.');
  if (!/^[\w.-]+\.json$/.test(jsonName)) throw new Error('Expected a local JSON filename.');
  const gateStatus = gate ? ({passed: 'Passed', failed: 'Failed', uncompared: 'Uncompared'}[gate.status] ?? 'Unknown')
    : 'Not requested';
  const thresholds = gate?.thresholds;
  const tolerance = thresholds && Number.isFinite(thresholds.relativeTolerance) &&
    Number.isFinite(thresholds.absoluteToleranceMs)
    ? `A slowdown must exceed both ${escape((thresholds.relativeTolerance * 100).toLocaleString('en-US'))}% and ${milliseconds(thresholds.absoluteToleranceMs)} to fail this gate.`
    : '';
  const diagnostics = gate ? [
    ...(gate.errors ?? []).map(error => `<li>${escape(error)}</li>`),
    ...(gate.regressions ?? []).map(item => `<li>${escape(item.baseline)}: ${escape(item.metric)} · ${milliseconds(item.baselineMedianMs)} → ${milliseconds(item.currentMedianMs)}</li>`),
  ] : [];
  const sections = report.applications.map(app => `<section>
    <h2>${escape(app.id)}</h2>
    <p>Fresh profile startup: <strong>${milliseconds(app.startup?.coldMs)}</strong> ·
      Cached reload: <strong>${milliseconds(app.startup?.warmMs)}</strong></p>
    ${app.error ? `<p class="error">${escape(app.error)}</p>` : ''}
    <div class="table-wrap"><table><thead><tr><th>Example or operation</th><th>First use</th><th>Warmed</th><th>Result</th></tr></thead><tbody>
    ${app.examples.map(example => `<tr><th>${escape(example.title ?? example.id)}<small>${escape(example.id)}</small></th>
      <td>${milliseconds(example.milliseconds)}</td><td>${example.warmUnsupported ? escape(example.warmUnsupported) : milliseconds(example.warmMilliseconds)}</td><td class="${example.passed === false || example.error ? 'error' : ''}">
      ${escape(example.error ?? (example.passed === false ? 'Failed' : 'Passed'))}</td></tr>`).join('\n')}
    </tbody></table></div></section>`).join('\n');
  return `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Application performance / miricl</title><style>
body{margin:0;padding:24px;background:#9999cc;color:#222;font:14px "Courier New",monospace;line-height:1.5}
main{max-width:1100px;margin:auto;border:1px solid #222;background:#ffffea;box-shadow:4px 4px #555588}
header,footer{padding:12px 20px;background:#eaffff}header{border-bottom:1px solid #222}footer{border-top:1px solid #222}
article{padding:12px 20px}h1{font-size:25px}h2{font-size:20px;margin-top:28px}a{color:#004488}
.table-wrap{overflow:auto}table{border-collapse:collapse;width:100%}th,td{border:1px solid #999;padding:8px;text-align:left;vertical-align:top}
thead{background:#eeeecc}small{display:block;font-weight:normal;color:#555}.error{color:#a00}code{overflow-wrap:anywhere}
</style></head><body><main><header><a href="index.html">Common Lisp REPL</a> · <a href="image-sizes.html">Image sizes</a></header>
<article><h1>Application performance</h1>
<p>Local browser startup and application example measurements from the supplied site files.
Applications run sequentially. The displayed values are medians of ${escape(report.samples ?? 1)} fresh-session samples.
Worker evaluations end when their result reaches the browser; DOM and canvas operations include their paint wait.</p>
<p>Compare runs on the same machine and browser. First evaluations include compilation and initialization;
Each self-contained example starts a fresh runtime so earlier examples cannot hide lazy-loading costs.
Explicit delays and interactive debugger handling contribute to elapsed time.
Stateful examples are measured on first use; only repeatable calculations have warmed measurements.</p>
<p>Performance measurements are for local use only. Comparisons use explicitly supplied reference reports.
${tolerance}</p>
<p>Behavior checks: <strong>${report.passed ? 'Passed' : 'Failed'}</strong><br>
Performance comparison: <strong${gate?.status === 'failed' ? ' class="error"' : ''}>${gateStatus}</strong><br>
Browser: ${escape(report.browser)}<br>Measured: ${escape(report.generatedAt)}<br>Commit: <code>${escape(report.commit)}</code></p>
${diagnostics.length ? `<ul class="error">${diagnostics.join('\n')}</ul>` : ''}
${sections}<p><a href="${escape(jsonName)}" download>Download measurements as JSON</a></p></article>
<footer>Measured from matching application images and adapters. No JavaScript is needed to read this report.</footer></main></body></html>\n`;
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const [input, output, gateFile] = process.argv.slice(2);
  if (!input || !output) throw new Error('Usage: node build-application-performance-html.mjs INPUT.json OUTPUT.html [GATE.json]');
  const gate = gateFile ? JSON.parse(await fs.readFile(gateFile, 'utf8')) : undefined;
  await fs.writeFile(output, renderApplicationPerformance(JSON.parse(await fs.readFile(input, 'utf8')), path.basename(input), gate));
  console.log(`Wrote ${output}`);
}
