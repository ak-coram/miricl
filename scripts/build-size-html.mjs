#!/usr/bin/env node
// node scripts/build-size-html.mjs [build-directory] [output.html] [repl-page] [life-directory] [maxima-directory]
import fs from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import {measureFile, measureWasm} from './wasm-size.mjs';

const artifacts = [
  {file: 'miricl.wasm', description: 'Import-free Common Lisp interpreter and compiler', kind: 'wasm'},
  {file: 'miricl-repl.wasm', description: 'Terminal REPL and precompiled compiler for Node and WASI', kind: 'wasm'},
  {file: 'miricl.html', description: 'Complete browser REPL page', kind: 'html'},
  {file: 'todo.html', description: 'Complete Common Lisp + CLOS TODO page', kind: 'html'},
  {file: 'game-of-life/', description: 'Game of Life — total HTML, adapter, interpreter and application (4 files)',
    kind: 'application', files: ['game-of-life/build/index.html', 'game-of-life/build/miricl.js',
      'game-of-life/build/miricl.wasm', 'game-of-life/build/game-of-life.wasm']},
  {file: 'game-of-life/build/game-of-life.wasm', description: 'Game of Life — Lisp payload only (included in total above)', kind: 'wasm'},
  {file: 'maxima/', description: 'Maxima — initial HTML, scripts, interpreter, optimized image and filesystem (10 files)',
    kind: 'application', files: ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs',
      'miricl.mjs', 'maxima.mjs', 'miricl.wasm', 'maxima.wasm', 'files.json.gz'].map(file => `build/maxima/site/${file}`)},
  {file: 'build/maxima/site/maxima.wasm', description: 'Maxima — optimized Lisp payload (included in total above)', kind: 'wasm'},
];
const escape = value => String(value ?? '').replace(/[&<>"']/g,
  char => ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[char]));
const number = value => value.toLocaleString('en-US');
const kib = value => `${(value / 1024).toFixed(2)} KiB`;
const size = value => `<strong>${kib(value)}</strong><small>${number(value)} bytes</small>`;
const webURL = value => /^https?:\/\//.test(value ?? '');

export function collectImageSizes(directory, {commit = '', runURL = '', generatedAt = new Date().toISOString(),
  applicationDirectory = path.join(directory, 'game-of-life'),
  maximaDirectory = path.join(directory, 'build/maxima/site')} = {}) {
  const measure = (name, kind) => {
    const file = name.startsWith('game-of-life/')
      ? path.join(applicationDirectory, name.slice('game-of-life/'.length))
      : name.startsWith('build/maxima/site/') ? path.join(maximaDirectory, name.slice('build/maxima/site/'.length)) : path.join(directory, name);
    return {...(kind === 'wasm' ? measureWasm(file) : measureFile(file)), file: name};
  };
  return {schemaVersion: 1, generatedAt, commit, runURL,
    compression: {gzipLevel: 9, brotliQuality: 11},
    tools: {node: process.versions.node, zlib: process.versions.zlib},
    artifacts: artifacts.map(({files: names, ...artifact}) => {
      if (!names) return {...measure(artifact.file, artifact.kind), ...artifact};
      const files = names.map(name => measure(name));
      const totals = Object.fromEntries(['rawBytes', 'gzipBytes', 'brotliBytes'].map(key =>
        [key, files.reduce((sum, file) => sum + file[key], 0)]));
      return {...artifact, ...totals, files};
    })};
}

export function renderImageSizes(report, {replPage = 'miricl.html', jsonPage = 'image-sizes.json'} = {}) {
  if (report.schemaVersion !== 1 || !Array.isArray(report.artifacts) || report.artifacts.length !== artifacts.length)
    throw new Error('Expected measurements for the runtime, REPL, standalone pages, Game of Life and Maxima.');
  for (const artifact of report.artifacts) {
    for (const key of ['rawBytes', 'gzipBytes', 'brotliBytes']) {
      if (!Number.isSafeInteger(artifact[key]) || artifact[key] < 0) throw new Error(`Invalid ${key}.`);
      if (artifact.kind === 'application' && (!Array.isArray(artifact.files)
        || artifact.files.some(file => !Number.isSafeInteger(file[key]) || file[key] < 0)
        || artifact.files.reduce((sum, file) => sum + file[key], 0) !== artifact[key]))
        throw new Error(`Application totals do not match ${artifact.file}.`);
    }
  }
  // Links are generated locally; reject executable or external URL schemes.
  if (![replPage, jsonPage].every(value => /^[\w.-]+$/.test(value)))
    throw new Error('Related pages must be local filenames.');
  const row = artifact => `<tr data-artifact="${escape(artifact.file)}">
    <th scope="row"><code>${escape(artifact.file)}</code><small>${escape(artifact.description)}</small></th>
    <td data-size="rawBytes">${size(artifact.rawBytes)}</td>
    <td data-size="gzipBytes">${size(artifact.gzipBytes)}</td>
    <td data-size="brotliBytes">${size(artifact.brotliBytes)}</td>
  </tr>`;
  const rows = [
    ['Runtime files', artifact => artifact.kind === 'wasm' && !/^(game-of-life|build\/maxima)\//.test(artifact.file)],
    ['Standalone applications', artifact => artifact.kind === 'html'],
    ['Game of Life', artifact => artifact.file.startsWith('game-of-life/')],
    ['Maxima', artifact => artifact.file === 'maxima/' || artifact.file.startsWith('build/maxima/')],
  ].map(([title, matches]) => `<tbody><tr class="group"><th colspan="4" scope="rowgroup">${title}</th></tr>\n` +
    report.artifacts.filter(matches).map(row).join('\n') + '</tbody>').join('\n');
  const sections = report.artifacts.filter(artifact => artifact.kind === 'wasm').map(artifact => {
    if (!Array.isArray(artifact.sections) || artifact.sections.some(section =>
      !Number.isSafeInteger(section.bytes) || section.bytes < 0)
      || artifact.sections.reduce((sum, section) => sum + section.bytes, 0) !== artifact.rawBytes)
      throw new Error(`Section sizes do not match ${artifact.file}.`);
    const sectionRows = [...artifact.sections].sort((a, b) => b.bytes - a.bytes).map(section => {
      const share = artifact.rawBytes ? 100 * section.bytes / artifact.rawBytes : 0;
      return `<tr><th scope="row">${escape(section.name)}${section.count > 1 ? `<small>${number(section.count)} sections combined</small>` : ''}</th><td>${number(section.bytes)}</td>
        <td class="share"><span class="bar" style="--share:${share.toFixed(3)}%" aria-hidden="true"></span><span>${share.toFixed(1)}%</span></td></tr>`;
    }).join('\n');
    return `<details class="sections"><summary>${escape(artifact.file)} — raw section sizes</summary>
      <div class="table-wrap"><table><thead><tr><th scope="col">Section</th><th scope="col">Bytes</th><th scope="col">Share of image</th></tr></thead>
      <tbody>${sectionRows}</tbody></table></div></details>`;
  }).join('\n');
  const commit = report.commit ? `<dt>Commit</dt><dd><code>${escape(report.commit)}</code></dd>` : '';
  const metadata = `<dl><dt>Measured at</dt><dd>${escape(report.generatedAt)} (UTC)</dd>${commit}
    <dt>Compression tools</dt><dd>Node ${escape(report.tools?.node)} · zlib ${escape(report.tools?.zlib)}</dd></dl>
    ${webURL(report.runURL) ? `<p><a href="${escape(report.runURL)}">Workflow run</a></p>` : ''}`;
  const replacements = {'@@ROWS@@': rows, '@@SECTIONS@@': sections, '@@METADATA@@': metadata,
    '@@REPL@@': escape(replPage), '@@JSON@@': escape(jsonPage)};
  const template = fs.readFileSync(new URL('./image-sizes.html', import.meta.url), 'utf8');
  return template.replace(/@@[A-Z]+@@/g, marker => {
    if (!Object.hasOwn(replacements, marker)) throw new Error(`Unknown template marker: ${marker}`);
    return replacements[marker];
  });
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const directory = process.argv[2] ?? '.';
  const output = process.argv[3] ?? path.join(directory, 'image-sizes.html');
  if (!output.endsWith('.html')) throw new Error('Output filename must end in .html.');
  const json = output.slice(0, -5) + '.json';
  const runURL = process.env.GITHUB_RUN_ID && process.env.GITHUB_REPOSITORY
    ? `${process.env.GITHUB_SERVER_URL ?? 'https://github.com'}/${process.env.GITHUB_REPOSITORY}/actions/runs/${process.env.GITHUB_RUN_ID}` : '';
  const report = collectImageSizes(directory, {commit: process.env.GITHUB_SHA, runURL,
    ...(process.argv[5] ? {applicationDirectory: process.argv[5]} : {}),
    ...(process.argv[6] ? {maximaDirectory: process.argv[6]} : {})});
  const html = renderImageSizes(report, {replPage: process.argv[4] ?? 'miricl.html', jsonPage: path.basename(json)});
  fs.mkdirSync(path.dirname(path.resolve(output)), {recursive: true});
  fs.writeFileSync(json, JSON.stringify(report, null, 2) + '\n');
  fs.writeFileSync(output, html);
  console.log(`Wrote ${output} and ${json} (${report.artifacts.length} measured artifacts).`);
}
