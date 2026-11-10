#!/usr/bin/env node
// node scripts/build-size-html.mjs [build-directory] [output.html] [repl-page] [life-directory] [maxima-directory] [fricas-directory]
import fs from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import {measureFile, measureWasm} from './wasm-size.mjs';

const artifacts = [
  {file: 'miricl.wasm', description: 'Import-free Common Lisp interpreter and compiler', kind: 'wasm'},
  {file: 'miricl-interpreter.wasm', description: 'Import-free interpreter without the compiler; compilation is a no-op', kind: 'wasm'},
  {file: 'miricl-precompiled.wasm', description: 'Import-free interpreter with bootstrapped native compiler', kind: 'wasm'},
  {file: 'miricl-repl.wasm', description: 'Terminal REPL and precompiled compiler for Node and WASI', kind: 'wasm'},
  {file: 'miricl-asdf.wasm', description: 'Optional ASDF and UIOP library', kind: 'wasm'},
  {file: 'miricl.html', description: 'Complete browser REPL page', kind: 'html'},
  {file: 'todo.html', description: 'Complete Common Lisp + CLOS TODO page', kind: 'html'},
  {file: 'game-of-life/', description: 'Game of Life — total HTML, adapter, interpreter and application (4 files)',
    kind: 'application', files: ['game-of-life/build/index.html', 'game-of-life/build/miricl.js',
      'game-of-life/build/miricl.wasm', 'game-of-life/build/game-of-life.wasm']},
  {file: 'game-of-life/build/game-of-life.wasm', description: 'Game of Life — Lisp payload only (included in total above)', kind: 'wasm'},
  {file: 'maxima/', description: 'Maxima — HTML, scripts, interpreter and optimized core (9 files)',
    kind: 'application', files: ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs',
      'miricl.mjs', 'maxima.mjs', 'miricl.wasm', 'maxima.core'].map(file => `build/maxima/site/${file}`)},
  {file: 'build/maxima/site/maxima.core', description: 'Maxima — optimized Lisp core (included in total above)', kind: 'wasm'},
  {file: 'fricas/', description: 'FriCAS — initial HTML, scripts, interpreter, optimized image and algebra filesystem (12 files)',
    kind: 'application', files: ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs',
      'miricl.mjs', 'fricas.mjs', 'filesystem-archive.mjs', 'mounted-filesystem.mjs', 'miricl.wasm', 'fricas.wasm', 'filesystem.bin.gz'].map(file => `build/fricas/site/${file}`)},
  {file: 'build/fricas/site/fricas.wasm', description: 'FriCAS — optimized Lisp payload (included in total above)', kind: 'wasm'},
];
const acl2Scripts = ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs',
  'miricl.mjs', 'acl2.mjs', 'acl2-books.mjs', 'dev-filesystem.mjs', 'filesystem-archive.mjs'];
const acl2Preview = [
  {file: 'acl2/', description: 'ACL2 — HTML, scripts, interpreter, native compiled core and system sources (13 files; bootstrap unverified)',
    kind: 'application', files: [...acl2Scripts,
      'miricl.wasm', 'acl2.wasm', 'filesystem.bin.gz'].map(file => `build/acl2/site/${file}`)},
  {file: 'build/acl2/site/acl2.wasm', description: 'ACL2 — native compiled prover payload (included in total above; no initialized world)', kind: 'wasm'},
  {file: 'build/acl2/site/filesystem.bin.gz', description: 'ACL2 — compressed system sources (included in total above)', kind: 'data'},
];
const acl2ImageFile = 'acl2-image.wasm.gz';
const acl2Image = [
  {file: 'acl2/', description: 'ACL2 — HTML, scripts and initialized native prover image (11 files)',
    kind: 'application', files: [...acl2Scripts, acl2ImageFile].map(file => `build/acl2/site/${file}`)},
  {file: `build/acl2/site/${acl2ImageFile}`, description: 'ACL2 — compressed initialized world and runtime (included in total above)', kind: 'data'},
];
const booksArtifact = {file: 'build/acl2/books/filesystem.bin.gz',
  description: 'ACL2 — selected math book sources and dependencies, separate on-demand download; no precompiled books', kind: 'data'};
const coaltonArtifacts = [
  {file: 'coalton/', description: 'Coalton — interactive whirlwind tour, scripts, interpreter and native compiled compiler and standard library (7 files)',
    kind: 'application', files: ['index.html', 'app.mjs', 'worker.mjs', 'coalton.mjs',
      'miricl.mjs', 'miricl.wasm', 'coalton.wasm'].map(file => `build/coalton/site/${file}`)},
  {file: 'build/coalton/site/coalton.wasm', description: 'Coalton — native compiled compiler and standard library (included in total above)', kind: 'wasm'},
];
const escape = value => String(value ?? '').replace(/[&<>"']/g,
  char => ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[char]));
const number = value => value.toLocaleString('en-US');
const kib = value => `${(value / 1024).toFixed(2)} KiB`;
const size = value => `<strong>${kib(value)}</strong><small>${number(value)} bytes</small>`;
const webURL = value => /^https?:\/\//.test(value ?? '');

export function collectImageSizes(directory, {commit = '', runURL = '', generatedAt = new Date().toISOString(),
  applicationDirectory = path.join(directory, 'game-of-life'),
  maximaDirectory = path.join(directory, 'build/maxima/site'),
  fricasDirectory = path.join(directory, 'build/fricas/site'),
  acl2Directory = path.join(directory, 'build/acl2/site'),
  coaltonDirectory = path.join(directory, 'build/coalton/site'),
  acl2BooksDirectory = path.join(directory, 'build/acl2/books')} = {}) {
  const wasm = new Set([...artifacts, ...acl2Preview, ...coaltonArtifacts]
    .filter(artifact => artifact.kind === 'wasm').map(artifact => artifact.file));
  const measurements = new Map();
  const measure = (name, kind) => {
    const file = name.startsWith('game-of-life/')
      ? path.join(applicationDirectory, name.slice('game-of-life/'.length))
      : name.startsWith('build/acl2/site/') ? path.join(acl2Directory, name.slice('build/acl2/site/'.length))
      : name.startsWith('build/acl2/books/') ? path.join(acl2BooksDirectory, name.slice('build/acl2/books/'.length))
      : name.startsWith('build/coalton/site/') ? path.join(coaltonDirectory, name.slice('build/coalton/site/'.length))
      : name.startsWith('build/fricas/site/') ? path.join(fricasDirectory, name.slice('build/fricas/site/'.length))
      : name.startsWith('build/maxima/site/') ? path.join(maximaDirectory, name.slice('build/maxima/site/'.length)) : path.join(directory, name);
    if (!measurements.has(name)) measurements.set(name, wasm.has(name) ? measureWasm(file) : measureFile(file));
    const {sections, ...sizes} = measurements.get(name);
    return {...sizes, ...(kind === 'wasm' ? {sections} : {}), file: name};
  };
  let acl2Artifacts = [];
  const acl2Manifest = path.join(acl2Directory, 'build.json');
  if (fs.existsSync(acl2Manifest)) {
    acl2Artifacts = acl2Preview;
    if (fs.existsSync(path.join(acl2Directory, acl2ImageFile))) {
      const {imageBytes} = JSON.parse(fs.readFileSync(acl2Manifest, 'utf8'));
      if (!Number.isSafeInteger(imageBytes) || imageBytes < 1) throw new Error('Expected the uncompressed ACL2 image size.');
      const [application, archive] = acl2Image;
      acl2Artifacts = [application, {...archive, uncompressedBytes: imageBytes,
        description: `ACL2 — initialized world and runtime, ${number(imageBytes)} bytes before gzip (archive included in total above)`}];
    }
  }
  return {schemaVersion: 1, generatedAt, commit, runURL,
    compression: {gzipLevel: 9, brotliQuality: 11},
    tools: {node: process.versions.node, zlib: process.versions.zlib},
    artifacts: [...artifacts,
      ...acl2Artifacts,
      ...(fs.existsSync(path.join(acl2BooksDirectory, 'manifest.json')) ? [booksArtifact] : []),
      ...(fs.existsSync(path.join(coaltonDirectory, 'build.json')) ? coaltonArtifacts : []),
    ].map(({files: names, ...artifact}) => {
      if (!names) return {...measure(artifact.file, artifact.kind), ...artifact};
      const files = names.map(name => measure(name));
      const totals = Object.fromEntries(['rawBytes', 'gzipBytes', 'brotliBytes'].map(key =>
        [key, files.reduce((sum, file) => sum + file[key], 0)]));
      return {...artifact, ...totals, files};
    })};
}

export function renderImageSizes(report, {replPage = 'miricl.html', jsonPage = 'image-sizes.json'} = {}) {
  const known = [...artifacts, ...acl2Preview, ...acl2Image, booksArtifact, ...coaltonArtifacts].map(artifact => artifact.file);
  if (report.schemaVersion !== 1 || !Array.isArray(report.artifacts)
    || artifacts.some(required => !report.artifacts.some(artifact => artifact.file === required.file))
    || report.artifacts.some(artifact => !known.includes(artifact.file))
    || new Set(report.artifacts.map(artifact => artifact.file)).size !== report.artifacts.length)
    throw new Error('Expected measurements for the runtime, REPL, ASDF, standalone pages, Game of Life, Maxima and FriCAS.');
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
  const initializedAcl2 = report.artifacts.some(artifact => artifact.file.endsWith('/' + acl2ImageFile));
  const rows = [
    ['Runtime files', artifact => artifact.kind === 'wasm' && !/^(game-of-life|build\/(?:maxima|fricas|acl2|coalton))\//.test(artifact.file)],
    ['Standalone applications', artifact => artifact.kind === 'html'],
    ['Game of Life', artifact => artifact.file.startsWith('game-of-life/')],
    ['FriCAS', artifact => artifact.file === 'fricas/' || artifact.file.startsWith('build/fricas/')],
    ['Maxima', artifact => artifact.file === 'maxima/' || artifact.file.startsWith('build/maxima/')],
    [initializedAcl2 ? 'ACL2' : 'ACL2 (development preview)', artifact => artifact.file === 'acl2/' || artifact.file.startsWith('build/acl2/')],
    ['Coalton', artifact => artifact.file === 'coalton/' || artifact.file.startsWith('build/coalton/')],
  ].filter(([, matches]) => report.artifacts.some(matches)).map(([title, matches]) => `<tbody><tr class="group"><th colspan="4" scope="rowgroup">${title}</th></tr>\n` +
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
    '@@ACL2@@': report.artifacts.some(artifact => artifact.file === 'acl2/')
      ? `<a href="acl2/">${initializedAcl2 ? 'ACL2' : 'ACL2 preview'}</a>` : '',
    '@@COALTON@@': report.artifacts.some(artifact => artifact.file === 'coalton/') ? '<a href="coalton/">Coalton tour</a>' : '',
    '@@REPL@@': escape(replPage), '@@JSON@@': escape(jsonPage)};
  const template = fs.readFileSync(new URL('./image-sizes.html', import.meta.url), 'utf8');
  return template.replace(/@@[A-Z0-9]+@@/g, marker => {
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
    ...(process.argv[6] ? {maximaDirectory: process.argv[6]} : {}),
    ...(process.argv[7] ? {fricasDirectory: process.argv[7]} : {})});
  const html = renderImageSizes(report, {replPage: process.argv[4] ?? 'miricl.html', jsonPage: path.basename(json)});
  fs.mkdirSync(path.dirname(path.resolve(output)), {recursive: true});
  fs.writeFileSync(json, JSON.stringify(report, null, 2) + '\n');
  fs.writeFileSync(output, html);
  console.log(`Wrote ${output} and ${json} (${report.artifacts.length} measured artifacts).`);
}
