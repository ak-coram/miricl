#!/usr/bin/env node
// CHROMIUM=chromium node tests/size-html.mjs [build-directory] [report-directory]
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {pathToFileURL, fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
import {gzipSync, brotliCompressSync, constants} from 'node:zlib';
import {collectImageSizes, renderImageSizes} from '../scripts/build-size-html.mjs';

function checkMeasurements(report, directory) {
  const initializedAcl2 = fs.existsSync(path.join(directory, 'build/acl2/site/acl2-image.wasm.gz'));
  const initializedCoalton = fs.existsSync(path.join(directory, 'build/coalton/site/coalton-image.wasm.gz'));
  assert.deepEqual(report.artifacts.map(artifact => artifact.file),
    ['miricl.wasm', 'miricl-interpreter.wasm', 'miricl-precompiled.wasm', 'miricl-repl.wasm', 'miricl-asdf.wasm', 'miricl.html', 'todo.html',
      'game-of-life/', 'game-of-life/build/game-of-life.wasm', 'maxima/', 'build/maxima/site/maxima.core', 'fricas/', 'build/fricas/site/fricas.wasm',
      ...(fs.existsSync(path.join(directory, 'build/acl2/site/build.json')) ? ['acl2/', ...(initializedAcl2
        ? ['build/acl2/site/acl2-image.wasm.gz'] : ['build/acl2/site/acl2.wasm', 'build/acl2/site/filesystem.bin.gz'])] : []),
      ...(fs.existsSync(path.join(directory, 'build/acl2/books/manifest.json')) ? ['build/acl2/books/filesystem.bin.gz'] : []),
      ...(fs.existsSync(path.join(directory, 'build/coalton/site/build.json')) ? ['coalton/',
        `build/coalton/site/${initializedCoalton ? 'coalton-image.wasm.gz' : 'coalton.wasm'}`] : [])]);
  for (const artifact of report.artifacts) {
    if (artifact.kind === 'application') {
      assert.deepEqual(artifact.files.map(file => file.file), artifact.file === 'game-of-life/'
        ? ['game-of-life/build/index.html', 'game-of-life/build/miricl.js', 'game-of-life/build/miricl.wasm', 'game-of-life/build/game-of-life.wasm']
        : artifact.file === 'maxima/'
          ? ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs', 'miricl.mjs', 'maxima.mjs',
            'miricl.wasm', 'maxima.core'].map(file => `build/maxima/site/${file}`)
          : artifact.file === 'acl2/' ? ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs',
            'miricl.mjs', 'acl2.mjs', 'acl2-books.mjs', 'dev-filesystem.mjs', 'filesystem-archive.mjs',
            ...(initializedAcl2 ? ['acl2-image.wasm.gz'] : ['miricl.wasm', 'acl2.wasm', 'filesystem.bin.gz'])].map(file => `build/acl2/site/${file}`)
          : artifact.file === 'coalton/' ? ['index.html', 'client.mjs', 'repl-debugger.mjs', 'app.mjs', 'worker.mjs', 'examples.mjs', 'session-store.mjs', 'coalton.mjs',
            'miricl.mjs', 'build.json', ...(initializedCoalton ? ['coalton-image.wasm.gz'] : ['miricl.wasm', 'coalton.wasm'])].map(file => `build/coalton/site/${file}`)
          : ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs', 'miricl.mjs', 'fricas.mjs',
            'filesystem-archive.mjs', 'mounted-filesystem.mjs', 'miricl.wasm', 'fricas.wasm', 'filesystem.bin.gz'].map(file => `build/fricas/site/${file}`));
      const sizes = {rawBytes: 0, gzipBytes: 0, brotliBytes: 0};
      for (const file of artifact.files) {
        const bytes = fs.readFileSync(path.join(directory, file.file));
        const measured = {rawBytes: bytes.length, gzipBytes: gzipSync(bytes, {level: 9}).length,
          brotliBytes: brotliCompressSync(bytes, {params: {[constants.BROTLI_PARAM_QUALITY]: 11}}).length};
        for (const key of Object.keys(sizes)) {
          assert.equal(file[key], measured[key], file.file);
          sizes[key] += measured[key];
        }
      }
      for (const key of Object.keys(sizes)) assert.equal(artifact[key], sizes[key]);
      continue;
    }
    const bytes = fs.readFileSync(path.join(directory, artifact.file));
    assert.equal(artifact.rawBytes, bytes.length, artifact.file);
    assert.equal(artifact.gzipBytes, gzipSync(bytes, {level: 9}).length, artifact.file);
    assert.equal(artifact.brotliBytes, brotliCompressSync(bytes,
      {params: {[constants.BROTLI_PARAM_QUALITY]: 11}}).length, artifact.file);
    if (artifact.kind === 'wasm') {
      assert.equal(artifact.sections.reduce((sum, section) => sum + section.bytes, 0), artifact.rawBytes);
      assert.equal(new Set(artifact.sections.map(s => s.name)).size, artifact.sections.length,
        'Repeated Wasm section names must be combined');
    } else assert.equal(artifact.sections, undefined, 'HTML is measured as a complete file');
  }
}

function browserChecks(expected) {
  const check = (condition, message) => { if (!condition) throw new Error(message); };
  const output = document.createElement('output');
  output.id = 'browser-test-result';
  try {
    check(!globalThis.injected, 'Metadata executed as HTML or JavaScript');
    check(document.querySelectorAll('#artifact-sizes [data-artifact]').length === expected.length, 'All artifact rows are required');
    check(document.querySelectorAll('#artifact-sizes .group').length === 5
      + Number(expected.some(a => a.file.startsWith('build/acl2/')))
      + Number(expected.some(a => a.file === 'coalton/')), 'All application groups are required');
    for (const artifact of expected) {
      const row = document.querySelector(`[data-artifact="${artifact.file}"]`);
      for (const key of ['rawBytes', 'gzipBytes', 'brotliBytes']) {
        const cell = row.querySelector(`[data-size="${key}"]`);
        check(cell.querySelector('small').textContent === `${artifact[key].toLocaleString('en-US')} bytes`, `${artifact.file}: incorrect ${key}`);
        check(cell.querySelector('strong').textContent === `${(artifact[key] / 1024).toFixed(2)} KiB`, 'Incorrect KiB conversion');
      }
    }
    const sections = document.querySelectorAll('.sections');
    check(sections.length === expected.filter(a => a.kind === 'wasm').length, 'All Wasm section breakdowns are required');
    for (const section of sections) {
      const names = [...section.querySelectorAll('tbody th')].map(cell => cell.firstChild.textContent);
      check(new Set(names).size === names.length, 'Section chart has repeated names');
    }
    sections[0].querySelector('summary').click();
    check(sections[0].open && sections[0].querySelector('tbody tr'), 'Section breakdown did not open');
    check(getComputedStyle(document.querySelector('main')).backgroundColor === 'rgb(255, 255, 234)', 'Plan 9 paper color');
    check(document.documentElement.scrollWidth <= innerWidth, 'Page overflows the viewport');
    check(document.querySelector('a[download]').getAttribute('href').endsWith('.json'), 'JSON download missing');
    check(document.querySelectorAll('img, iframe, link[href^="http"]').length === 0, 'External asset or injected markup');
    output.textContent = 'PASS';
  } catch (error) { output.textContent = `FAIL: ${error.message}`; }
  document.body.append(output);
}

const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'miricl-size-html-'));
try {
  const header = Buffer.from([0, 97, 115, 109, 1, 0, 0, 0]);
  fs.writeFileSync(path.join(directory, 'miricl.wasm'), header);
  fs.writeFileSync(path.join(directory, 'miricl-interpreter.wasm'), header);
  const repeatedSections = Buffer.concat([header, Buffer.from([0, 2, 1, 120, 0, 2, 1, 120])]);
  fs.writeFileSync(path.join(directory, 'miricl-repl.wasm'), repeatedSections);
  fs.writeFileSync(path.join(directory, 'miricl-precompiled.wasm'), repeatedSections);
  fs.writeFileSync(path.join(directory, 'miricl-asdf.wasm'), repeatedSections);
  fs.writeFileSync(path.join(directory, 'miricl.html'), '<!doctype html><p>Complete REPL λ🙂</p>'.repeat(100));
  fs.writeFileSync(path.join(directory, 'todo.html'), '<script type="text/x-common-lisp">(print "TODO λ🙂")</script>'.repeat(150));
  fs.mkdirSync(path.join(directory, 'game-of-life/build'), {recursive: true});
  fs.writeFileSync(path.join(directory, 'game-of-life/build/index.html'), '<!doctype html><p>Life λ🙂</p>'.repeat(100));
  fs.writeFileSync(path.join(directory, 'game-of-life/build/miricl.js'), 'export class Miricl {}');
  for (const file of ['miricl.wasm', 'game-of-life.wasm'])
    fs.writeFileSync(path.join(directory, 'game-of-life/build', file), repeatedSections);
  fs.mkdirSync(path.join(directory, 'build/maxima/site'), {recursive: true});
  for (const file of ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs',
    'miricl.mjs', 'maxima.mjs'])
    fs.writeFileSync(path.join(directory, 'build/maxima/site', file), 'Maxima fixture λ🙂'.repeat(20));
  for (const file of ['miricl.wasm', 'maxima.core'])
    fs.writeFileSync(path.join(directory, 'build/maxima/site', file), repeatedSections);
  fs.mkdirSync(path.join(directory, 'build/fricas/site'), {recursive: true});
  for (const file of ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs',
    'miricl.mjs', 'fricas.mjs', 'filesystem-archive.mjs', 'mounted-filesystem.mjs', 'filesystem.bin.gz'])
    fs.writeFileSync(path.join(directory, 'build/fricas/site', file), 'FriCAS fixture λ🙂'.repeat(20));
  for (const file of ['miricl.wasm', 'fricas.wasm'])
    fs.writeFileSync(path.join(directory, 'build/fricas/site', file), repeatedSections);
  fs.mkdirSync(path.join(directory, 'build/acl2/site'), {recursive: true});
  for (const file of ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs', 'worker.mjs',
    'miricl.mjs', 'acl2.mjs', 'acl2-books.mjs', 'dev-filesystem.mjs', 'filesystem-archive.mjs', 'filesystem.bin.gz', 'build.json'])
    fs.writeFileSync(path.join(directory, 'build/acl2/site', file), 'ACL2 fixture λ🙂'.repeat(20));
  for (const file of ['miricl.wasm', 'acl2.wasm'])
    fs.writeFileSync(path.join(directory, 'build/acl2/site', file), repeatedSections);
  fs.mkdirSync(path.join(directory, 'build/acl2/books'), {recursive: true});
  fs.writeFileSync(path.join(directory, 'build/acl2/books/manifest.json'), '{}');
  fs.writeFileSync(path.join(directory, 'build/acl2/books/filesystem.bin.gz'), gzipSync('book sources'));
  fs.mkdirSync(path.join(directory, 'build/coalton/site'), {recursive: true});
  for (const file of ['index.html', 'client.mjs', 'repl-debugger.mjs', 'app.mjs', 'worker.mjs', 'examples.mjs', 'session-store.mjs', 'coalton.mjs', 'miricl.mjs', 'build.json'])
    fs.writeFileSync(path.join(directory, 'build/coalton/site', file), 'Coalton fixture λ🙂'.repeat(20));
  for (const file of ['miricl.wasm', 'coalton.wasm'])
    fs.writeFileSync(path.join(directory, 'build/coalton/site', file), repeatedSections);
  const hostile = '</dd><script>globalThis.injected=true</script><img src=x onerror="globalThis.injected=true"> & λ🙂';
  const report = collectImageSizes(directory, {generatedAt: '2026-11-05T12:00:00.000Z',
    commit: hostile, runURL: 'https://example.com/run?a=1&b=2'});
  checkMeasurements(report, directory);
  const checkpoint = path.join(directory, 'build/acl2/site/acl2-image.wasm.gz');
  fs.writeFileSync(checkpoint, gzipSync(repeatedSections));
  fs.writeFileSync(path.join(directory, 'build/acl2/site/build.json'), JSON.stringify({imageBytes: repeatedSections.length}));
  const restoredReport = collectImageSizes(directory);
  checkMeasurements(restoredReport, directory);
  assert.equal(restoredReport.artifacts.find(artifact => artifact.file.endsWith('/acl2-image.wasm.gz')).uncompressedBytes,
    repeatedSections.length);
  assert.match(renderImageSizes(restoredReport), /<a href="acl2\/">ACL2<\/a>/);
  fs.rmSync(checkpoint);
  const coaltonCheckpoint = path.join(directory, 'build/coalton/site/coalton-image.wasm.gz');
  fs.writeFileSync(coaltonCheckpoint, gzipSync(repeatedSections));
  fs.writeFileSync(path.join(directory, 'build/coalton/site/build.json'), JSON.stringify({snapshotBytes: repeatedSections.length}));
  const restoredCoalton = collectImageSizes(directory);
  checkMeasurements(restoredCoalton, directory);
  assert.equal(restoredCoalton.artifacts.find(artifact => artifact.file.endsWith('/coalton-image.wasm.gz')).uncompressedBytes,
    repeatedSections.length);
  assert.doesNotThrow(() => renderImageSizes(restoredCoalton));
  fs.rmSync(coaltonCheckpoint);
  const withoutAcl2 = {...report, artifacts: report.artifacts.filter(a => a.file !== 'acl2/' && !a.file.startsWith('build/acl2/'))};
  assert.doesNotThrow(() => renderImageSizes(withoutAcl2), 'Existing CI builds can omit the development ACL2 artifacts');
  assert.doesNotThrow(() => renderImageSizes({...withoutAcl2,
    artifacts: withoutAcl2.artifacts.filter(a => a.file !== 'coalton/' && !a.file.startsWith('build/coalton/'))}));
  const precompiled = report.artifacts.find(artifact => artifact.file === 'miricl-precompiled.wasm');
  assert.deepEqual(precompiled.sections.find(s => s.name === 'custom:x'),
    {name: 'custom:x', bytes: 8, count: 2});
  assert.match(precompiled.description, /bootstrapped native compiler/);
  const hostileReport = structuredClone(report);
  hostileReport.artifacts[0].description = hostile;
  hostileReport.artifacts[0].sections[0].name = hostile;
  const html = renderImageSizes(hostileReport);
  assert.ok(!html.includes(hostile));
  assert.ok(!html.includes('<script>'), 'The report itself needs no JavaScript');
  assert.ok(!html.includes('@@'));
  assert.match(html, /run\?a=1&amp;b=2/);
  assert.ok(!renderImageSizes({...report, runURL: 'javascript:alert(1)'}).includes('javascript:'));
  assert.throws(() => renderImageSizes({...report, artifacts: []}), /Expected measurements/);
  assert.throws(() => renderImageSizes({...report, artifacts: report.artifacts.map(a => ({...a, rawBytes: -1}))}), /Invalid/);
  assert.throws(() => renderImageSizes(report, {replPage: 'javascript:alert(1)'}), /local filenames/);
  const corrupt = structuredClone(report);
  corrupt.artifacts[0].sections[0].bytes++;
  assert.throws(() => renderImageSizes(corrupt), /Section sizes/);
  const incorrectTotal = structuredClone(report);
  incorrectTotal.artifacts.find(a => a.kind === 'application').rawBytes++;
  assert.throws(() => renderImageSizes(incorrectTotal), /Application totals/);

  const builder = fileURLToPath(new URL('../scripts/build-size-html.mjs', import.meta.url));
  const generated = spawnSync(process.execPath, [builder, directory], {encoding: 'utf8'});
  assert.ifError(generated.error);
  assert.equal(generated.status, 0, generated.stderr);
  checkMeasurements(JSON.parse(fs.readFileSync(path.join(directory, 'image-sizes.json'), 'utf8')), directory);
  assert.match(fs.readFileSync(path.join(directory, 'image-sizes.html'), 'utf8'), /href="miricl.html"/);
  assert.match(renderImageSizes(report, {replPage: 'index.html'}), /href="index.html"/);
  const missing = path.join(directory, 'todo.html');
  fs.renameSync(missing, missing + '.saved');
  assert.throws(() => collectImageSizes(directory), /todo\.html/, 'Missing browser pages must not silently disappear');
  fs.renameSync(missing + '.saved', missing);

  const browserReports = [{html, report: hostileReport, width: 390}];
  if (process.argv[2]) {
    const build = path.resolve(process.argv[2]);
    const published = path.resolve(process.argv[3] ?? build);
    const actual = JSON.parse(fs.readFileSync(path.join(published, 'image-sizes.json'), 'utf8'));
    checkMeasurements(actual, build);
    browserReports.push({html: fs.readFileSync(path.join(published, 'image-sizes.html'), 'utf8'), report: actual, width: 1200});
  }
  for (const [index, page] of browserReports.entries()) {
    const file = path.join(directory, `browser-${index}.html`);
    const expected = page.report.artifacts.map(({file, kind, rawBytes, gzipBytes, brotliBytes}) => ({file, kind, rawBytes, gzipBytes, brotliBytes}));
    fs.writeFileSync(file, page.html.replace('</body>', `<script>(${browserChecks.toString()})(${JSON.stringify(expected)});</script></body>`));
    const browser = spawnSync(process.env.CHROMIUM ?? 'chromium', [
      '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run',
      '--no-default-browser-check', '--disable-background-networking',
      '--host-resolver-rules=MAP * ~NOTFOUND', `--window-size=${page.width},844`,
      `--user-data-dir=${path.join(directory, `profile-${index}`)}`, '--dump-dom', pathToFileURL(file).href,
    ], {encoding: 'utf8', timeout: 30_000, maxBuffer: 4 * 1024 * 1024});
    assert.ifError(browser.error);
    assert.equal(browser.status, 0, browser.stderr);
    const result = browser.stdout.match(/<output id="browser-test-result">([^<]*)<\/output>/)?.[1];
    assert.equal(result, 'PASS', result ?? browser.stderr);
  }
  console.log('Image sizes: complete HTML and Wasm files, compression, section accounting, CLI, escaping, offline display, and mobile layout pass.');
} finally {
  fs.rmSync(directory, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}
