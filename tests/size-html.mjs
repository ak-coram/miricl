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
  assert.deepEqual(report.artifacts.map(artifact => artifact.file),
    ['miricl-interpreter.wasm', 'miricl-repl.wasm', 'miricl.html', 'todo.html']);
  for (const artifact of report.artifacts) {
    const bytes = fs.readFileSync(path.join(directory, artifact.file));
    assert.equal(artifact.rawBytes, bytes.length, artifact.file);
    assert.equal(artifact.gzipBytes, gzipSync(bytes, {level: 9}).length, artifact.file);
    assert.equal(artifact.brotliBytes, brotliCompressSync(bytes,
      {params: {[constants.BROTLI_PARAM_QUALITY]: 11}}).length, artifact.file);
    if (artifact.kind === 'wasm')
      assert.equal(artifact.sections.reduce((sum, section) => sum + section.bytes, 0), artifact.rawBytes);
    else assert.equal(artifact.sections, undefined, 'HTML is measured as a complete file');
  }
}

function browserChecks(expected) {
  const check = (condition, message) => { if (!condition) throw new Error(message); };
  const output = document.createElement('output');
  output.id = 'browser-test-result';
  try {
    check(!globalThis.injected, 'Metadata executed as HTML or JavaScript');
    check(document.querySelectorAll('#artifact-sizes tbody tr').length === 4, 'Four artifacts are required');
    for (const artifact of expected) {
      const row = document.querySelector(`[data-artifact="${artifact.file}"]`);
      for (const key of ['rawBytes', 'gzipBytes', 'brotliBytes']) {
        const cell = row.querySelector(`[data-size="${key}"]`);
        check(cell.querySelector('small').textContent === `${artifact[key].toLocaleString('en-US')} bytes`, `${artifact.file}: incorrect ${key}`);
        check(cell.querySelector('strong').textContent === `${(artifact[key] / 1024).toFixed(2)} KiB`, 'Incorrect KiB conversion');
      }
    }
    const sections = document.querySelectorAll('.sections');
    check(sections.length === 2, 'Both Wasm section breakdowns are required');
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
  fs.writeFileSync(path.join(directory, 'miricl-interpreter.wasm'), header);
  fs.writeFileSync(path.join(directory, 'miricl-repl.wasm'), Buffer.concat([header, Buffer.from([0, 2, 1, 120])]));
  fs.writeFileSync(path.join(directory, 'miricl.html'), '<!doctype html><p>Complete REPL λ🙂</p>'.repeat(100));
  fs.writeFileSync(path.join(directory, 'todo.html'), '<script type="text/x-common-lisp">(print "TODO λ🙂")</script>'.repeat(150));
  const hostile = '</dd><script>globalThis.injected=true</script><img src=x onerror="globalThis.injected=true"> & λ🙂';
  const report = collectImageSizes(directory, {generatedAt: '2026-11-05T12:00:00.000Z',
    commit: hostile, runURL: 'https://example.com/run?a=1&b=2'});
  checkMeasurements(report, directory);
  const hostileReport = structuredClone(report);
  hostileReport.artifacts[0].description = hostile;
  hostileReport.artifacts[0].sections[0].name = hostile;
  const html = renderImageSizes(hostileReport);
  assert.ok(!html.includes(hostile));
  assert.ok(!html.includes('<script>'), 'The report itself needs no JavaScript');
  assert.ok(!html.includes('@@'));
  assert.match(html, /run\?a=1&amp;b=2/);
  assert.ok(!renderImageSizes({...report, runURL: 'javascript:alert(1)'}).includes('javascript:'));
  assert.throws(() => renderImageSizes({...report, artifacts: []}), /both Wasm/);
  assert.throws(() => renderImageSizes({...report, artifacts: report.artifacts.map(a => ({...a, rawBytes: -1}))}), /Invalid/);
  assert.throws(() => renderImageSizes(report, {replPage: 'javascript:alert(1)'}), /local filenames/);
  const corrupt = structuredClone(report);
  corrupt.artifacts[0].sections[0].bytes++;
  assert.throws(() => renderImageSizes(corrupt), /Section sizes/);

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
    const expected = page.report.artifacts.map(({file, rawBytes, gzipBytes, brotliBytes}) => ({file, rawBytes, gzipBytes, brotliBytes}));
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
