#!/usr/bin/env node
// CHROMIUM=chromium node tests/ansi-html.mjs
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import {spawnSync} from 'node:child_process';
import {gzipSync, brotliCompressSync, constants} from 'node:zlib';
import {renderAnsiReport} from '../scripts/build-ansi-html.mjs';
import {measureWasmImages} from '../scripts/wasm-size.mjs';

const hostile = '</pre><script>globalThis.injected = true</script><img src=x onerror="globalThis.injected=true"> & λ 🙂';
const report = {
  profile: 'full', startedAt: '2026-11-03T12:00:00.000Z', elapsedMs: 1234,
  timeout: 10000, performanceBudgetMs: 2000, heapLimitMb: 512, inventory: 'SBCL inventory; WASM test bodies',
  imageSizes: [
    {file: '/unavailable/miricl-interpreter.wasm', rawBytes: 278479, gzipBytes: 128962, brotliBytes: 113743},
    {file: '/unavailable/repl&"λ.wasm', rawBytes: 279578, gzipBytes: 129646, brotliBytes: 114337},
  ],
  tests: [
    {name: 'CL-TEST::PASS', file: 'numbers/add.lsp', status: 'pass', output: ':PASS\n', elapsedMs: 2500, slow: true},
    {name: 'CL-TEST::FAIL', file: 'numbers/add.lsp', status: 'fail', output: hostile,
      error: 'Expected 3, got 2', code: 4},
    {name: 'CL-TEST::BLOCKED', file: 'arrays/load.lsp', status: 'blocked', output: ':BLOCKED\n'},
    {name: 'CL-TEST::TIMEOUT', file: 'arrays/load.lsp', status: 'timeout',
      error: 'Exceeded 2000 ms.', interrupted: true},
    {name: hostile, file: '__proto__', status: 'pass', output: ':PASS\n', extra: true},
  ],
  loadErrors: [{file: 'arrays/"<load>.lsp', source: hostile, error: 'Cannot register test', code: 4}],
  inventoryMismatches: [{file: '__proto__', extra: 1, tests: [hostile], source: '(deftest extra 1 1)'}],
  summary: {complete: true, passed: 2, failed: 1, blocked: 1, timedOut: 1, total: 5,
    expected: 5, referenceTests: 4, extraTests: 1},
  error: null,
};
const html = renderAnsiReport(report, {commit: 'abc123', runURL: 'https://example.com/run?a=1&b=2'});
assert.match(html, /2,500 ms, above performance budget/);
assert.match(html, /interpreter\.wasm raw<\/dt><dd>278,479 bytes/);
assert.match(html, /interpreter\.wasm gzip \(level 9\)<\/dt><dd>128,962 bytes/);
assert.match(html, /interpreter\.wasm Brotli \(quality 11\)<\/dt><dd>113,743 bytes/);
assert.match(html, /repl&amp;&quot;λ\.wasm raw<\/dt><dd>279,578 bytes/);
assert.match(html, /repl&amp;&quot;λ\.wasm gzip \(level 9\)<\/dt><dd>129,646 bytes/);
assert.match(html, /repl&amp;&quot;λ\.wasm Brotli \(quality 11\)<\/dt><dd>114,337 bytes/);
assert.match(renderAnsiReport({...report, imageSizes: undefined}), /Image sizes<\/dt><dd>Not recorded/);
const upstream = renderAnsiReport({...report, runner: 'upstream', totalElapsedMs: 2345,
  loadElapsedMs: 321, loadTimeout: 120000});
assert.match(upstream, /Duration<\/dt><dd>2\.3 seconds/);
assert.match(upstream, /Upstream initialization<\/dt><dd>0\.3 seconds/);
assert.match(upstream, /Per-test deadline<\/dt><dd>10,000 ms/);
assert.match(html, /Complete run · 5 \/ 5 tests accounted for/);
assert.match(html, /40\.0% of recorded tests passed/);
assert.match(html, /Load errors \(1\)/);
assert.match(html, /Inventory mismatches \(1\)/);
assert.match(html, /interrupted: true/);
assert.ok(!html.includes(hostile));
assert.ok(!html.includes('@@'));
assert.ok(!/<(?:script|link|img)[^>]+(?:src|href)="https?:/i.test(html), 'Assets must be embedded');
assert.match(html, /https:\/\/example.com\/run\?a=1&amp;b=2/);
assert.ok(!renderAnsiReport(report, {runURL: 'javascript:alert(1)'}).includes('javascript:'));
assert.throws(() => renderAnsiReport({...report, profile: 'smoke'}), /full ANSI/);
assert.throws(() => renderAnsiReport({...report, tests: {}}), /array/);
assert.throws(() => renderAnsiReport({...report, tests: [{status: 'toString'}]}), /Unknown test status/);
const checkpoint = renderAnsiReport({...report, summary: {complete: false, total: 5},
  progress: {totalTests: 10}});
assert.match(checkpoint, /Incomplete run · 5 \/ 10 tests accounted for/);
assert.match(checkpoint, /40\.0% of recorded tests passed/);
const aborted = renderAnsiReport({profile: 'full', summary: {complete: false}, error: hostile});
assert.match(aborted, /Incomplete run · 0 \/ unknown tests accounted for/);
assert.match(aborted, /No test results were recorded/);
assert.ok(!aborted.includes(hostile));
assert.match(renderAnsiReport({...report, error: 'Fatal error'}), /Incomplete run/);
assert.match(renderAnsiReport({...report, summary: {...report.summary, expected: 6}}), /Incomplete run/);

// Exercise the generated page directly from disk with all network access blocked.
// The inline probe runs after the page's own script, without browser dependencies.
function browserChecks() {
  const check = (condition, message) => { if (!condition) throw new Error(message); };
  const visibleTests = () => [...document.querySelectorAll('.test')].filter(test => !test.hidden);
  const setSearch = value => {
    document.getElementById('search').value = value;
    document.getElementById('search').dispatchEvent(new Event('input'));
  };
  const setStatus = value => {
    document.getElementById('status').value = value;
    document.getElementById('status').dispatchEvent(new Event('change'));
  };
  const result = document.createElement('output');
  result.id = 'browser-test-result';
  try {
    check(!globalThis.injected, 'Report text executed as JavaScript');
    const runInfo = document.querySelector('.run-info');
    runInfo.querySelector('summary').click();
    check(runInfo.open && runInfo.textContent.includes('128,962 bytes')
      && runInfo.textContent.includes('114,337 bytes'), 'Compressed image sizes are missing');
    runInfo.querySelector('summary').click();
    check(document.querySelectorAll('.test-body img, .test-body script').length === 0, 'Report text became markup');
    check(!document.getElementById('controls').hidden, 'Filter controls are unavailable');
    check(document.querySelectorAll('.group').length === 3, 'Incorrect category grouping');
    check(document.querySelectorAll('.file').length === 3, 'Incorrect source-file grouping');
    check(getComputedStyle(document.querySelector('main')).backgroundColor === 'rgb(255, 255, 234)', 'Plan 9 paper color');
    const firstGroup = document.querySelector('.group');
    firstGroup.querySelector('summary').click();
    check(firstGroup.open, 'Native category disclosure did not open');
    const failure = document.querySelector('.test[data-status="fail"]');
    failure.querySelector('summary').click();
    check(failure.open && failure.textContent.includes('Expected 3, got 2'), 'Failure details are missing');
    check(failure.querySelector('pre:last-child').textContent.includes('λ 🙂'), 'Output text lost Unicode');
    setStatus('not-passed');
    check(visibleTests().length === 3, 'Not-passed filter must include failures, blocked tests, and timeouts');
    setSearch('NUMBERS/ADD');
    check(visibleTests().length === 1 && visibleTests()[0].dataset.status === 'fail', 'Combined filters or case folding failed');
    check([...document.querySelectorAll('.group')].filter(group => !group.hidden).length === 1, 'Empty groups stayed visible');
    check(document.querySelector('.file:not([hidden])').open, 'Search did not reveal its results');
    setStatus('pass');
    check(visibleTests().length === 1, 'Passed filter failed');
    setSearch('no such test');
    check(!document.getElementById('empty').hidden && visibleTests().length === 0, 'Empty search state failed');
    setSearch('');
    setStatus('timeout');
    check(visibleTests().length === 1 && visibleTests()[0].textContent.includes('Exceeded 2000 ms.'), 'Timeout details missing');
    setStatus('blocked');
    check(visibleTests().length === 1 && visibleTests()[0].dataset.status === 'blocked', 'Blocked filter failed');
    setStatus('all');
    check(visibleTests().length === 5 && document.getElementById('visible-count').textContent === '5 tests shown', 'Reset filters failed');
    document.getElementById('expand').click();
    check([...document.querySelectorAll('.group, .file')].every(element => element.open), 'Expand groups failed');
    document.getElementById('collapse').click();
    check(document.querySelectorAll('details[open]').length === 0, 'Collapse all failed');
    check(document.documentElement.scrollWidth <= innerWidth, 'Page overflows a mobile viewport');
    result.textContent = 'PASS';
  } catch (error) {
    result.textContent = `FAIL: ${error.message}`;
  }
  document.body.append(result);
}
const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'miricl-ansi-html-'));
try {
  const binary = path.join(directory, 'miricl-interpreter.wasm');
  const companion = path.join(directory, 'miricl-repl.wasm');
  const core = Buffer.from([0, 97, 115, 109, 1, 0, 0, 0]);
  const repl = Buffer.concat([core, Buffer.from([0, 2, 1, 120])]);
  fs.writeFileSync(binary, core);
  assert.equal(measureWasmImages(binary).length, 1, 'A WASI image is optional');
  fs.writeFileSync(companion, repl);
  const imageSizes = measureWasmImages(binary);
  assert.deepEqual(imageSizes, [core, repl].map((bytes, index) => ({
    file: index ? companion : binary, rawBytes: bytes.length,
    gzipBytes: gzipSync(bytes, {level: 9}).length,
    brotliBytes: brotliCompressSync(bytes,
      {params: {[constants.BROTLI_PARAM_QUALITY]: 11}}).length,
  })));
  assert.equal(measureWasmImages(companion).length, 1, 'Do not duplicate the tested image');
  fs.unlinkSync(binary);
  fs.unlinkSync(companion);
  assert.match(renderAnsiReport(JSON.parse(JSON.stringify({...report, imageSizes}))),
    /interpreter\.wasm raw<\/dt><dd>8 bytes/);
  const file = path.join(directory, 'ansi-results.html');
  fs.writeFileSync(file, html.replace('</body>', `<script>(${browserChecks.toString()})();</script></body>`));
  const browser = spawnSync(process.env.CHROMIUM ?? 'chromium', [
    '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run',
    '--no-default-browser-check', '--disable-background-networking',
    '--host-resolver-rules=MAP * ~NOTFOUND', '--window-size=390,844',
    `--user-data-dir=${path.join(directory, 'profile')}`, '--dump-dom', pathToFileURL(file).href,
  ], {encoding: 'utf8', timeout: 30_000, maxBuffer: 4 * 1024 * 1024});
  if (browser.error) throw browser.error;
  assert.equal(browser.status, 0, browser.stderr);
  const result = browser.stdout.match(/<output id="browser-test-result">([^<]*)<\/output>/)?.[1];
  assert.equal(result, 'PASS', result ?? browser.stderr);
  console.log('ANSI HTML: image sizes, counts, partial runs, diagnostics, escaping, offline disclosures, filters, and mobile layout pass.');
} finally {
  fs.rmSync(directory, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}
