// Exercise the production tour, worker, native compiler and standard library.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import http from 'node:http';
import {spawn} from 'node:child_process';
const root = path.resolve(process.argv[2] ?? 'build/coalton/site');
// Startup includes loading the complete compiler and standard library. The CI
// session probe currently takes about three minutes before evaluating input.
const startupTimeout = 300000;
const prefix = '/miricl/coalton';
const server = http.createServer(async (request, response) => {
  try {
    const pathname = new URL(request.url, 'http://localhost').pathname;
    if (!pathname.startsWith(prefix + '/')) {response.writeHead(404).end(); return;}
    const relative = decodeURIComponent(pathname.slice(prefix.length));
    const file = path.resolve(root, '.' + (relative === '/' ? '/index.html' : relative));
    if (!file.startsWith(root + path.sep)) {response.writeHead(403).end(); return;}
    response.setHeader('Content-Type', file.endsWith('.mjs') ? 'text/javascript' :
      file.endsWith('.html') ? 'text/html' : file.endsWith('.wasm') ? 'application/wasm' : 'application/octet-stream');
    response.end(await fs.readFile(file));
  } catch {response.writeHead(404).end();}
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-coalton-browser-'));
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
  '--disable-background-networking', `--user-data-dir=${profile}`, '--no-startup-window', '--remote-debugging-pipe',
], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
const stopped = new Promise(resolve => browser.once('exit', resolve));
let diagnostics = '', received = '', nextId = 0;
const pending = new Map();
function fail(error) {
  for (const {reject, timer} of pending.values()) {clearTimeout(timer); reject(error);}
  pending.clear();
}
browser.stderr.on('data', bytes => {diagnostics = (diagnostics + bytes).slice(-4000);});
browser.on('error', fail);
browser.on('exit', code => fail(new Error(`Chromium exited (${code}): ${diagnostics}`)));
browser.stdio[4].on('data', bytes => {
  received += bytes;
  let end;
  while ((end = received.indexOf('\0')) !== -1) {
    const message = JSON.parse(received.slice(0, end)); received = received.slice(end + 1);
    const request = pending.get(message.id);
    if (!request) continue;
    pending.delete(message.id); clearTimeout(request.timer);
    if (message.error) request.reject(new Error(JSON.stringify(message.error)));
    else request.resolve(message.result);
  }
});
function send(method, params = {}, sessionId) {
  return new Promise((resolve, reject) => {
    const id = ++nextId;
    const timer = setTimeout(() => {pending.delete(id); reject(new Error(`Timed out: ${method}`));}, 45000);
    pending.set(id, {resolve, reject, timer});
    browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
  });
}
try {
  console.log(`Chromium: ${(await send('Browser.getVersion')).product}`);
  const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
  const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
  const command = (method, params) => send(method, params, sessionId);
  const js = async expression => {
    const result = await command('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true});
    assert.equal(result.exceptionDetails, undefined, JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };
  const waitFor = async (expression, milliseconds = 120000) => {
    const deadline = Date.now() + milliseconds;
    while (!(await js(expression))) {
      assert.ok(Date.now() < deadline, `Timed out: ${await js('document.querySelector("#status")?.textContent + " " + document.querySelector("#startup")?.textContent')}`);
      await new Promise(resolve => setTimeout(resolve, 100));
    }
  };
  await command('Page.enable'); await command('Network.enable');
  await command('Emulation.setDeviceMetricsOverride', {width: 1200, height: 900, deviceScaleFactor: 1, mobile: false});
  const start = performance.now();
  await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}${prefix}/`});
  await waitFor('document.querySelector(".run")?.disabled === false', startupTimeout);
  const startupMilliseconds = Math.round(performance.now() - start);
  console.log(`Coalton browser startup: ${startupMilliseconds} ms`);
  assert.equal(await js('document.querySelectorAll(".cell").length'), 139);
  const run = async (id, source, mode = 'lisp', failed = false) => {
    if (source !== null) await js(`document.querySelector('#${id} textarea').value = ${JSON.stringify(source)};
      document.querySelector('#${id} .mode').value = ${JSON.stringify(mode)}`);
    await js(`document.querySelector('#${id} .run').click()`);
    await waitFor(`document.querySelector('#${id} .timing').textContent !== 'Running…' && !document.querySelector('#${id} .run').disabled`);
    assert.equal(await js(`document.querySelector('#${id} .result').classList.contains('error')`), failed);
    return js(`document.querySelector('#${id} .result').textContent`);
  };
  assert.match(await run('example-1', '(+ 20 22)', 'expression'), /42/u);
  assert.match(await run('example-1', '(cl:+ 10 11) (cl:+ 20 22)'), /21\s+42/u);
  await run('example-13', null);
  assert.match(await run('example-14', null), /1\.0/u);
  await run('example-2', '(define (browser-square x) (* x x))', 'definitions');
  assert.match(await run('example-3', '(browser-square 12)', 'expression'), /144/u);
  await run('example-4', '(+ 1 "text")', 'expression', true);
  assert.match(await run('example-3', '(browser-square 7)', 'expression'), /49/u);
  await command('Network.emulateNetworkConditions', {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  assert.match(await run('example-1', '(+ 40 2)', 'expression'), /42/u);
  await command('Network.emulateNetworkConditions', {offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  await js(`document.querySelector('#example-1 .restore').click()`);
  assert.match(await js('document.querySelector("#example-1 textarea").value'), /asdf:defsystem/u);
  await js(`document.querySelector('#example-1 textarea').value = '(cl:loop)';
    document.querySelector('#example-1 .mode').value = 'lisp';
    document.querySelector('#example-1 .run').click()`);
  await waitFor('!document.querySelector("#cancel").hidden');
  await js('globalThis.ticks=0; setInterval(()=>ticks++,20)');
  await new Promise(resolve => setTimeout(resolve, 200));
  assert.ok(await js('ticks >= 3'), 'Evaluation must not block the page.');
  await js('document.querySelector("#cancel").click()');
  await waitFor('!document.querySelector(".run").disabled', startupTimeout);
  await run('example-3', '(browser-square 7)', 'expression', true);
  await command('Page.reload'); await waitFor('!document.querySelector(".run").disabled', startupTimeout);
  assert.equal(await js('document.querySelector("#example-3 textarea").value'), '(browser-square 7)');
  await run('example-1', '(+ 20 22)', 'expression');
  const desktop = await command('Page.captureScreenshot', {format: 'png'});
  await fs.writeFile(path.join(root, 'preview-desktop.png'), Buffer.from(desktop.data, 'base64'));
  await command('Emulation.setDeviceMetricsOverride', {width: 390, height: 844, deviceScaleFactor: 1, mobile: true});
  assert.equal(await js('document.documentElement.scrollWidth <= innerWidth'), true);
  const mobile = await command('Page.captureScreenshot', {format: 'png'});
  await fs.writeFile(path.join(root, 'preview-mobile.png'), Buffer.from(mobile.data, 'base64'));
  await fs.writeFile(path.join(root, 'browser-test.json'), JSON.stringify({passed: true, startupMilliseconds}) + '\n');
  console.log('Coalton browser: real tour examples, native compiler, type errors, shared definitions, offline evaluation, saved edits, restore, cancellation, restart and mobile layout passed.');
} finally {
  fail(new Error('Browser test finished.'));
  server.closeAllConnections(); await new Promise(resolve => server.close(resolve));
  if (browser.exitCode === null && browser.signalCode === null) browser.kill('SIGTERM');
  await stopped;
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
}
