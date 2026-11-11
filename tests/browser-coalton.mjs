// Exercise the Lisp/Coalton REPL, shared session UI and actual native compiler.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import http from 'node:http';
import {spawn} from 'node:child_process';
const root = path.resolve(process.argv[2] ?? 'build/coalton/site');
const build = JSON.parse(await fs.readFile(path.join(root, 'build.json'), 'utf8'));
const requested = new Set();
// Startup includes loading the complete compiler and standard library. The CI
// session probe currently takes about three minutes before evaluating input.
const startupTimeout = 300000;
const prefix = '/miricl/coalton';
const server = http.createServer(async (request, response) => {
  try {
    const pathname = new URL(request.url, 'http://localhost').pathname;
    requested.add(pathname);
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
  await waitFor('document.querySelector("#evaluate")?.disabled === false', startupTimeout);
  const startupMilliseconds = Math.round(performance.now() - start);
  console.log(`Coalton REPL browser startup: ${startupMilliseconds} ms`);
  if (build.initializedImage) {
    assert.ok(requested.has(`${prefix}/coalton-image.wasm.gz`));
    assert.ok(!requested.has(`${prefix}/coalton.wasm`), 'Snapshot startup must not replay the native bundle.');
    assert.ok(!requested.has(`${prefix}/miricl.wasm`), 'The snapshot includes its matching runtime.');
  }
  assert.equal(await js('document.querySelectorAll("#examples .example").length'), 6);
  assert.equal(await js('document.querySelectorAll("#source").length'), 1);
  const run = async (source, mode, failed = false) => {
    const count = await js('document.querySelectorAll("#history li").length');
    if (source !== null) await js(`document.querySelector("#source").value = ${JSON.stringify(source)}`);
    if (mode !== undefined) await js(`document.querySelector("#mode").value = ${JSON.stringify(mode)}`);
    await js('document.querySelector("#evaluate").click()');
    await waitFor(`document.querySelectorAll("#history li").length === ${count + 1} &&
      document.querySelector("#evaluate")?.disabled === false`);
    assert.equal(await js('!!document.querySelector("#history li:last-child .error")'), failed);
    return js('[...document.querySelectorAll("#history li:last-child pre:not(.input)")].map(node => node.textContent).join("\\n")');
  };
  assert.match(await run('(+ 20 22)', 'lisp'), /42/u);
  assert.match(await run('(values 20 22) (values) (values 42)', 'lisp'), /20\s+22\s+42/u);
  await run('(defun browser-square (x) (* x x))', 'lisp');
  assert.match(await run('(browser-square 12)', 'lisp'), /144/u);
  await run('(defpackage :browser-lisp (:use :cl)) (in-package :browser-lisp)', 'lisp');
  await run('(defparameter *counter* 40)', 'lisp');
  assert.match(await run('(incf *counter* 2)', 'lisp'), /42/u);
  await run('(in-package :cl-user)', 'lisp');
  await run('(define (browser-coalton-square x) (* x x))', 'definitions');
  assert.match(await run('(browser-coalton-square 12)', 'expression'), /144/u);
  await run('(+ 1 "text")', 'expression', true);
  assert.match(await run('(browser-square 7)', 'lisp'), /49/u);
  await js('document.querySelector("[data-example=fibonacci]").click()');
  assert.equal(await js('document.querySelector("#mode").value'), 'expression');
  assert.match(await run(null), /354224848179261915075/u);
  await js('document.querySelector("[data-example=tree]").click()');
  assert.match(await run(null), /150/u);
  assert.match(await run('(+ 20 22)', 'lisp'), /42/u);
  await command('Network.emulateNetworkConditions', {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  assert.match(await run('(browser-coalton-square 7)', 'expression'), /49/u);
  await command('Network.emulateNetworkConditions', {offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  await js('document.querySelector("#source").value = "(browser-square 8)"; document.querySelector("#source").dispatchEvent(new Event("input")); document.querySelector("#mode").value = "lisp"; document.querySelector("#mode").dispatchEvent(new Event("change")); globalThis.__coaltonTestReload = true');
  await command('Page.reload');
  await waitFor('globalThis.__coaltonTestReload !== true && document.querySelector("#resume")?.disabled === false', startupTimeout);
  assert.equal(await js('document.querySelector("#source").value'), '(browser-square 8)');
  assert.equal(await js('document.querySelector("#mode").value'), 'lisp');
  assert.equal(await js('document.querySelector("#resume-bar").hidden'), false);
  await js('document.querySelector("#resume").click()');
  await waitFor('document.querySelector("#evaluate")?.disabled === false', startupTimeout);
  assert.match(await run(null), /64/u);
  assert.match(await run('(browser-coalton-square 9)', 'expression'), /81/u);
  console.log('Coalton REPL browser: Lisp and Coalton, package persistence, examples, errors, offline evaluation, saved draft and mixed-mode session replay passed.');
  await run(`(defun browser-debug-price (quantity)
    (declare (optimize (debug 3)))
    (let ((subtotal (* 12 quantity)))
      (restart-case (error "Choose a discount")
        (use-discount (discount) (* subtotal (- 1 (/ discount 100)))))))`, 'lisp');
  await js(`document.querySelector('#source').value = '(browser-debug-price 3)';
    document.querySelector('#evaluate').click()`);
  await waitFor('document.querySelector("#debugger").open && document.querySelector("#debug-condition").textContent.includes("Choose a discount")', 15000);
  await js(`const frames = document.querySelector('#debug-frame');
    frames.selectedIndex = [...frames.options].findIndex(option => option.textContent.includes('BROWSER-DEBUG-PRICE'));
    frames.dispatchEvent(new Event('change'))`);
  assert.match(await js('document.querySelector("#debug-locals").textContent'), /SUBTOTAL = 36/u);
  await js(`document.querySelector('#debug-expression').value = '(values subtotal quantity)';
    document.querySelector('#debug-evaluate').click()`);
  await waitFor('document.querySelector("#debug-result .debug-evaluation pre:last-child")?.textContent === "36\\n3"');
  await js(`document.querySelector('#debug-arguments').value = '10';
    [...document.querySelectorAll('#debug-restarts button')].find(button => button.textContent.includes('USE-DISCOUNT')).click()`);
  await waitFor('!document.querySelector("#debug-command-error").hidden && !document.querySelector("#debug-restarts button").disabled');
  assert.match(await js('document.querySelector("#debug-command-error").textContent'), /Restart failed/u);
  await js(`document.querySelector('#debug-arguments').value = '(list 10)';
    [...document.querySelectorAll('#debug-restarts button')].find(button => button.textContent.includes('USE-DISCOUNT')).click()`);
  await waitFor('!document.querySelector("#debugger").open && !document.querySelector("#evaluate").disabled');
  assert.match(await js('document.querySelector("#history li:last-child").textContent'), /162\/5/u);
  // Native code retains its calling Lisp frame when the user requests DEBUG 3.
  await js(`document.querySelector('#source').value = ${JSON.stringify(`(cl:in-package :coalton-user)
    (cl:locally (cl:declare (cl:optimize (cl:debug 3)))
      (coalton (lisp (-> Integer) () (cl:break "Coalton inline Lisp breakpoint") 42)))
    (cl:in-package :cl-user)`)};
    document.querySelector('#mode').value = 'lisp'; document.querySelector('#evaluate').click()`);
  await waitFor('document.querySelector("#debugger").open && document.querySelector("#debug-condition").textContent.includes("Coalton inline Lisp breakpoint")', 15000);
  await js(`[...document.querySelectorAll('#debug-restarts button')].find(button => button.textContent.includes('CONTINUE')).click()`);
  await waitFor('!document.querySelector("#debugger").open && !document.querySelector("#evaluate").disabled');
  assert.match(await js('document.querySelector("#history li:last-child").textContent'), /42/u);
  console.log('Coalton debugger: Lisp frames and locals, frame evaluation, failed restart recovery and continuing from compiled Coalton inline Lisp passed.');
  await js('document.querySelector("#source").value = "(cl:loop)"; document.querySelector("#mode").value = "lisp"; document.querySelector("#evaluate").click(); globalThis.ticks=0; setInterval(()=>ticks++,20)');
  await waitFor('document.querySelector("#evaluate").disabled === true');
  await new Promise(resolve => setTimeout(resolve, 200));
  assert.ok(await js('ticks >= 3'), 'Evaluation must not block the page.');
  await js('document.querySelector("#restart").click()');
  await waitFor('document.querySelector("#evaluate")?.disabled === false', startupTimeout);
  await run('(browser-square 7)', 'lisp', true);
  assert.match(await run('(+ 20 22)', 'lisp'), /42/u);
  const desktop = await command('Page.captureScreenshot', {format: 'png'});
  await fs.writeFile(path.join(root, 'preview-desktop.png'), Buffer.from(desktop.data, 'base64'));
  await command('Emulation.setDeviceMetricsOverride', {width: 390, height: 844, deviceScaleFactor: 1, mobile: true});
  assert.equal(await js('document.documentElement.scrollWidth <= innerWidth'), true);
  const mobile = await command('Page.captureScreenshot', {format: 'png'});
  await fs.writeFile(path.join(root, 'preview-mobile.png'), Buffer.from(mobile.data, 'base64'));
  await fs.writeFile(path.join(root, 'browser-test.json'), JSON.stringify({passed: true, startupMilliseconds,
    initializedImage: Boolean(build.initializedImage)}) + '\n');
  console.log('Coalton browser: Lisp/Coalton input, documentation examples, saved sessions, cancellation, restart and mobile layout passed.');
} finally {
  fail(new Error('Browser test finished.'));
  server.closeAllConnections(); await new Promise(resolve => server.close(resolve));
  if (browser.exitCode === null && browser.signalCode === null) browser.kill('SIGTERM');
  await stopped;
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
}
