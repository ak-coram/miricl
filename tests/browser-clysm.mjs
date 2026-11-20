// Exercise source preloading, an explicit browser bootstrap, and offline use.
// CHROMIUM=chromium node tests/browser-clysm.mjs [build/clysm/site/index.html]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import http from 'node:http';
import {pathToFileURL} from 'node:url';
import {spawn} from 'node:child_process';

const html = path.resolve(process.argv[2] ?? 'build/clysm/site/index.html');
const timeout = Number(process.env.MIRICL_CLYSM_STARTUP_TIMEOUT ?? 1800000);
assert.ok(Number.isSafeInteger(timeout) && timeout > 0, 'Invalid startup timeout.');
const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-clysm-browser-'));
const standalone = path.join(directory, 'clysm.html');
await fs.copyFile(html, standalone);
const archive = path.join(path.dirname(html), 'filesystem.bin.gz');
const prefix = '/miricl/clysm/';
const served = [];
const server = http.createServer(async (request, response) => {
  const name = new URL(request.url, 'http://localhost').pathname;
  served.push(name);
  try {
    const file = name === prefix ? html : name === prefix + 'filesystem.bin.gz' ? archive : null;
    if (!file) {response.writeHead(404).end(); return;}
    response.setHeader('Content-Type', file === html ? 'text/html; charset=utf-8' : 'application/octet-stream');
    response.end(await fs.readFile(file));
  } catch {response.writeHead(500).end();}
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const origin = `http://127.0.0.1:${server.address().port}`;
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run',
  '--no-default-browser-check', '--disable-background-networking',
  `--user-data-dir=${path.join(directory, 'profile')}`, '--no-startup-window',
  '--remote-debugging-pipe',
], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
let diagnostics = '', received = '', nextId = 0;
const pending = new Map(), requests = [], exceptions = [], setupErrors = [], workers = [];
const stopped = new Promise(resolve => browser.once('exit', resolve));
function fail(error) {
  for (const {reject, timer} of pending.values()) {clearTimeout(timer); reject(error);}
  pending.clear();
}
browser.stderr.on('data', bytes => {diagnostics = (diagnostics + bytes).slice(-8000);});
browser.on('error', fail);
browser.on('exit', code => fail(new Error(`Chromium exited (${code}): ${diagnostics}`)));
browser.stdio[3].on('error', fail);
browser.stdio[4].on('error', fail);
browser.stdio[4].on('data', bytes => {
  received += bytes;
  let end;
  while ((end = received.indexOf('\0')) !== -1) {
    const message = JSON.parse(received.slice(0, end)); received = received.slice(end + 1);
    if (message.method === 'Network.requestWillBeSent') requests.push(message.params.request.url);
    if (message.method === 'Runtime.exceptionThrown') exceptions.push(message.params.exceptionDetails);
    if (message.method === 'Target.attachedToTarget' && message.params.targetInfo.type === 'worker') {
      // Worker targets cannot emulate network conditions. Block network URLs
      // before imports run; Network events also audit all worker requests.
      const session = message.params.sessionId;
      workers.push(session);
      (async () => {
        try {
          await send('Network.enable', {}, session);
          await send('Network.setBlockedURLs', {urls: ['http://*', 'https://*']}, session);
          await send('Runtime.enable', {}, session);
        } catch (error) {setupErrors.push(error.message);}
        finally {
          // A setup failure must report promptly instead of leaving the
          // paused worker hanging until the full bootstrap timeout.
          try {await send('Runtime.runIfWaitingForDebugger', {}, session);}
          catch (error) {setupErrors.push(error.message);}
        }
      })().catch(error => {setupErrors.push(error.message);});
    }
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
    const timer = setTimeout(() => {
      pending.delete(id); reject(new Error(`Timed out: ${method}. ${diagnostics}`));
    }, 45000);
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
  async function waitFor(expression, milliseconds = 15000) {
    const deadline = Date.now() + milliseconds;
    let previousStatus;
    while (!(await js(expression))) {
      assert.deepEqual(setupErrors, [], 'Worker offline setup failed.');
      const state = await js('({state: document.body?.dataset.state, status: document.querySelector("#status")?.textContent, output: document.querySelector("#output")?.textContent.slice(-2000)})');
      assert.notEqual(state.state, 'failed', JSON.stringify(state));
      if (state.status !== previousStatus) {console.log(state.status); previousStatus = state.status;}
      assert.ok(Date.now() < deadline, `Timed out: ${JSON.stringify(state)}`);
      await new Promise(resolve => setTimeout(resolve, 250));
    }
  }
  const ready = 'document.body?.dataset.state === "ready" && !document.querySelector("#evaluate").disabled';
  const canStart = 'document.querySelector("#start") && !document.querySelector("#start").disabled';
  async function run(source) {
    const offset = await js(`(() => {
      document.querySelector('#source').value = ${JSON.stringify(source)};
      document.querySelector('#form').requestSubmit();
      return document.querySelector('#output').textContent.length;
    })()`);
    await waitFor(ready);
    return js(`document.querySelector('#output').textContent.slice(${offset})`);
  }
  await command('Page.enable'); await command('Runtime.enable'); await command('Network.enable');
  await command('Target.setAutoAttach', {autoAttach: true, waitForDebuggerOnStart: true, flatten: true});

  // Check local worker loading before the expensive full bootstrap. A copied
  // page uses an explicitly selected source image without a server.
  await command('Network.emulateNetworkConditions',
    {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  await command('Page.navigate', {url: pathToFileURL(standalone).href});
  await waitFor('document.querySelector("input[type=file]") !== null');
  const {root: documentRoot} = await command('DOM.getDocument');
  const {nodeId} = await command('DOM.querySelector', {nodeId: documentRoot.nodeId, selector: 'input[type=file]'});
  await command('DOM.setFileInputFiles', {nodeId, files: [archive]});
  await waitFor(canStart);
  assert.equal(await js('document.querySelector("#evaluate").disabled'), true);
  await js('document.querySelector("#start").click()');
  await waitFor('document.body.dataset.state === "bootstrapping" && /Loading Clysm compiler/.test(document.querySelector("#status").textContent)', 120000);
  await waitFor('Number(document.querySelector("#timer").dataset.milliseconds) > 0');
  await js('document.querySelector("#restart").click()');
  await waitFor(canStart);
  assert.equal(await js('Number(document.querySelector("#timer").dataset.milliseconds)'), 0);

  await command('Network.emulateNetworkConditions',
    {offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  const previousWorkers = workers.length;
  await command('Page.navigate', {url: origin + prefix});
  await waitFor(canStart);
  assert.ok(served.includes(prefix + 'filesystem.bin.gz'), 'Load the source filesystem when the page opens.');
  assert.equal(await js('document.querySelector("#evaluate").disabled'), true);
  assert.equal(await js('Number(document.querySelector("#timer").dataset.milliseconds ?? 0)'), 0);
  await new Promise(resolve => setTimeout(resolve, 300));
  assert.equal(workers.length, previousWorkers, 'Only Start may launch the compiler worker.');
  assert.equal(await js('Number(document.querySelector("#timer").dataset.milliseconds ?? 0)'), 0);
  // The fetched source image is sufficient; bootstrap needs no network.
  await command('Network.emulateNetworkConditions',
    {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  await js('document.querySelector("#start").click()');
  await waitFor('document.body.dataset.state === "bootstrapping"');
  const timerBefore = await js('Number(document.querySelector("#timer").dataset.milliseconds)');
  await new Promise(resolve => setTimeout(resolve, 350));
  assert.ok(await js(`Number(document.querySelector("#timer").dataset.milliseconds) > ${timerBefore}`),
    'The bootstrap timer must advance while worker compilation runs.');
  await waitFor(ready, timeout);
  assert.equal(await js('JSON.parse(document.querySelector("#build-data").textContent).metadata.bootstrap'),
    'browser-miricl-source');
  assert.ok(await js('Number(document.body.dataset.clysmBytes) > 0'), 'Clysm must emit its runtime at startup.');
  console.log(`Clysm browser bootstrap: ${await js('document.body.dataset.bootstrapMilliseconds')} ms; ${await js('document.body.dataset.clysmBytes')} generated Wasm bytes.`);
  const stoppedTimer = await js('document.querySelector("#timer").dataset.milliseconds');
  await new Promise(resolve => setTimeout(resolve, 250));
  assert.equal(await js('document.querySelector("#timer").dataset.milliseconds'), stoppedTimer,
    'The bootstrap timer must stop when the REPL is ready.');
  assert.equal((await run("(reduce #'+ '(19 23))")).trim(), '42');
  await run('(setq clysm::*native-eval-failure* nil)');
  await run('(defun clysm-page-answer (n) (+ n 17))');
  assert.equal((await run('(null clysm::*native-eval-failure*)')).trim(), 'T',
    'The new definition must not fall back to Clysm interpretation.');
  assert.equal((await run("(multiple-value-bind (name warnings failure) (compile 'clysm-page-answer '(lambda (n) (+ n 17))) (declare (ignore warnings)) (list failure (compiled-function-p (fdefinition name)) (funcall name 25)))")).trim(), '(NIL T 42)');
  assert.equal((await run('(values 73 91)')).trim(), '73\n91');
  const printed = await run('(progn (write-string "<img src=x onerror=alert(1)> λ 🙂") (terpri) 81)');
  assert.match(printed, /<img src=x onerror=alert\(1\)> λ 🙂/u);
  assert.match(printed, /81\s*$/u);
  assert.equal(await js('document.querySelectorAll("#output img").length'), 0);
  assert.match(await run('(error "clysm-page-recovery")'), /clysm-page-recovery/u);
  assert.match(await run(')'), /error/iu);
  assert.equal((await run('(clysm-page-answer 6)')).trim(), '23');
  await run('(defpackage "CLYSM-PAGE-TEST" (:use "COMMON-LISP"))');
  await run('(in-package "CLYSM-PAGE-TEST")');
  assert.match(await js('document.querySelector("#prompt").textContent'), /CLYSM-PAGE-TEST/u);
  assert.equal((await run('(let ((a 9))\n  (* a a))')).trim(), '81');
  await run('(in-package "CL-USER")');
  await command('Emulation.setDeviceMetricsOverride', {width: 390, height: 844, deviceScaleFactor: 1, mobile: true});
  assert.equal(await js('document.documentElement.scrollWidth <= innerWidth'), true);

  // The page must remain responsive even when Lisp does not return.
  await js('document.querySelector("#source").value = "(loop)"; document.querySelector("#form").requestSubmit()');
  await waitFor('document.body.dataset.state === "evaluating"');
  await js('document.querySelector("#restart").click()');
  await waitFor(canStart);
  assert.equal(await js('document.querySelector("#evaluate").disabled'), true);
  assert.equal(await js('Number(document.querySelector("#timer").dataset.milliseconds)'), 0);

  assert.deepEqual(setupErrors, []);
  assert.deepEqual(exceptions, []);
  assert.deepEqual(requests.filter(url => !/^(?:file:|blob:|data:)/u.test(url)
    && url !== origin + prefix && url !== origin + prefix + 'filesystem.bin.gz'), [],
    'Only the page and its source filesystem may be downloaded.');
  console.log('Clysm browser: HTTP source preload, manual offline bootstrap, timer, native compilation, REPL state, values, error recovery, restart and local-file bootstrap startup pass.');
} finally {
  if (browser.exitCode === null && browser.signalCode === null) {browser.kill(); await stopped;}
  await new Promise(resolve => server.close(resolve));
  await fs.rm(directory, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}
