// Browser UI protocol and restored native prover checks.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import http from 'node:http';
import {spawn} from 'node:child_process';
import {examples} from '../acl2/examples.mjs';
const root = path.resolve(process.argv[2] ?? 'build/acl2/preview');
const initialized = process.argv[3] === '--initialized';
const prefix = process.env.MIRICL_ACL2_TEST_PREFIX ?? '';
if (prefix && !/^\/[\w-]+(?:\/[\w-]+)*$/u.test(prefix)) throw new Error('Expected an absolute URL path prefix without a trailing slash.');
let fixture = false, rejectReplay = false, bookRequests = 0;
const fixtureWorker = () => `
  let value = 0;
  onmessage = ({data}) => {
    if (data.type === 'start') {
      postMessage({type:'status', message:'Initializing ACL2 test protocol'});
      postMessage({type:'output', output:'Fixture startup output'});
      setTimeout(() => postMessage({type:'ready', output:''}), 300);
    } else if (data.type === 'evaluate') {
      if (data.source === 'hang') {postMessage({type:'output', output:'Working…'}); return;}
      if (data.source === 'fatal') {postMessage({type:'error',message:'Fixture host trap',fatal:true}); return;}
      if (data.source === 'set 42') value = 42;
      const failed = data.source === 'reject' || (data.source === 'set 42' && ${rejectReplay});
      postMessage({type:'result',results:[],output:failed?'Input rejected.':data.source==='get'?String(value):data.source,failed,milliseconds:1});
    }
  };`;
const server = http.createServer(async (request, response) => {
  try {
    let pathname = new URL(request.url, 'http://localhost').pathname;
    if (prefix && !pathname.startsWith(prefix + '/')) {response.writeHead(404).end(); return;}
    pathname = pathname.slice(prefix.length);
    if (pathname === '/books/filesystem.bin.gz') bookRequests++;
    if (fixture && pathname === '/worker.mjs') {response.setHeader('Content-Type','text/javascript'); response.end(fixtureWorker()); return;}
    const file = path.resolve(root, '.' + decodeURIComponent(pathname === '/' ? '/index.html' : pathname));
    if (!file.startsWith(root + path.sep)) {response.writeHead(403).end(); return;}
    response.setHeader('Content-Type', file.endsWith('.mjs') ? 'text/javascript' :
      file.endsWith('.html') ? 'text/html' : file.endsWith('.wasm') ? 'application/wasm' : 'application/octet-stream');
    response.end(await fs.readFile(file));
  } catch {response.writeHead(404).end();}
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-acl2-browser-'));
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
    const timer = setTimeout(() => {pending.delete(id); reject(new Error(`Timed out: ${method}`));}, ['Browser.getVersion', 'Target.createTarget'].includes(method) ? 45000 : 15000);
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
  const waitFor = async (expression, timeout = 15000) => {
    const deadline = Date.now() + timeout;
    while (!(await js(expression))) {
      assert.ok(Date.now() < deadline, `Timed out: ${await js('document.body.innerText')}`);
      await new Promise(resolve => setTimeout(resolve, 50));
    }
  };
  const run = async (input, {failed = false} = {}) => {
    const count = await js('document.querySelectorAll("#history li").length');
    await js(`document.querySelector('#source').value = ${JSON.stringify(input)}; document.querySelector('#form').requestSubmit()`);
    await waitFor(`document.querySelectorAll('#history li').length === ${count + 1} && !document.querySelector('#evaluate').disabled`);
    assert.equal(await js('!!document.querySelector("#history li:last-child .error")'), failed);
    return js('document.querySelector("#history li:last-child").textContent');
  };
  await command('Page.enable'); await command('Network.enable');
  await command('Emulation.setDeviceMetricsOverride', {width: 1200, height: 900, deviceScaleFactor: 1, mobile: false});
  const started = performance.now();
  await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}${prefix}/`});
  if (initialized) {
    await waitFor('document.querySelector("#evaluate")?.disabled === false || document.querySelector("#startup-panel")?.dataset.state === "error"', 60000);
    assert.equal(await js('document.querySelector("#evaluate").disabled'), false, await js('document.querySelector("#startup-output").textContent'));
    const startup = Math.round(performance.now() - started);
    assert.match(await run('(+ 20 22)'), /\b42\b/);
    await run('(defun miricl-double (x) (declare (xargs :guard (rationalp x))) (+ x x))');
    await run('(defthm miricl-double-correct (implies (rationalp x) (equal (miricl-double x) (* 2 x))))');
    await run('(defthm miricl-double-false (equal (miricl-double 1) 3))', {failed: true});
    assert.match(await run('(miricl-double 21)'), /\b42\b/);
    assert.equal(bookRequests, 0);
    await js('document.querySelector("#restart").click()');
    await waitFor('document.querySelector("#evaluate").disabled === false', 60000);
    const book = examples.find(example => example.id === 'books');
    const output = await run(book.code);
    assert.match(output, /factorial\.wasm/);
    assert.match(output, /\b3628800\b/);
    assert.equal(bookRequests, 1);
    console.log(`ACL2 browser: restored native prover ready in ${startup} ms, proofs, recovery and on-demand book compilation pass.`);
  } else {
    await waitFor('document.querySelector("#startup-panel")?.dataset.state === "error"');
    assert.equal(await js('document.querySelector("#evaluate").disabled'), true);
    assert.match(await js('document.querySelector("#startup-output").textContent'), /core is not included/);
    assert.equal(await js('document.querySelector("#startup-details").open'), true);
    assert.equal(await js('document.querySelector("#supported-books").open'), false);
    await js('document.querySelector("#supported-books > summary").click()');
    assert.equal(await js('document.querySelector("#supported-books").open'), true);
    assert.ok(await js('document.querySelectorAll("#book-catalog .book-list li").length > 0'));
    assert.match(await js('document.querySelector("#book-catalog").textContent'), /arithmetic\/factorial/);
    assert.equal(await js('new URL(document.querySelector("#book-catalog a").href).pathname'), `${prefix}/books/manifest.json`);
    assert.match(await js('document.querySelector("#supported-books").textContent'), /load-compiled-file :comp/);
    assert.equal(await js('performance.getEntriesByType("resource").filter(r => r.name.includes("books/filesystem") || r.name.includes("books/manifest")).length'), 0);
    await js('document.querySelector("#supported-books > summary").click()');
    fixture = true;
    await command('Page.reload');
    await waitFor('document.querySelector("#evaluate")?.disabled === false');
    assert.equal(await js('document.querySelector("#startup-panel").hidden'), true);
    for (const example of examples) {
      await js(`document.querySelector('[data-example="${example.id}"]').click()`);
      assert.equal(await js('document.querySelector("#source").value'), example.code);
      assert.match(await js('document.querySelector("#example-note a").href'), /acl2\/manuals/);
    }
    await command('Network.emulateNetworkConditions', {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
    assert.match(await run('λ🙂 <img src=x onerror="globalThis.injected=true">'), /λ🙂/);
    assert.equal(await js('!!globalThis.injected'), false);
    await run('reject', {failed: true});
    await run('set 42'); assert.match(await run('get'), /42/);
    await js(`document.querySelector('#session-name').value = 'ACL2 notebook λ'; document.querySelector('#session-name').dispatchEvent(new Event('input'))`);
    const savedCount = await js('JSON.parse(localStorage.getItem("miricl.acl2.sessions.v1")).sessions[0].entries.length');
    await js('document.querySelector("#record").click()'); await run('unrecorded');
    assert.equal(await js('JSON.parse(localStorage.getItem("miricl.acl2.sessions.v1")).sessions[0].entries.length'), savedCount);
    await js(`globalThis.exported = null; const original = URL.createObjectURL;
      URL.createObjectURL = blob => {exported = blob; return original(blob)}; document.querySelector('#export').click()`);
    assert.equal(await js('(async()=>JSON.parse(await exported.text()).session.name)()'), 'ACL2 notebook λ');
    await command('Network.emulateNetworkConditions', {offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
    await command('Page.reload'); await waitFor('document.querySelector("#resume")?.disabled === false');
    assert.equal(await js('document.querySelector("#evaluate").disabled'), true);
    assert.equal(await js('document.querySelectorAll("#history li").length'), savedCount);
    await js('document.querySelector("#resume").click()'); await waitFor('document.querySelector("#evaluate").disabled === false');
    assert.match(await run('get'), /42/);
    rejectReplay = true;
    await js('document.querySelector("#resume").click()');
    await waitFor('document.querySelector("#status").textContent.startsWith("Resume stopped:")');
    assert.equal(await js('document.querySelector("#evaluate").disabled'), true);
    rejectReplay = false;
    await js('document.querySelector("#restart").click()'); await waitFor('!document.querySelector("#evaluate").disabled');
    await js(`document.querySelector('#source').value = 'hang'; document.querySelector('#form').requestSubmit()`);
    await waitFor('document.querySelector("#history").textContent.includes("Working…")');
    await js('globalThis.ticks=0; setInterval(()=>ticks++,20)');
    await new Promise(resolve => setTimeout(resolve, 150));
    assert.ok(await js('ticks >= 3'), 'Worker activity must not block the UI.');
    await js('document.querySelector("#restart").click()'); await waitFor('!document.querySelector("#evaluate").disabled');
    assert.equal(await js('document.querySelectorAll("#history li").length'), 0);
    assert.match(await run('get'), /0/);
    await js(`document.querySelector('#source').value = 'fatal'; document.querySelector('#form').requestSubmit()`);
    await waitFor('document.querySelector("#startup-panel").dataset.state === "error"');
    assert.equal(await js('document.querySelector("#evaluate").disabled'), true);
    assert.match(await js('document.querySelector("#history li:last-child").textContent'), /host trap/);
    await js('document.querySelector("#restart").click()'); await waitFor('!document.querySelector("#evaluate").disabled');
    await run('(+ 1 2)');
    await js(`document.querySelector('.preview-note').textContent = 'UI protocol fixture · ACL2 prover is not connected.'`);
    const desktop = await command('Page.captureScreenshot', {format: 'png'});
    await fs.writeFile(path.join(root, 'preview-desktop.png'), Buffer.from(desktop.data, 'base64'));
    await command('Emulation.setDeviceMetricsOverride', {width: 390, height: 844, deviceScaleFactor: 1, mobile: true});
    await js('document.querySelector("#supported-books").open = true; document.querySelector("#book-catalog details").open = true');
    assert.equal(await js('document.querySelectorAll("#book-catalog .book-list li").length > 0'), true);
    assert.equal(await js('document.documentElement.scrollWidth <= innerWidth'), true);
    const mobile = await command('Page.captureScreenshot', {format: 'png'});
    await fs.writeFile(path.join(root, 'preview-mobile.png'), Buffer.from(mobile.data, 'base64'));
    console.log('ACL2 frontend: honest missing-core state, worker protocol, examples, math catalog, deferred source download, literal output, offline input, saved sessions, replay failures, export, cancellation, fatal errors, restart and mobile layout pass. Prover validation is separate.');
  }
} finally {
  fail(new Error('Browser app test finished.'));
  server.closeAllConnections(); await new Promise(resolve => server.close(resolve));
  if (browser.exitCode === null && browser.signalCode === null) browser.kill('SIGTERM');
  await stopped;
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
}
