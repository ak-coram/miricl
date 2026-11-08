// Exercise the shipped static application in a real browser.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import http from 'node:http';
import {spawn} from 'node:child_process';
import {examples} from '../maxima/examples.mjs';
const root = path.resolve(process.argv[2] ?? 'build/maxima/site');
const server = http.createServer(async (request, response) => {
  try {
    const pathname = new URL(request.url, 'http://localhost').pathname;
    const file = path.resolve(root, '.' + decodeURIComponent(pathname === '/' ? '/index.html' : pathname));
    if (!file.startsWith(root + path.sep)) {response.writeHead(403).end(); return;}
    response.setHeader('Content-Type', file.endsWith('.mjs') ? 'text/javascript' :
      file.endsWith('.html') ? 'text/html' : file.endsWith('.wasm') ? 'application/wasm' : 'application/octet-stream');
    response.end(await fs.readFile(file));
  } catch {response.writeHead(404).end();}
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-maxima-browser-'));
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
  '--disable-background-networking', `--user-data-dir=${profile}`, '--remote-debugging-pipe',
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
    const timer = setTimeout(() => {pending.delete(id); reject(new Error(`Timed out: ${method}`));}, 15000);
    pending.set(id, {resolve, reject, timer});
    browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
  });
}
try {
  const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
  const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
  const command = (method, params) => send(method, params, sessionId);
  const js = async expression => {
    const result = await command('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true});
    assert.equal(result.exceptionDetails, undefined, JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };
  const waitFor = async expression => {
    const deadline = Date.now() + 180000;
    while (!(await js(expression))) {
      assert.ok(Date.now() < deadline, `Timed out: ${await js('document.body.innerText')}`);
      await new Promise(resolve => setTimeout(resolve, 100));
    }
  };
  await command('Page.enable'); await command('Network.enable');
  await command('Emulation.setDeviceMetricsOverride', {width: 1200, height: 900, deviceScaleFactor: 1, mobile: false});
  await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}/`});
  await waitFor('!document.querySelector("#evaluate").disabled');
  await js('globalThis.ticks = 0; setInterval(() => ticks++, 20)');
  await command('Network.emulateNetworkConditions', {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  const run = async (source, expected, answers = '', interactiveAnswer) => {
    const count = await js('document.querySelectorAll("#history li").length');
    await js(`document.querySelector('#source').value = ${JSON.stringify(source)};
      document.querySelector('#answers').value = ${JSON.stringify(answers)};
      document.querySelector('#form').requestSubmit()`);
    if (interactiveAnswer !== undefined) {
      await waitFor('!document.querySelector("#prompt").hidden');
      assert.ok(await js('document.querySelector("#question").textContent.length > 0'));
      await js(`document.querySelector('#answer-text').value = ${JSON.stringify(interactiveAnswer)};
        document.querySelector('#answer').click()`);
    }
    await waitFor(`document.querySelectorAll('#history li').length > ${count} && !document.querySelector('#evaluate').disabled`);
    const output = await js('Array.from(document.querySelector("#history li:last-child").querySelectorAll("pre:not(.input)"), node => node.textContent).join(String.fromCharCode(10))');
    assert.match(output, expected, source + '\n' + output);
    assert.equal(await js('!!document.querySelector("#history li:last-child .error")'), false, output);
    console.log(`Browser Maxima app: ${source.split('\n')[0]}`);
  };
  await run('factor(x^4-1);', /\(x-1\)\*\(x\+1\)\*\(x\^2\+1\)/);
  await run('a:7$\nf(x):=x^2; f(a);', /49/);
  await run('%+1;', /50/);
  await run('load("stringproc")$ sconcat("λ", "🙂");', /λ🙂/);
  await run('parse_string("ex: ​23;");', /ex:23/);
  await run('kill(a)$ integrate(x^a,x);', /x\^\(a\+1\)\/\(a\+1\)/, '', 'no');
  await run('read("Enter a number");', /42/, '', '42');
  await run('? integrate;', /Attempts to symbolically compute the integral/);
  await run('1+;', /incorrect syntax/);
  await run('2+2;', /4/);
  for (const example of examples) {
    await js(`document.querySelector('[data-example="${example.id}"]').click()`);
    assert.equal(await js('document.querySelector("#source").value'), example.code);
    assert.ok(await js('document.querySelector("#example-note a").getAttribute("href").startsWith("doc/html/")'));
    await run(example.code, new RegExp(example.expected.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')));
  }
  await run('persist_value:13$ persisted(x):=x+persist_value$ persisted(29);', /42/);
  await js(`document.querySelector('#session-name').value = 'Notebook λ'; document.querySelector('#session-name').dispatchEvent(new Event('input'))`);
  const savedCount = await js('JSON.parse(localStorage.getItem("miricl.maxima.sessions.v1")).sessions.find(s => s.name === "Notebook λ").entries.length');
  await js('document.querySelector("#record").click()');
  await run('123456789;', /123456789/);
  assert.equal(await js('JSON.parse(localStorage.getItem("miricl.maxima.sessions.v1")).sessions.find(s => s.name === "Notebook λ").entries.length'), savedCount);
  assert.ok(await js('ticks > 10'), 'The UI must remain responsive.');
  await command('Network.emulateNetworkConditions', {offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  await command('Page.reload');
  await waitFor('!document.querySelector("#resume").disabled');
  assert.equal(await js('document.querySelector("#session-name").value'), 'Notebook λ');
  assert.equal(await js('document.querySelectorAll("#history li").length'), savedCount);
  assert.equal(await js('document.querySelector("#evaluate").disabled'), true);
  await js('document.querySelector("#resume").click()');
  await waitFor('!document.querySelector("#evaluate").disabled');
  await run('persisted(29);', /42/);
  if (process.env.MIRICL_MAXIMA_PREVIEW) {
    const desktop = await command('Page.captureScreenshot', {format: 'png'});
    await fs.writeFile(process.env.MIRICL_MAXIMA_PREVIEW + '-desktop.png', Buffer.from(desktop.data, 'base64'));
  }
  await command('Emulation.setDeviceMetricsOverride', {width: 390, height: 844, deviceScaleFactor: 1, mobile: true});
  assert.equal(await js('document.documentElement.scrollWidth <= innerWidth'), true, 'Mobile layout must fit the viewport.');
  assert.equal(await js('getComputedStyle(document.querySelector(".worksheet")).backgroundColor'), 'rgb(255, 255, 234)');
  if (process.env.MIRICL_MAXIMA_PREVIEW) {
    const mobile = await command('Page.captureScreenshot', {format: 'png'});
    await fs.writeFile(process.env.MIRICL_MAXIMA_PREVIEW + '-mobile.png', Buffer.from(mobile.data, 'base64'));
  }
  await js(`document.querySelector('#source').value = 'while true do a:a+1;'; document.querySelector('#form').requestSubmit()`);
  await new Promise(resolve => setTimeout(resolve, 100));
  await js(`document.querySelector('#restart').click()`);
  await waitFor('!document.querySelector("#evaluate").disabled');
  await run('f(3);', /f\(3\)/);
  console.log('Browser Maxima app: offline algebra, manual examples, Unicode, prompts, help, persistence, replay, recording controls, responsive layout, cancellation and restart pass.');
} finally {
  fail(new Error('Browser app test finished.'));
  server.closeAllConnections(); await new Promise(resolve => server.close(resolve));
  if (browser.exitCode === null && browser.signalCode === null) browser.kill('SIGTERM');
  await stopped;
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
}
