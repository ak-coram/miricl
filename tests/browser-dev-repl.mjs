// Real webapp, production opt-out, DOM/state access, terminal, and TCP clients.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';
import {connectTerminal, createWebapp, startHost, testPorts, waitFor} from './repl-bridge-fixture.mjs';

const binary = path.resolve(process.argv[2] ?? 'interpreter.wasm');
const wasi = process.argv[3] && path.resolve(process.argv[3]);
const httpOrigin = `http://127.0.0.1:${testPorts.http}`;
const directory = await createWebapp(binary);
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-app-chromium-'));
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run',
  '--no-default-browser-check', '--disable-background-networking',
  `--user-data-dir=${profile}`, '--remote-debugging-pipe'
], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
const browserExited = new Promise(resolve => browser.once('exit', resolve));
let diagnostics = '', received = '', nextId = 0;
const pending = new Map();
const requests = [], sockets = [], exceptions = [];
browser.stderr.on('data', bytes => { diagnostics = (diagnostics + bytes).slice(-8000); });
function fail(error) {
  for (const {reject, timer} of pending.values()) { clearTimeout(timer); reject(error); }
  pending.clear();
}
browser.on('error', fail);
browser.on('exit', () => fail(new Error(`Chromium exited: ${diagnostics}`)));
browser.stdio[4].on('data', bytes => {
  received += bytes;
  let end;
  while ((end = received.indexOf('\0')) !== -1) {
    const message = JSON.parse(received.slice(0, end)); received = received.slice(end + 1);
    if (message.method === 'Network.requestWillBeSent') requests.push(message.params.request.url);
    if (message.method === 'Network.webSocketCreated') sockets.push(message.params.url);
    if (message.method === 'Runtime.exceptionThrown') exceptions.push(message.params.exceptionDetails);
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
    const timer = setTimeout(() => { pending.delete(id); reject(new Error(`CDP timeout: ${method}\n${diagnostics}`)); }, 15000);
    pending.set(id, {resolve, reject, timer});
    browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
  });
}

try {
  const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
  const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
  const command = (method, params) => send(method, params, sessionId);
  async function js(expression) {
    const result = await command('Runtime.evaluate', {expression, returnByValue: true, awaitPromise: true});
    assert.equal(result.exceptionDetails, undefined, JSON.stringify(result.exceptionDetails));
    return result.result.value;
  }
  await command('Page.enable'); await command('Runtime.enable'); await command('Network.enable');
  for (const [runtime, image] of [['node', binary], ...(wasi ? [['wasmtime', wasi]] : [])]) {
    const host = await startHost(runtime, image, 'dev-server+repl-bridge', directory);
    let terminal;
    try {
      const startRequests = requests.length, startSockets = sockets.length;
      await command('Page.navigate', {url: `${httpOrigin}/`});
      await waitFor(() => js('globalThis.ready === true'), 'production webapp startup');
      assert.equal(await js('app.dev === undefined'), true);
      assert.equal(requests.slice(startRequests).some(url => url.includes('dev-repl.mjs')), false);
      assert.equal(sockets.length, startSockets, 'Production must not open a WebSocket.');
      await js('document.getElementById("counter").click()');
      await waitFor(() => js('document.getElementById("value").value === "42"'), 'production app evaluation');
      await command('Page.navigate', {url: `${httpOrigin}/?dev`});
      await waitFor(() => js('location.search === "?dev" && globalThis.ready === true && app.dev?.state === "connected"'), 'development attachment');
      terminal = await connectTerminal(); await terminal.ready();
      assert.equal(await terminal.send('(incf *app-count*)'), '42\nmiricl> ');
      await js('document.getElementById("counter").click()');
      await waitFor(() => js('document.getElementById("value").value === "43"'), 'shared Lisp state');
      assert.equal(await terminal.send('*app-count*'), '43\nmiricl> ');
      assert.equal(await terminal.send('(setf (js:property (js:global "webapp") "count") 99)'), '99\nmiricl> ');
      assert.equal(await js('webapp.count'), 99);
      const start = host.output.length;
      host.child.stdin.write('(setf (js:property (js:invoke (js:global "document") "getElementById" "value") "value") "from Lisp")\n');
      await waitFor(() => js('document.getElementById("value").value === "from Lisp"'), 'terminal access to the webapp DOM');
      await waitFor(() => host.output.slice(start).includes('"from Lisp"\nmiricl> '), 'terminal response from browser');
      assert.deepEqual(exceptions, []);
      console.log(`PASS ${runtime} real browser webapp: development opt-in, shared state, DOM access, and terminal/TCP input`);
      const beforeReload = terminal.output.length;
      await command('Page.navigate', {url: 'about:blank'});
      await waitFor(() => terminal.output.slice(beforeReload).includes('[Browser disconnected; waiting for a browser]'), 'TCP browser unload status');
      assert.equal(terminal.ended, false);
      await command('Page.navigate', {url: `${httpOrigin}/?dev`});
      await waitFor(() => js('globalThis.ready === true && app.dev?.state === "connected"'), 'webapp reload');
      await waitFor(() => terminal.output.slice(beforeReload).includes('[Browser connected]\nmiricl> '), 'TCP browser reload status');
      assert.equal(await terminal.send('*app-count*'), '41\nmiricl> ');
      console.log(`PASS ${runtime} TCP REPL reports browser unload/reload and keeps the same connection`);
      host.child.stdin.write(':quit\n');
      await host.waitForExit();
      await waitFor(() => terminal.ended, 'terminal :quit disconnects TCP');
      assert.equal(await js('app.dev.state'), 'closed');
      await js('document.getElementById("counter").click()');
      await waitFor(() => js('document.getElementById("value").value === "42"'), 'webapp still works after :quit');
      console.log(`PASS ${runtime} browser development adapter detaches on :quit and the webapp keeps working`);
      await command('Page.navigate', {url: 'about:blank'});
    } finally { terminal?.socket.destroy(); await host.close(); }
  }
} finally {
  await send('Browser.close').catch(() => {});
  browser.kill();
  await browserExited;
  await fs.rm(directory, {recursive: true, force: true});
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}
