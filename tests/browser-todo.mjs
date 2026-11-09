// CHROMIUM=chromium node tests/browser-todo.mjs [todo.html]
// Exercise the built artifact over file:// in a real browser, without a server.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import {spawn} from 'node:child_process';

const html = path.resolve(process.argv[2] ?? 'todo.html');
await fs.access(html, fs.constants.R_OK);
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-todo-browser-'));
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run',
  '--no-default-browser-check', '--disable-background-networking',
  `--user-data-dir=${profile}`, '--no-startup-window', '--remote-debugging-pipe',
], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
let diagnostics = '';
browser.stderr.on('data', chunk => { diagnostics = (diagnostics + chunk).slice(-8000); });
let nextId = 0;
let received = '';
const pending = new Map();
const requests = [];
const exceptions = [];
const browserLogs = [];
function failPending(error) {
  for (const {reject, timer} of pending.values()) {
    clearTimeout(timer);
    reject(error);
  }
  pending.clear();
}
browser.on('error', failPending);
browser.on('exit', code => failPending(new Error(`Chromium exited (${code}). ${diagnostics}`)));
browser.stdio[3].on('error', failPending);
browser.stdio[4].on('error', failPending);
browser.stdio[4].on('data', chunk => {
  received += chunk;
  let end;
  while ((end = received.indexOf('\0')) !== -1) {
    const message = JSON.parse(received.slice(0, end));
    received = received.slice(end + 1);
    if (message.method === 'Network.requestWillBeSent') requests.push(message.params.request.url);
    if (message.method === 'Runtime.exceptionThrown') exceptions.push(message.params.exceptionDetails);
    if (message.method === 'Log.entryAdded') browserLogs.push(message.params.entry);
    const request = pending.get(message.id);
    if (!request) continue;
    pending.delete(message.id);
    clearTimeout(request.timer);
    if (message.error) request.reject(new Error(JSON.stringify(message.error)));
    else request.resolve(message.result);
  }
});
function send(method, params = {}, sessionId) {
  return new Promise((resolve, reject) => {
    const id = ++nextId;
    const timer = setTimeout(() => {
      pending.delete(id);
      reject(new Error(`Timed out: ${method}. ${diagnostics}`));
    }, ['Browser.getVersion', 'Target.createTarget'].includes(method) ? 45000 : 15000);
    pending.set(id, {resolve, reject, timer});
    browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
  });
}

try {
  const version = await send('Browser.getVersion');
  console.log(`Chromium: ${version.product}`);
  const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
  const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
  const command = (method, params) => send(method, params, sessionId);
  async function js(expression) {
    const result = await command('Runtime.evaluate', {expression, returnByValue: true, awaitPromise: true});
    assert.equal(result.exceptionDetails, undefined, JSON.stringify(result.exceptionDetails));
    return result.result.value;
  }
  async function waitFor(expression) {
    const deadline = Date.now() + 10000;
    while (Date.now() < deadline) {
      if (await js(expression)) return;
      await new Promise(resolve => setTimeout(resolve, 25));
    }
    throw new Error(`Timed out: ${expression}: ${await js('document.body.innerText')}\n${JSON.stringify(browserLogs)}`);
  }
  const ready = "document.getElementById('add-todo') && !document.getElementById('add-todo').disabled";
  const titles = () => js("Array.from(document.querySelectorAll('.todo-title'), node => node.textContent)");
  const click = selector => js(`document.querySelector(${JSON.stringify(selector)}).click()`);
  async function add(title) {
    await js(`document.getElementById('new-todo').value = ${JSON.stringify(title)};
      document.getElementById('new-todo-form').requestSubmit()`);
    assert.doesNotMatch(await js("document.getElementById('status').textContent"), /^Error:/);
  }
  async function edit(index, value, key = 'Enter') {
    await js(`{ document.querySelectorAll('.todo-title')[${index}].dispatchEvent(new MouseEvent('dblclick', {bubbles: true}));
      const editor = document.querySelector('.editing .edit');
      editor.value = ${JSON.stringify(value)};
      editor.dispatchEvent(new KeyboardEvent('keydown', {key: ${JSON.stringify(key)}, bubbles: true})); }`);
    assert.doesNotMatch(await js("document.getElementById('status').textContent"), /^Error:/);
  }

  await command('Page.enable');
  await command('Runtime.enable');
  await command('Log.enable');
  await command('Network.enable');
  await command('Network.emulateNetworkConditions', {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  await command('Page.navigate', {url: pathToFileURL(html).href});
  await waitFor(ready);
  assert.equal(await js("document.querySelector('script[type=\"text/x-common-lisp\"]').id"), '');
  assert.match(await js("document.getElementById('lisp-source').textContent"), /\(defclass todo-item/);
  assert.deepEqual(await titles(), []);
  assert.equal(await js("document.getElementById('remaining').textContent"), '0 items left');
  await add('   ');
  assert.deepEqual(await titles(), []);
  await add('  Read a book λ 🙂  ');
  await add('Write some Lisp');
  await add('<img src=x onerror=alert(1)>');
  assert.deepEqual(await titles(), ['Read a book λ 🙂', 'Write some Lisp', '<img src=x onerror=alert(1)>']);
  assert.equal(await js("document.querySelectorAll('#todo-list img').length"), 0);
  assert.equal(await js("todoInterpreter.evaluate(\"(and (typep todo-demo::*application* 'todo-demo::todo-application) (every (lambda (item) (typep item 'todo-demo::todo-item)) (todo-demo::items todo-demo::*application*)))\").output"), 'T\n');
  assert.equal(await js("document.getElementById('remaining').textContent"), '3 items left');

  await click('.todo .toggle');
  assert.equal(await js("document.querySelectorAll('.todo.completed').length"), 1);
  assert.equal(await js("document.getElementById('remaining').textContent"), '2 items left');
  await click('#filter-active');
  assert.deepEqual(await titles(), ['Write some Lisp', '<img src=x onerror=alert(1)>']);
  assert.equal(await js("document.getElementById('filter-active').getAttribute('aria-pressed')"), 'true');
  await click('#filter-completed');
  assert.deepEqual(await titles(), ['Read a book λ 🙂']);
  await click('#filter-all');

  await edit(1, 'Write more Lisp');
  assert.equal((await titles())[1], 'Write more Lisp');
  await edit(1, 'Discard this edit', 'Escape');
  assert.equal((await titles())[1], 'Write more Lisp');
  await edit(2, '');
  assert.deepEqual(await titles(), ['Read a book λ 🙂', 'Write more Lisp']);
  await js(`document.querySelectorAll('.todo-title')[1].dispatchEvent(new KeyboardEvent('keydown', {key: 'Enter', bubbles: true}));
    document.querySelector('.editing .edit').value = 'Saved on blur';
    document.querySelector('.editing .edit').blur()`);
  assert.deepEqual(await titles(), ['Read a book λ 🙂', 'Saved on blur']);
  await click('#toggle-all');
  assert.equal(await js("document.querySelectorAll('.todo.completed').length"), 2);
  await click('#filter-active');
  assert.deepEqual(await titles(), []);
  assert.equal(await js("document.getElementById('empty').textContent"), 'No active tasks.');
  await click('#filter-all');
  await click('#toggle-all');
  assert.equal(await js("document.querySelectorAll('.todo.completed').length"), 0);
  await click('.todo .toggle');
  await click('#clear-completed');
  assert.deepEqual(await titles(), ['Saved on blur']);
  assert.equal(await js("document.getElementById('remaining').textContent"), '1 item left');
  assert.equal(await js("document.getElementById('clear-completed').disabled"), true);

  await command('Page.reload');
  await waitFor(ready);
  assert.deepEqual(await titles(), ['Saved on blur']);
  await add('Persist another task');
  assert.deepEqual(await js("Array.from(document.querySelectorAll('.todo'), row => row.dataset.id)"), ['2', '3']);
  await click('.todo .delete');
  assert.deepEqual(await titles(), ['Persist another task']);
  await js("localStorage.setItem('miricl.todo.v1', '{broken')");
  await command('Page.reload');
  await waitFor(ready);
  assert.deepEqual(await titles(), []);
  assert.match(await js("document.getElementById('status').textContent"), /Could not read/);
  await add('Recovered list');
  assert.deepEqual(await titles(), ['Recovered list']);

  if (process.env.MIRICL_TODO_SCREENSHOT) {
    await add('Try the Common Lisp REPL');
    await add('Finish the little things');
    await click('.todo .toggle');
    await command('Emulation.setDeviceMetricsOverride', {width: 1200, height: 900, deviceScaleFactor: 1, mobile: false});
    const {data} = await command('Page.captureScreenshot', {format: 'png'});
    await fs.writeFile(process.env.MIRICL_TODO_SCREENSHOT, Buffer.from(data, 'base64'));
  }
  await command('Emulation.setDeviceMetricsOverride', {width: 390, height: 844, deviceScaleFactor: 1, mobile: true});
  assert.equal(await js('document.documentElement.scrollWidth <= innerWidth'), true);
  if (process.env.MIRICL_TODO_SCREENSHOT) {
    const {data} = await command('Page.captureScreenshot', {format: 'png'});
    await fs.writeFile(process.env.MIRICL_TODO_SCREENSHOT.replace(/\.png$/, '-mobile.png'), Buffer.from(data, 'base64'));
  }

  // Separate script tags execute in document order and share one interpreter.
  const multiple = (await fs.readFile(html, 'utf8'))
    .replace('<script type="text/x-common-lisp">',
      '<script type="text/x-common-lisp">(defparameter cl-user::*script-order* (list :first))</script>\n<script type="text/x-common-lisp">')
    .replace('<script id="miricl-wasm"',
      '<script type="text/x-common-lisp">(push :second cl-user::*script-order*)</script>\n<script id="miricl-wasm"');
  const multiplePath = path.join(profile, 'multiple.html');
  await fs.writeFile(multiplePath, multiple);
  await command('Page.navigate', {url: pathToFileURL(multiplePath).href});
  await waitFor(ready);
  assert.equal(await js("todoInterpreter.evaluate(\"(equal (reverse cl-user::*script-order*) '(:first :second))\").output"), 'T\n');

  await command('Page.addScriptToEvaluateOnNewDocument', {source:
    "Object.defineProperty(globalThis, 'localStorage', {get() {throw new DOMException('Blocked', 'SecurityError');}})"});
  await command('Page.reload');
  await waitFor(ready);
  assert.match(await js("document.getElementById('status').textContent"), /Storage unavailable/);
  await add('Works without storage');
  assert.deepEqual(await titles(), ['Works without storage']);
  assert.deepEqual(exceptions, []);
  assert.deepEqual(requests.filter(url => !/^(file:|blob:|data:)/.test(url)), []);
  console.log('TODO browser: CLOS model, CRUD, editing, filters, bulk actions, persistence, script order, offline use, storage recovery, and mobile layout pass.');
} finally {
  const exited = new Promise(resolve => browser.once('exit', resolve));
  if (browser.exitCode === null && browser.signalCode === null) { browser.kill(); await exited; }
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}
