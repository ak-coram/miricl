// Real webapp, production opt-out, DOM/state access, terminal, and TCP clients.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';
import {connectTerminal, createWebapp, startHost, testPorts, waitFor} from './repl-bridge-fixture.mjs';

const binary = path.resolve(process.argv[2] ?? 'miricl.wasm');
const wasi = process.argv[3] && path.resolve(process.argv[3]);
const httpOrigin = `http://127.0.0.1:${testPorts.http}`;
const directory = await createWebapp(binary);
await fs.copyFile(process.argv[4] ?? 'build/asdf/asdf.lisp', path.join(directory, 'asdf.lisp'));
await fs.mkdir(path.join(directory, 'project/dependency'), {recursive: true});
const sources = [
  ['project/dependency/remote-dependency.asd', '(asdf:defsystem "remote-dependency" :components ((:file "macro")))'],
  ['project/dependency/macro.lisp', '(defpackage :remote-dependency (:use :cl) (:export :twice)) (in-package :remote-dependency) (defmacro twice (x) `(+ ,x ,x))'],
  ['project/remote-app.asd', '(asdf:defsystem "remote-app" :depends-on ("remote-dependency") :serial t :components ((:file "package") (:file "hello")) :build-operation "monolithic-compile-bundle-op" :build-pathname #P"dist/remote-app.wasm")'],
  ['project/package.lisp', '(defpackage :remote-app (:use :cl :remote-dependency) (:export :answer))'],
  ['project/hello.lisp', '(in-package :remote-app) (defvar *loads* 0) (incf *loads*) (defun answer () (twice 21))'],
];
for (const [name, source] of sources) {
  await fs.writeFile(path.join(directory, name), source);
  await fs.utimes(path.join(directory, name), 1700000000, 1700000000);
}
await fs.writeFile(path.join(directory, 'project/raw data λ.bin'), Uint8Array.of(0, 255, 128, 65));
await fs.writeFile(path.join(directory, 'production.html'), `<!doctype html>
<script type="module">
  import {Miricl} from './scripts/miricl.mjs';
  const bytes = await (await fetch('./miricl.wasm')).arrayBuffer();
  globalThis.app = await Miricl.instantiate(bytes);
  app.mountFile('/app.wasm', await (await fetch('./app.wasm')).arrayBuffer());
  await app.evaluateAsync('(load "/app.wasm")');
  const result = await app.evaluateAsync('(remote-app:answer)');
  document.body.textContent = result.output.trim();
  globalThis.ready = true;
</script>`);
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
    const timer = setTimeout(() => { pending.delete(id); reject(new Error(`CDP timeout: ${method}\n${diagnostics}`)); }, 60000);
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
      assert.equal(requests.slice(startRequests).some(url => url.includes('dev-filesystem.mjs')), false);
      assert.equal(sockets.length, startSockets, 'Production must not open a WebSocket.');
      await js('document.getElementById("counter").click()');
      await waitFor(() => js('document.getElementById("value").value === "42"'), 'production app evaluation');
      await command('Page.navigate', {url: `${httpOrigin}/?dev&no-filesystem`});
      await waitFor(() => js('globalThis.ready === true && app.dev?.state === "connected"'), 'file access opt-out');
      const disabledRequests = requests.length;
      assert.equal(await js('app.dev.filesystem === undefined && app.vm["filesystem-services"].get(0) === null'), true);
      assert.equal(await js(`app.evaluateAsync('(null (probe-file "/project/remote-app.asd"))').then(result => result.output)`), 'T\n');
      assert.equal(requests.slice(disabledRequests).some(url => url.includes('/__miricl__/fs/')), false);
      await command('Page.navigate', {url: `${httpOrigin}/?dev`});
      await waitFor(() => js('location.search === "?dev" && globalThis.ready === true && app.dev?.state === "connected"'), 'development attachment');
      terminal = await connectTerminal(); await terminal.ready();
      assert.equal(await terminal.send('(incf *app-count*)'), '42\nmiricl> ');
      assert.equal(await terminal.send('(defun remote-browser-answer () *app-count*)'), 'REMOTE-BROWSER-ANSWER\nmiricl> ');
      assert.equal(await terminal.send('(remote-browser-answer)'), '42\nmiricl> ');
      assert.equal(await js(`app.evaluate("(null (function-lambda-expression #'remote-browser-answer))").output`), 'T\n');
      assert.equal(await js('app.evaluate("miricl-compiler::*compiler-state*").output'), ':COLD\n');
      await js('document.getElementById("counter").click()');
      await waitFor(() => js('document.getElementById("value").value === "43"'), 'shared Lisp state');
      assert.equal(await terminal.send('*app-count*'), '43\nmiricl> ');
      assert.equal(await terminal.send('(setf (js:property (js:global "webapp") "count") 99)'), '99\nmiricl> ');
      assert.equal(await js('webapp.count'), 99);
      assert.equal(await js('app.dev.filesystem !== undefined'), true, 'File access is enabled by default in dev mode.');
      assert.equal(await terminal.send('(file-write-date "/project/hello.lisp")'), '3908988800\nmiricl> ');
      await js(`app.dev.filesystem.url = new URL(${JSON.stringify(`http://localhost:${testPorts.http}/`)});`);
      assert.equal(await terminal.send(`(with-open-file (s "/project/raw data λ.bin" :element-type '(unsigned-byte 8)) (loop for byte = (read-byte s nil nil) while byte collect byte))`),
        '(0 255 128 65)\nmiricl> ');
      assert.deepEqual(await js('app.readFile("/project/raw data λ.bin").then(bytes => Array.from(bytes))'), [0, 255, 128, 65]);
      assert.equal(await terminal.send('(null (probe-file "/project/missing"))'), 'T\nmiricl> ');
      await js(`app.dev.filesystem.url = new URL(${JSON.stringify(httpOrigin + '/')});`);
      await js(`globalThis.guestDate = 3908988801;
        globalThis.originalClock = app.hostService.bind(app);
        app.hostService = (op, argument) => op === 0 ? guestDate : originalClock(op, argument);`);
      assert.equal(await js(`app.evaluateAsync('(load "/asdf.lisp")').then(result => result.output)`), 'T\n');
      assert.equal(await js(`app.evaluateAsync(\`(progn
        (asdf:initialize-source-registry '(:source-registry (:tree "/project/") :ignore-inherited-configuration))
        (asdf:initialize-output-translations '(:output-translations (t "/build-cache/") :ignore-inherited-configuration))
        (asdf:load-system "remote-app"))\`).then(result => result.output)`), 'T\n');
      assert.equal(await terminal.send('(list (remote-app:answer) remote-app::*loads*)'), '(42 1)\nmiricl> ');
      assert.equal(await js(`app.evaluateAsync('(asdf:load-system "remote-app")').then(result => result.output)`), 'T\n');
      assert.equal(await terminal.send('remote-app::*loads*'), '1\nmiricl> ');
      await fs.writeFile(path.join(directory, 'project/hello.lisp'),
        '(in-package :remote-app) (defvar *loads* 0) (incf *loads*) (defun answer () (twice 22))');
      await fs.utimes(path.join(directory, 'project/hello.lisp'), 1700000002, 1700000002);
      await js('guestDate = 3908988803');
      assert.equal(await js(`app.evaluateAsync('(asdf:load-system "remote-app")').then(result => result.output)`), 'T\n');
      assert.equal(await terminal.send('(list (remote-app:answer) remote-app::*loads*)'), '(44 2)\nmiricl> ');
      await assert.rejects(fs.stat(path.join(directory, 'build-cache')), {code: 'ENOENT'});
      console.log(`PASS ${runtime} browser ASDF: host files, nested system discovery, binary data, caching, and host edits`);
      assert.equal(await js(`app.evaluateAsync('(asdf:make "remote-app")').then(result => result.output)`), 'T\n');
      const bundled = await js('app.readFile("/project/dist/remote-app.wasm").then(bytes => Array.from(bytes))');
      await fs.writeFile(path.join(directory, 'app.wasm'), Uint8Array.from(bundled));
      await assert.rejects(fs.stat(path.join(directory, 'project/dist')), {code: 'ENOENT'});
      const productionRequests = requests.length, productionSockets = sockets.length;
      await js(`globalThis.production = document.createElement('iframe');
        production.src = './production.html'; document.body.append(production);`);
      await waitFor(() => js('production.contentWindow.ready === true'), 'production FASL bundle startup');
      assert.equal(await js('production.contentDocument.body.textContent'), '44');
      assert.equal(await js(`production.contentWindow.app.evaluateAsync(
        '(and (null (find-package :asdf)) (null (probe-file "/project/remote-app.asd")))'
      ).then(result => result.output)`), 'T\n');
      assert.equal(await js('production.contentWindow.app.dev === undefined'), true);
      assert.equal(sockets.length, productionSockets, 'Production bundle must not open a WebSocket.');
      assert.equal(requests.slice(productionRequests).some(url => /dev-(repl|filesystem)\.mjs|\/__miricl__\/fs\//.test(url)), false);
      await js('production.contentWindow.app.close(); production.remove();');
      console.log(`PASS ${runtime} browser ASDF bundle: build, export, and fresh production startup without ASDF or dev access`);
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
    } finally {
      terminal?.socket.destroy(); await host.close();
      await fs.writeFile(path.join(directory, 'project/hello.lisp'), sources.at(-1)[1]);
      await fs.utimes(path.join(directory, 'project/hello.lisp'), 1700000000, 1700000000);
    }
  }
} finally {
  await send('Browser.close').catch(() => {});
  browser.kill();
  await browserExited;
  await fs.rm(directory, {recursive: true, force: true});
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}
