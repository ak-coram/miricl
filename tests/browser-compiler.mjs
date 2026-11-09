// Compile Lisp inside Chromium using only the distributed adapter and images.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import http from 'node:http';
import {spawn} from 'node:child_process';
import {compilerSmokeCases} from './compiler-cases.mjs';

const image = path.resolve(process.argv[2] ?? 'miricl.wasm');
const adapter = path.resolve(process.argv[3] ?? path.join(path.dirname(image), 'miricl.js'));
const precompiled = process.argv.includes('--precompiled');
const assets = new Map(await Promise.all([
  ['/miricl.wasm', image], ['/miricl.js', adapter],
].map(async ([url, file]) => [url, await fs.readFile(file)])));
const html = `<!doctype html><button id="compiled">Native callback</button><script type="module">
import {Miricl} from './miricl.js';
globalThis.Miricl = Miricl;
try {
  globalThis.image = await (await fetch('./miricl.wasm')).arrayBuffer();
  globalThis.app = await Miricl.instantiate(image);
  globalThis.ready = true;
} catch (error) {globalThis.failure = String(error);}
</script>`;
const requests = [], exceptions = [];
const server = http.createServer((request, response) => {
  const url = new URL(request.url, 'http://localhost').pathname;
  if (url === '/') {response.setHeader('Content-Type', 'text/html'); response.end(html); return;}
  const bytes = assets.get(url);
  if (!bytes) {response.writeHead(404).end(); return;}
  response.setHeader('Content-Type', url.endsWith('.js') ? 'text/javascript' : 'application/wasm');
  response.end(bytes);
});
await new Promise((resolve, reject) => {server.once('error', reject); server.listen(0, '127.0.0.1', resolve);});
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-compiler-browser-'));
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
browser.stderr.on('data', bytes => {diagnostics = (diagnostics + bytes).slice(-8000);});
browser.on('error', fail);
browser.on('exit', code => fail(new Error(`Chromium exited (${code}): ${diagnostics}`)));
browser.stdio[4].on('data', chunk => {
  received += chunk;
  let end;
  while ((end = received.indexOf('\0')) !== -1) {
    const message = JSON.parse(received.slice(0, end)); received = received.slice(end + 1);
    if (message.method === 'Network.requestWillBeSent') requests.push(message.params.request.url);
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
    // Cold self-compilation includes the optimizer kernels and can exceed 30s
    // in Chromium under the inspector. Keep a finite bound for stalled calls.
    const timer = setTimeout(() => {pending.delete(id); reject(new Error(`Timed out: ${method}. ${diagnostics}`));}, 60000);
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
  const js = async expression => {
    const result = await command('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true});
    assert.equal(result.exceptionDetails, undefined, JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };
  const evaluate = source => js(`app.evaluate(${JSON.stringify(source)}, {eof: true}).output`);
  await command('Page.enable'); await command('Runtime.enable'); await command('Network.enable');
  await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}/`});
  const deadline = Date.now() + 15000;
  while (!(await js('globalThis.ready === true'))) {
    const error = await js('globalThis.failure');
    assert.ok(!error, error);
    assert.ok(Date.now() < deadline, 'Browser interpreter initialization timed out.');
    await new Promise(resolve => setTimeout(resolve, 20));
  }
  assert.equal(await evaluate("(fboundp 'miricl-compiler:compile-native)"), 'T\n');
  assert.equal(await evaluate('miricl-compiler::*compiler-state*'), precompiled ? ':READY\n' : ':COLD\n');
  assert.equal(await evaluate('miricl-compiler:*compile-evaluations*'), precompiled ? 'T\n' : 'NIL\n');
  if (precompiled) {
    assert.equal(await evaluate('miricl-compiler::*compiler-origin*'), ':IMAGE\n');
    assert.equal(await evaluate(`(every (lambda (name)
      (and (compiled-function-p (symbol-function name))
           (null (function-lambda-expression (symbol-function name)))))
      miricl-compiler::*native-compiler-seeds*)`), 'T\n');
    await evaluate(`(setf (symbol-function 'miricl-compiler::compile-compiler)
      (lambda () (error "Precompiled image tried to bootstrap again."))) (values)`);
    await evaluate('(defun browser-default-native (x) (+ x 2))');
    assert.equal(await evaluate(`(and (= (browser-default-native 40) 42)
      (null (function-lambda-expression #'browser-default-native)))`), 'T\n');
    assert.equal(await evaluate(`(let ((miricl-compiler:*compile-evaluations* nil))
      (eval '(defun browser-default-interpreted () 42))
      (not (null (function-lambda-expression #'browser-default-interpreted))))`), 'T\n');
    // The shared cases use an interpreted driver and explicitly compile bodies.
    await evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
  }
  assert.equal(requests.some(url => url.includes('miricl-compiler.wasm')), false);
  // All subsequent compilation happens offline in this browser instance.
  await command('Network.emulateNetworkConditions', {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  if (!precompiled) assert.equal(await evaluate(`(progn (miricl-compiler::ensure-compiler)
    (multiple-value-bind (installed unsupported)
      (miricl-compiler::compile-compiler)
    (and (>= (length installed) 93) (null unsupported)
      (every (lambda (name) (null (function-lambda-expression (symbol-function name))))
        '(miricl-compiler::hir-form miricl-compiler:compile-to-hir
          miricl-compiler::emit-native-module miricl-compiler:compile-native)))))`), 'T\n');
  for (const [name, source] of compilerSmokeCases) assert.equal(await evaluate(source), 'T\n', name);
  assert.equal(await evaluate(`(let ((function (miricl-compiler:compile-native
      '(lambda (x) (js:from-js (js:invoke (js:global "Math") "sqrt" x))))))
    (= (funcall function 81) 9))`), 'T\n');
  assert.equal(await evaluate(`(progn (defparameter *compiler-clicks* 0)
    (setf (js:property (js:invoke (js:global "document") "getElementById" "compiled") "onclick")
      (js:callback (miricl-compiler:compile-native
        '(lambda (event) (declare (ignore event)) (incf *compiler-clicks*)))))
    t)`), 'T\n');
  await js('document.getElementById("compiled").click()');
  assert.equal(await evaluate('*compiler-clicks*'), '1\n');
  assert.equal(await js('app.evaluateAsync("(funcall *compiler-test-function* 40)").then(r => r.output)'), '42\n');
  await command('Network.emulateNetworkConditions', {offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  assert.equal(await js(`(async () => {
    const second = await Miricl.instantiate(image);
    try {return second.evaluate("(funcall (miricl-compiler:compile-native '(lambda () 42)))").output;}
    finally {second.close();}
  })()`), '42\n');
  assert.equal(requests.some(url => /miricl-repl|asdf|dev-repl|dev-filesystem|^wss?:/.test(url)), false);
  assert.deepEqual(exceptions, []);
  await js('app.close()');
  console.log(`Browser compiler: ${precompiled ? 'precompiled startup without bootstrap' : 'offline self-compilation'}, native test bodies, JS interop and native DOM callbacks pass.`);
} finally {
  fail(new Error('Browser compiler test finished.'));
  server.closeAllConnections(); await new Promise(resolve => server.close(resolve));
  if (browser.exitCode === null && browser.signalCode === null) browser.kill('SIGTERM');
  await stopped;
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
}
