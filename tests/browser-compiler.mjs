// Compile Lisp inside Chromium using only the distributed adapter and images.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import http from 'node:http';
import {spawn} from 'node:child_process';
import {gzipSync} from 'node:zlib';
import {compilerSmokeCases} from './compiler-cases.mjs';

const image = path.resolve(process.argv[2] ?? 'miricl-cold.wasm');
const adapter = path.resolve(process.argv[3] ?? path.join(path.dirname(image), 'miricl.js'));
const precompiled = process.argv.includes('--precompiled');
const imageURL = precompiled ? '/miricl.wasm.gz' : '/miricl.wasm';
const assets = new Map(await Promise.all([
  [imageURL, image], ['/miricl.js', adapter],
].map(async ([url, file]) => [url, precompiled && file === image
  ? gzipSync(await fs.readFile(file), {level: 9}) : await fs.readFile(file)])));
const html = `<!doctype html><button id="compiled">Native callback</button><script type="module">
import {Miricl} from './miricl.js';
globalThis.Miricl = Miricl;
try {
  globalThis.image = await (await fetch(${JSON.stringify('.' + imageURL)})).arrayBuffer();
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
let diagnostics = '', received = '', nextId = 0, phase = 'browser startup';
const trace = process.env.MIRICL_BROWSER_COMPILER_TRACE === '1';
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
    if (message.method === 'Runtime.consoleAPICalled' && trace)
      console.log('Browser:', ...message.params.args.map(arg => arg.value ?? arg.description));
    const request = pending.get(message.id);
    if (!request) continue;
    pending.delete(message.id); clearTimeout(request.timer);
    if (message.error) request.reject(new Error(JSON.stringify(message.error)));
    else request.resolve(message.result);
  }
});
function send(method, params = {}, sessionId, timeoutMs = 60000) {
  return new Promise((resolve, reject) => {
    const id = ++nextId;
    // Keep ordinary CDP calls bounded. Cold self-compilation gets a separate
    // budget below because it compiles every kernel under Chromium's inspector.
    const timer = setTimeout(() => {
      pending.delete(id);
      const expression = params.expression ? `\nExpression: ${params.expression.slice(0, 500)}` : '';
      reject(new Error(`Timed out after ${timeoutMs} ms: ${method}, ${phase}.${expression}\n${diagnostics}`));
    }, timeoutMs);
    pending.set(id, {resolve, reject, timer});
    browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
  });
}
try {
  const version = await send('Browser.getVersion');
  console.log(`Chromium: ${version.product}`);
  const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
  const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
  const command = (method, params, timeoutMs) => send(method, params, sessionId, timeoutMs);
  const js = async (expression, timeoutMs) => {
    const result = await command('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true}, timeoutMs);
    assert.equal(result.exceptionDetails, undefined, JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };
  const evaluate = (source, timeoutMs) => js(`app.evaluate(${JSON.stringify(source)}, {eof: true}).output`, timeoutMs);
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
  if (!precompiled) {
    phase = 'cold compiler bootstrap';
    console.log('Browser compiler: starting cold bootstrap.');
    // This compiles the complete optimizer and emitter, not one application
    // function. The measured cold run takes several minutes under Chromium.
    // The integration runner's ten-minute test limit remains the outer bound.
    const bootstrapStarted = performance.now();
    const heartbeat = setInterval(() => console.log(
      `Browser compiler: cold bootstrap still running (${Math.round((performance.now() - bootstrapStarted) / 1000)} s).`), 30000);
    try {
      assert.equal(await evaluate(`(progn (miricl-compiler::ensure-compiler)
    (multiple-value-bind (installed unsupported)
      (miricl-compiler::compile-compiler)
    (and (>= (length installed) 93) (null unsupported)
      (every (lambda (name) (null (function-lambda-expression (symbol-function name))))
        '(miricl-compiler::hir-form miricl-compiler:compile-to-hir
          miricl-compiler::emit-native-module miricl-compiler:compile-native)))))`, 480000), 'T\n');
    } finally {clearInterval(heartbeat);}
    console.log(`Browser compiler self-bootstrap: ${Math.round(performance.now() - bootstrapStarted)} ms`);
  }
  for (const [name, source] of compilerSmokeCases) {
    phase = name;
    if (trace) console.log(`Browser compiler: ${name}`);
    assert.equal(await evaluate(source), 'T\n', name);
  }
  phase = 'JavaScript interop and native DOM callbacks';
  assert.equal(await evaluate(`(let ((function (miricl-compiler:compile-native
      '(lambda (x) (js:from-js (js:invoke (js:global "Math") "sqrt" x))))))
    (= (funcall function 81) 9))`), 'T\n');
  phase = 'weak hash tables';
  assert.equal(await evaluate(`(progn
    (defparameter *browser-weak-table* (js:make-weak-hash-table :weakness :key))
    (setf (gethash :retained *browser-weak-table*) (vector 42))
    (and (eq (js:hash-table-weakness *browser-weak-table*) :key)
         (equalp (gethash :retained *browser-weak-table*) #(42))))`), 'T\n');
  phase = 'native DOM callbacks';
  assert.equal(await evaluate(`(progn (defparameter *compiler-clicks* 0)
    (setf (js:property (js:invoke (js:global "document") "getElementById" "compiled") "onclick")
      (js:callback (miricl-compiler:compile-native
        '(lambda (event) (declare (ignore event)) (incf *compiler-clicks*)))))
    t)`), 'T\n');
  await js('document.getElementById("compiled").click()');
  assert.equal(await evaluate('*compiler-clicks*'), '1\n');
  assert.equal(await js('app.evaluateAsync("(funcall *compiler-test-function* 40)").then(r => r.output)'), '42\n');
  if (precompiled) {
    phase = 'saved image restoration';
    assert.equal(await js(`(async () => {
      const restored = await Miricl.instantiate(app.saveImage());
      try {return restored.evaluate("(list *compiler-clicks* (funcall *compiler-test-function* 40) (aref (gethash :retained *browser-weak-table*) 0))").output;}
      finally {restored.close();}
    })()`), '(1 42 42)\n');
  }
  await command('Network.emulateNetworkConditions', {offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  phase = 'fresh interpreter isolation';
  assert.equal(await js(`(async () => {
    const second = await Miricl.instantiate(image);
    try {
      const state = second.evaluate('miricl-compiler::*compiler-state*').output;
      if (state !== ${JSON.stringify(precompiled ? ':READY\n' : ':COLD\n')}) throw new Error('Unexpected fresh compiler state: ' + state);
      // Cold first-use compilation was exercised above. Avoid repeating the
      // entire bootstrap merely to check that a second browser VM is isolated.
      return second.evaluate(${JSON.stringify(precompiled
        ? "(funcall (miricl-compiler:compile-native '(lambda () 42)))" : '(+ 20 22)')}).output;
    }
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
