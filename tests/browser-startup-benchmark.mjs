// node tests/browser-startup-benchmark.mjs [site directory] [samples=5] [first calculation]
// Each sample uses a fresh Chromium profile, followed by a cached reload.
// Assets are served unchanged over loopback; worker instrumentation is transient.
// Game of Life sites also measure ten generations and ten ASCII renders.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';

const root = path.resolve(process.argv[2] ?? 'build/maxima/site');
const samples = Number(process.argv[3] ?? 5);
const calculation = process.argv[4];
const gameOfLife = await fs.access(path.join(root, 'game-of-life.wasm')).then(() => true, () => false);
assert.ok(Number.isInteger(samples) && samples > 0);
const workerProbe = `
  const startupStarted = performance.now(), startupPhases = [];
  let startupCompileMs = 0, startupNativeCompileMs = 0, startupNativeModules = 0;
  const startupInstantiate = WebAssembly.instantiate;
  WebAssembly.instantiate = async (...args) => {
    const start = performance.now();
    try {return await startupInstantiate(...args);}
    finally {startupCompileMs += performance.now() - start;}
  };
  const startupModule = WebAssembly.Module;
  WebAssembly.Module = new Proxy(startupModule, {construct(target, args) {
    const start = performance.now();
    const module = Reflect.construct(target, args);
    startupNativeCompileMs += performance.now() - start; startupNativeModules++;
    return module;
  }});
  const startupPost = postMessage;
  postMessage = (data, ...rest) => {
    if (data.type === 'status') startupPhases.push({message: data.message, ms: performance.now() - startupStarted});
    if (data.type === 'ready') data = {...data, startup: {
      workerMs: performance.now() - startupStarted, phases: startupPhases,
      instantiateMs: startupCompileMs, nativeCompileMs: startupNativeCompileMs,
      nativeModules: startupNativeModules,
      resources: performance.getEntriesByType('resource').map(e => ({
        name: new URL(e.name).pathname, ms: e.duration, bytes: e.decodedBodySize, transferBytes: e.transferSize,
      })),
    }};
    startupPost(data, ...rest);
  };
`;
const server = http.createServer(async (request, response) => {
  try {
    const name = new URL(request.url, 'http://localhost').pathname;
    const file = path.resolve(root, '.' + decodeURIComponent(name === '/' ? '/index.html' : name));
    if (!file.startsWith(root + path.sep)) {response.writeHead(403).end(); return;}
    let bytes = await fs.readFile(file);
    if (path.basename(file) === 'worker.mjs') bytes = Buffer.concat([Buffer.from(workerProbe), bytes]);
    response.writeHead(200, {
      'Content-Type': file.endsWith('.wasm') ? 'application/wasm' :
        /\.m?js$/.test(file) ? 'text/javascript' : file.endsWith('.html') ? 'text/html' : 'application/octet-stream',
      'Cache-Control': 'public, max-age=3600',
    });
    response.end(bytes);
  } catch {response.writeHead(404).end();}
});

async function measure() {
  const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-startup-'));
  const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
    '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
    '--disable-background-networking', `--user-data-dir=${profile}`, '--no-startup-window', '--remote-debugging-pipe',
  ], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
  const stopped = new Promise(resolve => browser.once('exit', resolve));
  let received = '', diagnostics = '', nextId = 0;
  const pending = new Map();
  const fail = error => {
    for (const {reject, timer} of pending.values()) {clearTimeout(timer); reject(error);}
    pending.clear();
  };
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
  const send = (method, params = {}, sessionId) => new Promise((resolve, reject) => {
    const id = ++nextId;
    const timer = setTimeout(() => {pending.delete(id); reject(new Error(`Timed out: ${method}`));}, 45000);
    pending.set(id, {resolve, reject, timer});
    browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
  });
  try {
    const version = (await send('Browser.getVersion')).product;
    const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
    const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
    const command = (method, params) => send(method, params, sessionId);
    const js = async expression => {
      const result = await command('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true});
      assert.equal(result.exceptionDetails, undefined, JSON.stringify(result.exceptionDetails));
      return result.result.value;
    };
    await command('Page.enable');
    await command('Page.addScriptToEvaluateOnNewDocument', {source: `
      // The app saves its transcript on pagehide. Clear it in the next
      // document before its modules run, retaining HTTP and Wasm caches.
      try {localStorage.clear();} catch {}
      globalThis.startupResult = null;
      globalThis.startupEvaluation = null;
      ${gameOfLife ? `
      let ready = false;
      Object.defineProperty(globalThis, 'lifeReady', {
        get: () => ready,
        set: value => {
          ready = value;
          if (value) startupResult = {pageMs: performance.now()};
        },
      });` : ''}
      const OriginalWorker = Worker;
      globalThis.Worker = class extends OriginalWorker {
        constructor(...args) {
          super(...args);
          this.addEventListener('message', ({data}) => {
            if (data.type === 'ready') startupResult = {...data.startup, pageMs: performance.now()};
            if (data.type === 'result') startupEvaluation = {pageMs: performance.now(), ms: data.milliseconds};
            if (data.type === 'error') startupResult = startupEvaluation = {error: data.message};
          });
          this.addEventListener('error', event => {startupResult = {error: event.message};});
        }
      };
    `});
    const results = {};
    for (const cache of ['cold', 'warm']) {
      await command('Page.navigate', {url: 'about:blank'});
      await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}/`});
      const deadline = Date.now() + 180000;
      let result;
      while (!(result = await js('globalThis.startupResult'))) {
        assert.ok(Date.now() < deadline, `Startup timed out: ${await js('document.body.innerText')}`);
        assert.equal(await js('!!document.querySelector("#status.error")'), false,
          await js('document.querySelector("#status")?.textContent'));
        await new Promise(resolve => setTimeout(resolve, 20));
      }
      assert.equal(result.error, undefined);
      if (gameOfLife) {
        const measurements = await js(`(async () => {
          const vm = globalThis.miriclInterpreter;
          const start = performance.now();
          const result = await vm.evaluateAsync('(loop with w = (life:seed :r-pentomino) repeat 10 do (setq w (life:next-generation w)) finally (return (list (life:world-generation w) (fset2:size (life:world-cells w)))))', {eof:true});
          const tenGenerationsMs = performance.now() - start;
          const asciiStart = performance.now();
          const ascii = await vm.evaluateAsync('(loop repeat 10 always (= (length (life:ascii-grid)) 400))', {eof:true});
          return {output: result.output, ascii: ascii.output, tenGenerationsMs,
                  tenAsciiGridsMs: performance.now() - asciiStart};
        })()`);
        assert.equal(measurements.output, '(10 11)\n');
        assert.equal(measurements.ascii, 'T\n');
        result.tenGenerationsMs = measurements.tenGenerationsMs;
        result.tenAsciiGridsMs = measurements.tenAsciiGridsMs;
      } else assert.ok(result.workerMs > 0 && result.phases.length >= 3, 'Worker measurements must be present.');
      if (calculation && !gameOfLife) {
        await js(`document.querySelector('#source').value = ${JSON.stringify(calculation)};
          document.querySelector('#form').requestSubmit()`);
        let evaluated;
        while (!(evaluated = await js('globalThis.startupEvaluation'))) {
          assert.ok(Date.now() < deadline, 'First calculation timed out.');
          await new Promise(resolve => setTimeout(resolve, 20));
        }
        assert.equal(evaluated.error, undefined);
        assert.equal(await js('!!document.querySelector("#history li:last-child .error")'), false);
        result.firstEvaluationMs = evaluated.ms;
        result.firstResultMs = evaluated.pageMs;
      }
      results[cache] = result;
    }
    return {version, ...results};
  } finally {
    fail(new Error('Startup benchmark finished.'));
    if (browser.exitCode === null && browser.signalCode === null) browser.kill('SIGTERM');
    await stopped;
    await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
  }
}

await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
try {
  const runs = [];
  for (let i = 0; i < samples; i++) runs.push(await measure());
  const median = values => [...values].sort((a, b) => a - b)[Math.floor(values.length / 2)];
  const summary = Object.fromEntries(['cold', 'warm'].map(cache => [cache,
    Object.fromEntries((gameOfLife ? ['pageMs', 'tenGenerationsMs', 'tenAsciiGridsMs'] :
      ['pageMs', 'workerMs', 'instantiateMs', 'nativeCompileMs',
       ...(calculation ? ['firstEvaluationMs', 'firstResultMs'] : [])]).map(key =>
      [key, median(runs.map(run => run[cache][key]))])),
  ]));
  console.log(JSON.stringify({root, samples, calculation, browser: runs[0].version, summary, runs}, null, 2));
} finally {
  server.closeAllConnections(); await new Promise(resolve => server.close(resolve));
}
