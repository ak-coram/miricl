// node tests/browser-maxima.mjs [miricl.wasm] [output.json] [maxima.wasm]
// The UI stays responsive while an offline worker loads and exercises Maxima.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import http from 'node:http';
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {maximaProbeFiles, maximaProbeRevision} from '../scripts/probe-maxima.mjs';

const image = path.resolve(process.argv[2] ?? 'miricl.wasm');
const output = path.resolve(process.argv[3] ?? 'build/maxima/browser.json');
const revision = maximaProbeRevision();
const bytes = await fs.readFile(image);
const files = maximaProbeFiles();
const core = process.argv[4] && await fs.readFile(process.argv[4]);
if (core) files.push(['/maxima-core.wasm', core]);
const sources = files.map(([name, bytes]) => [name, bytes.toString('base64')]);
const assets = new Map([
  ['/miricl.wasm', ['application/wasm', bytes]],
  ['/sources.json', ['application/json', JSON.stringify(sources)]],
  ['/scripts/miricl.mjs', ['text/javascript', await fs.readFile(new URL('../scripts/miricl.mjs', import.meta.url))]],
  ['/scripts/maxima-probe.mjs', ['text/javascript', await fs.readFile(new URL('../scripts/maxima-probe.mjs', import.meta.url))]],
  ['/worker.mjs', ['text/javascript', `
    import {probeMaxima} from '/scripts/maxima-probe.mjs';
    onmessage = async ({data}) => {
      try {postMessage({report: await probeMaxima(data.image, data.files, event => postMessage({event}), 'source', {compiled: data.compiled})});}
      catch (error) {postMessage({failure: String(error)});}
    };
    postMessage({ready: true});`]],
  ['/', ['text/html', `<!doctype html><title>Maxima compatibility probe</title><script type="module">
    globalThis.ticks = 0;
    setInterval(() => ticks++, 20);
    globalThis.events = [];
    globalThis.worker = new Worker('/worker.mjs', {type: 'module'});
    worker.onmessage = ({data}) => {
      if (data.ready) globalThis.workerReady = true;
      if (data.event) events.push(data.event);
      if (data.report) globalThis.report = data.report;
      if (data.failure) globalThis.failure = data.failure;
    };
    worker.onerror = event => {globalThis.failure = event.message;};
    globalThis.image = await (await fetch('/miricl.wasm')).arrayBuffer();
    globalThis.files = (await (await fetch('/sources.json')).json())
      .map(([name, data]) => [name, Uint8Array.from(atob(data), character => character.charCodeAt(0))]);
    globalThis.ready = true;
    </script>`]],
]);
const server = http.createServer((request, response) => {
  const asset = assets.get(new URL(request.url, 'http://localhost').pathname);
  if (!asset) {response.writeHead(404).end(); return;}
  response.setHeader('Content-Type', asset[0]); response.end(asset[1]);
});
await new Promise((resolve, reject) => {
  server.once('error', reject); server.listen(0, '127.0.0.1', resolve);
});
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
  let previousStage;
  const waitFor = async (expression, timeout) => {
    const deadline = Date.now() + timeout;
    while (!(await js(expression))) {
      const failure = await js('globalThis.failure');
      assert.ok(!failure, failure);
      const stage = await js('globalThis.events?.filter(event => event.kind === "stage").at(-1)?.name');
      if (stage && stage !== previousStage) {console.log(`Browser Maxima: ${stage}`); previousStage = stage;}
      assert.ok(Date.now() < deadline, `Browser probe timed out: ${await js('JSON.stringify(events.at(-1))')}`);
      await new Promise(resolve => setTimeout(resolve, 100));
    }
  };
  await command('Page.enable'); await command('Network.enable');
  await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}/`});
  await waitFor('globalThis.ready && globalThis.workerReady', 15000);
  await command('Network.emulateNetworkConditions', {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  const initialTicks = await js('ticks');
  await js(`worker.postMessage({image, files, compiled: ${!!core}}, [image])`);
  await waitFor('!!globalThis.report', Number(process.env.MIRICL_MAXIMA_TIMEOUT_MS ?? 180000));
  const report = await js('report');
  Object.assign(report, {revision, image, imageSha256: createHash('sha256').update(bytes).digest('hex'),
    userAgent: await js('navigator.userAgent'), uiTicks: (await js('ticks')) - initialTicks, offline: true, compiled: !!core});
  await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
  assert.equal(report.passed, true, JSON.stringify(report.failure ?? report.tests));
  assert.ok(report.uiTicks > 10, 'The main thread must remain responsive during Maxima loading.');
  console.log(`Browser Maxima: ${report.tests.length} smoke checks pass offline in a worker; ${report.uiTicks} UI ticks. Report: ${output}`);
} finally {
  fail(new Error('Browser probe finished.'));
  server.closeAllConnections(); await new Promise(resolve => server.close(resolve));
  if (browser.exitCode === null && browser.signalCode === null) browser.kill('SIGTERM');
  await stopped;
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
}
