// Diagnostic workload, not a demo conformance test. No Lisp evaluation probes.
// node mcclim/profile.mjs [packaged-site] [output-prefix] [cpu-phase=input]
// Sample one phase, requesting a stop after one second. Long deep-stack profiles
// can retain huge call trees and disturb the application being measured.
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';

const site = path.resolve(process.argv[2] ?? 'build/mcclim/site');
const output = process.argv[3] ?? 'mcclim/build/performance';
const cpuPhase = process.argv[4] ?? 'input';
if (!['input', 'startup', 'open', 'none'].includes(cpuPhase))
  throw new Error('CPU phase must be input, startup, open or none.');
const samplingIntervalUs = 10000, profileWindowMs = 1000;
const memoryLimitBytes = 512 * 1024 * 1024;
await fs.mkdir(path.dirname(output), {recursive: true});
const server = http.createServer(async (request, response) => {
  try {
    const name = new URL(request.url, 'http://localhost').pathname;
    const file = path.resolve(site, '.' + (name === '/' ? '/index.html' : name));
    if (!file.startsWith(site + path.sep)) {response.writeHead(403).end(); return;}
    response.setHeader('Content-Type', /\.(mjs|js)$/.test(file) ? 'text/javascript'
      : file.endsWith('.html') ? 'text/html' : 'application/octet-stream');
    response.end(await fs.readFile(file));
  } catch (error) {response.writeHead(404).end(String(error));}
});
await new Promise((resolve, reject) => {
  server.once('error', reject); server.listen(0, '127.0.0.1', resolve);
});
const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-mcclim-profile-'));
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run',
  '--no-default-browser-check', '--disable-background-networking',
  `--user-data-dir=${directory}`, '--no-startup-window', '--remote-debugging-pipe',
], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe'], detached: process.platform !== 'win32'});
// CLOSE also follows spawn failures, for which EXIT is never emitted.
const stopped = new Promise(resolve => browser.once('close', resolve));
let received = '', nextId = 0, diagnostics = '', failure;
const pending = new Map(), exceptions = [], report = {phases: {}, input: []};
function fail(error) {
  failure ??= error;
  for (const {reject, timer} of pending.values()) {clearTimeout(timer); reject(error);}
  pending.clear();
}
function terminateBrowser(signal = 'SIGTERM') {
  try {
    // A dedicated process group includes renderers retaining the CDP pipes
    // after their parent exits, so escalation also terminates those children.
    if (process.platform === 'win32') browser.kill(signal);
    else if (browser.pid) process.kill(-browser.pid, signal);
  } catch (error) {if (error.code !== 'ESRCH') throw error;}
}
// Count only this process and its Chromium descendants. Summing RSS is
// conservative because shared pages can occur in more than one process.
let checkingMemory = false;
const memoryWatch = process.platform === 'linux' ? setInterval(async () => {
  if (checkingMemory || !browser.pid || browser.exitCode !== null) return;
  checkingMemory = true;
  try {
    const remaining = [browser.pid], owned = new Set();
    let rss = process.memoryUsage().rss;
    while (remaining.length) {
      const pid = remaining.pop();
      if (owned.has(pid)) continue;
      owned.add(pid);
      try {
        const status = await fs.readFile(`/proc/${pid}/status`, 'utf8');
        rss += Number(status.match(/^VmRSS:\s+(\d+) kB$/m)?.[1] ?? 0) * 1024;
        const children = await fs.readFile(`/proc/${pid}/task/${pid}/children`, 'utf8');
        for (const child of children.trim().split(/\s+/)) if (child) remaining.push(Number(child));
      } catch (error) {if (error.code !== 'ENOENT' && error.code !== 'ESRCH') throw error;}
    }
    report.peakRssBytes = Math.max(report.peakRssBytes ?? 0, rss);
    if (rss > memoryLimitBytes) {
      fail(new Error('Profiler exceeded its 512 MiB process-tree RSS budget.'));
      for (const pid of owned) {
        try {process.kill(pid, 'SIGTERM');} catch (error) {if (error.code !== 'ESRCH') throw error;}
      }
    }
  } catch (error) {fail(error); terminateBrowser();}
  finally {checkingMemory = false;}
}, 500) : null;
browser.stderr.on('data', bytes => {diagnostics = (diagnostics + bytes).slice(-4000);});
browser.on('error', fail);
browser.on('exit', code => fail(new Error(`Chromium exited ${code}: ${diagnostics}`)));
for (const pipe of [browser.stderr, browser.stdio[3], browser.stdio[4]]) pipe.on('error', fail);
browser.stdio[4].on('data', bytes => {
  received += bytes;
  for (let end; (end = received.indexOf('\0')) !== -1;) {
    const message = JSON.parse(received.slice(0, end)); received = received.slice(end + 1);
    if (message.method === 'Runtime.exceptionThrown') exceptions.push(message.params.exceptionDetails);
    const request = pending.get(message.id);
    if (!request) continue;
    pending.delete(message.id); clearTimeout(request.timer);
    if (message.error) request.reject(new Error(JSON.stringify(message.error)));
    else request.resolve(message.result);
  }
});
function send(method, params = {}, sessionId, timeoutMs = 10000) {
  if (failure) return Promise.reject(failure);
  return new Promise((resolve, reject) => {
    const id = ++nextId, timer = setTimeout(() => {
      pending.delete(id); reject(new Error(`Timed out: ${method}\n${diagnostics}`));
    }, timeoutMs);
    pending.set(id, {resolve, reject, timer});
    browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
  });
}
// Only instrument browser operations. Do not wrap Lisp generic functions: that
// changes their dispatch/cache behaviour and makes the timing unrepresentative.
function instrument() {
  const p = globalThis.mcclimProfile = {
    bridge: {count: 0, milliseconds: 0, operations: {}}, canvas: {},
    labels: [], presents: [], firstPaint: null, lastPaint: null,
    modules: {count: 0, bytes: 0, milliseconds: 0},
  };
  const fronts = globalThis.mcclimProfileFronts = new WeakMap();
  const measure = CanvasRenderingContext2D.prototype.measureText;
  for (const name of ['save', 'restore', 'setTransform', 'transform', 'beginPath',
    'moveTo', 'lineTo', 'rect', 'clip', 'fill', 'stroke', 'fillRect', 'clearRect',
    'fillText', 'drawImage', 'measureText']) {
    const original = CanvasRenderingContext2D.prototype[name];
    CanvasRenderingContext2D.prototype[name] = function (...args) {
      const start = performance.now(), stats = p.canvas[name] ??= {count: 0, milliseconds: 0};
      stats.count++;
      if (name === 'fillText') {
        p.firstPaint ??= start; p.lastPaint = start;
        p.labels.push({text: String(args[0]), canvas: this.canvas,
          x: args[1], y: args[2], width: Reflect.apply(measure, this, [String(args[0])]).width,
          height: Number(this.font.match(/([0-9.]+)px/)?.[1] ?? 12),
          align: this.textAlign, matrix: this.getTransform()});
      }
      if (name === 'drawImage' && this.canvas.isConnected) {
        fronts.set(args[0], this.canvas); p.presents.push({time: start, canvas: this.canvas});
      }
      try {return Reflect.apply(original, this, args);}
      finally {stats.milliseconds += performance.now() - start;}
    };
  }
  WebAssembly.Module = new Proxy(WebAssembly.Module, {
    construct(target, args, newTarget) {
      const start = performance.now();
      try {return Reflect.construct(target, args, newTarget);}
      finally {
        p.modules.count++; p.modules.bytes += args[0].byteLength;
        p.modules.milliseconds += performance.now() - start;
      }
    },
  });
  Object.defineProperty(globalThis, 'mcclim', {
    configurable: true,
    set(app) {
      Object.defineProperty(globalThis, 'mcclim', {value: app, writable: true, configurable: true});
      const service = app.javascript.service;
      app.javascript.service = function (op, ...args) {
        const start = performance.now(), stats = p.bridge;
        stats.count++; stats.operations[op] = (stats.operations[op] ?? 0) + 1;
        try {return service.call(this, op, ...args);}
        finally {stats.milliseconds += performance.now() - start;}
      };
    },
  });
}
try {
  const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
  const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
  const command = (method, params, timeoutMs) => send(method, params, sessionId, timeoutMs);
  const js = async expression => {
    const result = await command('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true});
    if (result.exceptionDetails) throw new Error(JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };
  const wait = async (expression, description, timeout = 120000) => {
    const until = Date.now() + timeout;
    while (!await js(expression)) {
      const failure = await js("document.getElementById('status')?.classList.contains('error') ? document.getElementById('status').textContent : null");
      if (failure) throw new Error(failure);
      if (Date.now() > until) throw new Error(`Timed out waiting for ${description}`);
      await new Promise(resolve => setTimeout(resolve, 100));
    }
  };
  const snapshot = () => js(`JSON.parse(JSON.stringify({
    bridge: mcclimProfile.bridge, canvas: mcclimProfile.canvas, modules: mcclimProfile.modules,
    firstPaint: mcclimProfile.firstPaint, lastPaint: mcclimProfile.lastPaint,
    windows: document.querySelectorAll('.mcclim-window').length,
    labels: mcclimProfile.labels.length, presents: mcclimProfile.presents.length}))`);
  await command('Runtime.enable'); await command('Page.enable');
  await command('Emulation.setDeviceMetricsOverride', {width: 1400, height: 1000, deviceScaleFactor: 1, mobile: false});
  await command('Page.addScriptToEvaluateOnNewDocument', {source: `(${instrument})();`});
  if (cpuPhase !== 'none') {
    await command('Profiler.enable');
    await command('Profiler.setSamplingInterval', {interval: samplingIntervalUs});
  }
  let previous;
  const delta = (after, before = {}) => Object.fromEntries(Object.entries(after).map(([key, value]) =>
    [key, typeof value === 'number' ? value - (before[key] ?? 0) : delta(value, before[key])]));
  async function profile(name, work) {
    if (name === cpuPhase) {
      await command('Profiler.start');
      const started = performance.now();
      let stopping;
      const stop = () => stopping ??= command('Profiler.stop', {}, 2000).catch(error => {
        fail(error); terminateBrowser(); throw error;
      });
      const timer = setTimeout(() => {stop().catch(() => {});}, profileWindowMs);
      try {await work();}
      finally {
        clearTimeout(timer);
        const {profile} = await stop();
        report.cpuProfile = {phase: name, samplingIntervalUs, profileWindowMs,
          collectedMs: (profile.endTime - profile.startTime) / 1000,
          phaseMs: performance.now() - started, nodes: profile.nodes.length,
          samples: profile.samples?.length ?? 0};
        await fs.writeFile(`${output}-${name}.cpuprofile`, JSON.stringify(profile));
      }
    } else await work();
    const current = await snapshot();
    report.phases[name] = {...current,
      bridge: delta(current.bridge, previous?.bridge),
      canvas: delta(current.canvas, previous?.canvas),
      modules: delta(current.modules, previous?.modules),
      labels: current.labels - (previous?.labels ?? 0),
      presents: current.presents - (previous?.presents ?? 0)};
    previous = current;
    console.log(name, JSON.stringify(report.phases[name]));
  }
  await profile('startup', async () => {
    await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}/`});
    await wait(`document.getElementById('status')?.textContent==='Ready'
      && mcclimProfile.labels.some(e=>e.text==='Calculator')
      && performance.now()-mcclimProfile.lastPaint>1000`, 'launcher painting');
  });
  const click = async text => {
    const point = await js(`(()=>{
      const e=mcclimProfile.labels.findLast(e=>e.text===${JSON.stringify(text)}
        && (mcclimProfileFronts.get(e.canvas)??e.canvas).isConnected);
      if(!e)throw Error('Missing label: '+${JSON.stringify(text)});
      const c=mcclimProfileFronts.get(e.canvas)??e.canvas,r=c.getBoundingClientRect(),m=e.matrix;
      const x=e.x+(e.align==='center'?0:/right|end/.test(e.align)?-e.width/2:e.width/2),y=e.y-e.height*.35;
      return {x:r.left+(m.a*x+m.c*y+m.e)*r.width/c.width,
        y:r.top+(m.b*x+m.d*y+m.f)*r.height/c.height};})()`);
    const start = await js('performance.now()');
    for (const [type, buttons] of [['mouseMoved', 0], ['mousePressed', 1], ['mouseReleased', 0]])
      await command('Input.dispatchMouseEvent', {type, ...point, button: type === 'mouseMoved' ? 'none' : 'left', buttons, clickCount: 1});
    return start;
  };
  await profile('open', async () => {
    const windows = await js("document.querySelectorAll('.mcclim-window').length");
    report.openedAt = await click('Calculator');
    await wait(`document.querySelectorAll('.mcclim-window').length>${windows}
      && mcclimProfile.labels.some(e=>e.text==='AC')
      && performance.now()-mcclimProfile.lastPaint>1000`, 'Calculator painting');
  });
  await profile('input', async () => {
    for (const label of ['7', '+', '8', '=']) {
      const started = await click(label);
      await new Promise(resolve => setTimeout(resolve, 650));
      const result = await js(`(()=>{
        const paints=mcclimProfile.presents.filter(e=>e.time>${started});
        return {firstPresentMs:paints[0]?.time-${started},lastPresentMs:paints.at(-1)?.time-${started}};})()`);
      report.input.push({label, ...result}); console.log('input', label, result);
    }
  });
  report.exceptions = exceptions;
  report.recentText = await js('mcclimProfile.labels.slice(-24).map(entry => entry.text)');
  const screenshot = await command('Page.captureScreenshot', {format: 'png'});
  await fs.writeFile(`${output}.png`, Buffer.from(screenshot.data, 'base64'));
  await fs.writeFile(`${output}.json`, JSON.stringify(report, null, 2) + '\n');
} finally {
  clearInterval(memoryWatch);
  fail(new Error('Profiling ended')); terminateBrowser();
  const forceStop = setTimeout(() => terminateBrowser('SIGKILL'), 2000);
  let deadline;
  try {
    await Promise.race([stopped, new Promise((_, reject) => {
      deadline = setTimeout(() => reject(new Error('Chromium did not close after termination.')), 5000);
    })]);
  } finally {
    clearTimeout(forceStop); clearTimeout(deadline);
    server.closeAllConnections();
    await new Promise(resolve => server.close(resolve));
    await fs.rm(directory, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
  }
}
