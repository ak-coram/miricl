// Diagnostic workload, not a demo conformance test. No Lisp evaluation probes.
// node mcclim/profile.mjs [packaged-site] [output-prefix]
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';

const site = path.resolve(process.argv[2] ?? 'build/mcclim/site');
const output = process.argv[3] ?? 'mcclim/build/performance';
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
], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
const stopped = new Promise(resolve => browser.once('exit', resolve));
let received = '', nextId = 0, diagnostics = '';
const pending = new Map(), exceptions = [], report = {phases: {}, input: []};
function fail(error) {
  for (const {reject, timer} of pending.values()) {clearTimeout(timer); reject(error);}
  pending.clear();
}
browser.stderr.on('data', bytes => {diagnostics = (diagnostics + bytes).slice(-4000);});
browser.on('error', fail);
browser.on('exit', code => fail(new Error(`Chromium exited ${code}: ${diagnostics}`)));
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
function send(method, params = {}, sessionId) {
  return new Promise((resolve, reject) => {
    const id = ++nextId, timer = setTimeout(() => {
      pending.delete(id); reject(new Error(`Timed out: ${method}\n${diagnostics}`));
    }, 60000);
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
  const command = (method, params) => send(method, params, sessionId);
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
  await command('Profiler.enable'); await command('Profiler.setSamplingInterval', {interval: 1000});
  let previous;
  const delta = (after, before = {}) => Object.fromEntries(Object.entries(after).map(([key, value]) =>
    [key, typeof value === 'number' ? value - (before[key] ?? 0) : delta(value, before[key])]));
  async function profile(name, work) {
    await command('Profiler.start');
    try {await work();}
    finally {
      const {profile} = await command('Profiler.stop');
      await fs.writeFile(`${output}-${name}.cpuprofile`, JSON.stringify(profile));
    }
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
  fail(new Error('Profiling ended')); browser.kill(); await stopped;
  await new Promise(resolve => server.close(resolve));
  await fs.rm(directory, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}
