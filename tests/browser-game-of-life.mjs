// CHROMIUM=chromium node tests/browser-game-of-life.mjs [game-of-life/build/index.html]
// Exercise production HTML and its separate runtime and FASL assets.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import http from 'node:http';
import {spawn} from 'node:child_process';

const html = path.resolve(process.argv[2] ?? 'game-of-life/build/index.html');
await fs.access(html, fs.constants.R_OK);
const root = path.dirname(html);
const server = http.createServer(async (request, response) => {
  try {
    const file = path.resolve(root, '.' + decodeURIComponent(new URL(request.url, 'http://localhost').pathname));
    if (!file.startsWith(root + path.sep)) { response.writeHead(403).end(); return; }
    const bytes = await fs.readFile(file);
    response.setHeader('Content-Type', file.endsWith('.js') || file.endsWith('.mjs') ? 'text/javascript' :
      file.endsWith('.html') ? 'text/html' : 'application/octet-stream');
    response.end(bytes);
  } catch { response.writeHead(404).end(); }
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const origin = `http://127.0.0.1:${server.address().port}`;
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-life-browser-'));
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run',
  '--no-default-browser-check', '--disable-background-networking',
  `--user-data-dir=${profile}`, '--remote-debugging-pipe',
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
    if (message.method === 'Runtime.consoleAPICalled') browserLogs.push(message.params);
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
    }, 600000);
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
  async function waitFor(expression) {
    const deadline = Date.now() + 600000;
    while (Date.now() < deadline) {
      if (await js(expression)) return;
      if (await js('document.getElementById("status")?.classList.contains("error")')) {
        const details = [];
        for (const entry of browserLogs) for (const arg of entry.args ?? []) {
          if (!arg.objectId) continue;
          const properties = await command('Runtime.getProperties', {objectId: arg.objectId, ownProperties: true});
          details.push(...properties.result.filter(p => ['message', 'output', 'backtrace'].includes(p.name))
            .map(p => `${p.name}: ${p.value?.value ?? p.value?.description}`));
        }
        if (await js('!!globalThis.miriclInterpreter')) {
          try {
            details.push('Lisp diagnostic: ' + await js(`miriclInterpreter.evaluate(
              '(handler-case (life:start) (type-error (c) (list (type-error-datum c) (type-error-expected-type c))))',
              {eof: true}).output`));
          } catch (error) {details.push('Diagnostic failed: ' + error.message);}
        }
        throw new Error([await js('document.getElementById("status").textContent'), ...details].join('\n'));
      }
      await new Promise(resolve => setTimeout(resolve, 40));
    }
    throw new Error(`Timed out: ${expression}: ${await js('document.body.innerText')}`);
  }
  const evaluate = code => js(`miriclInterpreter.evaluate(${JSON.stringify(code)}, {eof: true}).output`);
  await command('Page.enable'); await command('Runtime.enable'); await command('Network.enable');
  // Optional V8 profiles keep the production application free of benchmark code.
  // MIRICL_LIFE_PROFILE=/tmp/life writes startup, simulation and rendering data.
  const profilePrefix = process.env.MIRICL_LIFE_PROFILE;
  const timings = {};
  async function startProfile() {
    if (profilePrefix) await command('Profiler.start');
    return performance.now();
  }
  async function endProfile(name, start) {
    timings[name] = Math.round(performance.now() - start);
    if (profilePrefix) {
      const {profile} = await command('Profiler.stop');
      await fs.writeFile(`${profilePrefix}-${name}.cpuprofile`, JSON.stringify(profile));
    }
  }
  if (profilePrefix) await command('Profiler.enable');
  const startupStarted = await startProfile();
  await command('Page.navigate', {url: origin + '/' + path.basename(html)});
  await waitFor('globalThis.lifeReady === true && document.getElementById("population").textContent === "005"');
  await endProfile('startupMs', startupStarted);
  if (profilePrefix) {
    const simulationStarted = await startProfile();
    assert.equal(await evaluate(`
      (loop with world = (life:seed :r-pentomino)
            repeat 10 do (setq world (life:next-generation world))
            finally (return (list (life:world-generation world)
                                  (fset2:size (life:world-cells world)))))`), '(10 11)\n');
    await endProfile('tenGenerationsMs', simulationStarted);
    const asciiStarted = await startProfile();
    assert.equal(await evaluate('(loop repeat 10 always (= (length (life:ascii-grid)) 400))'), 'T\n');
    await endProfile('tenAsciiGridsMs', asciiStarted);
    await fs.writeFile(`${profilePrefix}.json`, JSON.stringify(timings, null, 2) + '\n');
    console.log('Browser timings (ms):', timings);
  }
  assert.equal(await evaluate(`(and (fset2:set? (life:world-cells life:*world*))
    (notany #'find-package '("ASDF" "UIOP" "MGL-PAX")))`), 'T\n');
  await evaluate('(defparameter *before* life:*world*)');
  // A blinker returns to its initial state after two pure generations.
  assert.equal(await evaluate(`(let* ((a (life:seed :blinker)) (b (life:next-generation a)) (c (life:next-generation b)))
    (and (fset2:equal? (life:world-cells a) (life:world-cells c))
      (not (fset2:equal? (life:world-cells a) (life:world-cells b)))
      (= (life:world-generation a) 0) (= (life:world-generation c) 2)))`), 'T\n');
  await js('document.getElementById("clear").click()');
  assert.equal(await js('document.getElementById("population").textContent'), '000');
  const box = await js('({x:document.getElementById("grid").getBoundingClientRect().x, y:document.getElementById("grid").getBoundingClientRect().y, width:document.getElementById("grid").getBoundingClientRect().width})');
  const point = (x, y) => ({x: box.x + (x + .5) * box.width / 24, y: box.y + (y + .5) * box.width / 24});
  await command('Input.dispatchMouseEvent', {type: 'mousePressed', ...point(3, 3), button: 'left', buttons: 1, clickCount: 1});
  await command('Input.dispatchMouseEvent', {type: 'mouseMoved', ...point(5, 3), button: 'left', buttons: 1});
  await command('Input.dispatchMouseEvent', {type: 'mouseReleased', ...point(5, 3), button: 'left', buttons: 0, clickCount: 1});
  assert.equal(await js('document.getElementById("population").textContent'), '003');
  assert.equal(await evaluate('(fset2:size (life:world-cells *before*))'), '5\n', 'Editing must not mutate the previous world.');
  const ascii = await evaluate('(life:print-grid)');
  assert.equal(ascii.split('\n')[3], '...###..................');
  assert.equal(await js('document.getElementById("ascii").textContent'), ascii);
  await js('document.getElementById("step").click()');
  assert.equal(await js('document.getElementById("generation").textContent'), '0001');
  assert.equal(await evaluate('(life:print-grid)'), '........................\n'.repeat(2) +
    '....#...................\n'.repeat(3) + '........................\n'.repeat(11));
  const printed = await evaluate('life:*world*');
  assert.match(printed, /#<Life 24x16, generation 1, 3 alive/);
  assert.match(printed, /\.\.\.\.#/);
  await js('document.getElementById("run").click()');
  await waitFor('Number(document.getElementById("generation").textContent) >= 2');
  await js('document.getElementById("run").click()');
  assert.equal(await js('document.getElementById("run").textContent'), 'Run');
  await js('document.querySelector("[data-pattern=glider]").click()');
  assert.equal(await js('document.getElementById("population").textContent'), '005');
  assert.equal(await js('document.getElementById("generation").textContent'), '0000');
  await command('Emulation.setDeviceMetricsOverride', {width: 390, height: 844, deviceScaleFactor: 1, mobile: true});
  assert.equal(await js('document.documentElement.scrollWidth <= innerWidth'), true);
  if (process.env.MIRICL_LIFE_SCREENSHOT) {
    await command('Emulation.setDeviceMetricsOverride', {width: 1000, height: 900, deviceScaleFactor: 1, mobile: false});
    const {data} = await command('Page.captureScreenshot', {format: 'png'});
    await fs.writeFile(process.env.MIRICL_LIFE_SCREENSHOT, Buffer.from(data, 'base64'));
  }
  assert.deepEqual(exceptions, []);
  assert.equal(requests.some(url => /^wss?:/.test(url) ||
    (/^https?:/.test(url) && /asdf|quicklisp|dev-repl|dev-filesystem/.test(new URL(url).pathname))),
    false, 'Production must not fetch development dependencies.');
  for (const file of ['miricl.js', 'miricl.wasm', 'game-of-life.wasm'])
    assert.ok(requests.includes(`${origin}/${file}`), `Production must load its separate ${file} asset.`);
  console.log('Game of Life: separate HTML, adapter, interpreter and FSet 2 bundle, immutable generations, mouse drawing, ASCII REPL state, playback and mobile layout pass.');
} catch (error) { console.error(error); throw error; } finally {
  server.closeAllConnections();
  await new Promise(resolve => server.close(resolve));
  const stopped = browser.exitCode !== null || browser.signalCode !== null
    ? Promise.resolve() : new Promise(resolve => browser.once('exit', resolve));
  browser.kill('SIGTERM');
  await stopped;
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
}
