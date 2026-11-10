// CHROMIUM=chromium node tests/browser-repl.mjs [miricl.html]
// Exercise the built artifact over file:// in a real browser, without a server.
import assert from 'node:assert/strict';
import {threadCases} from './thread-cases.mjs';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import {spawn} from 'node:child_process';

const html = path.resolve(process.argv[2] ?? 'miricl.html');
await fs.access(html, fs.constants.R_OK);
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-browser-'));
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
const consoleCalls = [];
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
    if (message.method === 'Target.attachedToTarget' && message.params.targetInfo.type === 'worker')
      send('Runtime.enable', {}, message.params.sessionId).catch(failPending);
    if (message.method === 'Runtime.consoleAPICalled')
      consoleCalls.push({...message.params, sessionId: message.sessionId});
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
  async function waitFor(expression, timeout = 10000) {
    const deadline = Date.now() + timeout;
    while (Date.now() < deadline) {
      if (await js(expression)) return;
      await new Promise(resolve => setTimeout(resolve, 25));
    }
    throw new Error(`Timed out waiting for ${expression}: ${await js('document.body.innerText')}\n${JSON.stringify(browserLogs)}`);
  }
  const ready = "!document.getElementById('run').disabled";
  const completed = "document.getElementById('stop').disabled";
  const output = () => js("Array.from(document.querySelectorAll('#transcript pre')).filter(p => !p.className).map(p => p.textContent).join('')");
  async function input(source, selection) {
    await js(`document.getElementById('clear').click();
      document.getElementById('source').value = ${JSON.stringify(source)};
      document.getElementById('source').setSelectionRange(${selection ? selection.join(',') : '0, 0'});
      document.getElementById('run').click()`);
    await waitFor(completed);
    return output();
  }

  await command('Page.enable');
  await command('Target.setAutoAttach', {autoAttach: true, waitForDebuggerOnStart: false, flatten: true});
  await command('Runtime.enable');
  await command('Log.enable');
  await command('Network.enable');
  await command('Network.emulateNetworkConditions', {offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1});
  await command('Page.navigate', {url: pathToFileURL(html).href});
  await waitFor(`document.getElementById('run') && ${ready}`);
  assert.equal(await input('(+ 1 2)'), '3\n');
  assert.equal(await input('miricl-compiler::*compiler-state*'), ':COLD\n');
  await js("document.getElementById('examples').value = 'compilation'; document.getElementById('examples').dispatchEvent(new Event('change')); document.getElementById('run').click()");
  await waitFor(completed, 60000);
  const compilation = await output();
  assert.equal((compilation.match(/Packed Lisp code: [1-9][0-9]* bytes/g) ?? []).length, 2);
  assert.equal((compilation.match(/Wasm module: [1-9][0-9]* bytes/g) ?? []).length, 2);
  assert.equal((compilation.match(/Wasm instructions for function/g) ?? []).length, 1);
  assert.match(compilation, /local\.get/);
  assert.equal((compilation.match(/\d+ ms real time, \d+ ms run time/g) ?? []).length, 2);
  assert.match(compilation, /First compilation \(includes compiler bootstrap\):[\s\S]*Second compilation \(compiler already bootstrapped\):/);
  assert.match(compilation, /144\n25\n$/);
  assert.equal(await input('miricl-compiler::*compiler-origin*'), ':FIRST-USE\n');
  for (const [name, source] of threadCases)
    assert.equal(await input(source), 'T\n', `Browser green threads: ${name}`);
  assert.match(await input('(progn (miricl-threads:make-thread (lambda () (sleep 0.02) (format t "background-thread-output~%"))) (values))'), /^(?:background-thread-output\n)?$/);
  await waitFor("Array.from(document.querySelectorAll('#transcript pre')).filter(p => !p.className).some(p => p.textContent.includes('background-thread-output\\n'))");
  assert.equal(await input(`(and (js:available-p)
    (= (js:from-js (js:invoke (js:global "Math") "sqrt" 81)) 9)
    (handler-case (js:call (js:global "notAFunction")) (js:javascript-error () t)))`), 'T\n');
  assert.equal(await input('(defun browser-double (n) (* 2 n))'), 'BROWSER-DOUBLE\n');
  assert.equal(await input('(browser-double 21)'), '42\n');
  assert.equal(await input('(values "λ 🙂" 1/3 (expt 2 100))'), '"λ 🙂"\n1/3\n1267650600228229401496703205376\n');
  assert.equal(await input(`(let* ((a (make-random-state)) (b (make-random-state a)))
    (and (random-state-p a) (loop repeat 50 always (= (random 100000 a) (random 100000 b)))))`), 'T\n');
  assert.equal(await input('(progn (format t "before λ~%") (error "after"))'), 'before λ\n');
  assert.match(await js("document.getElementById('transcript').innerText"), /Error: after/);
  await input('(defun browser-trace () (error "browser trace"))');
  await input('(browser-trace)');
  assert.match(await js("document.getElementById('transcript').innerText"),
    /Error: browser trace[\s\S]*Backtrace \(most recent call first\):\n   0: COMMON-LISP:ERROR\n   1: COMMON-LISP-USER::BROWSER-TRACE/);
  assert.equal(await input('(browser-double 3)'), '6\n');
  // The shipped example exercises the debugger using the visible controls.
  await js("document.getElementById('clear').click(); document.getElementById('examples').value = 'debugger'; document.getElementById('examples').dispatchEvent(new Event('change')); document.getElementById('run').click()");
  await waitFor("document.getElementById('debugger').open && document.querySelectorAll('#debug-threads [role=tab]').length === 2");
  await js(`Array.from(document.querySelectorAll('#debug-threads [role=tab]')).find(tab => !tab.textContent.includes('Greeting worker')).click()`);
  await waitFor("document.getElementById('debugger').open && document.getElementById('debug-condition').textContent.includes('Discount 150')");
  assert.match(await js("document.getElementById('debug-stack').textContent"), /\n   0: COMMON-LISP:ERROR\n/);
  assert.equal(await js("document.getElementById('debug-stack-details').open && document.getElementById('debug-panel').lastElementChild.id === 'debug-stack-details'"), true);
  assert.equal(await js("getComputedStyle(document.getElementById('debug-evaluate')).borderTopColor"), 'rgb(0, 0, 0)');
  assert.match(await js("document.getElementById('debug-locals').textContent"), /SUBTOTAL = 36/);
  await js(`document.getElementById('debug-expression').value = '(values subtotal discount)'; document.getElementById('debug-evaluate').click()`);
  await waitFor("document.querySelector('#debug-result .debug-evaluation pre:last-child')?.textContent === '36\\n150'");
  for (const value of [7, 8]) {
    await js(`document.getElementById('debug-expression').value = '${value}'; document.getElementById('debug-evaluate').click()`);
    await waitFor(`document.querySelector('#debug-result .debug-evaluation:last-child pre:last-child')?.textContent === '${value}'`);
  }
  assert.equal(await js("document.querySelectorAll('#debug-result .debug-evaluation').length"), 3);
  await js(`const frames = document.getElementById('debug-frame');
    frames.selectedIndex = Array.from(frames.options).findIndex(option => option.textContent.includes('DEBUG-ORDER'));
    frames.dispatchEvent(new Event('change'));`);
  assert.match(await js("document.getElementById('debug-locals').textContent"), /QUANTITY = 3/);
  // A failed restart must remain usable and explain the mistake inline.
  for (const argumentsSource of ['10', '(list']) {
    await js(`document.getElementById('debug-arguments').value = ${JSON.stringify(argumentsSource)};
      Array.from(document.querySelectorAll('#debug-restarts button')).find(button => button.textContent.includes('USE-DISCOUNT')).click()`);
    await waitFor("!document.getElementById('debug-command-error').hidden && !document.getElementById('debug-restarts').firstElementChild.disabled");
    assert.match(await js("document.getElementById('debug-command-error').textContent"), /Restart failed:/);
    if (argumentsSource === '10') {
      assert.match(await js("document.getElementById('debug-command-error').textContent"), /proper list.*got 10.*\(list value\)/);
    }
    assert.equal(await js("document.getElementById('debug-arguments').getAttribute('aria-invalid')"), 'true');
    assert.equal(await js("document.querySelectorAll('#debug-threads [role=tab]').length"), 2);
    assert.match(await js("document.getElementById('debug-condition').textContent"), /Discount 150/);
  }
  await js("document.querySelector('#debug-threads [aria-selected=false]').click()");
  assert.equal(await js("document.getElementById('debug-command-error').hidden"), true);
  await js("document.querySelector('#debug-threads [aria-selected=false]').click()");
  assert.equal(await js("document.getElementById('debug-command-error').hidden"), false);
  assert.equal(await js("document.getElementById('debug-arguments').value"), '(list');
  await js(`document.getElementById('debug-arguments').value = '(list 10)';
    Array.from(document.querySelectorAll('#debug-restarts button')).find(button => button.textContent.includes('USE-DISCOUNT')).click()`);
  await waitFor("document.getElementById('debugger').open && document.getElementById('debug-condition').textContent.includes('worker is paused')");
  await waitFor("document.querySelectorAll('#debug-threads [role=tab]').length === 1");
  await waitFor("document.getElementById('transcript').textContent.includes('Recovered total: 162/5')");
  assert.match(await output(), /Recovered total: 162\/5/);
  assert.equal(await js("document.getElementById('debug-command-error').hidden"), true);
  assert.match(await js("document.getElementById('debug-threads').textContent"), /Greeting worker/);
  assert.match(await js("document.getElementById('debug-locals').textContent"), /MESSAGE = "Hello from the worker"/);
  assert.equal(await js("document.getElementById('debug-steps').hidden"), true);
  await js(`Array.from(document.querySelectorAll('#debug-restarts button')).find(button => button.textContent.includes('CONTINUE')).click()`);
  await waitFor("!document.getElementById('debugger').open");
  await waitFor("document.getElementById('transcript').textContent.includes('Hello from the worker')");
  await waitFor(completed);
  assert.equal(await js("document.getElementById('debugger').hidden"), true);

  // Multiple paused threads share a dialog and preserve their own UI state.
  await input(`(dotimes (i 2) (let ((quantity (1+ i)))
    (miricl-threads:make-thread (lambda () (debug-order quantity)) :name "Browser order")))`);
  await waitFor("document.querySelectorAll('#debug-threads [role=tab]').length === 2");
  const firstThread = await js("document.querySelector('#debug-threads [aria-selected=true]').id");
  await js(`document.getElementById('debug-expression').value = '42'; document.getElementById('debug-evaluate').click()`);
  await waitFor("document.querySelector('#debug-result .debug-evaluation pre:last-child')?.textContent === '42'");
  await js("document.querySelector('#debug-threads [aria-selected=false]').click()");
  assert.equal(await js("document.querySelectorAll('#debug-result .debug-evaluation').length"), 0);
  await js(`document.getElementById(${JSON.stringify(firstThread)}).click()`);
  assert.equal(await js("document.querySelectorAll('#debug-result .debug-evaluation').length"), 1);
  assert.equal(await js("document.getElementById('debug-expression').value"), '42');
  for (const remaining of [1, 0]) {
    await js(`document.getElementById('debug-arguments').value = '(list 0)';
      Array.from(document.querySelectorAll('#debug-restarts button')).find(button => button.textContent.includes('USE-DISCOUNT')).click()`);
    await waitFor(`document.querySelectorAll('#debug-threads [role=tab]').length === ${remaining}`);
  }
  assert.equal(await js("document.getElementById('debugger').hidden"), true);
  assert.equal(await js("document.getElementById('debugger').open"), false);
  assert.equal(await input(`(let ((start (get-internal-real-time)))
    (and (null (sleep 1/50)) (>= (- (get-internal-real-time) start) 15)
      (integerp (get-universal-time))))`), 'T\n');
  await input('(defparameter *browser-incomplete* 42)\n(');
  assert.match(await js("document.getElementById('status').textContent"), /Waiting for complete input/);
  assert.equal(await input("(boundp '*browser-incomplete*)"), 'NIL\n');
  assert.equal(await input('(defparameter *browser-incomplete* 42)\n(+ *browser-incomplete* 1)'), '*BROWSER-INCOMPLETE*\n43\n');
  assert.equal(await input('(+ 2 3)\n(error "must not run")', [0, 7]), '5\n');
  assert.equal(await input('(format t "<img src=x onerror=alert(1)>")'), '<img src=x onerror=alert(1)>NIL\n');
  assert.equal(await js("document.querySelectorAll('#transcript img').length"), 0);
  await js("document.getElementById('source').value = '(+ 8 9)'; document.getElementById('source').setSelectionRange(0, 0); document.getElementById('source').dispatchEvent(new KeyboardEvent('keydown', {key: 'Enter', ctrlKey: true, bubbles: true}))");
  await waitFor(completed);
  assert.match(await output(), /17\n$/);
  await js("document.getElementById('source').value = 'draft'; document.getElementById('source').dispatchEvent(new KeyboardEvent('keydown', {key: 'ArrowUp', altKey: true, bubbles: true}))");
  assert.equal(await js("document.getElementById('source').value"), '(+ 8 9)');
  await js("document.getElementById('source').dispatchEvent(new KeyboardEvent('keydown', {key: 'ArrowDown', altKey: true, bubbles: true}))");
  assert.equal(await js("document.getElementById('source').value"), 'draft');
  await js("document.getElementById('clear').click()");
  assert.equal(await js("document.getElementById('transcript').textContent"), '');
  assert.equal(await input('(browser-double 5)'), '10\n');

  // An infinite WASM loop must leave Stop and the rest of the page usable.
  await js("document.getElementById('source').value = '(loop)'; document.getElementById('source').setSelectionRange(0, 0); document.getElementById('run').click()");
  await waitFor("!document.getElementById('stop').disabled");
  await js("document.getElementById('stop').click()");
  await waitFor(ready);
  assert.equal(await input("(fboundp 'browser-double)"), 'NIL\n');
  assert.equal(await input('(defparameter *reset-me* 9)'), '*RESET-ME*\n');
  await js("document.getElementById('reset').click()");
  await waitFor(ready);
  assert.equal(await input("(boundp '*reset-me*)"), 'NIL\n');
  await input(':quit');
  assert.equal(await js("document.getElementById('run').disabled"), true);
  await js("document.getElementById('reset').click()");
  await waitFor(ready);
  assert.equal(await input('(+ 20 22)'), '42\n');
  await js("document.getElementById('examples').value = 'function'; document.getElementById('examples').dispatchEvent(new Event('change')); document.getElementById('run').click()");
  await waitFor(completed);
  assert.match(await output(), /FACTORIAL\n265252859812191058636308480000000\n$/);

  await js("document.getElementById('clear').click(); document.getElementById('examples').value = 'environment'; document.getElementById('examples').dispatchEvent(new Event('change')); document.getElementById('run').click()");
  await waitFor(completed);
  const environmentOutput = await output();
  const environmentHeading = 'Browser environment (also logged to the browser console):\n';
  assert.ok(environmentOutput.startsWith(environmentHeading));
  const printedEnvironment = JSON.parse(environmentOutput.slice(environmentHeading.length));
  const environmentLog = consoleCalls.find(call => call.type === 'log' && call.args[0]?.value === 'Browser environment');
  assert.ok(environmentLog, 'The Lisp example must call console.log in the worker');
  const logged = await send('Runtime.callFunctionOn', {
    objectId: environmentLog.args[1].objectId, functionDeclaration: 'function() { return this; }', returnByValue: true,
  }, environmentLog.sessionId);
  assert.equal(logged.exceptionDetails, undefined);
  const environment = logged.result.value;
  assert.deepEqual(printedEnvironment, environment, 'The REPL and console must show the same environment details');
  assert.equal(environment.timeZone, await js('Intl.DateTimeFormat().resolvedOptions().timeZone'));
  assert.deepEqual(environment.preferredLocales, await js('navigator.languages'));
  assert.equal(environment.locale, await js('Intl.DateTimeFormat().resolvedOptions().locale'));
  assert.equal(typeof environment.calendar, 'string');
  assert.equal(typeof environment.numberingSystem, 'string');
  assert.equal(typeof environment.userAgent, 'string');
  assert.ok(environment.hardwareConcurrency >= 1);
  assert.equal(typeof environment.onLine, 'boolean');
  assert.equal(typeof environment.isSecureContext, 'boolean');
  assert.equal(typeof environment.crossOriginIsolated, 'boolean');
  const workerOrigin = await send('Runtime.evaluate', {expression: 'location.origin', returnByValue: true}, environmentLog.sessionId);
  assert.equal(environment.origin, workerOrigin.result.value);

  await js("document.getElementById('clear').click(); document.getElementById('examples').value = 'timeout'; document.getElementById('examples').dispatchEvent(new Event('change')); document.getElementById('run').click()");
  await waitFor(completed);
  assert.equal(await output(), 'Timer scheduled. You can keep using the REPL while it waits.\n');
  assert.equal(await js("document.getElementById('run').disabled"), false);
  await waitFor("Array.from(document.querySelectorAll('#transcript pre')).some(p => !p.className && p.textContent.includes('Hello from a Lisp callback!'))");
  assert.equal(await output(), 'Timer scheduled. You can keep using the REPL while it waits.\nHello from a Lisp callback!\n');
  assert.ok(consoleCalls.some(call => call.type === 'log' && call.args[0]?.value === 'Hello from a Lisp callback!'));
  assert.equal(await input('(+ 20 22)'), '42\n');

  // A main-thread interpreter has DOM access; the REPL worker has worker globals.
  const adapterURL = 'data:text/javascript;base64,' + Buffer.from(
    await fs.readFile(new URL('../scripts/miricl.mjs', import.meta.url), 'utf8')).toString('base64');
  assert.equal(await js(`(async () => {
    const {Miricl} = await import(${JSON.stringify(adapterURL)});
    const bytes = Uint8Array.from(atob(document.getElementById('miricl-wasm').textContent.trim()), c => c.charCodeAt(0));
    globalThis.domMiricl = await Miricl.instantiate(bytes);
    return domMiricl.evaluate(\`(progn
      (defparameter *dom-clicks* 0)
      (defparameter *dom-button* (js:invoke (js:global "document") "createElement" "button"))
      (setf (js:property *dom-button* "id") "lisp-dom-test")
      (setf (js:property *dom-button* "textContent") "Lisp button")
      (js:invoke *dom-button* "addEventListener" "click"
        (js:callback (lambda (event)
          (when (string= (js:from-js (js:property event "type")) "click") (incf *dom-clicks*)))))
      (js:invoke (js:property (js:global "document") "body") "appendChild" *dom-button*) t)\`).output;
  })()`), 'T\n');
  assert.equal(await js(`document.getElementById('lisp-dom-test').click();
    domMiricl.evaluate('(= *dom-clicks* 1)').output`), 'T\n');
  await js(`domMiricl.evaluate('(js:invoke *dom-button* "remove")'); delete globalThis.domMiricl`);

  await command('Emulation.setDeviceMetricsOverride', {width: 390, height: 844, deviceScaleFactor: 1, mobile: true});
  assert.equal(await js('document.documentElement.scrollWidth <= innerWidth'), true, 'Mobile page must fit the viewport');
  assert.deepEqual(exceptions, []);
  assert.deepEqual(requests.filter(url => !/^(file:|blob:|data:)/.test(url)), [], 'The standalone page must not request external resources');
  if (process.env.MIRICL_BROWSER_SCREENSHOT) {
    await command('Emulation.setDeviceMetricsOverride', {width: 1280, height: 900, deviceScaleFactor: 1, mobile: false});
    const {data} = await command('Page.captureScreenshot', {format: 'png'});
    await fs.writeFile(process.env.MIRICL_BROWSER_SCREENSHOT, Buffer.from(data, 'base64'));
  }
  console.log('Browser REPL: offline file:// startup, persistent Lisp, Unicode, errors, incomplete input, selection, history, clear, stop, reset, quit, debugger dialog, thread tabs, evaluation history, and mobile layout pass.');
} finally {
  const exited = new Promise(resolve => browser.once('exit', resolve));
  if (browser.exitCode === null && browser.signalCode === null) {
    browser.kill();
    await exited;
  }
  await fs.rm(profile, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}
