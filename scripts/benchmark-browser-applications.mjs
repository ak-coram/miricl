// Local-only measurements of shipped pages, using their actual example buttons and matching assets.
// node scripts/benchmark-browser-applications.mjs SITE OUTPUT.json [--app NAME] [--samples 3]
// Timings describe first use after a cached reload, including lazy compilation.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import {spawn, execFileSync} from 'node:child_process';
import {StringDecoder} from 'node:string_decoder';
import {createHash} from 'node:crypto';
import {examples as maximaExamples} from '../maxima/examples.mjs';
import {examples as fricasExamples} from '../fricas/examples.mjs';
import {examples as acl2Examples} from '../acl2/examples.mjs';
import {examples as coaltonExamples} from '../coalton/examples.mjs';
import {examples as lispExamples} from './repl-examples.mjs';

const [site, output, ...options] = process.argv.slice(2);
if (!site || !output) throw new Error('Expected SITE OUTPUT.json [--app NAME] [--timeout MILLISECONDS].');
const root = path.resolve(site), selected = [], all = ['repl', 'todo', 'game-of-life', 'maxima', 'fricas', 'acl2', 'coalton', 'mcclim'];
let timeout = 300000, samples = 3, sourceCommit, baseline = false;
const selectedExamples = [];
for (let i = 0; i < options.length; i++) {
  if (options[i] === '--timeout') timeout = Number(options[++i]);
  else if (options[i] === '--samples') samples = Number(options[++i]);
  else if (options[i] === '--commit') sourceCommit = options[++i];
  else if (options[i] === '--baseline') baseline = true;
  else if (options[i] === '--example') selectedExamples.push(options[++i]);
  else selected.push(options[i] === '--app' ? options[++i] : options[i]);
}
assert.ok(Number.isFinite(timeout) && timeout > 0, 'Invalid timeout.');
assert.ok(Number.isInteger(samples) && samples > 0, 'Invalid sample count.');
for (const name of selected) assert.ok(all.includes(name), `Unknown application: ${name}`);
const manifests = {repl: lispExamples, maxima: maximaExamples, fricas: fricasExamples, acl2: acl2Examples, coalton: coaltonExamples};
const quote = value => JSON.stringify(value);
const pause = milliseconds => new Promise(resolve => setTimeout(resolve, milliseconds));
const report = {schemaVersion: 1, generatedAt: new Date().toISOString(), node: process.version, samples,
  baseline, ...(selectedExamples.length ? {selectedExamples} : {}),
  scope: 'Sequential fresh Chromium profiles; cold and cached Ready. Each self-contained worker example starts a fresh session before timing dispatch through worker result delivery, including lazy compilation; UI rendering follows delivery. Canvas/DOM operations include two animation frames. Repeatable examples warm twice before the measured replay; stateful examples explicitly omit replay.',
  environment: {node: process.version, platform: process.platform, arch: process.arch,
    osRelease: os.release(), cpuModel: os.cpus()[0]?.model, cpuCount: os.cpus().length, totalMemoryBytes: os.totalmem()},
  commit: sourceCommit ?? execFileSync('git', ['rev-parse', 'HEAD'], {encoding: 'utf8'}).trim(), passed: false, applications: []};
const server = http.createServer(async (request, response) => {
  try {
    let pathname = decodeURIComponent(new URL(request.url, 'http://localhost').pathname);
    if (pathname.endsWith('/')) pathname += 'index.html';
    const file = path.resolve(root, '.' + pathname);
    if (!file.startsWith(root + path.sep)) {response.writeHead(403).end(); return;}
    const bytes = await fs.readFile(file);
    response.writeHead(200, {'Content-Type': /\.m?js$/.test(file) ? 'text/javascript' :
      file.endsWith('.html') ? 'text/html' : file.endsWith('.wasm') ? 'application/wasm' : 'application/octet-stream',
    'Cache-Control': 'public, max-age=3600'});
    response.end(bytes);
  } catch {response.writeHead(404).end();}
});

async function browserSession() {
  const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-application-performance-'));
  const child = spawn(process.env.CHROMIUM ?? 'chromium', [
    '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
    '--disable-background-networking', '--js-flags=--max-old-space-size=1024',
    `--user-data-dir=${profile}`, '--no-startup-window', '--remote-debugging-pipe',
  ], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
  const stopped = new Promise(resolve => {child.once('exit', resolve); child.once('error', resolve);});
  let received = '', diagnostics = '', next = 0, closed = false, lastCommand = '';
  const decoder = new StringDecoder('utf8');
  const pending = new Map();
  function fail(error) {
    for (const entry of pending.values()) {clearTimeout(entry.timer); entry.reject(error);}
    pending.clear();
  }
  child.on('error', fail);
  child.stdio[3].on('error', fail);
  child.stdio[4].on('error', fail);
  child.on('exit', (code,signal) => fail(new Error(`Chromium exited (${code ?? signal}) during ${lastCommand}: ${diagnostics}`)));
  child.stderr.on('data', chunk => {diagnostics = (diagnostics + chunk).slice(-6000);});
  child.stdio[4].on('data', chunk => {
    received += decoder.write(chunk);
    let end;
    while ((end = received.indexOf('\0')) !== -1) {
      const message = JSON.parse(received.slice(0, end)); received = received.slice(end + 1);
      const entry = pending.get(message.id);
      if (!entry) continue;
      pending.delete(message.id); clearTimeout(entry.timer);
      if (message.error) entry.reject(new Error(JSON.stringify(message.error)));
      else entry.resolve(message.result);
    }
  });
  function send(method, params = {}, sessionId) {
    return new Promise((resolve, reject) => {
      const id = ++next;
      const label=method+(method==='Runtime.evaluate' ? ': '+params.expression.slice(0,240) : '');
      lastCommand=label;
      const timer = setTimeout(() => {pending.delete(id); reject(new Error(`Timed out: ${label}. ${diagnostics}`));}, timeout);
      pending.set(id, {resolve, reject, timer});
      child.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
    });
  }
  try {
    const version = await send('Browser.getVersion');
    report.browser = report.environment.browser = version.product;
    report.environment.javascript = version.jsVersion;
    const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
    const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
    const command = (method, params) => send(method, params, sessionId);
    const js = async expression => {
      const result = await command('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true});
      if (result.exceptionDetails) throw new Error(result.exceptionDetails.exception?.description ?? result.exceptionDetails.text);
      return result.result.value;
    };
    const wait = async (expression, label = expression) => {
      const deadline = Date.now() + timeout;
      while (!await js(expression)) {
        const error = await js('globalThis.performanceFailure || globalThis.performanceWorkerError || document.querySelector("#status.error")?.textContent');
        if (error) throw new Error(String(error));
        if (Date.now() >= deadline) throw new Error(`Timed out: ${label}\n${await js('document.body.innerText')}`);
        await pause(25);
      }
    };
    await command('Page.enable');
    await command('Emulation.setDeviceMetricsOverride', {width: 1200, height: 900, deviceScaleFactor: 1, mobile: false});
    await command('Page.addScriptToEvaluateOnNewDocument', {source: `
      try {localStorage.clear();} catch {}
      globalThis.performanceResults = [];
      globalThis.performanceWorkerError = null;
      globalThis.performanceFailures = [];
      addEventListener('error', event => {globalThis.performanceFailure = event.error?.stack || event.message;});
      addEventListener('unhandledrejection', event => {globalThis.performanceFailure = String(event.reason?.stack || event.reason);});
      const originalError = console.error;
      console.error = (...args) => {performanceFailures.push(args.map(String).join(' ')); originalError(...args);};
      const OriginalWorker = Worker;
      globalThis.Worker = class extends OriginalWorker {
        constructor(...args) {super(...args); globalThis.performanceWorker=this; this.addEventListener('message', ({data}) => {
          if(globalThis.performanceWorker!==this) return;
          if (data.type === 'result' || data.type === 'error') performanceResults.push({...data,benchmarkAt:performance.now()});
          if (data.type === 'error') performanceWorkerError = [data.message,data.output,data.backtrace].filter(Boolean).join('\\n');
        }); this.addEventListener('error', event => {if(globalThis.performanceWorker===this) performanceWorkerError = event.message;});}
      };
    `});
    return {command, js, wait, async close() {
      if (closed) return;
      closed = true;
      fail(new Error('Benchmark finished.'));
      if (child.exitCode === null && child.signalCode === null) child.kill('SIGTERM');
      await stopped;
      await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
    }};
  } catch (error) {
    if (child.exitCode === null && child.signalCode === null) child.kill('SIGTERM');
    await stopped;
    await fs.rm(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
    throw error;
  }
}

async function measureExample(application, example, run, repeatable = false) {
  const result = {id: example.id, title: example.title ?? example.id, passed: false};
  application.examples.push(result);
  const started = performance.now();
  try {
    Object.assign(result, await run());
    result.milliseconds ??= performance.now() - started;
    assert.ok(Number.isFinite(result.milliseconds) && result.milliseconds >= 0, 'Invalid first-use measurement.');
    if (repeatable) {
      await run(); await run();
      const warmStarted = performance.now(), warm = await run();
      result.warmMilliseconds = warm?.milliseconds ?? performance.now() - warmStarted;
      assert.ok(Number.isFinite(result.warmMilliseconds) && result.warmMilliseconds >= 0, 'Invalid warmed measurement.');
    } else result.warmUnsupported ??= 'Creates or changes application state; replay would measure a different workload.';
    result.passed = true;
  }
  catch (error) {result.error = error.stack ?? String(error); console.error(`${application.id}/${example.id}: ${error.message}`);}
  result.milliseconds ??= performance.now() - started;
  console.log(`${application.id}/${example.id}: ${result.passed ? 'PASS' : 'FAIL'} ${Math.round(result.milliseconds)} ms`);
}

async function workerExamples(application, {js, wait}) {
  const buttons = await js('Array.from(document.querySelectorAll("[data-example]"), b => ({id:b.dataset.example,title:b.textContent.trim()}))');
  if (!baseline) assert.deepEqual(new Set(buttons.map(button => button.id)), new Set(manifests[application.id].map(example => example.id)), 'Every provided example must be covered.');
  // Reference assets execute only in the browser sandbox. Their own manifest
  // supplies expectations when an example retains its ID but changes source.
  const expectations = baseline && application.id !== 'repl'
    ? await js(`import(new URL('./examples.mjs', location.href).href).then(module => module.examples.map(
        ({id,title,code,expected,checks,pattern,mode}) => ({id,title,code,expected,checks,pattern,mode})))`)
    : manifests[application.id];
  assert.ok(Array.isArray(expectations), 'Expected the application example manifest.');
  for (const button of buttons) {
    if (selectedExamples.length && !selectedExamples.includes(button.id)) continue;
    const example = expectations.find(example => example.id === button.id) ?? button;
    if (application.id !== 'repl' || !baseline)
      assert.equal(typeof example.code, 'string', `Missing manifest source for ${button.id}.`);
    // Every provided worker example is self-contained. A new logical world
    // prevents earlier examples from hiding lazy-load and compilation costs.
    await js('performanceWorkerError=null; document.querySelector("#restart").click()');
    await wait('!document.querySelector("#evaluate").disabled', `Fresh session for ${example.id}`);
    // The standalone REPL embeds its manifest. An older example with different
    // source cannot inherit today's expected result. Its historical debugger
    // protocol still needs automation before a result can be delivered.
    const embeddedSourceMatches = !baseline || application.id !== 'repl' ||
      await js(`document.querySelector('[data-example="${example.id}"]').click(); document.querySelector('#source').value`) === example.code;
    const debuggerExample = application.id === 'repl' && example.id === 'debugger';
    const timerExample = embeddedSourceMatches && application.id === 'repl' && example.id === 'timeout';
    await measureExample(application, example, async () => {
      const workload = await js(`(() => {performanceWorkerError=null; document.querySelector('[data-example="${example.id}"]').click();
        const input={source:document.querySelector('#source').value,mode:document.querySelector('#mode')?.value ?? 'default'};
        globalThis.performanceExampleStart=performance.now();
        globalThis.performanceResultCount=performanceResults.length;
        document.querySelector('#form').requestSubmit();
        return input;})()`);
      if (!baseline) assert.equal(workload.source, example.code, `${example.id}: source differs from the current manifest.`);
      if (debuggerExample) {
        await wait('document.querySelector("#debugger").open', 'Debugger pause');
        // Both thread stops may arrive in either order. Resume every visible
        // CONTINUE/USE-DISCOUNT restart through the same controls a user uses.
        await wait(`(() => {
          for(const tab of document.querySelectorAll('#debug-threads button')) {
            tab.click();
            const restart=Array.from(document.querySelectorAll('#debug-restarts button')).find(b=>/USE-DISCOUNT|CONTINUE/.test(b.textContent));
            if(restart && !restart.disabled) {
              document.querySelector('#debug-arguments').value=/USE-DISCOUNT/.test(restart.textContent)?'(list 10)':'';
              restart.click(); return false;
            }
          }
          return performanceResults.length>performanceResultCount && !document.querySelector('#debugger').open;
        })()`, 'Resume both debugger threads');
      }
      await wait('performanceResults.length>performanceResultCount && !document.querySelector("#evaluate").disabled', `${example.id} result`);
      if (timerExample)
        await wait('Array.from(document.querySelectorAll("#history li:last-child pre:not(.input)"),n=>n.textContent).join("\\n").includes("Hello from a Lisp callback!")', 'Timer callback output');
      const measured = await js(`({milliseconds:${timerExample ? 'performance.now()' : 'performanceResults.at(-1).benchmarkAt'}-performanceExampleStart,
        executionMilliseconds:performanceResults.at(-1).milliseconds,
        output:Array.from(document.querySelectorAll('#history li:last-child pre:not(.input)'),n=>n.textContent).join('\\n'),
        error:document.querySelector('#history li:last-child .error')?.textContent})`);
      assert.ok(!measured.error, measured.error);
      const checkedExample = embeddedSourceMatches ? example : {};
      const expected = checkedExample.expected ?? (application.id === 'repl' && embeddedSourceMatches ? {arithmetic: '1267650600228229401496703205376',
        function: '265252859812191058636308480000000', compilation: '144', debugger: 'Recovered total: 162/5',
        loop: '(1 4 9 16 25)', environment: 'hardwareConcurrency', timeout: 'Hello from a Lisp callback!'}[example.id] : undefined);
      if (expected) assert.ok(measured.output.includes(expected), `${example.id}: missing ${expected}\n${measured.output}`);
      for (const check of checkedExample.checks ?? []) assert.ok(measured.output.includes(check), `${example.id}: missing ${check}\n${measured.output}`);
      if (checkedExample.pattern) assert.match(measured.output, new RegExp(checkedExample.pattern));
      assert.doesNotMatch(measured.output, /System error|Internal Error|ACL2 Error|HARD ACL2 ERROR/i);
      return {milliseconds: measured.milliseconds, executionMilliseconds: measured.executionMilliseconds, output: measured.output,
        expectationSource:!embeddedSourceMatches ? 'Changed embedded reference source: worker and error checks only.'
          : baseline && application.id !== 'repl' ? 'Reference application manifest.' : 'Current application manifest.',
        ...(!embeddedSourceMatches ? {warmUnsupported:'Changed embedded reference source has no repeatability contract.'} : {}),
        timingBoundary:timerExample ? 'Input dispatch through timer callback output, including its intentional one-second delay.'
          : 'Input dispatch to worker result delivery; automatic debugger interaction is included where applicable.',
        workloadSha256: createHash('sha256').update(JSON.stringify(workload)).digest('hex')};
    }, embeddedSourceMatches && (application.id === 'maxima' || application.id === 'fricas'
      || application.id === 'coalton' && example.mode === 'expression'
      || application.id === 'acl2' && ['arithmetic','inspect'].includes(example.id)
      || application.id === 'repl' && ['arithmetic','loop','environment'].includes(example.id)));
  }
}

async function lifeExamples(application, {js}) {
  const patterns = await js('Array.from(document.querySelectorAll("[data-pattern]"),b=>({id:b.dataset.pattern,title:b.textContent}))');
  assert.ok(patterns.length, 'No Life presets discovered.');
  for (const pattern of patterns.filter(pattern=>!selectedExamples.length || selectedExamples.includes(pattern.id))) await measureExample(application, pattern, async () => {
    const measured = await js(`(async () => {
      const started=performance.now();
      document.querySelector('[data-pattern="${pattern.id}"]').click();
      const steps=${pattern.id === 'glider' ? 4 : 10};
      for(let i=0;i<steps;i++) document.querySelector('#step').click();
      await new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)));
      return {milliseconds:performance.now()-started,generation:Number(document.querySelector('#generation').textContent),
        population:Number(document.querySelector('#population').textContent),
        output:miriclInterpreter.evaluate('(life:ascii-grid)',{eof:true}).output};
    })()`);
    assert.equal(measured.generation, pattern.id === 'glider' ? 4 : 10);
    assert.equal(measured.population, pattern.id === 'glider' ? 5 : 11);
    return measured;
  }, true);
}

async function todoExamples(application, {js}) {
  const add = text => `document.querySelector('#new-todo').value=${quote(text)};document.querySelector('#new-todo-form').requestSubmit();`;
  const operations = [
    ['add', `${add('Read a book λ 🙂')}${add('Write some Lisp')}`, `Array.from(document.querySelectorAll('.todo-title'),n=>n.textContent)`, ['Read a book λ 🙂','Write some Lisp']],
    ['toggle', `document.querySelector('.todo .toggle').click()`, `document.querySelectorAll('.todo.completed').length`, 1],
    ['filter-active', `document.querySelector('#filter-active').click()`, `Array.from(document.querySelectorAll('.todo-title'),n=>n.textContent)`, ['Write some Lisp']],
    ['filter-completed', `document.querySelector('#filter-completed').click()`, `Array.from(document.querySelectorAll('.todo-title'),n=>n.textContent)`, ['Read a book λ 🙂']],
    ['edit', `document.querySelector('#filter-all').click();document.querySelector('.todo-title').dispatchEvent(new MouseEvent('dblclick',{bubbles:true}));const editor=document.querySelector('.editing .edit');editor.value='Read more Lisp';editor.dispatchEvent(new KeyboardEvent('keydown',{key:'Enter',bubbles:true}))`, `document.querySelector('.todo-title').textContent`, 'Read more Lisp'],
    ['toggle-all', `document.querySelector('#toggle-all').click()`, `document.querySelectorAll('.todo.completed').length`, 2],
    ['clear-completed', `document.querySelector('#clear-completed').click()`, `document.querySelectorAll('.todo').length`, 0],
    ['delete', `${add('Delete this task')}document.querySelector('.todo .delete').click()`, `document.querySelectorAll('.todo').length`, 0],
  ];
  for (const [id, action, query, expected] of operations) await measureExample(application, {id}, async () => {
    const measured = await js(`(async()=>{const started=performance.now();${action};await new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)));return {milliseconds:performance.now()-started,value:(${query}),status:document.querySelector('#status').textContent};})()`);
    assert.doesNotMatch(measured.status, /^Error:/);
    assert.deepEqual(measured.value, expected);
    return {milliseconds: measured.milliseconds, output: JSON.stringify(measured.value)};
  });
}

async function mcclimExamples(application, initialBrowser) {
  // Discover the live launcher rather than maintaining a second demo inventory.
  const discover = async js => {await js(`mcclim.evaluate(${quote(`
    (defparameter cl-user::*performance-demo-buttons*
      (labels ((walk (sheet)
        (append (when (typep sheet 'clim:push-button) (list sheet))
          (mapcan #'walk (clim:sheet-children sheet)))))
        (walk (clim:frame-panes mcclim-browser::*demo-frame*))))
    (setf (js:property (js:global "globalThis") "performanceDemoLabels")
      (apply #'js:array (mapcar #'clim:gadget-label cl-user::*performance-demo-buttons*)))
    (values)`)},{eof:true})`); return js('performanceDemoLabels');};
  const labels = await discover(initialBrowser.js);
  assert.ok(labels.length, 'No McCLIM launcher demos discovered.');
  const url = await initialBrowser.js('location.href');
  await initialBrowser.close();
  for (let index = 0; index < labels.length; index++) {
    if (selectedExamples.length && !selectedExamples.includes(labels[index])) continue;
    // Animation and concurrent-drawing demos create background Lisp threads.
    // A fresh browser disposes their threads, timers and retained Wasm heaps.
    // No demo startup overlaps another browser or enters the measured interval.
    let browser, stage='browser startup';
    try {
    browser = await browserSession();
    const {command,js,wait}=browser;
    stage='launcher navigation';
    await command('Page.navigate', {url});
    stage='launcher Ready';
    await wait('document.querySelector("#status")?.textContent==="Ready" && document.querySelector("canvas")', 'Fresh McCLIM launcher');
    stage='launcher discovery';
    const currentLabels = await discover(js);
    const buttonIndex = currentLabels.indexOf(labels[index]);
    assert.ok(buttonIndex >= 0, `Missing launcher button ${labels[index]}`);
    await measureExample(application, {id: labels[index], title: labels[index]}, async () => {
      try {
      stage='callback dispatch';
      await js(`performanceFailures.length=0;globalThis.performanceDemoError=null;
        globalThis.performanceDemoWindows=new Set(document.querySelectorAll('.mcclim-window'));
        mcclim.evaluateAsync(${quote(`
          (defparameter cl-user::*performance-demo-thread*
            (clim-sys:make-process
              (lambda ()
                (handler-case
                    (let ((button (nth ${buttonIndex} cl-user::*performance-demo-buttons*)))
                      (clim:activate-callback button (clim:gadget-client button) (clim:gadget-id button)))
                  (error (condition)
                    (setf (js:property (js:global "globalThis") "performanceDemoError")
                      (princ-to-string condition)))))
              :name "Application performance demo")) (values)`)},{eof:true})`);
      stage='first demo window';
      await wait('performanceDemoError || performanceFailures.length || Array.from(document.querySelectorAll(".mcclim-window")).some(w=>!performanceDemoWindows.has(w)&&w.querySelector("canvas"))', labels[index]);
      assert.equal(await js('performanceDemoError'), null);
      assert.deepEqual(await js('performanceFailures'), []);
      stage='settled paint';
      await js('new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)))');
      stage='output collection';
      return {output: await js('Array.from(document.querySelectorAll(".mcclim-window")).filter(w=>!performanceDemoWindows.has(w)).map(w=>w.querySelector(".mcclim-titlebar")?.textContent).join("; ")')};
      } catch(error) {throw new Error(`McCLIM ${labels[index]} (${stage}): ${error.stack??error}`);}
    });
    } catch(error) {
      application.examples.push({id:labels[index],title:labels[index],passed:false,error:`${stage}: ${error.stack??String(error)}`});
      console.error(`${application.id}/${labels[index]}: ${error.message}`);
    } finally {await browser?.close();}
  }
}

await new Promise((resolve, reject) => {server.once('error', reject); server.listen(0, '127.0.0.1', resolve);});
const median = values => {
  const sorted = values.toSorted((a,b)=>a-b), middle = Math.floor(sorted.length/2);
  return sorted.length%2 ? sorted[middle] : (sorted[middle-1]+sorted[middle])/2;
};
function mergeSample(application, sample) {
  for (const phase of ['cold','warm']) {
    const values = application.startup[phase + 'SamplesMs'] ??= [];
    if (Number.isFinite(sample.startup[phase + 'Ms'])) values.push(sample.startup[phase + 'Ms']);
    if (values.length) application.startup[phase + 'Ms'] = median(values);
  }
  for (const result of sample.examples) {
    let example = application.examples.find(example=>example.id===result.id);
    if (!example) {
      example = {id:result.id,title:result.title,passed:true,firstSamplesMs:[],warmSamplesMs:[]};
      application.examples.push(example);
    }
    if (result.passed) {
      if (example.workloadSha256 && example.workloadSha256!==result.workloadSha256) {
        example.passed=false;
        example.error=`Example ${result.id} changed workloads between samples.`;
        result.passed=false;
        result.error=example.error;
        sample.passed=false;
        continue;
      }
      example.firstSamplesMs.push(result.milliseconds);
      if (Number.isFinite(result.warmMilliseconds)) example.warmSamplesMs.push(result.warmMilliseconds);
      example.output = result.output;
      if (result.timingBoundary) example.timingBoundary=result.timingBoundary;
      if (result.expectationSource) example.expectationSource=result.expectationSource;
      if (result.workloadSha256) example.workloadSha256=result.workloadSha256;
      example.milliseconds = median(example.firstSamplesMs);
      if (example.warmSamplesMs.length) example.warmMilliseconds = median(example.warmSamplesMs);
    } else {example.passed=false; (example.errors ??= []).push(result.error); example.error=result.error;}
    if (result.warmUnsupported) example.warmUnsupported=result.warmUnsupported;
  }
  if (sample.error) {(application.errors ??= []).push(sample.error); application.error=sample.error;}
  application.samples.push(sample);
  application.passed = application.samples.length===samples && application.samples.every(sample=>sample.passed);
}
try {
  for (const id of selected.length ? [...new Set(selected)] : all) {
    const aggregate = {id, timingBoundary:manifests[id] ? 'Input dispatch to worker result delivery; UI rendering follows delivery.'
      : 'Input dispatch through two animation frames after the visible result.', startup:{},examples:[],samples:[],passed:false};
    report.applications.push(aggregate);
    for (let sample = 0; sample < samples; sample++) {
    const application = {id, sample:sample+1,startup:{},examples:[],passed:false};
    let browser;
    try {
      browser = await browserSession();
      const {command, js, wait} = browser;
      const pathname = id === 'repl' ? '/index.html' : id === 'todo' ? '/todo.html' : `/${id}/`;
      const ready = id === 'todo' ? 'document.querySelector("#add-todo")?.disabled===false'
        : id === 'game-of-life' ? 'globalThis.lifeReady===true'
        : id === 'mcclim' ? 'document.querySelector("#status")?.textContent==="Ready" && document.querySelector("canvas")'
        : 'document.querySelector("#evaluate")?.disabled===false';
      for (const cache of ['cold', 'warm']) {
        await command('Page.navigate', {url: 'about:blank'});
        await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}${pathname}`});
        await wait(ready, `${id} ${cache} Ready`);
        application.startup[cache + 'Ms'] = await js('performance.now()');
        console.log(`${id}: ${cache} Ready ${Math.round(application.startup[cache + 'Ms'])} ms`);
      }
      if (manifests[id]) await workerExamples(application, browser);
      else if (id === 'todo') await todoExamples(application, browser);
      else if (id === 'game-of-life') await lifeExamples(application, browser);
      else await mcclimExamples(application, browser);
      application.passed = application.examples.length > 0 && application.examples.every(example => example.passed);
    } catch (error) {application.error = error.stack ?? String(error); console.error(`${id}: ${error.message}`);}
    finally {await browser?.close();}
    mergeSample(aggregate,application);
    await fs.mkdir(path.dirname(path.resolve(output)), {recursive: true});
    await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
    }
  }
  report.passed = report.applications.length > 0 && report.applications.every(application => application.passed);
} finally {
  server.closeAllConnections(); await new Promise(resolve => server.close(resolve));
  await fs.mkdir(path.dirname(path.resolve(output)), {recursive: true});
  await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
}
if (!report.passed) process.exitCode = 1;
