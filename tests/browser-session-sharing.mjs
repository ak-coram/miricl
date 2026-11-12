// Test the real shared UI for all four apps with a worker protocol fixture.
// Native compiler and application semantics are covered by their browser suites.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import http from 'node:http';
import {spawn} from 'node:child_process';
import {sessionFromURL, sharedSessionURL, newSession} from '../app-common/session-store.mjs';
const root = path.resolve(import.meta.dirname, '..');
const replHTML = process.argv[2] ?? 'miricl.html';
const apps = ['maxima', 'fricas', 'acl2', 'coalton', 'lisp'];
const requests = [];
const fixtureWorker = `
  let value = 0;
  onmessage = ({data}) => {
    if (data.type === 'start' || data.type === 'initialize') setTimeout(() => postMessage({type:'ready'}), 300);
    if (data.type === 'evaluate') {
      if (data.source === 'set 42') value = 42;
      if (data.source === 'ask') {postMessage({type:'prompt', question:'A sign?'}); return;}
      postMessage({type:'result', output:data.source === 'get' ? String(value) : data.source,
        failed:data.source === 'error', milliseconds:1});
    }
    if (data.type === 'answer') postMessage({type:'result', output:data.text, milliseconds:1});
  };`;
const server = http.createServer(async (request, response) => {
  try {
    requests.push(request.url);
    const match = /^\/miricl\/(maxima|fricas|acl2|coalton|lisp)\/(.*)$/u.exec(new URL(request.url, 'http://localhost').pathname);
    if (!match) {response.writeHead(404).end(); return;}
    const [, app, requested] = match;
    const name = requested || 'index.html';
    if (app === 'lisp' && name === 'index.html') {
      response.setHeader('Content-Type', 'text/html');
      const html = await fs.readFile(replHTML, 'utf8');
      response.end(html.replace(/(<script id="miricl-worker" type="text\/plain">)[\s\S]*?(<\/script>)/u,
        (_, open, close) => open + fixtureWorker + close));
      return;
    }
    if (!['index.html', 'app.mjs', 'session-store.mjs', 'examples.mjs', 'worker.mjs', 'client.mjs', 'repl-debugger.mjs'].includes(name)) {
      response.writeHead(404).end(); return;
    }
    response.setHeader('Content-Type', name.endsWith('.html') ? 'text/html' : 'text/javascript');
    response.end(name === 'worker.mjs' ? fixtureWorker : await fs.readFile(path.join(root,
      name === 'repl-debugger.mjs' ? 'scripts' : ['app.mjs', 'session-store.mjs'].includes(name) ? 'app-common' : app, name)));
  } catch {response.writeHead(404).end();}
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const origin = `http://127.0.0.1:${server.address().port}`;
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-sharing-'));
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
  '--disable-background-networking', `--user-data-dir=${profile}`, '--no-startup-window', '--remote-debugging-pipe',
], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
const stopped = new Promise(resolve => browser.once('exit', resolve));
let received = '', diagnostics = '', nextId = 0;
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
    const timer = setTimeout(() => {pending.delete(id); reject(new Error(`Timed out: ${method}. ${diagnostics}`));}, 30000);
    pending.set(id, {resolve, reject, timer});
    browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
  });
}
try {
  console.log(`Chromium: ${(await send('Browser.getVersion')).product}`);
  await send('Browser.grantPermissions', {origin, permissions:['clipboardReadWrite', 'clipboardSanitizedWrite']});
  const {targetId} = await send('Target.createTarget', {url:'about:blank'});
  const {sessionId} = await send('Target.attachToTarget', {targetId, flatten:true});
  const command = (method, params) => send(method, params, sessionId);
  const js = async expression => {
    const result = await command('Runtime.evaluate', {expression, awaitPromise:true, returnByValue:true, userGesture:true});
    assert.equal(result.exceptionDetails, undefined, JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };
  const waitFor = async expression => {
    const deadline = Date.now() + 15000;
    while (!(await js(expression))) {
      assert.ok(Date.now() < deadline, `Timed out: ${expression}: ${await js('document.body.innerText')}`);
      await new Promise(resolve => setTimeout(resolve, 25));
    }
  };
  await command('Page.enable');
  await command('Page.addScriptToEvaluateOnNewDocument', {source:`
    globalThis.evaluations = []; globalThis.workerStarts = 0;
    const OriginalWorker = Worker;
    globalThis.Worker = class extends OriginalWorker {
      constructor(...args) {super(...args); workerStarts++;}
      postMessage(data) {if (data.type === 'evaluate') evaluations.push(data); super.postMessage(data);}
    };`});
  for (const app of apps) {
    const key = `miricl.${app}.sessions.v1`, base = `${origin}/miricl/${app}/?view=worksheet`;
    const name = `${app} notebook λ🙂 <img src=x onerror="globalThis.injected=true">`;
    const modes = app === 'coalton' ? ['lisp', 'expression', 'definitions'] : [];
    await command('Emulation.setDeviceMetricsOverride', {width:1200,height:900,deviceScaleFactor:1,mobile:false});
    await command('Page.navigate', {url:base});
    await waitFor('document.querySelector("#evaluate")?.disabled === false');
    const run = async (source, mode) => {
      const count = await js('document.querySelectorAll("#history li").length');
      await js(`document.querySelector('#source').value = ${JSON.stringify(source)};
        ${mode ? `document.querySelector('#mode').value = ${JSON.stringify(mode)};` : ''}
        document.querySelector('#form').requestSubmit()`);
      if (source === 'ask') {
        await waitFor('!document.querySelector("#prompt").hidden');
        await js(`document.querySelector('#answer-text').value = 'positive λ🙂'; document.querySelector('#answer').click()`);
      }
      await waitFor(`document.querySelectorAll('#history li').length === ${count+1} && !document.querySelector('#evaluate').disabled`);
    };
    await run('set 42', app === 'coalton' ? 'definitions' : undefined);
    await run('ask', app === 'coalton' ? 'expression' : undefined);
    await run('get', app === 'coalton' ? 'lisp' : undefined);
    await run('error');
    await js(`document.querySelector('#session-name').value = ${JSON.stringify(name)};
      document.querySelector('#session-name').dispatchEvent(new Event('input'))`);
    const original = await js(`JSON.parse(localStorage.getItem(${JSON.stringify(key)})).sessions[0]`);
    const copy = async expression => {
      await js(`document.querySelector('#save-status').textContent = ''; ${expression}.click()`);
      await waitFor('document.querySelector("#save-status").textContent === "Session link copied."');
      return js('navigator.clipboard.readText()');
    };
    const link = await copy('document.querySelector("#share")');
    assert.ok(link.startsWith(base + '#session='));
    assert.deepEqual([...Buffer.from(new URL(link).hash.slice('#session='.length), 'base64url').subarray(0, 3)],
      [31, 139, 8], 'Browser clipboard links use native gzip.');
    const decoded = await sessionFromURL(link, key, modes);
    assert.equal(decoded.name, name);
    assert.deepEqual(decoded.entries, original.entries.map(({recorded, ...entry}) => entry));
    await js('document.querySelector("#new").click()');
    await waitFor('!document.querySelector("#evaluate").disabled');
    await run('different notebook');
    const savedLink = await copy('document.querySelector("#sessions li:last-child .session-copy")');
    assert.equal((await sessionFromURL(savedLink, key, modes)).name, name);
    assert.equal(await js('document.querySelector("#session-name").value'), 'Untitled session', 'Copying a saved session must not open it.');
    const evaluationCount = await js('evaluations.length');
    // This changes only the fragment, exercising import without a page reload.
    await command('Page.navigate', {url:savedLink});
    await waitFor(`document.querySelector('#session-name').value === ${JSON.stringify(name)} && !document.querySelector('#resume').disabled`);
    assert.equal(await js('document.querySelector("#evaluate").disabled'), true);
    assert.equal(await js('document.querySelectorAll("#history li").length'), 4);
    assert.equal(await js('evaluations.length'), evaluationCount, 'Import must not execute shared inputs.');
    assert.equal(await js('!!globalThis.injected'), false);
    assert.equal(await js('location.hash'), '');
    const imported = await js(`JSON.parse(localStorage.getItem(${JSON.stringify(key)}))`);
    assert.equal(imported.sessions.length, 3);
    assert.notEqual(imported.active, original.id);
    await js('document.querySelector("#resume").click()');
    await waitFor('!document.querySelector("#evaluate").disabled');
    assert.equal(await js('evaluations.length'), evaluationCount + 3, 'Only completed inputs are replayed.');
    await run('get');
    assert.match(await js('document.querySelector("#history li:last-child").textContent'), /42/u);
    await command('Page.reload');
    await waitFor('document.querySelector("#resume")?.disabled === false');
    assert.equal(await js(`JSON.parse(localStorage.getItem(${JSON.stringify(key)})).sessions.length`), 3, 'Reload must not re-import a consumed fragment.');
    assert.equal(await js('document.querySelector("#session-name").value'), name);
    assert.equal(await js('evaluations.length'), 0);
    await command('Page.navigate', {url:base + '#session=!!!'});
    await waitFor('document.querySelector("#save-status").textContent.includes("Invalid shared session")');
    const wrongApp = await sharedSessionURL(newSession(), base, 'another application');
    await command('Page.navigate', {url:wrongApp});
    await waitFor('document.querySelector("#save-status").textContent.includes("another application")');
    assert.equal(await js(`JSON.parse(localStorage.getItem(${JSON.stringify(key)})).sessions.length`), 3);
    assert.equal(await js('document.querySelector("#session-name").value'), name);
    await js(`Object.defineProperty(navigator, 'clipboard', {configurable:true,value:{writeText:()=>Promise.reject(new Error('Denied'))}});
      document.querySelector('#share').click()`);
    await waitFor('document.querySelector("#share-link")?.hidden === false');
    const fallback = await js('document.querySelector("#share-link").value');
    assert.equal((await sessionFromURL(fallback, key, modes)).name, name);
    assert.equal(await js('document.querySelector("#share-link").selectionEnd'), fallback.length);
    await command('Emulation.setDeviceMetricsOverride', {width:360,height:800,deviceScaleFactor:1,mobile:true});
    assert.equal(await js('document.documentElement.scrollWidth <= innerWidth'), true, `${app} mobile layout`);
    assert.equal(await js(`Array.from(document.querySelectorAll('#share, .session-copy')).every(button => {
      const box=button.getBoundingClientRect(); return box.width>0 && box.left>=0 && box.right<=innerWidth;
    })`), true);
    // A recipient without usable browser storage can still inspect and copy the
    // imported transcript, and the unconsumed URL survives a reload.
    await js(`Storage.prototype.setItem = () => {throw new Error('Storage unavailable')}`);
    await command('Page.navigate', {url:link});
    await waitFor('document.querySelector("#save-status").textContent.includes("storage is unavailable")');
    assert.equal(await js('document.querySelector("#session-name").value'), name);
    assert.ok((await js('location.hash')).startsWith('#session='));
    assert.equal(await js('evaluations.length'), 0);
    await command('Page.reload');
    await waitFor('document.querySelector("#resume")?.disabled === false && location.hash === ""');
    assert.equal(await js('document.querySelector("#session-name").value'), name, 'A shared URL imports on a fresh page load.');
    assert.equal(await js('evaluations.length'), 0);
    assert.equal(await js(`JSON.parse(localStorage.getItem(${JSON.stringify(key)})).sessions.length`), 4);
    const starts = await js('workerStarts');
    await js('document.querySelector("#sessions .session-open").click()');
    await waitFor('!document.querySelector("#resume").disabled');
    assert.equal(await js('workerStarts'), starts, 'Opening an unresumed transcript reuses the untouched VM.');
    await js('document.querySelector("#remove").click()');
    await waitFor('!document.querySelector("#evaluate").disabled');
    assert.equal(await js('workerStarts'), starts, 'Deleting an unresumed transcript must not initialize another VM.');
    await run('set 42');
    await js('document.querySelector("#new").click()');
    await waitFor('!document.querySelector("#evaluate").disabled');
    assert.equal(await js('workerStarts'), starts + 1, 'New after evaluation requires a clean VM.');
    await run('get');
    assert.equal(await js('document.querySelector("#history li:last-child pre:not(.input)").textContent'), '0');
    await command('Page.reload');
    await waitFor('document.querySelector("#examples .example") && document.querySelector("#resume")?.disabled === true');
    assert.equal(await js('workerStarts'), 1);
    await js('document.querySelector("#remove").click()');
    await waitFor('!document.querySelector("#evaluate").disabled');
    assert.equal(await js('workerStarts'), 1, 'Deleting during startup must keep the initialization already in progress.');
    await js('document.querySelector("#restart").click()');
    await waitFor('!document.querySelector("#evaluate").disabled');
    assert.equal(await js('workerStarts'), 2, 'Explicit Restart still replaces the VM.');
    const large = {...newSession(), name: `${app} large notebook`, entries: [
      {source: '(print "λ🙂")\n'.repeat(4000), outputs: [{text: 'Large output 世界\n'.repeat(4000)}], state: 'complete'},
    ]};
    const largeLink = await sharedSessionURL(large, base, key);
    const legacyLink = new URL('#session=' + Buffer.from(JSON.stringify({version: 1, application: key,
      session: {...newSession(), name: `${app} legacy notebook`}})).toString('base64url'), base).href;
    await command('Page.navigate', {url:largeLink});
    await waitFor(`document.querySelector('#session-name').value === ${JSON.stringify(large.name)}`);
    assert.equal(await js('document.querySelector("#history .input").textContent'), large.entries[0].source);
    assert.equal(await js('evaluations.length'), 0, 'Large compressed imports never execute input automatically.');
    await command('Page.navigate', {url:legacyLink});
    await waitFor(`document.querySelector('#session-name').value === ${JSON.stringify(`${app} legacy notebook`)}`);
    // Delay the first decode so a later navigation finishes first. Only the
    // newest request may change the session, even after the old stream resumes.
    await js(`globalThis.originalDecompressionStream = DecompressionStream;
      let firstDecode = true;
      globalThis.DecompressionStream = class {
        constructor(format) {
          const stream = new originalDecompressionStream(format);
          const gate = firstDecode ? new Promise(resolve => {globalThis.releaseSharedDecode = resolve;}) : Promise.resolve();
          firstDecode = false;
          this.writable = stream.writable;
          this.readable = stream.readable.pipeThrough(new TransformStream({
            async transform(chunk, controller) {await gate; controller.enqueue(chunk);}
          }));
        }
      };`);
    const latest = {...newSession(), name: `${app} latest notebook`};
    const latestLink = await sharedSessionURL(latest, base, key);
    await command('Page.navigate', {url:largeLink});
    await waitFor('typeof releaseSharedDecode === "function"');
    await command('Page.navigate', {url:latestLink});
    await waitFor(`document.querySelector('#session-name').value === ${JSON.stringify(latest.name)}`);
    await js(`releaseSharedDecode(); globalThis.DecompressionStream = originalDecompressionStream;
      new Promise(resolve => setTimeout(resolve, 100))`);
    assert.equal(await js('document.querySelector("#session-name").value'), latest.name,
      'An older asynchronous decode must not overwrite the latest navigation.');
    console.log(`PASS ${app}: native gzip links, large/legacy imports, decode races, clipboard, replay, prompts, invalid links, storage fallback and mobile layout.`);
  }
  assert.ok(requests.every(url => !url.includes('#session=')), 'Shared transcript stays in the URL fragment.');
} finally {
  fail(new Error('Browser sharing test finished.'));
  server.closeAllConnections(); await new Promise(resolve => server.close(resolve));
  if (browser.exitCode === null && browser.signalCode === null) browser.kill('SIGTERM');
  await stopped;
  await fs.rm(profile, {recursive:true,force:true,maxRetries:10,retryDelay:100});
}
