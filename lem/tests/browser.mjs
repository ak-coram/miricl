// Basic behavior of the packaged real LEM command loop and Lisp Canvas frontend.
// node lem/tests/browser.mjs [build/lem/site]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createReadStream} from 'node:fs';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';
const localHeap = process.env.LEM_BROWSER_HEAP_MIB;
if (localHeap !== undefined && !/^[1-9][0-9]{0,6}$/.test(localHeap)) throw new Error('LEM_BROWSER_HEAP_MIB must be a positive number of MiB.');
const site = path.resolve(process.argv[2] ?? 'build/lem/site');
const evidence = process.env.LEM_BROWSER_EVIDENCE ?? await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-lem-observation-'));
await fs.mkdir(evidence, {recursive:true});
const observations=[];
const server = http.createServer(async (request, response) => {
  try {
    const name = decodeURIComponent(new URL(request.url, 'http://localhost').pathname);
    const file = path.resolve(site, '.' + (name === '/' ? '/index.html' : name));
    if (!file.startsWith(site + path.sep)) {response.writeHead(403).end(); return;}
    const stat=await fs.stat(file); if(!stat.isFile())throw Error('Not a regular file');
    response.setHeader('Content-Type', /\.m?js$/.test(file) ? 'text/javascript' : file.endsWith('.html') ? 'text/html' : 'application/octet-stream');
    createReadStream(file).on('error', error => response.destroy(error)).pipe(response);
  } catch {response.writeHead(404).end();}
});
await new Promise((resolve, reject) => {server.once('error', reject); server.listen(0, '127.0.0.1', resolve);});
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-lem-browser-'));
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
  '--disable-background-networking', '--renderer-process-limit=1', ...(localHeap ? [`--js-flags=--max-old-space-size=${localHeap}`] : []),
  `--user-data-dir=${profile}`, '--no-startup-window', '--remote-debugging-pipe',
], {stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
const stopped = new Promise(resolve => browser.once('exit', resolve));
let received = '', sequence = 0, diagnostics = '';
const pending = new Map(), exceptions = [], consoleEvents=[];
let command, js, lisp;
let stage='connect', lastTimeout;
const requests=[];
function fail(error) {for (const {reject, timer} of pending.values()) {clearTimeout(timer); reject(error);} pending.clear();}
browser.stderr.on('data', bytes => {diagnostics = (diagnostics + bytes).slice(-6000);});
browser.on('error', fail);
browser.on('exit', code => fail(new Error(`Chromium exited ${code}: ${diagnostics}`)));
browser.stdio[4].on('data', bytes => {
  received += bytes;
  let end;
  while ((end = received.indexOf('\0')) !== -1) {
    const message = JSON.parse(received.slice(0, end)); received = received.slice(end + 1);
    if (message.method === 'Runtime.exceptionThrown') exceptions.push(message.params.exceptionDetails);
    if(message.method==='Runtime.consoleAPICalled'){consoleEvents.push({type:message.params.type,args:message.params.args.map(a=>a.value??a.description)});if(consoleEvents.length>100)consoleEvents.shift();}
    const entry = pending.get(message.id); if (!entry) continue;
    pending.delete(message.id); clearTimeout(entry.timer);
    if (message.error) entry.reject(new Error(JSON.stringify(message.error))); else entry.resolve(message.result);
  }
});
function send(method, params = {}, sessionId) {
  return new Promise((resolve, reject) => {
    const id = ++sequence;
    const request={id,method,sessionId,stage,start:Date.now(),
      expression:typeof params.expression==='string'?params.expression.slice(0,512):undefined,
      expressionLength:typeof params.expression==='string'?params.expression.length:undefined};
    requests.push(request); if(requests.length>128)requests.shift();
    const timer = setTimeout(() => {pending.delete(id);lastTimeout=request;reject(new Error(`Timed out: ${method}\n${JSON.stringify(request)}\n${diagnostics}`));}, 60000);
    pending.set(id, {resolve, reject, timer,request});
    browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
  });
}
try {
  console.log((await send('Browser.getVersion')).product);
  const {targetId} = await send('Target.createTarget', {url: 'about:blank'});
  const {sessionId} = await send('Target.attachToTarget', {targetId, flatten: true});
  command = (method, params) => send(method, params, sessionId);
  js = async expression => {
    const result = await command('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true});
    assert.equal(result.exceptionDetails, undefined, JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };
  lisp = source => js(`lem.evaluateAsync(${JSON.stringify(source)}, {eof:true}).then(result => result.output)`);
  const wait = async (predicate, message) => {
    const deadline = Date.now() + 60000;
    for (;;) {
      const failure = await js('globalThis.lemFailure');
      assert.ok(!failure, JSON.stringify(failure));
      assert.notEqual(await js('document.getElementById("lem-desktop")?.dataset.lemState'), 'failed');
      if (await predicate()) return;
      assert.ok(Date.now() < deadline, message);
      await new Promise(resolve => setTimeout(resolve, 25));
    }
  };
  await command('Page.enable'); await command('Runtime.enable');
  await command('Emulation.setDeviceMetricsOverride', {width: 1100, height: 800, deviceScaleFactor: 1, mobile: false});
  await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}/`});
  stage='startup';
  await wait(() => js('globalThis.lemReady === true && !globalThis.lem?.threads?.main'), 'LEM did not finish starting');
  stage='initial-lisp';
  const healthy = async () => {
    assert.equal(await js('!globalThis.lemFailure && document.getElementById("lem-desktop").dataset.lemState === "ready"'), true);
    assert.equal(await lisp('(eq :ready (getf (lem-browser:editor-status) :state))'), 'T\n');
  };
  assert.equal(await lisp(`(and (getf (lem-browser:editor-status) :editor-alive)
    (getf (lem-browser:editor-status) :bridge-alive)
    (eq :ready (getf (lem-browser:editor-status) :state)) t)`), 'T\n');
  const state = (text, line, column) => lisp(`(let* ((buffer (lem:window-buffer (lem:current-window)))
      (point (lem:buffer-point buffer)))
    (and (string= (lem:buffer-text buffer) ${JSON.stringify(text).replaceAll('\\n', '\n')})
         (= (lem:line-number-at-point point) ${line}) (= (lem:point-column point) ${column}) t))`);
  const expectState = (text, line, column) => wait(async () => await state(text, line, column) === 'T\n', `Wrong buffer/point: ${JSON.stringify({text,line,column})}`);
  const key = async (name, {shift = false, text = name.length === 1 ? name : undefined} = {}) => {
    const params = {type: 'keyDown', key: name, modifiers: shift ? 8 : 0};
    if (text !== undefined) params.text = text;
    await command('Input.dispatchKeyEvent', params);
    await command('Input.dispatchKeyEvent', {type: 'keyUp', key: name, modifiers: shift ? 8 : 0});
  };
  await js('document.querySelector(".lem-editor").focus()');
  stage='editing';
  await expectState('', 1, 0);
  for (const character of 'abc') await key(character);
  await key('Enter');
  for (const character of 'def') await key(character);
  await expectState('abc\ndef', 2, 3);
  await key('ArrowLeft'); await key('Backspace'); await key('Delete');
  await expectState('abc\nd', 2, 1);
  for (const name of ['ArrowLeft', 'ArrowUp', 'ArrowRight', 'ArrowDown']) await key(name);
  await expectState('abc\nd', 2, 1);
  await key('A', {shift: true}); await key('!', {shift: true}); await key('λ');
  await expectState('abc\ndA!λ', 2, 4);
  // Observe actual body glyphs, cursor and modeline pixels using Lisp geometry.
  stage='drawing';
  let pixels;
  await wait(async () => {
    const rawMetrics=await lisp('(let* ((i (lem:implementation)) (v (lem:window-view (lem:current-window)))) (setf (js:property (js:global "globalThis") "lemObservedSource") (lem-browser::view-canvas v) (js:property (js:global "globalThis") "lemObservedFrame") (lem-browser::browser-frames i)) (format t "[~D,~D,~D,~D,~D]" (lem-if:get-char-width i) (lem-if:get-char-height i) (lem-if:view-height i v) (lem-browser::view-cursor-x v) (lem-browser::view-cursor-y v)) (values))');
    observations.push({initialRawMetrics:rawMetrics});
    const metrics=JSON.parse(rawMetrics);
    assert.equal(metrics.length,5);
    pixels = await js(`(() => {const c=globalThis.lemObservedSource,x=c.getContext('2d');
      const [cw,ch,h,cx,cy]=${JSON.stringify(metrics)};
      const differs=(a,b)=>a.some((v,i)=>v!==b[i]);
      const sample=(y,w,hh)=>{const data=x.getImageData(0,y,w,hh).data,base=Array.from(data.slice(0,4));
        for(let i=4;i<data.length;i+=4)if(differs(Array.from(data.slice(i,i+4)),base))return true;return false;};
      return {canvas:{index:Array.from(document.querySelectorAll('.lem-view')).indexOf(c),connected:c.isConnected,width:c.width,height:c.height},frame:globalThis.lemObservedFrame,
        text:sample(0,3*cw,ch),modeline:sample(h-ch,Math.min(c.width,400),ch),
        cursor:Array.from(x.getImageData(cx+1,cy+1,1,1).data),near:Array.from(x.getImageData(cx+cw+1,cy+1,1,1).data)};})()`);
    observations.push({initialMetrics:metrics,pixels});
    return pixels.text&&pixels.modeline&&pixels.cursor.some((value,index)=>value!==pixels.near[index]);
  },'Automatic text/modeline/EOL cursor drawing did not become visible');
  assert.ok(pixels.text && pixels.modeline, JSON.stringify(pixels));
  assert.notDeepEqual(pixels.cursor, pixels.near, 'EOL cursor is not visible');
  const oldColumns = await lisp('(lem-if:display-width (lem:implementation))');
  stage='resize';
  await command('Emulation.setDeviceMetricsOverride', {width: 700, height: 520, deviceScaleFactor: 1, mobile: false});
  await wait(async () => await lisp('(lem-if:display-width (lem:implementation))') !== oldColumns, 'Display did not resize');
  await expectState('abc\ndA!λ', 2, 4);
  await key('z'); await expectState('abc\ndA!λz', 2, 5);
  const narrowColumns = await lisp('(lem-if:display-width (lem:implementation))');
  await js('document.getElementById("lem-desktop").style.cssText="width:1px;height:1px;min-height:0"');
  await wait(async () => await lisp('(lem-if:display-width (lem:implementation))') !== narrowColumns, 'Tiny container did not update');
  await js('document.getElementById("lem-desktop").removeAttribute("style")');
  await wait(async () => await lisp('(lem-if:display-width (lem:implementation))') === narrowColumns, 'Container did not recover');
  await command('Emulation.setDeviceMetricsOverride', {width: 1200, height: 850, deviceScaleFactor: 1, mobile: false});
  await wait(async () => await lisp('(lem-if:display-width (lem:implementation))') !== narrowColumns, 'Wider display did not recover');
  await expectState('abc\ndA!λz', 2, 5);
  await healthy();
  // Commands use the real editor queue, so popup creation/redraw shares the
  // same frame/implementation context as ordinary keyboard commands.
  stage='floating-popups';
  let eventSequence=0;
  await lisp('(defparameter *browser-test-event-sequence* 0) (values)');
  const queue = async form => {
    const expected=++eventSequence;
    await lisp(`(lem:send-event (lambda () ${form} (lem:redraw-display) (setf *browser-test-event-sequence* ${expected}))) (values)`);
    await wait(async()=>await lisp(`(>= *browser-test-event-sequence* ${expected})`)==='T\n','Queued editor operation did not finish');
  };
  await lisp(`(defparameter *browser-test-source* (lem:current-window))
    (defparameter *browser-test-threads* (set-difference (bt2:all-threads)
      (list (lem-browser::browser-editor lem-browser:*browser*) (lem-browser::browser-bridge lem-browser:*browser*))))
    (defparameter *browser-test-popup* nil) (defparameter *browser-test-popup-view* nil)
    (defparameter *browser-test-new-popup* nil) (defparameter *browser-test-split* nil) (values)`);
  const show = (text, timeout = 'nil', style = "'(:gravity :follow-cursor :background-color \"#314159\" :cursor-invisible t)") =>
    queue(`(setf *browser-test-popup* (lem:show-message ${JSON.stringify(text).replaceAll('\\n','\n')} :timeout ${timeout} :style ${style}))`);
  const popupLive = () => lisp(`(and *browser-test-popup* (not (lem:deleted-window-p *browser-test-popup*))
    (eq *browser-test-source* (lem:current-window))
    (eq *browser-test-popup* (lem-browser::view-window (lem:window-view *browser-test-popup*)))
    (member (lem:window-view *browser-test-popup*) (lem-browser::browser-views lem-browser:*browser*)) t)`);
  const exposePopup = () => lisp(`(let ((view (lem:window-view *browser-test-popup*)))
    (setf *browser-test-popup-view* view)
    (setf (js:property (js:global "globalThis") "lemObservedGroup") (lem-browser::view-group view)
          (js:property (js:global "globalThis") "lemObservedPopupFrame") (lem-browser::browser-frames lem-browser:*browser*))
    (format t "[~D,~D,~D,~D,~D,~D,~D,~,8F]"
      (lem-browser::view-x view) (lem-browser::view-y view)
      (lem-browser::view-columns view) (lem-browser::view-body-rows view)
      (lem:window-border *browser-test-popup*) (lem-if:get-char-width (lem:implementation))
      (lem-if:get-char-height (lem:implementation)) (lem-browser::browser-dpr lem-browser:*browser*)) (values))`);
  let lastPopupBodyPoint;
  const inspectPopup = async () => {
    await wait(async () => await popupLive() === 'T\n', 'Real floating message did not become live');
    const inspection={popupInspection:observations.length,probes:0,first:null,last:null};
    observations.push(inspection);
    let m,dropCurtain,geometry;
    await wait(async () => {
      const rawMetrics=await exposePopup();
      const sample={rawMetrics};
      if(!inspection.first)inspection.first=sample;
      inspection.last=sample;inspection.probes++;
      m = JSON.parse(rawMetrics);sample.popup=m;
      dropCurtain=await lisp('(eq :drop-curtain (lem:floating-window-border-shape *browser-test-popup*))')==='T\n';
      geometry = await js(`(() => {const g=globalThis.lemObservedGroup,c=g.querySelector('canvas'),r=g.getBoundingClientRect(),b=c.getBoundingClientRect(),root=document.querySelector('.lem-editor').getBoundingClientRect();
      const p=c.getContext('2d').getImageData(c.width-2,c.height-2,1,1).data;
      let text=false;const data=c.getContext('2d').getImageData(0,0,c.width,c.height).data;
      for(let i=0;i<data.length;i+=4)if(data[i]!==p[0]||data[i+1]!==p[1]||data[i+2]!==p[2]){text=true;break;}
      return {group:[r.left-root.left,r.top-root.top,r.width,r.height],body:[b.left-root.left,b.top-root.top,b.width,b.height],
        root:[root.left,root.top],absoluteGroup:[r.left,r.top],canvas:[c.width,c.height],frame:globalThis.lemObservedPopupFrame,background:Array.from(p),text,
        focus:document.activeElement===document.querySelector('.lem-editor'),z:Number(g.style.zIndex),edges:g.querySelectorAll('.lem-border').length};})()`);
      sample.geometry=geometry;sample.dropCurtain=dropCurtain;
      const [x,y,w,h,b,cw,ch,dpr]=m;
      const same=(a,b)=>a.length===b.length&&a.every((value,index)=>value===b[index]);
      return same(geometry.group,[(x-b)*cw,(y-b)*ch,(w+2*b)*cw,(h+2*b)*ch])&&
        same(geometry.body,[x*cw,y*ch,w*cw,h*ch])&&
        same(geometry.canvas,[Math.ceil(w*cw*dpr),Math.ceil(h*ch*dpr)])&&
        same(geometry.background,[49,65,89,255])&&geometry.text&&geometry.focus&&geometry.z>0;
    },'Automatic popup geometry/text/background/focus did not become visible');
    const [x,y,w,h,b,cw,ch,dpr] = m;
    assert.deepEqual(geometry.group,[(x-b)*cw,(y-b)*ch,(w+2*b)*cw,(h+2*b)*ch]);
    assert.deepEqual(geometry.body,[x*cw,y*ch,w*cw,h*ch]);
    assert.deepEqual(geometry.canvas,[Math.ceil(w*cw*dpr),Math.ceil(h*ch*dpr)]);
    assert.deepEqual(geometry.background,[49,65,89,255]);
    assert.ok(geometry.text && geometry.focus && geometry.z>0, JSON.stringify(geometry));
    if(b){
      const png=await command('Page.captureScreenshot',{format:'png'});
      const [gx,gy]=geometry.absoluteGroup;
      const samples=[[Math.floor(gx+b*cw/2+2),Math.floor(gy+b*ch/2)],
                     [Math.floor(gx+b*cw+w*cw-2),Math.floor(gy+b*ch+h*ch-2)],
                     [Math.floor(gx+b*cw/2),Math.floor(gy+1)]];
      lastPopupBodyPoint=samples[1];
      const composite=await js(`new Promise((resolve,reject)=>{const image=new Image();image.onerror=reject;image.onload=()=>{const c=document.createElement('canvas');c.width=image.width;c.height=image.height;const ctx=c.getContext('2d');ctx.drawImage(image,0,0);resolve(${JSON.stringify(samples)}.map(([x,y])=>Array.from(ctx.getImageData(x,y,1,1).data)));};image.src='data:image/png;base64,${png.data}';})`);
      assert.notDeepEqual(composite[0],geometry.background,'Visible border pixel missing');
      assert.deepEqual(composite[1],geometry.background,'Popup blank body is not opaque above editor');
      if(dropCurtain) assert.notDeepEqual(composite[2],geometry.background,'Drop-curtain upper continuation is missing');
      else assert.deepEqual(composite[2],geometry.background,'Normal border unexpectedly extends through its upper margin');
      observations.push({popup:m,dropCurtain,geometry,composite});
    }
    return geometry;
  };
  const deletedView = () => lisp(`(let ((view *browser-test-popup-view*))
    (and (not (member view (lem-browser::browser-views lem-browser:*browser*)))
      (null (lem-browser::view-group view)) (null (lem-browser::view-edges view))
      (null (lem-browser::view-canvas view)) (null (lem-browser::view-backing view))
      (null (lem-browser::view-context view)) t))`);
  await show('Visible popup α message\nshort');
  await inspectPopup();
  await key('p'); await expectState('abc\ndA!λzp',2,6);
  assert.equal(await lisp('(eq *browser-test-source* (lem:current-window))'),'T\n');
  // The core deliberately clears SHOW-MESSAGE on a keyboard command.
  await wait(async()=>await deletedView()==='T\n','Typing did not release its old message view');
  await show('Visible popup α message\nshort');
  await inspectPopup();
  // A later ordinary split and a second genuine float must follow frame order.
  await queue(`(let ((before (lem:window-list))) (lem:split-window-vertically *browser-test-source*)
    (setf *browser-test-split* (first (set-difference (lem:window-list) before))))`);
  await wait(async () => await lisp('(and *browser-test-split* t)') === 'T\n','Real split was not created');
  await inspectPopup();
  assert.equal(await js(`Array.from(document.querySelectorAll('.lem-window:not(.lem-floating)'),g=>Number(g.style.zIndex)).every(z=>z<Number(globalThis.lemObservedGroup.style.zIndex))`),true);
  await queue(`(setf *browser-test-new-popup* (lem:display-popup-message "Second front popup\nshort" :timeout nil
    :style '(:gravity :top-display :background-color "#314159" :shape :drop-curtain)))`);
  assert.equal(await lisp(`(let ((a (lem:window-view *browser-test-popup*)) (b (lem:window-view *browser-test-new-popup*)))
    (< (parse-integer (js:from-js (js:property (js:property (lem-browser::view-group a) "style") "zIndex")))
       (parse-integer (js:from-js (js:property (js:property (lem-browser::view-group b) "style") "zIndex")))))`),'T\n');
  await queue('(defparameter *browser-test-back-popup* *browser-test-popup*) (setf *browser-test-popup* *browser-test-new-popup*)');
  await inspectPopup();
  await queue(`(defparameter *browser-test-external-entry* (find *browser-test-new-popup*
    (lem-browser::browser-popups lem-browser:*browser*) :key #'lem-browser::popup-entry-window))
    (lem:delete-window *browser-test-new-popup*)`);
  assert.equal(await deletedView(),'T\n');
  assert.equal(await lisp('(null (lem-browser::popup-entry-owner *browser-test-external-entry*))'),'T\n');
  await queue('(setf *browser-test-popup* *browser-test-back-popup*) (lem:delete-window *browser-test-split*)');
  await inspectPopup();
  await queue('(lem:clear-message) (lem:clear-message)');
  await wait(async () => await deletedView() === 'T\n','Deleted popup retained owned JS resources');
  assert.equal(await js('globalThis.lemObservedGroup.isConnected'),false);
  await lisp('(setf (js:property (js:global "globalThis") "lemObservedSource") (lem-browser::view-canvas (lem:window-view *browser-test-source*))) (values)');
  const underlying=await js(`(()=>{const c=globalThis.lemObservedSource,r=c.getBoundingClientRect(),[x,y]=${JSON.stringify(lastPopupBodyPoint)};if(x<r.left||y<r.top||x>=r.right||y>=r.bottom)throw Error('Popup observation is outside source view');return Array.from(c.getContext('2d').getImageData(Math.floor((x-r.left)*c.width/r.width),Math.floor((y-r.top)*c.height/r.height),1,1).data);})()`);
  const afterRemoval=await command('Page.captureScreenshot',{format:'png'});
  const exposed=await js(`new Promise((resolve,reject)=>{const image=new Image();image.onerror=reject;image.onload=()=>{const c=document.createElement('canvas');c.width=image.width;c.height=image.height;const ctx=c.getContext('2d');ctx.drawImage(image,0,0);const [x,y]=${JSON.stringify(lastPopupBodyPoint)};resolve(Array.from(ctx.getImageData(x,y,1,1).data));};image.src='data:image/png;base64,${afterRemoval.data}';})`);
  assert.deepEqual(exposed,underlying,'Popup removal did not expose retained source pixels');
  observations.push({underlyingRestored:{point:lastPopupBodyPoint,underlying,exposed}});
  await expectState('abc\ndA!λzp',2,6);
  // Replacement invalidates the old deadline; zero timeout is asynchronous.
  await show('Old timed message','3');
  await wait(async () => await popupLive() === 'T\n','Timed popup missing');
  await exposePopup();
  await lisp(`(defparameter *browser-test-replacement-deadline* (lem-browser::popup-entry-deadline
    (find *browser-test-popup* (lem-browser::browser-popups lem-browser:*browser*) :key #'lem-browser::popup-entry-window))) (values)`);
  await show('Replacement stays visible','nil');
  await wait(async () => await deletedView() === 'T\n','Replacement retained old view');
  const replacementRemaining=Number(await lisp('(max 0 (ceiling (* 1000 (- *browser-test-replacement-deadline* (get-internal-real-time))) internal-time-units-per-second))'));
  assert.ok(replacementRemaining>0,'Replacement did not precede its original deadline');
  await new Promise(resolve=>setTimeout(resolve,replacementRemaining+100));
  assert.equal(await lisp('(>= (get-internal-real-time) *browser-test-replacement-deadline*)'),'T\n');
  assert.equal(await popupLive(),'T\n');
  await queue('(lem:clear-message)');
  await queue('(setf *browser-test-popup* (lem:show-message "Zero expires next bridge turn" :timeout 0)) (assert (not (lem:deleted-window-p *browser-test-popup*)))');
  await wait(async () => await lisp('(null (lem:frame-message-window (lem:current-frame)))') === 'T\n','Zero-time popup did not expire');
  // Active follow-cursor popup survives ordinary/tiny/recovered geometry.
  await show('Resize popup remains readable\nshort');
  await inspectPopup();
  const popupColumns = await lisp('(lem-if:display-width (lem:implementation))');
  await command('Emulation.setDeviceMetricsOverride',{width:800,height:600,deviceScaleFactor:1,mobile:false});
  await wait(async()=>await lisp('(lem-if:display-width (lem:implementation))')!==popupColumns,'Popup resize did not update');
  await inspectPopup();
  await js('document.getElementById("lem-desktop").style.cssText="width:1px;height:1px;min-height:0"');
  await wait(async()=>await lisp('(lem-if:display-width (lem:implementation))')==='4\n','Active-popup tiny resize did not update');
  await js('document.getElementById("lem-desktop").removeAttribute("style")');
  await wait(async()=>await lisp('(lem-if:display-width (lem:implementation))')!=='4\n','Active-popup size did not recover');
  await inspectPopup();
  await queue('(lem:clear-message)');
  await key('t'); await expectState('abc\ndA!λzpt',2,7);
  const messageLength = () => lisp('(let ((b (lem:get-buffer "*Messages*"))) (if b (length (lem:buffer-text b)) 0))').then(Number);
  stage='unsupported-input';
  const messageAfter = (offset,text) => lisp(`(let ((buffer (lem:get-buffer "*Messages*")))
    (and buffer (search ${JSON.stringify(text)} (lem:buffer-text buffer) :start2 ${offset}) t))`);
  const unsupportedEvent = async (eventSource,reason) => {
    const start=await messageLength();
    const event=await js(eventSource);
    observations.push({unsupported:reason,event});
    await wait(async()=>await messageAfter(start,reason)==='T\n',`Missing new ${reason} message`);
    await wait(async()=>await lisp('(let ((w (lem:frame-message-window (lem:current-frame)))) (and w (not (lem:deleted-window-p w)) t))')==='T\n','Useful feedback popup missing');
    await healthy();
    await expectState('abc\ndA!λzpt',2,7);
  };
  await unsupportedEvent(`(()=>{const e=new CompositionEvent('compositionstart',{bubbles:true,cancelable:true});document.querySelector('.lem-editor').dispatchEvent(e);return {type:e.type};})()`,'composition input');
  await unsupportedEvent(`(()=>{const e=new KeyboardEvent('keydown',{key:'Dead',isComposing:true,bubbles:true,cancelable:true});const before={key:e.key,isComposing:e.isComposing};if(!before.isComposing)throw Error('Missing real composition modifier');document.querySelector('.lem-editor').dispatchEvent(e);return before;})()`,'keyboard composition input');
  await unsupportedEvent(`(()=>{const e=new KeyboardEvent('keydown',{key:'Dead',bubbles:true,cancelable:true});const before={key:e.key,isComposing:e.isComposing};document.querySelector('.lem-editor').dispatchEvent(e);return before;})()`,'dead-key input');
  await unsupportedEvent(`(()=>{const e=new KeyboardEvent('keydown',{key:'x',bubbles:true,cancelable:true,modifierAltGraph:true});const before={key:e.key,altGraph:e.getModifierState('AltGraph')};if(!before.altGraph)throw Error('AltGraph modifier is false');document.querySelector('.lem-editor').dispatchEvent(e);return before;})()`,'AltGraph modifier input');
  await unsupportedEvent(`(()=>{const e=new Event('paste',{bubbles:true,cancelable:true});document.querySelector('.lem-editor').dispatchEvent(e);return {type:e.type};})()`,'browser clipboard paste');
  await key('q'); await expectState('abc\ndA!λzptq',2,8);
  assert.equal(await lisp('(let ((record (list :resize))) (setf (cdr record) record) (handler-case (progn (lem-browser::deliver-input lem-browser:*browser* record) nil) (error () t)))'), 'T\n');
  const malformedOffset=await messageLength();
  stage='malformed-input';
  const dimensionsBeforeMalformed=await lisp('(list (lem-if:display-width (lem:implementation)) (lem-if:display-height (lem:implementation)))');
  await lisp('(lem-browser::enqueue-input lem-browser:*browser* (list :resize "invalid" 20 1)) (values)');
  await key('v'); await expectState('abc\ndA!λzptqv', 2, 9);
  await wait(async () => await messageAfter(malformedOffset,'Malformed browser display dimensions') === 'T\n', 'Missing malformed-input diagnostic');
  assert.equal(await lisp('(list (lem-if:display-width (lem:implementation)) (lem-if:display-height (lem:implementation)))'),dimensionsBeforeMalformed);
  await healthy();
  assert.equal(await lisp('(lem-browser:start-editor) (getf (lem-browser:editor-status) :editor-alive)'), 'T\n');
  assert.equal(await js('document.querySelectorAll(".lem-editor").length'), 1);
  // Real selected Lisp-mode/listener and compile commands on the owning editor
  // thread. The Return key goes through the same command loop as ordinary input.
  stage='local-lisp-provider';
  // Activating the genuine Lisp major mode may create its lazy connection.
  // No service work is submitted merely by starting the editor.
  assert.equal(await lisp(`(let ((connection (lem-lisp-mode/connection:current-connection)))
    (or (null connection)
        (and (eq connection (lem-browser::browser-services lem-browser:*browser*))
             (lem-lisp-mode/connection:connection-live-p connection)
             (null (lem-lisp-mode/connection:connection-requests connection))
             (not (miricl-ide-services:session-worker-alive-p
                    (lem-lisp-mode/connection:connection-session connection))) t)))`), 'T\n');
  await lisp('(defparameter *browser-test-original-buffer* (lem:window-buffer (lem:current-window))) (values)');
  await queue('(lem-lisp-mode/internal::start-lisp-repl t)');
  const providerIdle = () => lisp(`(let ((connection (lem-lisp-mode/connection:current-connection)))
    (and connection (null (lem-lisp-mode/connection:connection-requests connection))
      (not (miricl-ide-services:session-worker-alive-p (lem-lisp-mode/connection:connection-session connection)))
      (null (lem:buffer-value (lem-lisp-mode/internal::repl-buffer) 'lem-lisp-mode/internal::local-listener-owner)) t))`);
  const listener = async (source, expectedState, expectedValues, output) => {
    const prior = Number(await lisp('(miricl-ide-services::session-next-id (lem-lisp-mode/connection:connection-session (lem-lisp-mode/connection:current-connection)))'));
    await queue(`(lem:buffer-end (lem:current-point)) (lem:insert-string (lem:current-point) ${JSON.stringify(source)})`);
    await key('Enter');
    await wait(async () => await lisp(`(> (miricl-ide-services::session-next-id (lem-lisp-mode/connection:connection-session (lem-lisp-mode/connection:current-connection))) ${prior})`) === 'T\n' && await providerIdle() === 'T\n', 'Listener request did not finish and release input');
    assert.equal(await lisp(`(let* ((session (lem-lisp-mode/connection:connection-session (lem-lisp-mode/connection:current-connection)))
        (result (miricl-ide-services:result session (miricl-ide-services::session-next-id session))))
      (and (eq ${expectedState} (miricl-ide-services:request-state result))
        ${expectedValues === undefined ? 't' : `(equal '${expectedValues} (miricl-ide-services:request-values result))`}
        (not (lem:buffer-read-only-p (lem-lisp-mode/internal::repl-buffer))) t))`), 'T\n');
    if (output) assert.equal(await lisp(`(and (search ${JSON.stringify(output)} (lem:buffer-text (lem-lisp-mode/internal::repl-buffer))) t)`), 'T\n');
  };
  await listener('(values)', ':ok', 'nil', '; No value');
  await listener('(values nil)', ':ok', '(nil)');
  await listener('(values 7 nil :tail)', ':ok', '(7 nil :tail)');
  await listener('(defpackage :browser-test-package (:use :cl)) (in-package :browser-test-package) (values 9)', ':ok', '(9)');
  await listener('(package-name *package*)', ':ok', '("BROWSER-TEST-PACKAGE")');
  await listener('(progn (write-string "browser-output") (values 42))', ':ok', '(42)', 'browser-output');
  await listener('(in-package :cl-user) (error "browser-error")', ':error', undefined, 'browser-error');
  await listener('(+ 40 2)', ':ok', '(42)');
  await queue(`(lem:switch-to-buffer (lem:make-buffer "*browser-compile-check.lisp*" :temporary t))
    (lem:change-buffer-mode (lem:current-buffer) 'lem-lisp-mode/internal::lisp-mode)
    (lem:insert-string (lem:current-point) "(in-package :cl-user) (defun browser-compiled-check (x) (+ x 2))")
    (lem-lisp-mode/internal::lisp-compile-region (lem:buffer-start-point (lem:current-buffer))
                                               (lem:buffer-end-point (lem:current-buffer)))`);
  await wait(async () => await providerIdle() === 'T\n' && await lisp('(and (fboundp \'cl-user::browser-compiled-check) (= 42 (cl-user::browser-compiled-check 40)) t)') === 'T\n', 'Real compile-region did not compile, load and install its function');
  assert.equal(await lisp(`(let* ((session (lem-lisp-mode/connection:connection-session (lem-lisp-mode/connection:current-connection)))
      (result (miricl-ide-services:result session (miricl-ide-services::session-next-id session))))
    (and (eq :compile-and-load (miricl-ide-services:request-operation result))
      (getf (miricl-ide-services:request-compilation result) :loaded) t))`), 'T\n');
  await queue('(lem:switch-to-buffer *browser-test-original-buffer*)');
  await expectState('abc\ndA!λzptqv', 2, 9);
  observations.push({localListenerValuesOutputErrorRecovery:true,guestCompileRegionLoaded:true});
  stage='provider-stop-cancellation';
  await lisp('(defparameter *browser-test-provider-unwound* nil) (defparameter *browser-test-provider-connection* nil) (values)');
  await queue(`(setf *browser-test-provider-connection* (lem-lisp-mode/connection:current-connection))
    (lem-lisp-mode/internal::local-listener-eval
      "(unwind-protect (loop (sleep 0.01)) (setq cl-user::*browser-test-provider-unwound* t))")
    (lem:switch-to-buffer *browser-test-original-buffer*)`);
  await wait(async () => await lisp(`(let* ((connection *browser-test-provider-connection*)
      (request (first (lem-lisp-mode/connection:connection-requests connection))))
    (and request (eq :running (miricl-ide-services:request-state
      (lem-lisp-mode/connection:local-request-service request))) t))`) === 'T\n', 'Busy listener did not start before STOP');
  stage='ordered-stop';
  await queue(`(setf *browser-test-popup* (lem:display-popup-message "Pending at STOP" :timeout 5))`);
  await lisp(`(defparameter *browser-test-old-browser* lem-browser:*browser*)
    (defparameter *browser-test-old-frame* (lem:current-frame))
    (defparameter *browser-test-old-view* (lem:window-view *browser-test-popup*))
    (defparameter *browser-test-old-deadline* (lem-browser::popup-entry-deadline
      (find *browser-test-popup* (lem-browser::browser-popups lem-browser:*browser*) :key #'lem-browser::popup-entry-window)))
    (values)`);
  await lisp('(defparameter *browser-test-stopped-buffer* (lem:window-buffer (lem:current-window))) (values)');
  await key('S');
  // Do not wait for S: STOP must follow accepted input, and later input must
  // not leak through the persistent core event queue into a restarted editor.
  await lisp(`(defparameter *browser-test-queued-expiry-entry* nil)
    (defparameter *browser-test-queued-expiry-proved* nil)
    (lem:send-event (lambda ()
      (let ((pending (find *browser-test-popup* (lem-browser::browser-popups lem-browser:*browser*)
                           :key #'lem-browser::popup-entry-window)))
        (assert pending)
        (assert (eq :pending (lem-browser::popup-entry-state pending)))
        (assert (< (get-internal-real-time) *browser-test-old-deadline*)))
      (let ((window (lem:display-popup-message "Already queued at STOP" :timeout 0)))
        (setf *browser-test-queued-expiry-entry*
          (find window (lem-browser::browser-popups lem-browser:*browser*) :key #'lem-browser::popup-entry-window))
        (lem-browser::enqueue-popup-expirations lem-browser:*browser*)
        (assert (eq :queued (lem-browser::popup-entry-state *browser-test-queued-expiry-entry*)))
        (lem-browser:stop-editor)
        (assert (null (lem-browser::popup-entry-owner *browser-test-queued-expiry-entry*)))
        (setf *browser-test-queued-expiry-proved* t)
        (lem-browser::enqueue-input lem-browser:*browser* (list :key "X" nil nil nil nil))
        (lem-browser:stop-editor)))) (values)`);
  await wait(async () => await lisp('(getf (lem-browser:editor-status) :state)') === ':STOPPED\n', 'Editor did not stop normally');
  assert.equal(await js('document.querySelectorAll(".lem-editor").length'), 0);
  assert.equal(await lisp('*browser-test-queued-expiry-proved*'),'T\n');
  assert.equal(await lisp('(null (lem-lisp-mode/connection:current-connection))'), 'T\n');
  assert.equal(await lisp(`(and *browser-test-provider-unwound*
    (null (lem-lisp-mode/connection:connection-requests *browser-test-provider-connection*))
    (not (miricl-ide-services:session-worker-alive-p (lem-lisp-mode/connection:connection-session *browser-test-provider-connection*))) t)`), 'T\n');
  assert.equal(await lisp(`(and (null (lem:get-frame *browser-test-old-browser*))
    (null (lem-browser::browser-popups *browser-test-old-browser*))
    (null (lem-browser::browser-generation *browser-test-old-browser*))
    (null (lem-browser::view-group *browser-test-old-view*))
    (null (lem-browser::view-canvas *browser-test-old-view*))
    (null (lem-browser::view-backing *browser-test-old-view*))
    (null (lem-browser::view-context *browser-test-old-view*)) t)`),'T\n');
  await wait(async()=>await lisp('(null (set-exclusive-or *browser-test-threads* (bt2:all-threads) :test #\'eq))')==='T\n','Owned editor/message threads remained after STOP');
  assert.equal(await lisp('(string= (lem:buffer-text *browser-test-stopped-buffer*) "abc\ndA!λzptqvS")'), 'T\n');
  await lisp('(lem-browser:start-editor) (values)');
  stage='restart';
  await wait(() => js('document.getElementById("lem-desktop").dataset.lemState === "ready"'), 'Editor did not restart');
  assert.equal(await lisp('(string= (lem:buffer-text (lem:window-buffer (lem:current-window))) "abc\ndA!λzptqvS")'), 'T\n');
  assert.equal(await lisp('(and (not (eq *browser-test-old-browser* lem-browser:*browser*)) (not (eq *browser-test-old-frame* (lem:current-frame))) (lem-browser::browser-generation lem-browser:*browser*) t)'),'T\n');
  await queue('(setf *browser-test-popup* (lem:display-popup-message "New lifecycle remains" :timeout nil))');
  await key('R');
  await wait(async () => await lisp('(and (search "R" (lem:buffer-text (lem:window-buffer (lem:current-window)))) t)') === 'T\n', 'Restarted editor did not accept input');
  await healthy();
  const remainingMs=Number(await lisp('(max 0 (ceiling (* 1000 (- *browser-test-old-deadline* (get-internal-real-time))) internal-time-units-per-second))'));
  assert.ok(remainingMs>0,'Restart did not precede the retained old deadline');
  await new Promise(resolve=>setTimeout(resolve,remainingMs+150));
  assert.equal(await lisp('(and (>= (get-internal-real-time) *browser-test-old-deadline*) (not (lem:deleted-window-p *browser-test-popup*)) (string= "New lifecycle remains" (lem:buffer-text (lem:window-buffer *browser-test-popup*))))'),'T\n');
  observations.push({stopRestartPastOldDeadline:true,remainingMs});
  await lisp('(lem-browser:stop-editor) (values)');
  await wait(async () => await lisp('(getf (lem-browser:editor-status) :state)') === ':STOPPED\n', 'Restarted editor did not stop');
  await wait(async()=>await lisp('(null (set-exclusive-or *browser-test-threads* (bt2:all-threads) :test #\'eq))')==='T\n','Restart left owned threads');
  assert.equal(await js('document.querySelectorAll(".lem-editor").length'),0);
  assert.equal(await lisp('(and (null (lem:get-frame lem-browser:*browser*)) (null (lem-browser::browser-popups lem-browser:*browser*)) (null (lem-browser::browser-views lem-browser:*browser*)) t)'),'T\n');
  assert.deepEqual(exceptions, []);
  await fs.writeFile(path.join(evidence,'browser-observation.json'),JSON.stringify({passed:true,observations,console:consoleEvents,exceptions,requests},null,2));
  console.log('LEM packaged command loop, popups, local Lisp listener, guest compilation and lifecycle checks passed.',evidence);
} catch(error) {
  const result={error:String(error),stage,lastTimeout,requests,observations,console:consoleEvents,exceptions};
  if(js){try{result.dom=await js('({ready:globalThis.lemReady,failure:globalThis.lemFailure,state:document.getElementById("lem-desktop")?.dataset.lemState,status:document.getElementById("status")?.textContent,roots:document.querySelectorAll(".lem-editor").length,views:document.querySelectorAll(".lem-window").length,scheduler:globalThis.lem&&{main:!!lem.threads?.main,running:lem.threads?.running,segment:lem.threads?.segment,waiting:Array.from(lem.threads?.waiting.keys()??[]),depth:lem.javascript?.depth}})');}catch(c){result.domError=String(c);}}
  const busy=!result.dom?.scheduler || result.dom.scheduler.main || result.dom.scheduler.running || result.dom.scheduler.depth>0;
  if(busy) result.lispSkipped="Original evaluation or Lisp callback is active, or scheduler state is unavailable.";
  if(lisp&&!busy){try{result.lisp=await lisp("(let ((*print-circle* t) (*print-level* 6) (*print-length* 24))\n  (format t \"~S~%\"\n    (list :status (lem-browser:editor-status)\n          :threads (mapcar (lambda (thread)\n                             (list (miricl-threads:thread-name thread)\n                                   (miricl-threads:thread-alive-p thread)))\n                           (miricl-threads:all-threads))\n          :messages (let ((buffer (lem:get-buffer \"*Messages*\")))\n                      (when buffer\n                        (let ((text (lem:buffer-text buffer)))\n                          (subseq text (max 0 (- (length text) 4096))))))\n          :buffer (handler-case\n                      (let* ((frame (lem:current-frame))\n                             (window (and frame (lem:current-window)))\n                             (buffer (and window (lem:window-buffer window))))\n                        (when buffer\n                          (list :name (lem:buffer-name buffer)\n                                :text (lem:buffer-text buffer)\n                                :line (lem:line-number-at-point (lem:buffer-point buffer))\n                                :column (lem:point-column (lem:buffer-point buffer)))))\n                    (error (condition) (princ-to-string condition)))))\n  (values))\n");}catch(c){result.lispError=String(c);}}
  await fs.writeFile(path.join(evidence,'browser-observation.json'),JSON.stringify(result,null,2));
  if(command){try{const png=await command('Page.captureScreenshot',{format:'png'});await fs.writeFile(path.join(evidence,'browser-observation.png'),Buffer.from(png.data,'base64'));}catch(c){console.log('Screenshot failed',String(c));}}
  console.log('LEM browser failure evidence',evidence,JSON.stringify(result));
  throw error;
} finally {
  try {await send('Browser.close');} catch {}
  await Promise.race([stopped, new Promise(resolve => setTimeout(resolve, 2000))]);
  if (browser.exitCode === null) browser.kill('SIGTERM');
  await new Promise(resolve => server.close(resolve));
  await fs.rm(profile, {recursive: true, force: true});
}
