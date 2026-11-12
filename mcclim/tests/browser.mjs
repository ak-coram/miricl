// Browser integration for the Lisp backend, using the development core image.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';
const files = new Map([
  ['/miricl.js', await fs.readFile(new URL('../../miricl.js', import.meta.url))],
  ['/core.wasm', await fs.readFile(process.argv[2] ?? new URL('../build/mcclim-core.wasm', import.meta.url))],
]);
// A cached image can skip the build that normally creates this directory.
await fs.mkdir(new URL('../build/', import.meta.url), {recursive: true});
const backend = ['package', 'port', 'input', 'windows', 'medium', 'fonts', 'bitmaps'];
for (const name of backend) files.set(`/${name}.lisp`, await fs.readFile(new URL(`../backend/${name}.lisp`, import.meta.url)));
const frame = `(in-package :cl-user)
  (defparameter *browser-bitmap*
    (make-instance 'climi::image-pattern :array
      (make-array '(2 2) :element-type '(unsigned-byte 32)
        :initial-contents '((#xffff0000 #x8000ff00) (#xff0000ff #x00000000)))))
  (defparameter *browser-codec-ticks* 0)
  (defparameter *browser-codec-done* nil)
  (clim-sys:make-process (lambda () (loop until *browser-codec-done* do
    (incf *browser-codec-ticks*) (sleep 0.001))) :name "Codec progress")
  (climi::write-bitmap-file *browser-bitmap* "/roundtrip.png" :format :png)
  (multiple-value-bind (array inks) (climi::read-bitmap-file "/roundtrip.png" :format :png)
    (assert (null inks))
    (assert (equal (array-dimensions array) '(2 2)))
    (assert (equalp array (climi::pattern-array *browser-bitmap*))))
  (climi::write-bitmap-file *browser-bitmap* "/roundtrip.jpg" :format :jpeg)
  (assert (equal (array-dimensions (climi::read-bitmap-file "/roundtrip.jpg" :format :jpeg)) '(2 2)))
  (with-open-file (stream "/invalid.png" :direction :output :element-type '(unsigned-byte 8))
    (write-sequence #(1 2 3 4) stream))
  (assert (handler-case (progn (climi::read-bitmap-file "/invalid.png" :format :png) nil)
    (climi::unsupported-bitmap-format () t)))
  (assert (equal (array-dimensions (climi::read-bitmap-file "/roundtrip.png" :format :png)) '(2 2)))
  (setq *browser-codec-done* t)
  (assert (plusp *browser-codec-ticks*))
  (defparameter *browser-smoke-clicks* 0)
  (defparameter *browser-smoke-paints* 0)
  (clim:define-application-frame browser-smoke () ()
    (:menu-bar nil)
    (:panes
      (drawing :application :width 480 :height 240 :scroll-bars nil
        :display-function (lambda (frame pane)
          (declare (ignore frame)) (incf *browser-smoke-paints*)
          (setf (js:property (js:global "globalThis") "paintStage") "rectangle")
          (clim:draw-rectangle* pane 10 10 180 90 :ink clim:+blue+)
          (setf (js:property (js:global "globalThis") "paintStage") "pattern")
          (clim:draw-pattern* pane *browser-bitmap* 190 10)
          (setf (js:property (js:global "globalThis") "paintStage") "text")
          (clim:draw-text* pane "McCLIM in the browser" 20 45 :ink clim:+white+)
          (setf (js:property (js:global "globalThis") "paintStage") "painted")))
      (action :push-button :label "Count click"
        :activate-callback (lambda (gadget) (declare (ignore gadget)) (incf *browser-smoke-clicks*))))
    (:layouts (default (clim:vertically () drawing action))))
  (defparameter *browser-smoke-frame* (clim:find-application-frame 'browser-smoke :activate nil))
  (defparameter *browser-smoke-thread*
    (clim-sys:make-process
      (lambda ()
        (handler-bind ((serious-condition
          (lambda (condition)
            (let* ((app (js:global "app")) (exports (js:property app "vm"))
                   (ignored (js:invoke exports "capture-backtrace"))
                   (length (js:from-js (js:invoke exports "format-backtrace")))
                   (stack (js:from-js (js:invoke app "text" length))))
             (declare (ignore ignored))
             (setf (js:property (js:global "globalThis") "failure")
              (format nil "~A~@[ [~S]~]~@[ [datum ~S]~]~%~A" condition
                (when (typep condition 'cell-error) (cell-error-name condition))
                (when (typep condition 'type-error) (type-error-datum condition))
                stack))))))
          (clim:run-frame-top-level *browser-smoke-frame*)
          (setf (js:property (js:global "globalThis") "frameExited") t))) :name "Browser smoke"))
  (values)`;
const html = `<!doctype html><meta charset="utf-8"><style>
body {margin:0;background:#9999cc} #mcclim-desktop {width:960px;height:700px}
.mcclim-window {background:#ffffea;border:1px solid #333;box-shadow:4px 4px #3335}
</style><div id="mcclim-desktop"></div><script type="module">
import {Miricl} from './miricl.js';
globalThis.output = '';
addEventListener('error', event => {
  globalThis.callbackFailure = {message:event.error?.message,cause:String(event.error?.cause),
    causeName:event.error?.cause?.name,code:event.error?.code,backtrace:event.error?.backtrace};
});
try {
 globalThis.app = await Miricl.instantiate(await (await fetch('./core.wasm')).arrayBuffer(),
   {onOutput: text => {globalThis.output += text;},
    onThreadError: error => {globalThis.failure = error.message+'\\n'+error.backtrace;}});
 app.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
 const backendPresent=app.evaluate('(let ((package (find-package "MCCLIM-BROWSER"))) (and package (find-class (find-symbol "BROWSER-PORT" package) nil) t))').output === 'T\\n';
 for (const name of backendPresent && !${process.argv.includes('--source-backend')} ? [] : ${JSON.stringify(backend)}) {
   app.mountFile('/'+name+'.lisp', await (await fetch('./'+name+'.lisp')).text());
   await app.evaluateAsync('(load "/'+name+'.lisp") (values)', {eof:true});
 }
 await app.evaluateAsync(${JSON.stringify(frame)}, {eof:true});
 globalThis.ready = true;
} catch (error) {globalThis.failure = error.message+'\\n'+error.backtrace;}
</script>`;
const server = http.createServer((request, response) => {
  if (request.url === '/') {response.setHeader('Content-Type', 'text/html'); response.end(html); return;}
  const bytes = files.get(request.url);
  if (!bytes) {response.writeHead(404).end(); return;}
  response.setHeader('Content-Type', request.url.endsWith('.js') ? 'text/javascript' : 'application/octet-stream');
  response.end(bytes);
});
await new Promise((resolve, reject) => {server.once('error', reject); server.listen(0, '127.0.0.1', resolve);});
const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-mcclim-browser-'));
const browser = spawn(process.env.CHROMIUM ?? 'chromium', [
  '--headless', '--no-sandbox', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
  '--disable-background-networking', `--user-data-dir=${profile}`, '--no-startup-window', '--remote-debugging-pipe',
], {stdio:['ignore','ignore','pipe','pipe','pipe']});
const stopped = new Promise(resolve => browser.once('exit', resolve));
let received = '', nextId = 0, diagnostics = '';
const pending = new Map(), exceptions = [];
function fail(error) {for(const {reject,timer} of pending.values()) {clearTimeout(timer); reject(error);} pending.clear();}
browser.stderr.on('data', bytes => {diagnostics = (diagnostics+bytes).slice(-4000);});
browser.on('error', fail);
browser.on('exit', code => fail(new Error(`Chromium exited ${code}: ${diagnostics}`)));
browser.stdio[4].on('data', bytes => {
  received += bytes; let end;
  while((end=received.indexOf('\0')) !== -1) {
    const message=JSON.parse(received.slice(0,end)); received=received.slice(end+1);
    if(message.method==='Runtime.exceptionThrown') exceptions.push(message.params.exceptionDetails);
    const request=pending.get(message.id); if(!request) continue;
    pending.delete(message.id); clearTimeout(request.timer);
    if(message.error) request.reject(new Error(JSON.stringify(message.error))); else request.resolve(message.result);
  }
});
function send(method, params={}, sessionId) {
  return new Promise((resolve,reject) => {
    const id=++nextId, timer=setTimeout(()=>{pending.delete(id);reject(new Error(`Timed out: ${method}\n${diagnostics}`));},60000);
    pending.set(id,{resolve,reject,timer}); browser.stdio[3].write(JSON.stringify({id,method,params,sessionId})+'\0');
  });
}
try {
  console.log((await send('Browser.getVersion')).product);
  const {targetId}=await send('Target.createTarget',{url:'about:blank'});
  const {sessionId}=await send('Target.attachToTarget',{targetId,flatten:true});
  const command=(method,params)=>send(method,params,sessionId);
  const js=async expression=>{
    const result=await command('Runtime.evaluate',{expression,awaitPromise:true,returnByValue:true});
    assert.equal(result.exceptionDetails,undefined,JSON.stringify(result.exceptionDetails)); return result.result.value;
  };
  await command('Page.enable'); await command('Runtime.enable');
  await command('Emulation.setDeviceMetricsOverride',{width:1100,height:800,deviceScaleFactor:1,mobile:false});
  await command('Page.navigate',{url:`http://127.0.0.1:${server.address().port}/`});
  const deadline=Date.now()+60000;
  while(!await js('globalThis.ready === true')) {
    const failure=await js('globalThis.failure'); assert.ok(!failure,`${failure} (${await js('globalThis.paintStage')})`);
    assert.ok(Date.now()<deadline,`Frame startup timed out: ${await js('globalThis.output')}`);
    await new Promise(resolve=>setTimeout(resolve,100));
  }
  const paintDeadline=Date.now()+30000;
  while(!await js('document.querySelectorAll("canvas").length > 0')) {
    const failure=await js('globalThis.failure'); assert.ok(!failure,`${failure} (${await js('globalThis.paintStage')})`);
    assert.ok(Date.now()<paintDeadline,`Frame did not create a canvas: ${await js('globalThis.output')}`);
    await new Promise(resolve=>setTimeout(resolve,100));
  }
  const displayDeadline=Date.now()+30000;
  const bluePixel = `(() => {
    const canvas=document.querySelector('canvas');
    if(!canvas || canvas.width<40 || canvas.height<40) return false;
    const pixel=canvas.getContext('2d').getImageData(30,30,1,1).data;
    return pixel[2]>200 && pixel[0]<50 && pixel[1]<50 && pixel[3]===255;
  })()`;
  const redPixel = `(() => {
    const c=document.querySelector('canvas'); if(!c) return false;
    const d=c.getContext('2d').getImageData(0,0,c.width,c.height).data;
    for(let i=0;i<d.length;i+=4)
      if(d[i]===255 && d[i+1]===0 && d[i+2]===0 && d[i+3]===255) return true;
    return false;})()`;
  // A queued repaint can clear the canvas while the screenshot is being
  // captured. Observe all drawing checks together in one browser task.
  while(!await js(`(${bluePixel}) && (${redPixel}) && globalThis.paintStage==='painted'`)) {
    const failure=await js('globalThis.failure'); assert.ok(!failure,`${failure} (${await js('globalThis.paintStage')})`);
    assert.ok(!await js('globalThis.frameExited'),`Frame exited unexpectedly: ${await js('globalThis.callbackFailure ? JSON.stringify(globalThis.callbackFailure) : globalThis.output')}`);
    assert.equal(await js(`app.evaluate('(if (miricl-threads:thread-alive-p *browser-smoke-thread*) t (handler-case (progn (miricl-threads:join-thread *browser-smoke-thread*) :normal) (miricl-threads:abnormal-exit (c) (princ-to-string (miricl-threads:abnormal-exit-condition c)))))').output`),'T\n');
    assert.ok(Date.now()<displayDeadline,`Frame did not finish drawing: ${await js(`JSON.stringify({output:globalThis.output,callback:globalThis.callbackFailure,paints:app.evaluate('*browser-smoke-paints*').output,canvases:Array.from(document.querySelectorAll('canvas'),c=>({width:c.width,height:c.height,pixel:Array.from(c.getContext('2d').getImageData(30,30,1,1).data)}))})`)}`);
    await new Promise(resolve=>setTimeout(resolve,250));
  }
  assert.ok(await js('document.querySelectorAll("canvas").length > 0'),'No frame canvas');
  const screenshot=await command('Page.captureScreenshot',{format:'png'});
  await fs.writeFile(new URL('../build/browser-frame.png',import.meta.url),Buffer.from(screenshot.data,'base64'));
  assert.equal(await js(`app.evaluate('(and (plusp *browser-smoke-paints*) (plusp *browser-codec-ticks*))').output`),'T\n');
  const button=await js(`(() => {
    const bounds=document.querySelector('canvas').getBoundingClientRect();
    return {x:bounds.left+bounds.width/2,y:bounds.top+bounds.height-13};
  })()`);
  await command('Input.dispatchMouseEvent',{type:'mouseMoved',...button});
  await command('Input.dispatchMouseEvent',{type:'mousePressed',button:'left',buttons:1,clickCount:1,...button});
  await command('Input.dispatchMouseEvent',{type:'mouseReleased',button:'left',buttons:0,clickCount:1,...button});
  const clickDeadline=Date.now()+30000;
  while(await js(`app.evaluate('*browser-smoke-clicks*').output`) !== '1\n') {
    const failure=await js('globalThis.failure'); assert.ok(!failure,`${failure} (${await js('globalThis.paintStage')})`);
    assert.equal(await js('globalThis.callbackFailure'),undefined,
      JSON.stringify(await js('globalThis.callbackFailure')));
    assert.ok(Date.now()<clickDeadline,`CLIM gadget did not receive the click: ${await js(`app.evaluate('(let* ((port (clim:port *browser-smoke-frame*)) (button (clim:find-pane-named *browser-smoke-frame* (quote action)))) (list (mapcar (function car) (mcclim-browser::incoming-events port)) (mapcar (function car) (mcclim-browser::outgoing-events port)) (class-name (class-of (clim:pointer-sheet (clim:port-pointer port)))) (slot-value button (quote climi::pressedp)) (slot-value button (quote climi::armed))))').output`)}`);
    await new Promise(resolve=>setTimeout(resolve,100));
  }
  const waitFor = async (expression,label) => {
    const deadline=Date.now()+10000;
    while(!await js(expression)) {
      assert.ok(Date.now()<deadline,label);
      assert.ok(!await js('globalThis.failure'),await js('globalThis.failure'));
      assert.ok(!await js('globalThis.callbackFailure'),JSON.stringify(await js('globalThis.callbackFailure')));
      await new Promise(resolve=>setTimeout(resolve,100));
    }
  };
  const drag=async (selector,dx,dy) => {
    const start=await js(`(() => {
      const bounds=document.querySelector(${JSON.stringify(selector)}).getBoundingClientRect();
      return {x:bounds.left+bounds.width/2,y:bounds.top+bounds.height/2};
    })()`);
    await command('Input.dispatchMouseEvent',{type:'mouseMoved',...start});
    await command('Input.dispatchMouseEvent',{type:'mousePressed',button:'left',buttons:1,clickCount:1,...start});
    const end={x:start.x+dx,y:start.y+dy};
    await command('Input.dispatchMouseEvent',{type:'mouseMoved',button:'left',buttons:1,...end});
    await command('Input.dispatchMouseEvent',{type:'mouseReleased',button:'left',buttons:0,clickCount:1,...end});
  };
  await drag('.mcclim-titlebar',80,50);
  await waitFor(`document.querySelector('.mcclim-window').style.left==='96px' &&
    document.querySelector('.mcclim-window').style.top==='66px'`,'Window did not move');
  const movedScreenshot=await command('Page.captureScreenshot',{format:'png'});
  await fs.writeFile(new URL('../build/browser-frame.png',import.meta.url),Buffer.from(movedScreenshot.data,'base64'));
  await js(`document.querySelector('.mcclim-titlebar button').click()`);
  await waitFor(`document.querySelectorAll('.mcclim-window').length===0`,'Window did not close through CLIM');
  assert.deepEqual(exceptions,[]);
  console.log('PASS real McCLIM drawing, gadget input, window movement and close through the Lisp browser backend');
} finally {
  fail(new Error('Browser test ended')); browser.kill(); await stopped;
  await new Promise(resolve=>server.close(resolve));
  await fs.rm(profile,{recursive:true,force:true,maxRetries:5,retryDelay:100});
}
