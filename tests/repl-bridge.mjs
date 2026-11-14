// node tests/repl-bridge.mjs miricl.wasm [miricl-repl.wasm]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import net from 'node:net';
import path from 'node:path';
import {Miricl} from '../scripts/miricl.mjs';
import {connectTerminal, createWebapp, startHost, testPorts, waitFor} from './repl-bridge-fixture.mjs';

const binary = path.resolve(process.argv[2] ?? 'miricl.wasm');
const wasi = process.argv[3] && path.resolve(process.argv[3]);
const browserImage = path.resolve(process.argv[4] ?? path.join(path.dirname(binary), 'miricl.wasm'));
const bytes = await fs.readFile(browserImage);
const [relay] = WebAssembly.Module.customSections(await WebAssembly.compile(bytes), 'miricl.repl-bridge');
assert.ok(relay, 'The runtime must provide its terminal relay.');
assert.equal(WebAssembly.Module.imports(await WebAssembly.compile(relay)).some(imported => imported.name === 'compile'), false,
  'The relay must not import a host compiler.');
const devOptions = {url: `ws://127.0.0.1:${testPorts.websocket}/repl`};
const httpOrigin = `http://127.0.0.1:${testPorts.http}`;
const production = await Miricl.instantiate(bytes);
assert.equal(production.dev, undefined);
production.close();
const directory = await createWebapp(browserImage);
const large = Buffer.alloc(2 * 1024 * 1024 + 19, 97);
await fs.writeFile(path.join(directory, 'large.bin'), large);
await fs.writeFile(path.join(directory, 'λ.txt'), 'Unicode path 🙂');
await fs.mkdir(path.join(directory, 'nested'));
await fs.writeFile(path.join(directory, 'nested/index.html'), '<p>nested</p>');
await fs.symlink(path.resolve('README.org'), path.join(directory, 'outside.txt'));

try {
  for (const [runtime, image] of [['node', binary], ...(wasi ? [['wasmtime', wasi]] : [])]) {
    for (const mode of ['browser-repl', 'dev-server', 'repl-bridge']) {
      const offline = await startHost(runtime, image, mode, directory);
      try {
        offline.child.stdin.write('\":quit\"\n:quitter\n; :quit\n');
        await waitFor(() => offline.output.includes('[No browser connected; input discarded]'), 'offline ordinary input');
        assert.equal(offline.child.exitCode, null);
        const start = offline.output.length;
        offline.child.stdin.write(' \t:Qu');
        await waitFor(() => offline.output.length > start, 'fragmented offline command');
        offline.child.stdin.write('It \t; stop the host\r\n');
        await offline.waitForExit();
        console.log(`PASS ${runtime} ${mode}: terminal :quit without a browser, including fragmented input`);
      } finally { await offline.close(); }
    }
    const offlineTCP = await startHost(runtime, image, 'repl-bridge');
    let offlineTerminal;
    try {
      offlineTerminal = await connectTerminal();
      await waitFor(() => offlineTerminal.output.includes('Waiting for the browser interpreter.'), 'offline TCP status');
      offlineTerminal.socket.write(':quit\n');
      await offlineTCP.waitForExit();
      await waitFor(() => offlineTerminal.ended, 'offline TCP quit disconnects');
      console.log(`PASS ${runtime} TCP :quit without a browser`);
    } finally { offlineTerminal?.socket.destroy(); await offlineTCP.close(); }
    const standalone = await startHost(runtime, image, 'browser-repl');
    let standaloneVM;
    try {
      await assert.rejects(new Promise((resolve, reject) => {
        const socket = net.connect(testPorts.tcp, '127.0.0.1');
        socket.once('connect', () => { socket.destroy(); resolve(); });
        socket.once('error', reject);
      }));
      await assert.rejects(fetch(`${httpOrigin}/`, {signal: AbortSignal.timeout(500)}));
      standaloneVM = await Miricl.instantiate(bytes, {dev: devOptions});
      await standaloneVM.evaluateAsync('(defparameter *standalone-app* 41)');
      await waitFor(() => standaloneVM.dev.state === 'connected', 'standalone browser attachment');
      const start = standalone.output.length;
      standalone.child.stdin.write('(1+ *standalone-app*)\n');
      await waitFor(() => standalone.output.slice(start).includes('42\nmiricl> '), 'standalone browser/terminal REPL');
      standalone.child.stdin.write('\":quit\"\n');
      await waitFor(() => standalone.output.includes('\":quit\"\nmiricl> '), ':quit inside a string is ordinary input');
      standalone.child.stdin.write('(format t "before quit~%")\n:qu');
      await waitFor(() => standalone.output.includes('before quit\nNIL\nmiricl> '), 'output before quit');
      standalone.child.stdin.write('it\n');
      await standalone.waitForExit();
      assert.equal(standaloneVM.dev.state, 'closed');
      assert.equal((await standaloneVM.evaluateAsync('*standalone-app*')).output, '41\n');
      console.log(`PASS ${runtime} standalone browser REPL without HTTP or TCP listeners`);
      console.log(`PASS ${runtime} terminal :quit exits the host and preserves the webapp interpreter`);
    } finally { standaloneVM?.close(); await standalone.close(); }
    let vm, terminal;
    const host = await startHost(runtime, image, 'repl-bridge');
    try {
      await assert.rejects(fetch(`${httpOrigin}/`, {signal: AbortSignal.timeout(500)}));
      terminal = await connectTerminal();
      await waitFor(() => terminal.output.includes('Waiting for the browser interpreter.'), 'TCP waiting status');
      const statuses = [];
      vm = await Miricl.instantiate(bytes, {dev: {...devOptions, reconnectDelay: 25, onStatus: state => statuses.push(state)}});
      await vm.evaluateAsync('(defparameter *app-count* 41)');
      await waitFor(() => vm.dev.state === 'connected', 'automatic application attachment');
      await waitFor(() => host.output.includes('[Browser connected]'), 'terminal connected status');
      await terminal.ready();
      assert.match(terminal.output, /\[Browser connected\]\nmiricl> /);
      assert.equal(await terminal.send('(incf *app-count*)'), '42\nmiricl> ');
      await waitFor(() => host.output.includes('(incf *app-count*)\n42\nmiricl> '), 'ordinary terminal transcript');
      assert.doesNotMatch(host.output, /\[Emacs\]|\[Remote\]/);
      assert.equal((await vm.evaluateAsync('*app-count*')).output, '42\n');
      assert.equal(vm.vm['compiler-services'], undefined, 'The bridge must not install a compiler service.');
      assert.equal(await terminal.send('(setq miricl-compiler:*compile-evaluations* nil)'), 'NIL\nmiricl> ');
      assert.equal(await terminal.send('(defun remote-interpreted () 42)'), 'REMOTE-INTERPRETED\nmiricl> ');
      assert.equal(await terminal.send('(not (null (function-lambda-expression #\'remote-interpreted)))'), 'T\nmiricl> ');
      assert.equal(await terminal.send('(setq miricl-compiler:*compile-evaluations* t)'), 'T\nmiricl> ');
      assert.equal(await terminal.send('(defun remote-add (x)', '...> '), '...> ');
      assert.equal(await terminal.send('  (+ x *app-count*))'), 'REMOTE-ADD\nmiricl> ');
      assert.equal(await terminal.send('(remote-add 1)'), '43\nmiricl> ');
      const compiled = source => terminal.send(source.replace(/\s+/gu, ' '));
      assert.equal(await compiled(`(and (compiled-function-p #'remote-add)
        (null (function-lambda-expression #'remote-add)))`), 'T\nmiricl> ');
      assert.equal(await compiled('miricl-compiler::*compiler-origin*'), ':IMAGE\nmiricl> ',
        'The browser uses its bootstrapped local compiler.');
      vm.evaluate(`(defparameter *target-literal* (list :original))
        (defmacro target-literal () (list 'quote *target-literal*))
        (defparameter *target-js* (js:global "Math"))
        (defmacro target-js () (list 'quote *target-js*))
        (defparameter *target-load-values* 0)
        (defparameter *target-special* :outer)`);
      assert.equal(await compiled('(eq (target-literal) *target-literal*)'), 'T\nmiricl> ');
      vm.evaluate('(progn (setf (cdr *target-literal*) *target-literal*) (values))');
      assert.equal(await compiled('(eq (target-literal) (cdr *target-literal*))'), 'T\nmiricl> ');
      vm.evaluate('(progn (setf (cdr *target-literal*) nil) (values))');
      assert.equal(await compiled(`(eq (load-time-value
        (progn (incf *target-load-values*) *target-literal*)) *target-literal*)`), 'T\nmiricl> ');
      assert.equal(await compiled('*target-load-values*'), '1\nmiricl> ');
      assert.equal(await compiled('(progv nil nil (+ 20 22))'), '42\nmiricl> ');
      assert.match(await compiled('(let () (incf *target-load-values*) (go missing-tag))'), /Error:.*No lexical tag/);
      assert.equal(await compiled('*target-load-values*'), '1\nmiricl> ', 'Rejected compilation must not execute the form.');
      assert.equal(await compiled('(= (js:from-js (js:invoke (target-js) "sqrt" 81)) 9)'), 'T\nmiricl> ');
      assert.equal(await compiled(`(let ((*target-special* :inner))
        (flet ((read-special () *target-special*)) (read-special)))`), ':INNER\nmiricl> ');
      assert.equal(await compiled(`(defun remote-return (x) "Remote documentation."
        (declare (ignore x)) (return-from remote-return (values 10 20)))`), 'REMOTE-RETURN\nmiricl> ');
      assert.equal(await compiled('(remote-return nil)'), '10\n20\nmiricl> ');
      assert.equal(await compiled(`(block remote-exit
        (funcall (lambda () (return-from remote-exit (values 11 22)))))`), '11\n22\nmiricl> ');
      assert.equal(await compiled(`(equal (documentation 'remote-return 'function)
        "Remote documentation.")`), 'T\nmiricl> ');
      assert.equal(await compiled(`(progn (defparameter *remote-closure*
        (let ((x 40)) (lambda () (incf x)))) (funcall *remote-closure*))`), '41\nmiricl> ');
      assert.equal(await compiled('(funcall *remote-closure*)'), '42\nmiricl> ');
      assert.equal(await compiled('(null (function-lambda-expression *remote-closure*))'), 'T\nmiricl> ');
      assert.equal(await compiled(`(funcall (miricl-compiler:compile-native
        '(lambda () (target-literal))))`), '(:ORIGINAL)\nmiricl> ');
      assert.equal(await compiled('miricl-compiler::*compiler-state*'), ':READY\nmiricl> ');
      assert.equal(await compiled('(values 17 23)'), '17\n23\nmiricl> ');
      assert.equal(await compiled(`(and (= * 17) (equal / '(17 23))
        (equal + '(values 17 23)))`), 'T\nmiricl> ');
      console.log(`PASS ${runtime} forwarding-only bridge, browser evaluation policy, local native functions, macros/literals/JS objects, dynamic state and multiple values`);
      assert.equal(await terminal.send('(values 1 2 nil)'), '1\n2\nNIL\nmiricl> ');
      assert.equal(await terminal.send('(format t "hello~%")'), 'hello\nNIL\nmiricl> ');
      const start = terminal.output.length;
      // Split every UTF-8 byte, independently of WebSocket and TCP boundaries.
      for (const byte of Buffer.from('"λ🙂"\n')) terminal.socket.write(Buffer.from([byte]));
      await waitFor(() => terminal.output.length > start && terminal.output.endsWith('miricl> '), 'UTF-8 input');
      assert.equal(terminal.output.slice(start), '"λ🙂"\nmiricl> ');
      const error = await terminal.send('(/ 1 0)');
      assert.match(error, /Error: Division by zero/);
      assert.match(error, /COMMON-LISP:\//);
      assert.equal(await terminal.send('(+ 3 4)'), '7\nmiricl> ');
      const localStart = host.output.length;
      host.child.stdin.write('(remote-add 2)\n');
      await waitFor(() => host.output.slice(localStart).includes('44\nmiricl> '), 'terminal input evaluated in browser');
      assert.equal((await vm.evaluateAsync('*app-count*')).output, '42\n');
      const disconnected = statuses.filter(state => state === 'disconnected').length;
      await terminal.send('(+ 1', '...> ');
      terminal.socket.destroy();
      await waitFor(() => statuses.filter(state => state === 'disconnected').length > disconnected && vm.dev.state === 'connected', 'reconnection');
      terminal = await connectTerminal(); await terminal.ready();
      assert.match(terminal.output, /\[Browser connected\]\nmiricl> /);
      assert.equal(await terminal.send('*app-count*'), '42\nmiricl> ');
      const beforeClose = terminal.output.length;
      // Local compilation must keep working independently of the relay.
      const pendingCompilation = vm.evaluateAsync(`(funcall
        (miricl-compiler:compile-native '(lambda () 42)))`);
      vm.dev.close();
      assert.equal((await pendingCompilation).output, '42\n');
      assert.equal(vm.vm['compiler-services'], undefined);
      assert.equal((await vm.evaluateAsync('(+ 1 2)')).output, '3\n');
      vm.close(); vm = undefined;
      await waitFor(() => terminal.output.slice(beforeClose).includes('[Browser disconnected; waiting for a browser]'), 'TCP disconnected status');
      assert.equal(terminal.ended, false, 'TCP must survive a browser disconnect.');
      await waitFor(() => host.output.includes('[Browser disconnected; waiting for a browser]'), 'terminal disconnected status');
      terminal.socket.write('(defparameter *offline-command* 99)\n');
      await waitFor(() => terminal.output.includes('[No browser connected; input discarded]'), 'TCP offline input notice');
      vm = await Miricl.instantiate(bytes, {dev: devOptions});
      await waitFor(() => vm.dev.state === 'connected', 'replacement browser attachment');
      await waitFor(() => terminal.output.slice(beforeClose).includes('[Browser connected]\nmiricl> '), 'TCP reconnected status');
      assert.equal(await terminal.send("(boundp '*offline-command*)"), 'NIL\nmiricl> ');
      assert.equal(await terminal.send('(+ 20 22)'), '42\nmiricl> ');
      console.log(`PASS ${runtime} browser-backed terminal and TCP REPL`);
      console.log(`PASS ${runtime} TCP waiting/connected/disconnected status and reconnection on the same connection`);
      const beforeDisconnect = host.output.length;
      vm.close(); vm = undefined;
      await waitFor(() => host.output.slice(beforeDisconnect).includes('[Browser disconnected; waiting for a browser]'),
        'browser disconnect before offline quit');
      host.child.stdin.write(':quit\n');
      await host.waitForExit();
      console.log(`PASS ${runtime} terminal :quit after the browser disconnects`);
    } finally { terminal?.socket.destroy(); vm?.close(); await host.close(); }

    const server = await startHost(runtime, image, 'dev-server', directory);
    try {
      await assert.rejects(new Promise((resolve, reject) => {
        const socket = net.connect(testPorts.tcp, '127.0.0.1');
        socket.once('connect', () => { socket.destroy(); resolve(); });
        socket.once('error', reject);
      }));
      const get = (url, options = {}) => fetch(`${httpOrigin}${url}`, {signal: AbortSignal.timeout(5000), ...options});
      let response = await get('/');
      assert.equal(response.status, 200);
      assert.match(response.headers.get('content-type'), /^text\/html/);
      assert.match(await response.text(), /User webapp/);
      response = await get('/scripts/miricl.mjs');
      assert.match(response.headers.get('content-type'), /javascript/); await response.arrayBuffer();
      response = await get('/miricl.wasm');
      assert.equal(response.headers.get('content-type'), 'application/wasm');
      assert.deepEqual(Buffer.from(await response.arrayBuffer()), bytes);
      response = await get('/large.bin');
      assert.deepEqual(Buffer.from(await response.arrayBuffer()), large);
      response = await get('/%CE%BB.txt?fresh=1'); assert.equal(await response.text(), 'Unicode path 🙂');
      response = await get('/nested/'); assert.equal(await response.text(), '<p>nested</p>');
      response = await get('/', {method: 'HEAD'}); assert.equal(response.status, 200); assert.equal(await response.text(), '');
      response = await get('/outside.txt');
      assert.equal(response.status, runtime === 'node' ? 200 : 404); await response.arrayBuffer();
      for (const url of ['/missing', '/%2e%2e%2foutside.txt']) {
        response = await get(url); assert.equal(response.status, 404, url); await response.arrayBuffer();
      }
      response = await get('/', {method: 'POST'}); assert.equal(response.status, 405); await response.arrayBuffer();
      response = await get('/%FF'); assert.equal(response.status, 400); await response.arrayBuffer();
      await fs.writeFile(path.join(directory, 'changed.txt'), 'new content');
      response = await get('/changed.txt'); assert.equal(await response.text(), 'new content');
      console.log(`PASS ${runtime} dev-server static files, MIME types, streaming, and path bounds`);
    } finally { await server.close(); }

    const combined = await startHost(runtime, image, 'dev-server+repl-bridge', directory,
      {websocket: 4606, tcp: 4605, http: 8600});
    let customVM, customTerminal;
    try {
      customVM = await Miricl.instantiate(bytes, {dev: {url: 'ws://127.0.0.1:4606/repl'}});
      await waitFor(() => customVM.dev.state === 'connected', 'custom WebSocket port');
      customTerminal = await connectTerminal(4605); await customTerminal.ready();
      assert.equal(await customTerminal.send('(+ 20 22)'), '42\nmiricl> ');
      const response = await fetch('http://127.0.0.1:8600/', {signal: AbortSignal.timeout(5000)});
      assert.equal(response.status, 200); await response.arrayBuffer();
      assert.match(combined.output, /ws:\/\/127\.0\.0\.1:4606\/repl/);
      assert.match(combined.output, /TCP REPL: 127\.0\.0\.1:4605/);
      assert.match(combined.output, /http:\/\/127\.0\.0\.1:8600\//);
      customTerminal.socket.write('(format t "TCP output before quit~%")\n:quit\n');
      await combined.waitForExit();
      await waitFor(() => customTerminal.ended, 'TCP quit disconnects the client');
      assert.match(customTerminal.output, /TCP output before quit\nNIL\nmiricl> /);
      assert.match(combined.output, /TCP output before quit\nNIL\nmiricl> /);
      assert.equal(customVM.dev.state, 'closed');
      assert.equal((await customVM.evaluateAsync('(+ 20 22)')).output, '42\n');
      console.log(`PASS ${runtime} combined HTTP/TCP mode with configurable ports`);
      console.log(`PASS ${runtime} TCP :quit flushes output and stops the host`);
    } finally { customTerminal?.socket.destroy(); customVM?.close(); await combined.close(); }
  }
} finally { await fs.rm(directory, {recursive: true, force: true}); }
