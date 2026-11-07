import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import net from 'node:net';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';
import {setTimeout as delay} from 'node:timers/promises';

// Allow local checks alongside an application's running development server.
const portOffset = Number(process.env.MIRICL_TEST_PORT_OFFSET ?? 0);
export const testPorts = {tcp: 4005 + portOffset, websocket: 4006 + portOffset, http: 8000 + portOffset};

export async function waitFor(predicate, description, timeout = 10000) {
  const deadline = Date.now() + timeout;
  while (Date.now() < deadline) {
    if (await predicate()) return;
    await delay(20);
  }
  throw new Error(`Timed out: ${description}`);
}

export async function startHost(runtime, binary, mode, directory = '.', ports = {}) {
  ports = {...testPorts, ...ports};
  const modes = mode.split('+');
  const flags = modes.filter(mode => mode !== 'default').map(mode => `--${mode}`);
  for (const [name, port] of Object.entries(ports)) flags.push(`--${name}-port=${port}`);
  const args = runtime === 'node'
    ? ['scripts/repl.mjs', binary, ...flags, `--directory=${directory}`]
    : ['run', '-W', 'stack-switching=y', '-S', 'p3=y', '-S', 'inherit-network=y',
      ...(modes.includes('dev-server') ? ['--dir', `${directory}::.`] : []), binary, ...flags];
  const child = spawn(runtime === 'node' ? process.execPath : process.env.WASMTIME ?? 'wasmtime', args,
    {stdio: ['pipe', 'pipe', 'pipe']});
  let output = '', diagnostics = '', ended = false;
  const exited = new Promise(resolve => child.on('exit', (code, signal) => {
    ended = true; resolve({code, signal});
  }));
  child.on('error', error => { diagnostics += error.message; ended = true; });
  child.stdout.on('data', bytes => { output += bytes; });
  child.stderr.on('data', bytes => { diagnostics += bytes; });
  const host = {child, get output() { return output; }, get diagnostics() { return diagnostics; },
    async waitForExit() {
      await waitFor(() => ended, `${runtime} exits after :quit`);
      const result = await exited;
      assert.equal(result.code, 0, diagnostics);
      assert.equal(result.signal, null);
    },
    async close() {
      if (!ended) child.kill('SIGTERM');
      const timer = setTimeout(() => child.kill('SIGKILL'), 3000);
      const result = await exited; clearTimeout(timer);
      assert.notEqual(result.signal, 'SIGKILL', 'The server should shut down promptly.');
      assert.doesNotMatch(diagnostics, /wasm trap|CompileError|TypeError|RuntimeError|uncaughtException/, diagnostics);
    }};
  try {
    await waitFor(() => {
      if (ended) throw new Error(`${runtime} exited: ${diagnostics}`);
      return output.includes('Waiting for the browser interpreter.');
    }, `${runtime} ${mode} startup`);
    return host;
  } catch (error) { await host.close(); throw error; }
}

export async function connectTerminal(port = testPorts.tcp) {
  const socket = net.connect(port, '127.0.0.1');
  let output = '', ended = false, failure;
  socket.setEncoding('utf8');
  socket.on('data', bytes => { output += bytes; });
  socket.on('close', () => { ended = true; });
  socket.on('error', error => { failure = error; });
  const terminal = {socket, get output() { return output; }, get ended() { return ended; },
    async ready() {
      await waitFor(() => {
        if (failure) throw failure;
        return output.endsWith('miricl> ');
      }, 'remote REPL prompt');
    },
    async send(source, prompt = 'miricl> ') {
      const start = output.length;
      socket.write(source + '\n');
      await waitFor(() => {
        if (failure) throw failure;
        return output.length > start && output.endsWith(prompt);
      }, `response to ${source.slice(0, 80)}`);
      return output.slice(start);
    }};
  return terminal;
}

export async function createWebapp(binary) {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-webapp-'));
  await fs.mkdir(path.join(directory, 'scripts'));
  await fs.copyFile(binary, path.join(directory, 'miricl-interpreter.wasm'));
  for (const file of ['miricl.mjs', 'dev-repl.mjs', 'dev-filesystem.mjs'])
    await fs.copyFile(path.join('scripts', file), path.join(directory, 'scripts', file));
  await fs.writeFile(path.join(directory, 'index.html'), `<!doctype html><meta charset="utf-8">
<title>User webapp</title><button id="counter">Increment</button><output id="value">41</output>
<script type="module">
import {Miricl} from './scripts/miricl.mjs';
globalThis.webapp = {count: 41};
globalThis.app = await Miricl.instantiate(await (await fetch('./miricl-interpreter.wasm')).arrayBuffer(), {
  imports: {webapp}, dev: new URL(location).searchParams.has('dev')
    ? {url: ${JSON.stringify(`ws://127.0.0.1:${testPorts.websocket}/repl`)},
       filesystem: !new URL(location).searchParams.has('no-filesystem')} : false
});
await app.evaluateAsync('(defparameter *app-count* 41)');
document.getElementById('counter').onclick = async () => {
  const result = await app.evaluateAsync('(incf *app-count*)');
  document.getElementById('value').value = result.output.trim();
};
globalThis.ready = true;
</script>`);
  return directory;
}
