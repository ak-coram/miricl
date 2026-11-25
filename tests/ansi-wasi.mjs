import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {once} from 'node:events';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import readline from 'node:readline';
import {fileURLToPath} from 'node:url';
import {correctUpstreamTestInput} from './ansi-full.mjs';

const binary = process.argv[2] ?? 'miricl-repl.wasm';
const upstream = fileURLToPath(new URL('./ansi-test/', import.meta.url));
const source = fs.readFileSync(new URL('./ansi-wasi.lisp', import.meta.url));
if (!fs.existsSync(path.join(upstream, 'init.lsp'))) {
  throw new Error('ANSI submodule is missing. Run git submodule update --init tests/ansi-test.');
}

const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'miricl-ansi-wasi-'));
try {
  // Upstream INIT and filesystem tests write within the preopened directory.
  fs.cpSync(upstream, directory, {recursive: true, filter: file => path.basename(file) !== '.git'});
  for (const file of ['environment/encode-universal-time.lsp', 'printer/print-floats.lsp']) {
    const target = path.join(directory, file);
    const original = fs.readFileSync(target, 'utf8');
    const corrected = correctUpstreamTestInput(file, original, console.log);
    if (corrected !== original) fs.writeFileSync(target, corrected);
  }

  let result;
  let currentTest;
  let reported = 0;
  const child = spawn(process.env.WASMTIME ?? 'wasmtime',
    ['run', '-W', 'stack-switching=y', '-S', 'p3=y', '--dir', `${directory}::/ansi`, binary],
    {stdio: ['pipe', 'pipe', 'inherit']});
  child.stdin.on('error', error => {
    if (error.code !== 'EPIPE') child.emit('error', error);
  });
  const exit = once(child, 'close');
  const output = readline.createInterface({input: child.stdout});
  output.on('line', line => {
    const test = line.match(/^ANSI-WASI-TEST (.+)$/u);
    if (test) {
      currentTest = test[1];
      if (++reported % 1000 === 0) console.log(`ANSI / wasi: reported ${reported} test starts.`);
      return;
    }
    if (line) console.log(line);
    const match = line.match(/^ANSI-WASI-RESULT (\d+) (\d+)$/u);
    if (match) result = match.slice(1).map(Number);
  });
  child.stdin.end(source);
  const [status, signal] = await exit;
  const testContext = currentTest ? ` Last test reported: ${currentTest}.` : '';
  assert.equal(status, 0, `Wasmtime exited with ${signal ?? status}.${testContext}`);
  assert.ok(result, `WASI ANSI suite did not finish.${testContext}`);
  const [passed, total] = result;
  console.log(`ANSI full (wasi): ${passed} passed, ${total - passed} failed (${total} tests).`);
  assert.ok(total > 0, 'The ANSI suite registered no tests.');
  assert.equal(passed, total, 'The WASI ANSI suite has failures.');
} finally {
  fs.rmSync(directory, {recursive: true, force: true});
}
