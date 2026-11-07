#!/usr/bin/env node
import fs from 'node:fs/promises';
import readline from 'node:readline';
import {Miricl} from './miricl.mjs';

const options = {devServer: false, tcpBridge: false};
let image;
let help = false;
let browserRepl = false;
const args = process.argv.slice(2);
for (let i = 0; i < args.length; i++) {
  const arg = args[i];
  if (arg === '--browser-repl') { browserRepl = true; continue; }
  if (arg === '--repl-bridge') { options.tcpBridge = true; continue; }
  if (arg === '--dev-server') { options.devServer = true; continue; }
  if (arg === '--help' || arg === '-h') { help = true; continue; }
  const name = arg.split('=', 1)[0];
  const key = {'--directory': 'directory', '--tcp-port': 'tcpPort',
    '--websocket-port': 'websocketPort', '--http-port': 'httpPort'}[name];
  if (key) {
    const value = arg.includes('=') ? arg.slice(name.length + 1) : args[++i];
    if (value === undefined) throw new Error(`Missing value for ${name}.`);
    if (key === 'directory') options[key] = value;
    else {
      if (!/^\d{1,5}$/.test(value) || Number(value) < 1 || Number(value) > 65535)
        throw new RangeError(`${name} must be an integer from 1 to 65535.`);
      options[key] = Number(value);
    }
    continue;
  }
  if (arg.startsWith('-')) throw new Error(`Unknown option: ${arg}`);
  if (image !== undefined) throw new Error(`Unexpected argument: ${arg}`);
  image = arg;
}
if (help) {
  console.log(`Usage: node scripts/repl.mjs [interpreter.wasm] [options]

  (default)                  Local terminal REPL, without listeners
  --browser-repl             Browser REPL over WebSocket
  --dev-server               Browser REPL and local HTTP server
  --repl-bridge              Browser REPL and TCP listener
  --dev-server --repl-bridge  Browser REPL, HTTP server, and TCP listener

  --websocket-port PORT       Browser connection port (4006)
  --tcp-port PORT             TCP REPL port (4005)
  --http-port PORT            HTTP port (8000)
  --directory PATH           Served directory (current directory)`);
} else if (browserRepl || options.devServer || options.tcpBridge) {
  const {startReplBridge} = await import('./repl-bridge.mjs');
  const bridge = await startReplBridge(await fs.readFile(image ?? 'interpreter.wasm'), options);
  for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => bridge.close());
} else {
  const interpreter = await Miricl.instantiate(await fs.readFile(image ?? 'interpreter.wasm'), {
    onOutput: output => process.stdout.write(output), onThreadError: report
  });
  const terminal = Boolean(process.stdin.isTTY && process.stdout.isTTY);
  const input = readline.createInterface({input: process.stdin, output: process.stdout, terminal});
  let pending = '', quit = false;
  function prompt() {
    if (terminal) { input.setPrompt(pending ? '...> ' : 'miricl> '); input.prompt(); }
  }
  function report(error) {
    if (error.output) process.stdout.write(error.output);
    process.stderr.write(`Error: ${error.message}\n`);
    if (error.backtrace) process.stderr.write(error.backtrace);
  }
  input.on('SIGINT', () => {
    if (pending) { pending = ''; process.stdout.write('\n'); prompt(); }
    else input.close();
  });
  try {
    prompt();
    for await (const line of input) {
      pending += `${line}\n`;
      try {
        const result = await interpreter.evaluateAsync(pending);
        if (result.output) process.stdout.write(result.output);
        if (!result.incomplete) pending = '';
        if (result.quit) { quit = true; input.close(); break; }
      } catch (error) { report(error); pending = ''; }
      prompt();
    }
    if (pending && !quit) {
      try { process.stdout.write((await interpreter.evaluateAsync(pending, {eof: true})).output); }
      catch (error) { report(error); process.exitCode = 1; }
    }
  } finally { input.close(); interpreter.close(); }
}
