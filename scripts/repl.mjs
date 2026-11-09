#!/usr/bin/env node
import fs from 'node:fs/promises';
import readline from 'node:readline';
import path from 'node:path';
import {Miricl} from './miricl.mjs';

const options = {devServer: false, tcpBridge: false, entryPoints: [], keep: []};
let image;
let help = false;
let browserRepl = false;
const args = process.argv.slice(2);
const lispString = value => '"' + value.replace(/["\\]/g, '\\$&') + '"';
for (let i = 0; i < args.length; i++) {
  const arg = args[i];
  if (arg === '--browser-repl') { browserRepl = true; continue; }
  if (arg === '--repl-bridge') { options.tcpBridge = true; continue; }
  if (arg === '--dev-server') { options.devServer = true; continue; }
  if (arg === '--native') { options.native = true; continue; }
  if (arg === '--no-strip') { options.noStrip = true; continue; }
  if (arg === '--help' || arg === '-h') { help = true; continue; }
  const name = arg.split('=', 1)[0];
  const key = {'--directory': 'directory', '--bundle': 'bundle', '--optimize': 'optimize',
    '--entry': 'entryPoints', '--keep': 'keep', '--report': 'report', '--output': 'output', '--tcp-port': 'tcpPort',
    '--websocket-port': 'websocketPort', '--http-port': 'httpPort'}[name];
  if (key) {
    const value = arg.includes('=') ? arg.slice(name.length + 1) : args[++i];
    if (value === undefined) throw new Error(`Missing value for ${name}.`);
    if (['entryPoints', 'keep'].includes(key)) options[key].push(value);
    else if (['directory', 'bundle', 'optimize', 'output', 'report'].includes(key)) options[key] = value;
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
if (options.bundle && options.optimize) throw new Error('Choose --bundle or --optimize.');
if ((options.bundle || options.optimize) && (browserRepl || options.devServer || options.tcpBridge))
  throw new Error('Build operations cannot be combined with browser or server modes.');
if (options.output && !(options.bundle || options.optimize)) throw new Error('--output requires --bundle or --optimize.');
if (options.optimize && !options.noStrip && !options.entryPoints.length)
  throw new Error('--optimize requires --entry PACKAGE:FUNCTION or --no-strip.');
if (!options.optimize && (options.entryPoints.length || options.keep.length || options.report || options.noStrip || options.native))
  throw new Error('--entry, --keep, --report, --native and --no-strip require --optimize.');
if (help) {
  console.log(`Usage: node scripts/repl.mjs [miricl.wasm] [options]

  (default)                  Local terminal REPL, without listeners
  --browser-repl             Browser REPL over WebSocket
  --dev-server               Browser REPL and local HTTP server
  --repl-bridge              Browser REPL and TCP listener
  --dev-server --repl-bridge  Browser REPL, HTTP server, and TCP listener

  --bundle SYSTEM            Compile one ASDF system and its dependencies, then exit
  --optimize SYSTEM          Remove unreachable definitions from an ASDF FASL bundle
  --entry PACKAGE:FUNCTION   Production entry point (repeatable)
  --native                   Compile retained application code to WASM
  --no-strip                 Compact all definitions; no entry points required
  --keep PACKAGE:NAME        Retain a dynamically accessed definition (repeatable)
  --report PATH             Explain retained and removed definitions
  --output PATH              Application FASL output (ASDF default if omitted)

  --websocket-port PORT       Browser connection port (4006)
  --tcp-port PORT             TCP REPL port (4005)
  --http-port PORT            HTTP port (8000)
  --directory PATH           Served directory (current directory)`);
} else if (browserRepl || options.devServer || options.tcpBridge) {
  const {startReplBridge} = await import('./repl-bridge.mjs');
  const bridge = await startReplBridge(await fs.readFile(image ?? 'miricl.wasm'), options);
  for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => bridge.close());
} else {
  function report(error) {
    if (error.output) process.stdout.write(error.output);
    process.stderr.write(`Error: ${error.message}\n`);
    if (error.backtrace) process.stderr.write(error.backtrace);
  }
  const {replCompilerImage} = await import('./repl-bridge.mjs');
  const selectedImage = image ?? 'miricl.wasm';
  // Local REPL evaluation honors the selected core image's policy. Build
  // commands and an explicitly selected WASI component can reuse its Node core.
  const compilerImage = options.bundle || options.optimize || path.basename(selectedImage) === 'miricl-repl.wasm'
    ? await replCompilerImage(selectedImage) : undefined;
  const {TerminalDebugger} = await import('./terminal-debugger.mjs');
  const debuggerUI = options.bundle || options.optimize ? undefined : new TerminalDebugger();
  const interpreter = await Miricl.instantiate(compilerImage ?? await fs.readFile(selectedImage), {
    onOutput: output => process.stdout.write(output), onThreadError: report,
    onDebugger: debuggerUI && (event => debuggerUI.handle(event))
  });
  const {HostFilesystem} = await import('./host-filesystem.mjs');
  const filesystem = new HostFilesystem(interpreter, options.directory);
  if (options.bundle || options.optimize) {
    try {
      const system = lispString(options.bundle ?? options.optimize);
      const output = options.output ? ` :output ${lispString(options.output)}` : '';
      const optimization = options.optimize
        ? ` :entry-points '(${options.entryPoints.map(lispString).join(' ')}) :keep '(${options.keep.map(lispString).join(' ')})` +
          (options.noStrip ? ' :strip nil' : '') +
          (options.native ? ' :native t' : '') +
          (options.report ? ` :report ${lispString(options.report)}` : '')
        : '';
      const operation = options.optimize ? 'miricl-compiler:optimize-system' : 'miricl-bundle:dump-system';
      const result = await interpreter.evaluateAsync(`(${operation} ${system}${output}${optimization})`, {eof: true});
      if (result.output) process.stdout.write(result.output);
      await filesystem.publish();
    } catch (error) { report(error); process.exitCode = 1; }
    finally { filesystem.close(); interpreter.close(); }
  } else {
    const terminal = Boolean(process.stdin.isTTY && process.stdout.isTTY);
    const input = readline.createInterface({input: process.stdin, output: process.stdout, terminal});
    let pending = '', quit = false, ended = false, evaluating = false, finished = false;
    const lines = [];
    const commands = new Set();
    let finish;
    const done = new Promise(resolve => {finish = resolve;});
    function prompt() {
      if (ended || finished) return;
      const debugPrompt = debuggerUI.ready ? debuggerUI.prompt() : '';
      if (terminal) { input.setPrompt(debugPrompt || (pending ? '...> ' : 'miricl> ')); input.prompt(); }
      else if (debugPrompt) process.stdout.write(debugPrompt);
    }
    async function submit(source, eof = false) {
      evaluating = true;
      try {
        const result = await interpreter.evaluateAsync(source, {eof});
        if (result.output) process.stdout.write(result.output);
        await filesystem.publish();
        if (!result.incomplete) pending = '';
        if (result.quit) {quit = true; lines.length = 0; input.close();}
      } catch (error) {report(error); pending = ''; if (eof) process.exitCode = 1;}
      finally {evaluating = false; pump(); prompt();}
    }
    function pump() {
      if (finished) return;
      const stop = debuggerUI.active;
      if (stop) {
        const local = /^(sessions?|threads|help|\?)\b/i.test(lines[0] ?? '');
        if ((!debuggerUI.ready && !local) || commands.has(stop.stopId)) return;
        const line = lines.length ? lines.shift() : ended ? 'abort' : undefined;
        if (line === undefined) return;
        commands.add(stop.stopId);
        debuggerUI.command(line).finally(() => {commands.delete(stop.stopId); pump(); prompt();});
        return;
      }
      if (evaluating) return;
      if (lines.length) {pending += lines.shift() + '\n'; void submit(pending); return;}
      if (ended) {
        if (pending && !quit) {const source = pending; pending = ''; void submit(source, true);}
        else {finished = true; finish();}
      }
    }
    debuggerUI.onReady = () => {pump(); prompt();};
    input.on('line', line => {lines.push(line); pump();});
    input.on('close', () => {ended = true; pump();});
    input.on('SIGINT', () => {
      if (debuggerUI.active) {lines.unshift('abort'); pump();}
      else if (pending) {pending = ''; process.stdout.write('\n'); prompt();}
      else input.close();
    });
    try {
      prompt();
      await done;
    } finally { input.close(); filesystem.close(); interpreter.close(); }
  }
}
