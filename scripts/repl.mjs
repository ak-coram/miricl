#!/usr/bin/env node
import fs from 'node:fs/promises';
import readline from 'node:readline';
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
if (!options.optimize && (options.entryPoints.length || options.keep.length || options.report || options.noStrip))
  throw new Error('--entry, --keep, --report and --no-strip require --optimize.');
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
  --no-strip                 Compact all definitions; no entry points required
  --keep PACKAGE:NAME        Retain a dynamically accessed definition (repeatable)
  --report PATH             Explain retained and removed definitions
  --output PATH              Application FASL output (ASDF default if omitted)

  --websocket-port PORT       Browser connection port (4006)
  --tcp-port PORT             TCP REPL port (4005)
  --http-port PORT            HTTP port (8000)
  --directory PATH           Served directory (current directory)`);
} else if (browserRepl || options.devServer || options.tcpBridge) {
  const {startReplBridge, replCompilerImage} = await import('./repl-bridge.mjs');
  options.compilerImage = await replCompilerImage(image ?? 'miricl.wasm');
  const bridge = await startReplBridge(await fs.readFile(image ?? 'miricl.wasm'), options);
  for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => bridge.close());
} else {
  function report(error) {
    if (error.output) process.stdout.write(error.output);
    process.stderr.write(`Error: ${error.message}\n`);
    if (error.backtrace) process.stderr.write(error.backtrace);
  }
  const {replCompilerImage} = await import('./repl-bridge.mjs');
  const interpreter = await Miricl.instantiate(await replCompilerImage(image ?? 'miricl.wasm') ??
    await fs.readFile(image ?? 'miricl.wasm'), {
    onOutput: output => process.stdout.write(output), onThreadError: report
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
    let pending = '', quit = false;
    function prompt() {
      if (terminal) { input.setPrompt(pending ? '...> ' : 'miricl> '); input.prompt(); }
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
          await filesystem.publish();
          if (!result.incomplete) pending = '';
          if (result.quit) { quit = true; input.close(); break; }
        } catch (error) { report(error); pending = ''; }
        prompt();
      }
      if (pending && !quit) {
        try { process.stdout.write((await interpreter.evaluateAsync(pending, {eof: true})).output); await filesystem.publish(); }
        catch (error) { report(error); process.exitCode = 1; }
      }
    } finally { input.close(); filesystem.close(); interpreter.close(); }
  }
}
