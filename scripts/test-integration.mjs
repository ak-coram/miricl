#!/usr/bin/env node
// Shared by CI and just. Separate CI runners parallelize groups; tests within a
// group stay sequential because bridge fixtures bind fixed local ports.
import path from 'node:path';
import {spawn} from 'node:child_process';

const [group = 'all', directory = '.', ...options] = process.argv.slice(2);
const image = name => path.join(directory, name);
const wasm = image('miricl.wasm');
const repl = image('miricl-repl.wasm');
const precompiled = image('miricl-precompiled.wasm');
const asdf = image('build/asdf/asdf.lisp');
const test = (name, ...args) => [`tests/${name}.mjs`, ...args];
// These suites exercise backend behavior and already support the ready image.
// Keep first-use/bootstrap coverage in compiler-runtime, native-compilation,
// native-bundle and the separate cold compiler-input run below.
const readyCompilerTests = new Set([
  'compiler-types', 'compiler-type-flow', 'compiler-safety', 'compiler-inline-types',
  'compiler-inline-writes', 'compiler-modules', 'compiler-throughput', 'native-size', 'hir-optimization', 'compiler-integer-predicates', 'compiler-predicate-reuse', 'compiler-loops', 'compiler-licm',
]);
const groups = {
  runtime: [
    test('interpreter-only', image('miricl-interpreter.wasm'), wasm, asdf),
    test('threads', wasm, repl), test('standalone', wasm),
    ['--experimental-wasm-wasmfx', ...test('text-repl', wasm, repl)],
    test('repl-bridge', wasm, repl), test('repl-websocket', wasm, repl),
    test('dev-filesystem-server', wasm, repl), test('dev-filesystem', wasm), test('mounted-filesystem', wasm),
    test('terminal-debugger', wasm, image('miricl.js')),
    ...['backtrace', 'debugger', 'javascript', 'development', 'integer-tiers', 'printer-integers', 'float-bits', 'derived-types', 'readtable-iteration', 'reader-suppression', 'package-local-nicknames', 'hash-structural']
      .map(name => test(name, wasm)),
    test('ansi-report-check'),
  ],
  compiler: [
    test('hir-interpreter', precompiled, image('miricl-interpreter.wasm')),
    test('compiler-runtime', wasm), test('compiler-runtime', precompiled, '--precompiled'),
    test('compiler-input', precompiled),
    test('evaluation-policy', wasm, precompiled, repl),
    test('core-image', precompiled),
    test('native-arguments', precompiled),
    test('debugger', precompiled),
    ...['compilation', 'hir', 'hir-optimization', 'compiler-loops', 'compiler-predicate-reuse', 'compiler-licm', 'whole-program', 'class-reachability', 'generic-dispatch',
      'native-bundle', 'native-ltv', 'native-compilation', 'compiler-types', 'compiler-type-flow', 'compiler-safety', 'compiler-input', 'compiler-inline-types', 'compiler-inline-writes', 'compiler-integer-predicates', 'compiler-modules', 'tail-calls', 'native-disassembly', 'native-size', 'compiler-throughput',
      'fasl-format', 'fasl-hash-tables', 'fasl-shared-initialization', 'saved-slots', 'file-compilation', 'file-code-sharing', 'nested-backquote',
      'structure-printers', 'ansi-compiler-runner', 'ansi-runner', 'ansi-upstream-runner']
      .map(name => test(name, readyCompilerTests.has(name) ? precompiled : wasm)),
  ],
  asdf: [test('asdf-build'), test('asdf', wasm, asdf), test('asdf-bundle', wasm, asdf),
    test('bundle-command', wasm, repl, image('miricl-asdf.wasm')), test('application-setup')],
  browser: [test('session-sharing'), test('browser-session-sharing', image('miricl.html')),
    test('browser-repl', image('miricl.html')), test('browser-todo', image('todo.html')),
    test('browser-compiler', wasm, image('miricl.js')),
    test('browser-compiler', precompiled, image('miricl.js'), '--precompiled'),
    test('browser-dev-repl', wasm, repl, asdf),
    test('ansi-html'), test('filesystem-archive'), test('size-html')],
};
if (!['all', ...Object.keys(groups)].includes(group) || options.some(option => option !== '--list')) {
  throw new Error('Usage: node scripts/test-integration.mjs [all|runtime|compiler|asdf|browser] [build-directory] [--list]');
}
for (const [name, commands] of Object.entries(groups)) {
  if (group !== 'all' && group !== name) continue;
  for (const args of commands) {
    console.log(`[${name}] node ${args.join(' ')}`);
    if (options.includes('--list')) continue;
    const start = performance.now();
    const code = await new Promise((resolve, reject) => {
      const child = spawn(process.execPath, args, {stdio: 'inherit', timeout: 600_000, killSignal: 'SIGKILL'});
      child.once('error', reject);
      child.once('exit', (status, signal) => {
        if (signal) reject(new Error(`${args.join(' ')} stopped by ${signal}`));
        else resolve(status);
      });
    });
    if (code !== 0) process.exit(code ?? 1);
    console.log(`[${name}] passed in ${((performance.now() - start) / 1000).toFixed(1)}s`);
  }
}
