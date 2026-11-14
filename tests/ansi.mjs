#!/usr/bin/env node
// Run the unmodified upstream RT harness inside the standalone interpreter.
// node tests/ansi.mjs [image.wasm] [--compiler] [--helpers=cons] [upstream-file.lsp ...]
import fs from 'node:fs';
import path from 'node:path';
import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';
import {Miricl} from '../scripts/miricl.mjs';
import {installCompiler, compiledTest, compilerCounters, compilerPhase, summarizeCompilation} from './ansi-compiler.mjs';

const args = process.argv.slice(2);
const binary = args[0] && !args[0].startsWith('--') ? args.shift()
  : args.includes('--compiler') ? 'miricl.wasm' : 'miricl-interpreter.wasm';
if (args.includes('--full')) {
  const {main} = await import('./ansi-full.mjs');
  await main(binary, args.filter(arg => arg !== '--full'));
  process.exit(process.exitCode ?? 0);
}
const root = fileURLToPath(new URL('./ansi-test/', import.meta.url));
const helpers = args.includes('--helpers=cons') ? 'cons' : null;
const compiler = args.includes('--compiler');
const files = args.filter(arg => !['--helpers=cons', '--compiler'].includes(arg));
if (!files.length) files.push(
  'eval-and-compile/declaim.lsp',
  'eval-and-compile/ignore.lsp',
  'eval-and-compile/ignorable.lsp',
);
const reportPath = process.env.MIRICL_ANSI_REPORT ?? 'ansi-results.json';
fs.mkdirSync(path.dirname(reportPath), {recursive: true});
const report = {binary, files, execution: compiler ? 'compiler' : 'interpreter',
  ...(helpers ? {helpers} : {}), tests: [], error: null};
let vm;
const string = value => `"${value.replace(/["\\]/g, '\\$&')}"`;
try {
  if (!fs.existsSync(path.join(root, 'rt.lsp'))) {
    throw new Error('ANSI submodule is missing. Run git submodule update --init tests/ansi-test.');
  }
  const module = await WebAssembly.compile(fs.readFileSync(binary));
  assert.deepEqual(WebAssembly.Module.imports(module), []);
  vm = new Miricl(await WebAssembly.instantiate(module));
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  const evaluate = source => {
    const result = vm.evaluate(source, {eof: true});
    assert.equal(result.quit, false, source);
    return result.output;
  };
  // Mount files verbatim. Lisp reading, macros, RT registration, evaluation,
  // multiple-value comparison, and error handling all run in WASM.
  for (const file of ['rt-package.lsp', 'rt.lsp', ...files]) {
    const resolved = path.resolve(root, file);
    if (!resolved.startsWith(root)) throw new Error(`Path outside ANSI submodule: ${file}`);
    vm.mountFile(file, fs.readFileSync(resolved, 'utf8'));
  }
  evaluate('(load "rt-package.lsp") (load "rt.lsp")');
  evaluate('(defpackage :cl-test (:use :cl :rt)) (in-package :cl-test)');
  if (helpers === 'cons') {
    // Load upstream definitions verbatim, without the array/character
    // initialization needed by the full ANSI bootstrap. Tests requiring that
    // initialization still run and are reported as failures.
    const auxiliary = fs.readFileSync(path.join(root, 'auxiliary/ansi-aux.lsp'), 'utf8');
    const between = (start, end) => {
      const first = auxiliary.indexOf(start);
      const last = end === undefined ? auxiliary.length : auxiliary.indexOf(end, first);
      assert.ok(first >= 0 && last > first, `Missing upstream helper boundary: ${start}`);
      return auxiliary.slice(first, last);
    };
    for (const [start, end] of [
      ['(declaim ', '(defparameter +standard-chars+'],
      ['(defmacro defharmless ', '(defun rational-safely '],
      ['(defun typef ', undefined],
    ]) evaluate(between(start, end));
    vm.mountFile('auxiliary/cons-aux.lsp', fs.readFileSync(path.join(root, 'auxiliary/cons-aux.lsp'), 'utf8'));
    evaluate('(load "auxiliary/cons-aux.lsp")');
  }
  for (const file of files) evaluate(`(load ${string(file)})`);
  if (compiler) report.compilerBootstrap = installCompiler(vm);
  const pending = evaluate('(progn (format t "~{~S~%~}" (rt:pending-tests)) (values))').trim();
  const names = pending ? pending.split('\n') : [];
  assert.ok(names.length, 'No ANSI tests were registered.');
  // DO-TEST uses RT's own interpreter mode and comparison. Run individually
  // to retain a result for each test even if reporting a failure signals.
  for (const name of names) {
    let entry;
    try {
      const source = `(if (rt:do-test '${name}) :pass :fail)`;
      const output = evaluate(compiler ? compiledTest(source) : source);
      entry = {name, passed: output.endsWith(':PASS\n'), output};
    } catch (error) {
      entry = {name, passed: false, error: error.message, code: error.code, output: error.output ?? ''};
    }
    if (compiler) {
      entry.compilation = compilerCounters(vm);
      entry.phase = compilerPhase(entry.compilation);
      if (entry.passed && entry.compilation.compiled < 1) {
        entry.passed = false; entry.error = 'Test passed without native compilation.';
      }
    }
    report.tests.push(entry);
    console.log(`${entry.passed ? 'PASS' : 'FAIL'} ${name}`);
    if (!entry.passed) process.stdout.write(`${entry.output}${entry.error ?? ''}\n`);
  }
} catch (error) {
  report.error = error.stack ?? String(error);
  console.error(report.error);
  if (error.output) process.stderr.write(error.output);
} finally {
  vm?.close();
  const passed = report.tests.filter(test => test.passed).length;
  const failed = report.tests.length - passed;
  report.summary = {passed, failed, total: report.tests.length};
  summarizeCompilation(report);
  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`);
  console.log(`ANSI standalone (${report.execution}): ${passed} passed, ${failed} failed (${report.tests.length} tests).`);
  process.exitCode = report.error || failed || !report.tests.length ? 1 : 0;
}
