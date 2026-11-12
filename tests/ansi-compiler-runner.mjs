import assert from 'node:assert/strict';
import fs from 'node:fs';
import {runFull} from './ansi-full.mjs';
import {runUpstream} from './ansi-upstream.mjs';
import {Miricl} from '../scripts/miricl.mjs';
import {installCompiler} from './ansi-compiler.mjs';

const binary = process.argv[2] ?? 'miricl-precompiled.wasm';
const forms = [
  '(defpackage :cl-test (:use :cl :rt))',
  '(defparameter cl-test::*compiler-fallback-probe* 0)',
  '(rt:deftest cl-test::native-pass (+ 20 22) 42)',
  '(rt:deftest cl-test::wrong-value (+ 1 1) 3)',
  '(rt:deftest cl-test::invalid-exit (progn (incf cl-test::*compiler-fallback-probe*) (go missing-tag)) 1)',
  '(rt:deftest cl-test::no-fallback cl-test::*compiler-fallback-probe* 0)',
  '(rt:deftest cl-test::native-closure (flet ((twice (x) (+ x x))) (twice 21)) 42)',
];
const sources = ['rt-package.lsp', 'rt.lsp'].map(file => [file,
  fs.readFileSync(new URL(`./ansi-test/${file}`, import.meta.url), 'utf8')]);
const records = forms.map((source, index) => ({file: 'compiler-fixture.lsp', package: 'CL-USER', source,
  tests: index < 2 ? [] : [`CL-TEST::${['NATIVE-PASS', 'WRONG-VALUE', 'INVALID-EXIT', 'NO-FALLBACK', 'NATIVE-CLOSURE'][index - 2]}`]}));
// Upstream setup can use COMPILE before the harness installs its test hook.
const upstreamSources = [...sources, ['init.lsp', '(load "rt-package.lsp") (load "rt.lsp") (load "compiler-fixture.lsp") (compile nil \'(lambda () nil))'],
  ['compiler-fixture.lsp', forms.join('\n')]];
for (const [name, run, options] of [['diagnostic', runFull, {records, sources}],
  ['upstream', runUpstream, {sources: upstreamSources, expectedTests: 5}]]) {
  const report = await run({binary, execution: 'compiler', timeout: 2000, ...options});
  assert.equal(report.error, null, report.error);
  assert.equal(report.execution, 'compiler');
  assert.equal(report.compilerBootstrap.mode, 'self-compiled');
  assert.equal(report.compilerBootstrap.origin, 'image');
  assert.ok(report.compilerBootstrap.elapsedMs >= 0);
  assert.equal(report.summary.complete, true);
  assert.deepEqual(report.tests.map(test => test.status), ['pass', 'fail', 'fail', 'pass', 'pass'],
    JSON.stringify(report.tests, null, 2));
  assert.equal(report.tests[2].phase, 'compile');
  assert.equal(report.tests[1].phase, 'execute');
  assert.deepEqual(report.tests[2].compilation, {attempted: 1, compiled: 0, failed: 1});
  assert.ok(report.tests.filter(test => test.status === 'pass').every(test => test.compilation.compiled === 1));
  assert.equal(report.summary.failed, 2, 'Rejected compilation must count as a failure.');
  console.log(`ANSI compiler ${name}: self-compiled frontend/backend, native bodies, result failures, rejected compilation and no interpreter fallback pass.`);
}
// Reject invalid compiler state before RT hooks are installed, without a cold bootstrap.
const vm = await Miricl.instantiate(fs.readFileSync(binary));
try {
  vm.evaluate(`(setq miricl-compiler:*compile-evaluations* nil)
    (setf (symbol-function 'miricl-compiler::hir-form) (lambda (form) form))`);
  assert.throws(() => installCompiler(vm), /left an interpreted compiler function/);
  vm.evaluate('(setq miricl-compiler::*compiler-state* :failed)');
  assert.throws(() => installCompiler(vm), /Compiler bootstrap previously failed/);
  assert.equal(vm.evaluate('(find-package :miricl-ansi-compiler)').output, 'NIL\n');
  console.log('ANSI compiler rejects interpreted kernels and failed compiler state before installing the RT hook.');
} finally {vm.close();}
