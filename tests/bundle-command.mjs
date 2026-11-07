// Both terminal hosts build ordinary ASDF systems without a browser or sources
// copied into the interpreter. The distribution and application are separate.
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {Miricl} from '../scripts/miricl.mjs';
const image = path.resolve(process.argv[2] ?? 'miricl-interpreter.wasm');
const wasi = process.argv[3] && path.resolve(process.argv[3]);
const asdf = path.resolve(process.argv[4] ?? 'miricl-asdf.wasm');
const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-bundle-command-'));
async function run(program, args, input) {
  const child = spawn(program, args, {stdio: ['pipe', 'pipe', 'pipe']});
  let output = '', errors = '';
  child.stdout.on('data', data => { output += data; });
  child.stderr.on('data', data => { errors += data; });
  child.stdin.end(input);
  const timeout = setTimeout(() => child.kill('SIGKILL'), 600000);
  try {
    const code = await new Promise((resolve, reject) => { child.on('error', reject); child.on('exit', resolve); });
    return {code, output, errors};
  } finally { clearTimeout(timeout); }
}
try {
  await fs.copyFile(asdf, path.join(directory, 'miricl-asdf.wasm'));
  await fs.copyFile(new URL('../miricl.js', import.meta.url), path.join(directory, 'miricl.js'));
  await fs.copyFile(image, path.join(directory, 'miricl-interpreter.wasm'));
  if (wasi) await fs.copyFile(wasi, path.join(directory, 'miricl-repl.wasm'));
  await fs.mkdir(path.join(directory, 'tools'));
  await fs.symlink('../miricl.js', path.join(directory, 'tools', 'miricl.js'));
  await fs.writeFile(path.join(directory, 'demo.asd'), `(asdf:defsystem "demo" :serial t :build-pathname "existing.wasm"
    :components ((:file "macros") (:file "application")))`);
  await fs.writeFile(path.join(directory, 'macros.lisp'), `(defpackage :demo (:use :cl) (:export :answer))
    (in-package :demo) (defmacro twice (x) \`(+ ,x ,x))`);
  await fs.writeFile(path.join(directory, 'application.lisp'), `(in-package :demo)
    (defun answer () (twice 21))
    (eval-when (:load-toplevel) (defun browser-start () (js:global "document")))
    (defparameter *deep* '#.(loop with x = nil repeat 600 do (setq x (list x)) finally (return x)))`);
  // ASDF must be able to replace an old host output through its staging file.
  await fs.mkdir(path.join(directory, 'node'));
  await fs.writeFile(path.join(directory, 'existing.wasm'), 'old bundle');
  await fs.writeFile(path.join(directory, 'with-uiop.asd'), `(asdf:defsystem "with-uiop"
    :depends-on ("uiop") :components ((:file "with-uiop")))`);
  await fs.writeFile(path.join(directory, 'with-uiop.lisp'),
    '(defun join-words () (uiop:strcat "portable" " bundle"))');
  for (const host of ['node', ...(wasi ? ['wasi'] : [])]) {
    const target = `${host}/app λ.wasm`;
    const program = host === 'node' ? process.execPath : process.env.WASMTIME ?? 'wasmtime';
    const args = host === 'node'
      ? [path.join(directory, 'tools', 'miricl.js'), path.join(directory, 'miricl-interpreter.wasm'), '--directory', directory]
      : ['run', '-W', 'stack-switching=y', '-S', 'p3=y', '--dir', `${directory}::.`, path.join(directory, 'miricl-repl.wasm')];
    const result = await run(program, [...args, '--bundle', 'demo', '--output', target]);
    assert.equal(result.code, 0, result.output + result.errors);
    const bytes = await fs.readFile(path.join(directory, target));
    const module = await WebAssembly.compile(bytes);
    assert.deepEqual(WebAssembly.Module.imports(module), []);
    assert.equal(WebAssembly.Module.customSections(module, 'miricl.fasl').length, 2);
    assert.deepEqual(await fs.readdir(path.join(directory, host)), ['app λ.wasm']);
    const vm = await Miricl.instantiate(await fs.readFile(image));
    try {
      vm.mountFile('/application.wasm', bytes);
      assert.equal(vm.evaluate('(load "/application.wasm")').output, 'T\n');
      assert.equal(vm.evaluate('(and (null (find-package "ASDF")) (= (demo:answer) 42))').output, 'T\n');
    } finally { vm.close(); }
    const missing = await run(program, [...args, '--bundle', 'missing', '--output', `${host}/missing.wasm`]);
    assert.notEqual(missing.code, 0, missing.output + missing.errors);
    await assert.rejects(fs.access(path.join(directory, host, 'missing.wasm')));
    console.log(`PASS ${host}: browser-free ASDF bundling, separate assets, Unicode paths, fresh LOAD and failure status`);
    const dependency = await run(program, [...args, '--bundle', 'with-uiop', '--output', `${host}/with-uiop.wasm`]);
    assert.equal(dependency.code, 0, dependency.output + dependency.errors);
    const runtime = await Miricl.instantiate(await fs.readFile(image));
    try {
      runtime.mountFile('/with-uiop.wasm', await fs.readFile(path.join(directory, host, 'with-uiop.wasm')));
      const dependencyBytes = await fs.readFile(path.join(directory, host, 'with-uiop.wasm'));
      assert.equal(WebAssembly.Module.customSections(await WebAssembly.compile(dependencyBytes), 'miricl.fasl').length, 1,
        'ASDF/UIOP must not be added automatically, even when referenced by the application.');
      assert.equal(runtime.evaluate('(find-package "ASDF")').output, 'NIL\n');
      runtime.mountFile('/miricl-asdf.wasm', await fs.readFile(asdf));
      await runtime.evaluateAsync('(load "/miricl-asdf.wasm") (load "/with-uiop.wasm")');
      assert.equal(runtime.evaluate('(join-words)').output, '"portable bundle"\n');
    } finally { runtime.close(); }
    console.log(`PASS ${host}: ASDF/UIOP is loaded explicitly by the application host`);
  }
} finally { await fs.rm(directory, {recursive: true, force: true}); }
