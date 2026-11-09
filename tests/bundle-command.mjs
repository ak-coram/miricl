// Both terminal hosts build ordinary ASDF systems without a browser or sources
// copied into the interpreter. The distribution and application are separate.
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {Miricl} from '../scripts/miricl.mjs';
const image = path.resolve(process.argv[2] ?? 'miricl.wasm');
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
  const libraryVM = await Miricl.instantiate(await fs.readFile(image));
  try {
    libraryVM.mountFile('/miricl-asdf.wasm', await fs.readFile(asdf));
    assert.equal(libraryVM.evaluate(`(load "/miricl-asdf.wasm")
      (and (equal (asdf:asdf-version) "3.3.7")
        (eq (uiop:implementation-type) :miricl)
        (eq miricl-compiler::*compiler-state* :cold))`).output,
      'T\nT\n', 'The compact ASDF artifact must load without compiler startup.');
  } finally {libraryVM.close();}
  await fs.copyFile(asdf, path.join(directory, 'miricl-asdf.wasm'));
  await fs.copyFile(new URL('../miricl.js', import.meta.url), path.join(directory, 'miricl.js'));
  await fs.copyFile(image, path.join(directory, 'miricl.wasm'));
  await fs.copyFile(wasi ?? path.join(path.dirname(image), 'miricl-repl.wasm'), path.join(directory, 'miricl-repl.wasm'));
  await fs.mkdir(path.join(directory, 'tools'));
  await fs.symlink('../miricl.js', path.join(directory, 'tools', 'miricl.js'));
  await fs.writeFile(path.join(directory, 'demo.asd'), `(asdf:defsystem "demo" :serial t :build-pathname "existing.wasm"
    :components ((:file "macros") (:file "application"))
    :perform (asdf:monolithic-compile-bundle-op :after (operation system)
      (declare (ignore operation system))
      (assert (find "demo.asd" (directory "*.asd") :test #'equal :key #'file-namestring))
      (assert (find "probe.lisp" (directory "named/*.lisp") :test #'equal :key #'file-namestring))))`);
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
  await fs.mkdir(path.join(directory, 'named'));
  await fs.writeFile(path.join(directory, 'named/probe.lisp'), '; Directory lookup through an exact named preopen.');
  for (const host of ['node', ...(wasi ? ['wasi'] : [])]) {
    const target = `${host}/app λ.wasm`;
    const program = host === 'node' ? process.execPath : process.env.WASMTIME ?? 'wasmtime';
    const args = host === 'node'
      ? [path.join(directory, 'tools', 'miricl.js'), path.join(directory, 'miricl.wasm'), '--directory', directory]
      : ['run', '-W', 'stack-switching=y', '-S', 'p3=y', '--dir', `${directory}::.`,
          '--dir', `${directory}/named::named`, path.join(directory, 'miricl-repl.wasm')];
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
    const optimized = await run(program, [...args, '--optimize', 'demo', '--entry', 'DEMO:ANSWER',
      '--output', `${host}/optimized.wasm`, '--report', `${host}/retention.txt`]);
    assert.equal(optimized.code, 0, optimized.output + optimized.errors);
    const optimizedBytes = await fs.readFile(path.join(directory, host, 'optimized.wasm'));
    const optimizedVM = await Miricl.instantiate(await fs.readFile(image));
    try {
      optimizedVM.mountFile('/optimized.wasm', optimizedBytes);
      assert.equal(optimizedVM.evaluate('(load "/optimized.wasm")').output, 'T\n');
      assert.equal(optimizedVM.evaluate(`(and (= (demo:answer) 42)
        (not (fboundp 'demo::browser-start)) (not (boundp 'demo::*deep*))
        (null (macro-function 'demo::twice)))`).output, 'T\n');
      assert.ok(optimizedBytes.length < bytes.length);
      assert.match(await fs.readFile(path.join(directory, host, 'retention.txt'), 'utf8'), /DROP :FUNCTION \("BROWSER-START"/);
    } finally { optimizedVM.close(); }
    console.log(`PASS ${host}: whole-program entry points, dead definitions and retention report`);
    const native = await run(program, [...args, '--optimize', 'demo', '--native', '--entry', 'DEMO:ANSWER',
      '--output', `${host}/native.wasm`]);
    assert.equal(native.code, 0, native.output + native.errors);
    const nativeVM = await Miricl.instantiate(await fs.readFile(image));
    try {
      nativeVM.mountFile('/native.wasm', await fs.readFile(path.join(directory, host, 'native.wasm')));
      nativeVM.evaluate('(load "/native.wasm")');
      assert.equal(nativeVM.evaluate(`(and (= (demo:answer) 42)
        (eq miricl-compiler::*compiler-state* :cold)
        (plusp (miricl-compiler::compiler-primitive 26 #'demo:answer)))`).output, 'T\n');
    } finally {nativeVM.close();}
    console.log(`PASS ${host}: --native loads compiled application code without compiler startup`);
    const unstripped = await run(program, [...args, '--optimize', 'demo', '--no-strip',
      '--output', `${host}/unstripped.wasm`, '--report', `${host}/unstripped.txt`]);
    assert.equal(unstripped.code, 0, unstripped.output + unstripped.errors);
    const unstrippedVM = await Miricl.instantiate(await fs.readFile(image));
    try {
      unstrippedVM.mountFile('/unstripped.wasm', await fs.readFile(path.join(directory, host, 'unstripped.wasm')));
      assert.equal(unstrippedVM.evaluate('(load "/unstripped.wasm")').output, 'T\n');
      assert.equal(unstrippedVM.evaluate(`(and (= (demo:answer) 42) (fboundp 'demo::browser-start)
        (boundp 'demo::*deep*) (macro-function 'demo::twice) t)`).output, 'T\n');
      assert.doesNotMatch(await fs.readFile(path.join(directory, host, 'unstripped.txt'), 'utf8'), /^DROP /m);
    } finally { unstrippedVM.close(); }
    const invalid = await run(program, [...args, '--bundle', 'demo', '--no-strip']);
    assert.notEqual(invalid.code, 0, '--no-strip requires --optimize');
    const noEntry = await run(program, [...args, '--optimize', 'demo']);
    assert.notEqual(noEntry.code, 0, 'Stripping requires explicit entry points');
    console.log(`PASS ${host}: --no-strip retains unused definitions, macros and constants without entry points`);
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
