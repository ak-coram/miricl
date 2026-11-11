// Load the patched upstream library, compile dependencies, and reuse the FASLs.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const bytes = fs.readFileSync(process.argv[2] ?? 'miricl.wasm');
const asdf = fs.readFileSync(process.argv[3] ?? 'build/asdf/asdf.lisp', 'utf8');
const version = asdf.match(/^;;; This is ASDF ([\d.]+):/m)?.[1];
assert.ok(version, 'The assembled source must identify its upstream ASDF version.');
const vm = await Miricl.instantiate(bytes);
const fresh = await Miricl.instantiate(bytes);
const hostService = vm.hostService.bind(vm);
let date = 4000000000;
vm.hostService = (operation, argument) => operation === 0 ? date : hostService(operation, argument);
const check = async (source, expected = 'T', machine = vm) =>
  assert.equal((await machine.evaluateAsync(source, {eof: true})).output, `${expected}\n`, source);
try {
  await check('(null (find-package :asdf))');
  vm.mountFile('asdf.lisp', asdf);
  await check('(load "asdf.lisp")');
  await check('(list (asdf:asdf-version) (uiop:implementation-type) (namestring (uiop:getcwd)))',
    `(${JSON.stringify(version)} :MIRICL "/")`);
  await check(`(and (member :asdf3 *features*) (member :asdf-unicode *features*)
    (not (member :sbcl *features*)) (null (uiop:getenv "HOME"))
    (null (uiop:raw-command-line-arguments)))`);
  await check(`(and (uiop:os-unix-p) (not (uiop:os-windows-p))
    (let ((*features* '(:miricl :windows)))
      (and (uiop:os-unix-p) (not (uiop:os-windows-p))))
    (let ((*features* '(:linux))) (uiop:os-unix-p)))`);
  const sources = [
    ['app/dependency/dependency.asd', `(asdf:defsystem "dependency" :serial t
      :components ((:file "package") (:file "macro")))`],
    ['app/dependency/package.lisp', '(defpackage :dependency (:use :cl) (:export :twice))'],
    ['app/dependency/macro.lisp', '(in-package :dependency) (defmacro twice (x) `(+ ,x ,x))'],
    ['app/app.asd', `(asdf:defsystem "app" :depends-on ("dependency") :serial t
      :components ((:file "package") (:file "hello")))`],
    ['app/package.lisp', '(defpackage :app (:use :cl :dependency) (:export :hello))'],
    ['app/hello.lisp', `(in-package :app) (defvar *loads* 0) (incf *loads*)
      (defun hello () (twice 21))`],
  ];
  for (const [name, source] of sources) vm.mountFile(name, source);
  await check(`(progn
    (asdf:initialize-source-registry '(:source-registry (:tree "/app/") :ignore-inherited-configuration))
    (asdf:initialize-output-translations '(:output-translations (t "/compiled/") :ignore-inherited-configuration))
    (asdf:load-system "app"))`);
  await check('(list (app:hello) (compiled-function-p #\'app:hello) app::*loads*)', '(42 T 1)');
  await check('(asdf:load-system "app")');
  await check('app::*loads*', '1');
  await check('(uiop:with-current-directory (#P"/app/") (namestring (uiop:getcwd)))', '"/app/"');
  await check('(namestring (uiop:getcwd))', '"/"');
  await check(`(handler-case (asdf:find-system "missing-system")
    (asdf:missing-component () t))`);

  // A changed source timestamp triggers an incremental rebuild without forcing.
  date++;
  vm.mountFile('app/hello.lisp', `(in-package :app) (defvar *loads* 0) (incf *loads*)
    (defun hello () (twice 22))`);
  await check('(asdf:load-system "app")');
  await check('(list (app:hello) app::*loads*)', '(44 2)');
  await check('(asdf:load-system "app" :force t)');
  await check('(list (app:hello) app::*loads*)', '(44 3)');

  // The resulting compiled files load in a base interpreter without ASDF.
  const components = [['dependency', 'package'], ['dependency', 'macro'], ['app', 'package'], ['app', 'hello']];
  for (const [system, component] of components) {
    const pathname = `(first (asdf:output-files 'asdf:compile-op (asdf:find-component "${system}" "${component}")))`;
    const output = (await vm.evaluateAsync(`(with-open-file (s ${pathname} :element-type '(unsigned-byte 8))
      (let ((bytes (make-array (file-length s) :element-type '(unsigned-byte 8))))
        (read-sequence bytes s) bytes))`, {eof: true})).output;
    assert.match(output, /^#\(/);
    const binary = Uint8Array.from(output.match(/\d+/g).map(Number));
    const module = await WebAssembly.compile(binary);
    assert.equal(WebAssembly.Module.customSections(module, 'miricl.fasl').length, 1);
    const name = `${system}-${component}.wasm`;
    fresh.mountFile(name, binary);
    await check(`(load "${name}")`, 'T', fresh);
  }
  await check('(and (null (find-package :asdf)) (= (app:hello) 44) (compiled-function-p #\'app:hello))', 'T', fresh);
  console.log('Upstream ASDF: bootstrap, recursive discovery, dependencies, compiled macros, caching, rebuilds, and FASL reuse pass.');
} finally {
  vm.close();
  fresh.close();
}
