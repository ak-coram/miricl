// Build one production FASL using upstream ASDF and load it without ASDF.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl, MiriclError} from '../scripts/miricl.mjs';

const binary = fs.readFileSync(process.argv[2] ?? 'miricl.wasm');
const asdf = fs.readFileSync(process.argv[3] ?? 'build/asdf/asdf.lisp', 'utf8');
const vm = await Miricl.instantiate(binary);
const machines = [vm];
const fresh = async () => {
  const machine = await Miricl.instantiate(binary);
  machines.push(machine);
  return machine;
};
const check = async (machine, source, expected = 'T') =>
  assert.equal((await machine.evaluateAsync(source, {eof: true})).output, expected + '\n', source);
const unitCount = async bytes => {
  const module = await WebAssembly.compile(bytes);
  assert.deepEqual(WebAssembly.Module.imports(module), []);
  return WebAssembly.Module.customSections(module, 'miricl.fasl').length;
};

try {
  // Export uses byte codes, so embedded NULs, quotes and all high bytes survive.
  const raw = Uint8Array.from({length: 256}, (_, i) => i);
  const unusual = '/build/quote" and \\ λ.bin';
  vm.mountFile(unusual, raw);
  assert.deepEqual(await vm.readFile(unusual), raw);
  await assert.rejects(vm.readFile('/missing.bin'), MiriclError);
  await assert.rejects(vm.readFile(3), TypeError);
  vm.mountFile('/asdf.lisp', asdf);
  await check(vm, '(load "/asdf.lisp")');
  const sources = [
    ['shared/shared.asd', '(asdf:defsystem "shared" :components ((:file "shared")))'],
    ['shared/shared.lisp', `(defpackage :shared (:use :cl) (:export :twice :*events*))
      (in-package :shared) (defvar *events* nil) (push :shared *events*)
      (defmacro twice (x) \`(+ ,x ,x))`],
    ['left/left.asd', '(asdf:defsystem "left" :depends-on ("shared") :components ((:file "left")))'],
    ['left/left.lisp', `(defpackage :left (:use :cl :shared) (:export :answer))
      (in-package :left) (push :left *events*) (defun answer () (twice 10))`],
    ['right/right.asd', '(asdf:defsystem "right" :depends-on ("shared") :components ((:file "right")))'],
    ['right/right.lisp', `(defpackage :right (:use :cl :shared) (:export :answer))
      (in-package :right) (push :right *events*) (defun answer () (twice 11))`],
    ['app/app.asd', `(asdf:defsystem "app" :depends-on ("left" "right") :serial t
      :components ((:file "package") (:file "state") (:file "main") (:file "empty"))
      :build-operation "monolithic-compile-bundle-op" :build-pathname #P"dist/app.wasm")`],
    ['app/package.lisp', '(defpackage :app (:use :cl) (:export :start))'],
    ['app/state.lisp', `(in-package :app)
      (defvar *loads* 0) (incf *loads*)
      (push :app shared:*events*)
      (defparameter *literal* '#1=("λ𝄞" . #1#))
      (defparameter *large* "${'λ'.repeat(40000)}")
      (defparameter *once* (load-time-value (incf *loads*)))
      (defparameter *path* (list *load-pathname* *load-truename*))
      (setf *readtable* (copy-readtable nil))
      (set-macro-character #\\! (lambda (s c) (declare (ignore s c)) (error "Leaked reader.")))`],
    ['app/main.lisp', `(defparameter app::*unit-package* (package-name *package*))
      (defparameter app::*unit-reader* (get-macro-character #\\!))
      (defun app:start () (+ (left:answer) (right:answer)))`],
    ['app/empty.lisp', ''],
  ];
  for (const [name, source] of sources) vm.mountFile('/src/' + name, source);
  await check(vm, `(progn
    (asdf:initialize-source-registry '(:source-registry (:tree "/src/") :ignore-inherited-configuration))
    (asdf:initialize-output-translations '(:output-translations (t "/cache/") :ignore-inherited-configuration))
    (asdf:make "app"))`);
  const output = '/src/app/dist/app.wasm';
  const bundle = await vm.readFile(output);
  assert.equal(await unitCount(bundle), 7, 'Shared dependency must occur once in a diamond.');
  const production = await fresh();
  production.mountFile('/release/app.wasm', bundle);
  await check(production, '(and (null (find-package :asdf)) (null (probe-file "/src/app/app.asd")))');
  // Caller reader/printer choices must not affect FASL parsing or symbol identity.
  await check(production, `(let ((*read-base* 16) (*read-eval* nil) (*readtable* (copy-readtable nil)))
    (set-macro-character #\\( (lambda (s c) (declare (ignore s c)) (error "Caller reader.")))
    (load "/release/app.wasm" :external-format :ignored))`);
  await check(production, `(and (= (app:start) 42) (compiled-function-p #'app:start)
    (= app::*loads* 2) (= app::*once* 2)
    (equal (reverse shared:*events*) '(:shared :left :right :app))
    (eq app::*literal* (cdr app::*literal*)) (equal (car app::*literal*) "λ𝄞")
    (= (length app::*large*) 40000) (every (lambda (c) (char= c #\\λ)) app::*large*)
    (equal app::*unit-package* "COMMON-LISP-USER") (null app::*unit-reader*)
    (every (lambda (p) (equal (namestring p) "/release/app.wasm")) app::*path*)
    (null *load-pathname*) (null *load-truename*)
    (null (find-package :asdf)))`);

  // A cached make leaves the artifact alone; a changed dependency rebuilds it.
  await check(vm, '(asdf:make "app")');
  assert.deepEqual(await vm.readFile(output), bundle);
  vm.mountFile('/src/left/left.lisp', sources.find(([name]) => name === 'left/left.lisp')[1].replace('(twice 10)', '(twice 20)'));
  await check(vm, '(asdf:make "app" :force t)');
  const rebuilt = await fresh();
  rebuilt.mountFile('/app.wasm', await vm.readFile(output));
  await check(rebuilt, '(load "/app.wasm")');
  await check(rebuilt, '(= (app:start) 62)');

  // Non-monolithic bundles exclude dependencies; nested bundles flatten.
  await check(vm, `(progn (asdf:operate 'asdf:compile-bundle-op "left")
    (asdf:operate 'asdf:compile-bundle-op "right") t)`);
  await check(vm, `(progn (asdf:operate 'asdf:load-bundle-op "left")
    (asdf:operate 'asdf:monolithic-load-bundle-op "app") (= (app:start) 62))`);
  const bundlePath = system => `(first (asdf:output-files 'asdf:compile-bundle-op (asdf:find-system "${system}")))`;
  const left = (await vm.evaluateAsync(`(namestring ${bundlePath('left')})`)).output.trim().slice(1, -1);
  assert.equal(await unitCount(await vm.readFile(left)), 1);
  await check(vm, `(progn
    (asdf:operate 'asdf:compile-bundle-op "shared")
    (uiop:combine-fasls (list ${bundlePath('shared')} ${bundlePath('left')} ${bundlePath('right')}) "/nested.wasm")
    (uiop:combine-fasls '("/nested.wasm") "/flattened.wasm")
    (uiop:combine-fasls nil "/empty.wasm") t)`);
  const nested = await fresh();
  const flattened = await vm.readFile('/flattened.wasm');
  assert.equal(await unitCount(flattened), 3);
  nested.mountFile('/nested.wasm', flattened);
  await check(nested, '(load "/nested.wasm")');
  await check(nested, '(= (+ (left:answer) (right:answer)) 62)');
  const empty = await vm.readFile('/empty.wasm');
  assert.equal(await unitCount(empty), 0);
  nested.mountFile('/empty.wasm', empty);
  await check(nested, '(load "/empty.wasm")');

  // A single empty unit isolates bad framing/magic tests from application code.
  vm.mountFile('/unit.lisp', '');
  await check(vm, '(pathnamep (compile-file "/unit.lisp"))');
  const unit = await vm.readFile('/unit.wasm');
  const invalid = [
    unit.subarray(0, unit.length - 1),
    Uint8Array.from([...unit, 0]),
    Uint8Array.from([...unit.subarray(0, 8), 0, 0xff, 0xff, 0xff, 0xff, 0x10]),
    Uint8Array.from(unit, (byte, i) => i === 8 ? 1 : byte),
    Uint8Array.from(unit, (byte, i) => i === 11 ? 120 : byte),
  ];
  // Corrupt the format magic while keeping valid Wasm framing.
  const badMagic = unit.slice();
  assert.deepEqual([...badMagic.subarray(26, 30)], [77, 73, 82, 70]);
  badMagic[26] = 0;
  invalid.push(badMagic);
  for (const [i, bytes] of invalid.entries()) {
    nested.mountFile(`/invalid-${i}.wasm`, bytes);
    await assert.rejects(nested.evaluateAsync(`(load "/invalid-${i}.wasm")`), MiriclError);
  }
  vm.mountFile('/truncated.wasm', invalid[0]);
  await check(vm, `(handler-case (uiop:combine-fasls '("/truncated.wasm") "/bad.wasm") (error () t))`);
  await check(vm, '(null (probe-file "/bad.wasm"))');
  // Failed replacement restores an existing artifact rather than publishing a prefix.
  await check(vm, `(handler-case (uiop:combine-fasls '("/nested.wasm" "/truncated.wasm") "/flattened.wasm") (error () t))`);
  assert.deepEqual(await vm.readFile('/flattened.wasm'), flattened);
  console.log('ASDF bundles: dependency closure/order, fresh production loading, unit isolation, large Unicode constants, rebuilds, nested/empty bundles, and invalid artifacts pass.');
} finally {
  for (const machine of machines) machine.close();
}
