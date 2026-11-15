// Runtime import encoding must preserve real helpers, tags and saved entries.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
import {isRuntimeImport} from '../scripts/native-imports.mjs';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const word = number => {
  const result = [];
  do {const byte = number & 127; number = Math.floor(number / 128); result.push(byte | (number ? 128 : 0));} while (number);
  return result;
};
const text = string => {const bytes = [...new TextEncoder().encode(string)]; return [...word(bytes.length), ...bytes];};
const section = (kind, bytes) => [kind, ...word(bytes.length), ...bytes];
const header = [0,97,115,109,1,0,0,0];
const instances = [], OriginalInstance = WebAssembly.Instance;
WebAssembly.Instance = new Proxy(OriginalInstance, {construct(target, args) {
  const instance = Reflect.construct(target, args);
  if (WebAssembly.Module.imports(args[0]).some(isRuntimeImport)) instances.push({instance, module: args[0], imports: args[1]});
  return instance;
}});
let vm;
try {
  vm = await createCompilerTestVM(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
  const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
  const readCode = async bytes => {
    vm.mountFile('/import-code.wasm', bytes);
    vm.evaluate(`(with-open-file (in "/import-code.wasm" :element-type '(unsigned-byte 8))
      (defparameter *import-code* (make-array (file-length in) :element-type '(unsigned-byte 8)))
      (read-sequence *import-code* in)) (values)`);
  };
  const fixtures = [
    ["(lambda (x) (values (+ x 1) :extra))", "'(42 :extra)"],
    ["(lambda (x) (block done (funcall (lambda () (return-from done (values x :escaped)))) :bad))", "'(41 :escaped)"],
  ];
  let sawTag = false;
  for (const [source, expected] of fixtures) {
    vm.evaluate(`(multiple-value-bind (bytes literals)
        (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))
      (defparameter *import-literals* literals)
      (with-open-file (out "/emitted-imports.wasm" :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values)`);
    const compact = await vm.readFile('/emitted-imports.wasm');
    const imports = WebAssembly.Module.imports(await WebAssembly.compile(compact));
    assert.ok(imports.length > 0 && imports.every(entry => entry.module === 'm' && !entry.name.startsWith('native-')));
    sawTag ||= imports.some(entry => entry.kind === 'tag' && entry.name === 'transfer');
    await readCode(compact);
    vm.evaluate(`(defparameter *import-function* (miricl-compiler::compiler-primitive 17 *import-code* *import-literals* nil)) (values)`);
    check(`(equal (multiple-value-list (funcall *import-function* 41)) ${expected})`);
  }
  assert.ok(sawTag, 'A lexical nonlocal exit exercises the real transfer tag');
  vm.evaluate(`(in-package :miricl-compiler)
    (let* ((units (loop for i below 12 collect (compile-to-hir (list 'lambda '(x) (list '+ 'x i)))))
           (roots (mapcar (lambda (unit) (first (hir-unit-functions unit))) units))
           (unit (make-hir-unit :functions (mapcan (lambda (unit) (copy-list (hir-unit-functions unit))) units))))
      (multiple-value-bind (bytes literals) (emit-native-module unit :entries roots)
        (defparameter cl-user::*import-entry-literals* literals)
        (with-open-file (out "/import-entries.wasm" :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8)) (write-sequence bytes out))))
    (in-package :cl-user) (values)`);
  const compact = await vm.readFile('/import-entries.wasm');
  assert.equal(WebAssembly.Module.customSections(await WebAssembly.compile(compact), 'miricl.entries').length, 1);
  await readCode(compact);
  vm.evaluate(`(defparameter *import-entries*
    (loop for literals in *import-entry-literals* for index from 0 collect
      (miricl-compiler::compiler-primitive 17 (cons *import-code* index) literals nil))) (values)`);
  check('(loop for function in *import-entries* for i from 0 always (= (funcall function 30) (+ 30 i)))');
  // Only actual owned runtime exports may be linked from the compact namespace.
  for (const [namespace, name] of [
    ...['constructor', 'toString', '__proto__', 'hasOwnProperty'].map(name => ['m', name]),
    ['unsupported.runtime', 'primary'],
  ]) {
    const bytes = Uint8Array.from([...header,
      ...section(1, [1,0x60,2,0x6f,0x6f,1,0x6f]),
      ...section(2, [1,...text(namespace),...text(name),0,0]),
      ...section(7, [1,...text('entry'),0,0])]);
    assert.ok(WebAssembly.validate(bytes));
    await readCode(bytes);
    check(`(handler-case (progn (miricl-compiler::compiler-primitive 17 *import-code* #() nil) nil) (error () t))`);
    check('(= (funcall (first *import-entries*) 41) 41)');
  }
  const saved = vm.saveImage();
  const restored = await Miricl.instantiate(saved);
  try {
    assert.equal(restored.evaluate(`(and
      (equal (multiple-value-list (funcall *import-function* 41)) '(41 :escaped))
      (loop for function in *import-entries* for i from 0 always (= (funcall function 50) (+ 50 i))))`).output, 'T\n');
  } finally {restored.close();}
  const original = instances.filter(record => record.imports.m?.transfer === vm.vm['native-transfer']);
  assert.ok(original.length > 1);
  for (const record of original) {
    assert.equal(record.imports, original[0].imports, 'Reuse one import map per runtime instance');
    assert.equal(Object.getPrototypeOf(record.imports), null);
    assert.equal(Object.getPrototypeOf(record.imports.m), null);
    for (const entry of WebAssembly.Module.imports(record.module))
      if (entry.module === 'm') assert.equal(record.imports.m[entry.name], vm.vm[`native-${entry.name}`]);
    assert.equal(record.imports.m.transfer, vm.vm['native-transfer']);
  }
  console.log('Compact imports: real function/tag identity, entry tables, snapshots and prototype-field rejection/recovery pass.');
} finally {vm?.close(); WebAssembly.Instance = OriginalInstance;}
