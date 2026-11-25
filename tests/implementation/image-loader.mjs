// Distribution images carry their matching JavaScript adapter, including the
// compiler-free image. The generic launcher executes it only when requested.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {gzipSync} from 'node:zlib';
import {loadMiricl, getMiriclAdapter, extractComponentCore} from '../../scripts/miricl-loader.mjs';
import {Miricl as SuppliedMiricl} from '../../scripts/miricl.mjs';

const image = process.argv[2] ?? 'miricl.wasm';
const scripting = path.join(path.dirname(image), 'miricl-interpreter.wasm');
const sectionName = 'miricl.adapter.gz';
const compilerSection = 'miricl.node-compiler';
const bridgeSection = 'miricl.repl-bridge';
const word = value => {
  const bytes = [];
  do {const byte = value & 127; value >>>= 7; bytes.push(byte | (value ? 128 : 0));} while (value);
  return bytes;
};
const section = (bytes, name, payload) => {
  const text = new TextEncoder().encode(name), body = Uint8Array.from([...word(text.length), ...text, ...payload]);
  const prefix = Uint8Array.from([0, ...word(body.length)]), output = new Uint8Array(bytes.length + prefix.length + body.length);
  output.set(bytes); output.set(prefix, bytes.length); output.set(body, bytes.length + prefix.length);
  return output;
};
// Exercise the current image graph boundary independently of the outer Wasm
// validator. A runtime-owned layout supplies an immutable reference field.
const malformedImmutableImage = (vm, bytes, reference) => {
  const roots = Object.keys(vm.vm).find(name => name.startsWith('core-roots:')).split(':')[1];
  const type = Object.keys(vm.vm).map(name => name.split(':')).find(([kind, , signature]) =>
    kind === 'core-read' && signature?.includes('r') && !signature.includes('e'));
  assert.ok(type, 'The runtime exposes an immutable reference layout.');
  const slot = type[2].indexOf('r');
  const count = new Uint8Array(4); new DataView(count.buffer).setUint32(0, 1, true);
  // References are relative to object 1: 2 encodes itself; 6 encodes object 2.
  return section(bytes, 'miricl.core', Uint8Array.from([
    ...word(2), ...Array.from(roots, () => 0), ...count,
    ...word(0), ...word(Number(type[1])), ...word(2 ** slot), ...word(reference),
  ]));
};
const header = Uint8Array.from([0,97,115,109,1,0,0,0]);
const fake = gzipSync('export const Miricl = {instantiate() {return {};}}');
await assert.rejects(getMiriclAdapter(header), /embedded MIRICL JavaScript adapter/u);
await assert.rejects(getMiriclAdapter(section(section(header, sectionName, fake), sectionName, fake)), /one embedded/u);
await assert.rejects(getMiriclAdapter(section(header, sectionName, Uint8Array.of(1,2,3))));
await assert.rejects(getMiriclAdapter(section(header, sectionName, gzipSync(Uint8Array.of(255)))));
await assert.rejects(getMiriclAdapter(section(header, sectionName, gzipSync('export const other = true;'))), /does not export Miricl/u);
await assert.rejects(getMiriclAdapter(null), TypeError);
const trigger = gzipSync('globalThis.embeddedAdapterExecuted = true; export const Miricl = {instantiate() {return {};}}');
globalThis.embeddedAdapterExecuted = false;
await assert.rejects(getMiriclAdapter(section(Uint8Array.from([...header,1,1,255]), sectionName, trigger)), WebAssembly.CompileError);
assert.equal(globalThis.embeddedAdapterExecuted, false, 'Malformed Wasm is rejected before embedded JavaScript executes.');
await assert.rejects(SuppliedMiricl.instantiate(section(header, sectionName, trigger)), TypeError);
assert.equal(globalThis.embeddedAdapterExecuted, false, 'An explicitly supplied adapter does not execute an embedded replacement.');
delete globalThis.embeddedAdapterExecuted;

for (const filename of [image, scripting]) {
  const original = new Uint8Array(await fs.readFile(filename));
  const module = await WebAssembly.compile(original);
  const sections = WebAssembly.Module.customSections(module, sectionName);
  assert.equal(sections.length, 1, filename + ': matching adapter is embedded');
  assert.deepEqual(WebAssembly.Module.customSections(module, bridgeSection), [], filename + ': terminal relay is separate');
  let vm = await loadMiricl(filename);
  try {
    assert.equal(vm.evaluate('(+ 19 23)').output, '42\n');
    assert.equal(typeof vm.adapter.Miricl, 'function');
    assert.throws(() => vm.evaluate('(error "embedded adapter error")'), vm.adapter.MiriclError);
    assert.equal(vm.evaluate('(+ 19 23)').output, '42\n', 'Lisp errors leave the loaded image usable.');
    vm.evaluate(`(progn (defparameter *embedded-image-value* 42)
      (defparameter *embedded-image-key* (list :retained-key))
      (defparameter *embedded-image-graph*
        (let ((shared (list :shared)))
          (setf (cdr shared) shared)
          (vector shared shared *embedded-image-key*)))
      (defparameter *embedded-image-weak-key*
        (js:make-weak-hash-table :test #'eq :weakness :key))
      (defparameter *embedded-image-weak-value*
        (js:make-weak-hash-table :test #'eq :weakness :value))
      (setf (gethash *embedded-image-key* *embedded-image-weak-key*) *embedded-image-graph*
            (gethash :retained-value *embedded-image-weak-value*) *embedded-image-graph*)
      (values))`, {eof: true});
    if (filename === image) {
      assert.equal(vm.evaluate(`(let ((f (miricl-compiler:compile-native
        '(lambda (x) (declare (type integer x)) (+ x 2)))))
        (and (compiled-function-p f) (= (funcall f 40) 42)))`).output, 'T\n');
      const fixture = await fs.readFile(new URL('../fixtures/fasl-load-context.lisp', import.meta.url), 'utf8');
      assert.equal((await vm.evaluateAsync(fixture, {eof: true})).output, 'T\n',
        'Nested compiled LOAD preserves the outer reconstruction context and shared literal graph.');
    }
    for (const reference of [2, 6]) {
      await assert.rejects(loadMiricl(malformedImmutableImage(vm, original, reference)),
        /Invalid immutable reference/u);
      assert.equal(vm.evaluate('(+ 19 23)').output, '42\n', 'A malformed image leaves the current worker usable.');
    }
    const saved = vm.saveImage();
    assert.deepEqual(WebAssembly.Module.customSections(await WebAssembly.compile(saved), sectionName), sections,
      'Saving retains the matching adapter without duplicating it.');
    vm.close(); vm = null;
    vm = await loadMiricl(new Blob([gzipSync(saved)]));
    assert.equal(vm.evaluate('*embedded-image-value*').output, '42\n', 'The embedded adapter restores saved state.');
    assert.equal(vm.evaluate(`(let ((graph *embedded-image-graph*))
      (and (eq (svref graph 0) (svref graph 1))
        (eq (cdr (svref graph 0)) (svref graph 0))
        (eq (svref graph 2) *embedded-image-key*)
        (eq (js:hash-table-weakness *embedded-image-weak-key*) :key)
        (eq (js:hash-table-weakness *embedded-image-weak-value*) :value)
        (eq (gethash *embedded-image-key* *embedded-image-weak-key*) graph)
        (eq (gethash :retained-value *embedded-image-weak-value*) graph)))`, {eof: true}).output,
      'T\n', 'Restoration preserves weak tables, sharing and mutable cycles.');
  } finally {vm?.close();}
}

// Development helpers must be bundled too: relative imports cannot resolve from
// the adapter's data URL. Use an inert socket so this test starts no connection.
const previousSocket = globalThis.WebSocket;
class Socket {
  static OPEN = 1;
  constructor() {this.readyState = 0; this.bufferedAmount = 0;}
  close() {this.readyState = 3; this.onclose?.();}
  send() {}
}
globalThis.WebSocket = Socket;
let development;
try {
  development = await loadMiricl(image, {dev: {filesystem: 'http://127.0.0.1:8000/'}});
  assert.ok(development.dev.filesystem, 'The embedded adapter includes both development providers.');
  assert.equal((await development.evaluateAsync('(+ 19 23)')).output, '42\n');
} finally {development?.close(); globalThis.WebSocket = previousSocket;}

// The shared component reader selects the intended top-level core and rejects
// malformed framing even after finding it.
const componentHeader = Uint8Array.of(0,97,115,109,13,0,1,0);
const core = section(header, sectionName, fake);
const component = section(section(componentHeader, bridgeSection, header), compilerSection, core);
const extracted = await getMiriclAdapter(component);
assert.deepEqual(extracted.image, core);
assert.deepEqual(extractComponentCore(component, bridgeSection), header);
await assert.rejects(getMiriclAdapter(component.subarray(0, component.length - 1)), /section exceeds/u);
assert.throws(() => extractComponentCore(header, bridgeSection), /Wasm component/u);
assert.throws(() => extractComponentCore(componentHeader, bridgeSection), /Missing miricl\.repl-bridge/u);
assert.throws(() => extractComponentCore(section(component, bridgeSection, header), bridgeSection), /Duplicate miricl\.repl-bridge/u);
assert.throws(() => extractComponentCore(section(componentHeader, bridgeSection, componentHeader), bridgeSection), /core Wasm image/u);
assert.throws(() => extractComponentCore(Uint8Array.of(...component, 0, 1, 128, 0), bridgeSection), /Truncated image section length/u);
assert.throws(() => extractComponentCore(Uint8Array.of(...component, 0, 1, 1, 0), bridgeSection), /Truncated image custom section name/u);
assert.throws(() => extractComponentCore(Uint8Array.of(...component, 0, 128, 128, 128, 128, 16), bridgeSection), /exceeds u32/u);

if (process.argv[3]) {
  const terminal = new Uint8Array(await fs.readFile(process.argv[3]));
  await WebAssembly.compile(extractComponentCore(terminal, bridgeSection));
  const terminalImage = await getMiriclAdapter(terminal);
  assert.deepEqual(WebAssembly.Module.imports(await WebAssembly.compile(terminalImage.image)), [],
    'The terminal component carries an import-free compiler image for Node.');
}
console.log('Embedded image adapters load, compile, recover and retain saved state.');
