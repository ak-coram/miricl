// Compact native entry tables preserve ordered entries, literal ownership,
// direct calling contracts and snapshots, and reject untrusted metadata.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';
import {Miricl} from '../scripts/miricl.mjs';

const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const nativeInstances = [];
const OriginalInstance = WebAssembly.Instance;
WebAssembly.Instance = new Proxy(OriginalInstance, {construct(target, args) {
  const instance = Reflect.construct(target, args);
  if (instance.exports.entries instanceof WebAssembly.Table) nativeInstances.push(instance);
  return instance;
}});
const vm = await createCompilerTestVM(image);
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
const word = value => {
  const bytes = [];
  do {const byte = value & 127; value = Math.floor(value / 128); bytes.push(byte | (value ? 128 : 0));} while (value);
  return bytes;
};
const section = (id, payload) => Uint8Array.from([id, ...word(payload.length), ...payload]);
const sections = bytes => {
  let position = 8;
  const read = () => {let value = 0, shift = 0, byte; do {byte = bytes[position++]; value += (byte & 127) * 2 ** shift; shift += 7;} while (byte & 128); return value;};
  const result = [];
  while (position < bytes.length) {const id = bytes[position++], length = read(); result.push([id, bytes.slice(position, position + length)]); position += length;}
  return result;
};
const moduleBytes = parts => Uint8Array.from([0,97,115,109,1,0,0,0, ...parts.flatMap(([id, payload]) => [...section(id, payload)])]);
const metadataName = [...new TextEncoder().encode('miricl.entries')];
const metadata = payload => [metadataName.length, ...metadataName, ...payload];
const sources = [
  '(lambda () :first)', '(lambda () :second)',
  '(lambda () (values :zero :extra))', '(lambda (a) a)',
  '(lambda (a b) (declare (ignore a) (optimize (debug 0))) b)',
  '(lambda (a b c) (declare (ignore b) (optimize (debug 0))) (values a c))',
  '(lambda (a b c d) (declare (ignore a c) (optimize (debug 0))) (list b d))',
  '(lambda (&optional (x :default supplied)) (values x supplied))',
  '(lambda (x) (declare (optimize (debug 3))) (lambda () (incf x)))',
  '(lambda (x) (cl-user::entry-table-live x))',
];
try {
  vm.evaluate(`(in-package :miricl-compiler)
    (defparameter *entry-table-sources* '(${sources.join('\n')}))
    (defparameter *entry-table-units* (mapcar #'compile-to-hir *entry-table-sources*))
    (defparameter *entry-table-roots* (mapcar (lambda (unit) (first (hir-unit-functions unit))) *entry-table-units*))
    (defparameter *entry-table-unit* (make-hir-unit :functions (mapcan (lambda (unit) (copy-list (hir-unit-functions unit))) *entry-table-units*)))
    (defun write-entry-table-module (compact path)
      (multiple-value-bind (bytes literals)
          (emit-native-module *entry-table-unit* :entries *entry-table-roots* :entry-tables compact)
        (defparameter *entry-table-literals* literals)
        (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
          (write-sequence bytes out))))
    (write-entry-table-module nil "/entry-named.wasm")
    (write-entry-table-module t "/entry-table.wasm") (values)`);
  const named = await vm.readFile('/entry-named.wasm'), compact = await vm.readFile('/entry-table.wasm');
  assert.ok(WebAssembly.validate(named) && WebAssembly.validate(compact));
  assert.ok(compact.length < named.length, 'Compact layout must shrink the complete module');
  const compiled = await WebAssembly.compile(compact);
  assert.equal(WebAssembly.Module.customSections(compiled, 'miricl.entries').length, 1);
  const loadCode = async (bytes, name = '/entry-code.wasm') => {
    vm.mountFile(name, bytes);
    vm.evaluate(`(with-open-file (in "${name}" :element-type '(unsigned-byte 8))
      (defparameter *entry-table-code* (make-array (file-length in) :element-type '(unsigned-byte 8)))
      (read-sequence *entry-table-code* in)) (values)`);
  };
  const install = `(defparameter *entry-table-functions*
    (loop for literals in *entry-table-literals* for index from 0 collect
      (compiler-primitive 17 (cons *entry-table-code* index) literals nil))) (values)`;
  const behavior = `(and
    (not (eq (first *entry-table-functions*) (second *entry-table-functions*)))
    (eq (funcall (first *entry-table-functions*)) :first)
    (eq (funcall (second *entry-table-functions*)) :second)
    (equal (multiple-value-list (funcall (nth 2 *entry-table-functions*))) '(:zero :extra))
    (= (funcall (nth 3 *entry-table-functions*) 17) 17)
    (= (funcall (nth 4 *entry-table-functions*) :ignored 29) 29)
    (equal (multiple-value-list (funcall (nth 5 *entry-table-functions*) 1 :ignored 3)) '(1 3))
    (equal (funcall (nth 6 *entry-table-functions*) :ignored 2 :ignored 4) '(2 4))
    (equal (multiple-value-list (funcall (nth 7 *entry-table-functions*))) '(:default nil))
    (equal (multiple-value-list (funcall (nth 7 *entry-table-functions*) nil)) '(nil t))
    (let ((counter (funcall (nth 8 *entry-table-functions*) 40)))
      (and (= (funcall counter) 41) (= (funcall counter) 42))))`;
  for (const bytes of [named, compact]) {
    await loadCode(bytes); vm.evaluate(install); check(behavior);
    check(`(let ((first (first *entry-table-functions*)))
      ${install.replace('(values)', '')}
      (and (not (eq first (first *entry-table-functions*))) ${behavior}))`);
  }
  // Later cached installs retain their copied references if an initializer
  // changes the module's exported mutable table.
  const table = nativeInstances.at(-1).exports.entries;
  const originalEntry = table.get(0);
  table.set(0, table.get(1));
  vm.evaluate(install); check(behavior);
  table.set(0, originalEntry);
  vm.evaluate(`(in-package :cl-user) (defun entry-table-live (x) (list :old x))
    (in-package :miricl-compiler) (values)`);
  check(`(equal (funcall (nth 9 *entry-table-functions*) 1) '(:old 1))`);
  vm.evaluate(`(setf (symbol-function 'cl-user::entry-table-live) (lambda (x) (list :new x))) (values)`);
  check(`(equal (funcall (nth 9 *entry-table-functions*) 2) '(:new 2))`);
  // Actual funcrefs keep their original function indices when saved.
  const saved = vm.saveImage();
  const restored = await Miricl.instantiate(saved);
  try {restored.evaluate('(in-package :miricl-compiler)'); assert.equal(restored.evaluate(behavior).output, 'T\n');}
  finally {restored.close();}

  const parts = sections(compact);
  const customIndex = parts.findIndex(([id, payload]) => id === 0 &&
    new TextDecoder().decode(payload.slice(1, 1 + payload[0])) === 'miricl.entries');
  const original = parts[customIndex][1].slice(1 + metadataName.length);
  const altered = payload => moduleBytes(parts.map((part, index) => index === customIndex ? [0, metadata(payload)] : part));
  const corruptions = [
    altered([2, ...original.slice(1)]),
    altered([1, 0, ...original.slice(2)]),
    altered([1, original[1] | 128, 0, ...original.slice(2)]),
    altered([1, original[1], 2, ...original.slice(3)]),
    altered([...original, 0]), altered(original.slice(0, -1)),
    altered([1, original[1], 1, 6, ...original.slice(4)]),
    altered([1, original[1], 1, 8, ...original.slice(4)]),
    altered([1, original[1], 1, 0xf9, ...original.slice(4)]),
    moduleBytes([...parts.slice(0, customIndex), parts[customIndex], ...parts.slice(customIndex)]),
    moduleBytes(parts.filter((_, index) => index !== customIndex)),
    moduleBytes(parts.map(([id, payload]) => id === 4 ? [id,
      [1, 0x70, 1, payload[3] + 1, payload[4] + 1]] : [id, payload])),
    moduleBytes(parts.map(([id, payload]) => id === 4 ? [id,
      [1, 0x70, 1, payload[3] - 1, payload[4]]] : [id, payload])),
    moduleBytes(parts.map(([id, payload]) => id === 7 ? [id,
      [1, 7, ...new TextEncoder().encode('missing'), 1, 0]] : [id, payload])),
  ];
  // Valid Wasm with incompatible functions must become a Lisp condition,
  // rather than reaching an unchecked function-type cast.
  const tiny = (direct, start) => moduleBytes([
    [1, [2, 0x60, 2, 0x6f, 0x6f, 1, 0x6f, 0x60, 0, 0]],
    [3, [2, 0, 1]], [4, [1, 0x70, 1, direct ? 2 : 1, direct ? 2 : 1]],
    [7, [1, 7, ...new TextEncoder().encode('entries'), 1, 0]],
    ...(start ? [[8, [1]]] : []),
    [9, [1, 0, 0x41, 0, 0x0b, direct ? 2 : 1, ...(direct ? [0, 1] : [1])]],
    [0, metadata(direct ? [1, 1, 1, 1] : [1, 1, 0])],
    [10, [2, 4, 0, 0xd0, 0x6f, 0x0b, 2, 0, 0x0b]],
  ]);
  corruptions.push(tiny(false, false), tiny(true, false), tiny(false, true));
  for (const bytes of corruptions) {
    assert.ok(WebAssembly.validate(bytes), 'Metadata corruption fixture remains valid Wasm');
    await loadCode(bytes);
    check(`(handler-case
      (progn (compiler-primitive 17 (cons *entry-table-code* 0) (first *entry-table-literals*) nil) nil)
      (error (condition) (not (null (search "native" (princ-to-string condition))))))`);
    await loadCode(compact); vm.evaluate(install); check(behavior);
  }
  await loadCode(compact);
  check(`(handler-case
    (progn (compiler-primitive 17 (cons *entry-table-code* (length *entry-table-literals*)) #() nil) nil)
    (error () t))`);
  vm.evaluate(install); check(behavior);
  // Factory-based COMPILE-NATIVE uses the current named entry layout.
  check(`(equal (multiple-value-list (funcall (compile-native '(lambda () (values 17 29))))) '(17 29))`);
  // A small explicit bundle keeps named exports when the table costs more.
  vm.evaluate(`(progn
    (setq *entry-table-unit* (compile-to-hir '(lambda (&rest args) args))
          *entry-table-roots* (list (first (hir-unit-functions *entry-table-unit*))))
    (write-entry-table-module nil "/entry-small-named.wasm")
    (write-entry-table-module t "/entry-small-auto.wasm") (values))`);
  const smallNamed = await vm.readFile('/entry-small-named.wasm');
  const smallAuto = await vm.readFile('/entry-small-auto.wasm');
  assert.deepEqual(smallAuto, smallNamed, 'An unprofitable table preserves the named layout');
  assert.equal(WebAssembly.Module.customSections(await WebAssembly.compile(smallAuto), 'miricl.entries').length, 0);
  // Entry ordinals and active-element indices also cross LEB boundaries.
  for (const rest of [false, true]) {
    const count = rest ? 16 : 140;
    vm.evaluate(`(progn (setq *entry-table-units* (mapcar #'compile-to-hir
        '(${Array.from({length: count}, (_, index) => rest ? '(lambda (&rest args) args)' : `(lambda () ${index})`).join(' ')})))
      (setq *entry-table-roots* (mapcar (lambda (unit) (first (hir-unit-functions unit))) *entry-table-units*)
            *entry-table-unit* (make-hir-unit :functions
              (mapcan (lambda (unit) (copy-list (hir-unit-functions unit))) *entry-table-units*)))
      (write-entry-table-module t "/entry-wide.wasm") (values))`);
    const bytes = await vm.readFile('/entry-wide.wasm');
    assert.ok(WebAssembly.validate(bytes));
    assert.equal(WebAssembly.Module.customSections(await WebAssembly.compile(bytes), 'miricl.entries').length, 1);
    await loadCode(bytes); vm.evaluate(install);
    if (rest) check(`(equal (funcall (car (last *entry-table-functions*)) 17 29) '(17 29))`);
    else check(`(equal (list (funcall (first *entry-table-functions*))
                    (funcall (nth 127 *entry-table-functions*))
                    (funcall (car (last *entry-table-functions*)))) '(0 127 ${count - 1}))`);
  }
  console.log(`Native entry tables: ${named.length} to ${compact.length} bytes; direct entries, ownership, snapshots and malformed-input recovery pass.`);
} finally {vm.close(); WebAssembly.Instance = OriginalInstance;}
