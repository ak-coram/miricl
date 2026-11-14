import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {runInNewContext} from 'node:vm';
import {gzipSync} from 'node:zlib';
import {Miricl} from '../scripts/miricl.mjs';

const runtime = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const source = await Miricl.instantiate(runtime);
let saved;
try {
  source.mountFile('/persistent.bin', Uint8Array.of(0, 128, 255));
  await source.evaluateAsync(`(defpackage :core-test (:use :cl) (:local-nicknames (:kernel :miricl-core)))
    (in-package :core-test) (values)`, {eof: true});
  await source.evaluateAsync(`(progn
    (defstruct example value)
    (define-condition saved-error (error)
      ((value :initform 42 :reader saved-error-value)))
    (defparameter *example* (make-example :value (ash 1 80)))
    (defparameter *total* 0)
    (defparameter *ring* (list *example*))
    (setf (cdr *ring*) *ring*)
    (defparameter *table* (make-hash-table :test 'eq))
    (setf (gethash *table* *table*) *ring*)
    (defparameter *values* (list -536870912 536870911 -9223372036854775808
                                9223372036854775807 3/5 -0.0d0))
    (defparameter *octets* (make-array 256 :element-type '(unsigned-byte 8)))
    (dotimes (i 256) (setf (aref *octets* i) i))
    (defparameter *byte-string* (map 'string #'code-char *octets*))
    (defparameter *wide-string* (map 'string #'code-char '(0 127 128 255 256 955 128512)))
    (defparameter *signed-array* (make-array 5 :element-type '(signed-byte 32)
      :initial-contents '(-2147483648 -1 0 255 2147483647)))
    (defparameter *deep* nil)
    (dotimes (index 100000) (push index *deep*))
    (defparameter *counter* (funcall
      (miricl-compiler:compile-native '(lambda (count) (lambda () (incf count)))) 40))
    (defparameter *hir-counters* (multiple-value-list (funcall
      (miricl-compiler:compile-hir '(lambda (count)
        (values (lambda () (incf count)) (lambda () count)))) 40)))
    ;; Exercise repeated HIR execution before saving. Transient execution
    ;; data must not capture stale values of shared lexical cells.
    (dotimes (i 3) (unless (= (funcall (second *hir-counters*)) 40) (error "HIR counter fixture")))
    (defparameter *saved-hashes* (mapcar #'sxhash (list *counter* *table* *example*)))
    (js:define-import restart-event ("fixture" "restart") (:boolean) :void)
    (defun restart ()
      (incf *total*) (restart-event t) (setf *readtable* (copy-readtable nil)))
    (setf *readtable* (copy-readtable nil))
    (set-macro-character #\\( (lambda (stream char) (declare (ignore stream char)) :different))
    (kernel:save-image #P"/test-image.wasm" :toplevel 'restart)
    (values))`, {eof: true});
  saved = await source.readFile('/test-image.wasm');
} finally {source.close();}

const restarts = [];
const options = {imports: {fixture: {restart(value) {restarts.push(value);}}}};
const restored = await Miricl.instantiate(saved, options);
try {
  assert.deepEqual(restarts, [true]);
  assert.deepEqual(await restored.readFile('/persistent.bin'), Uint8Array.of(0, 128, 255));
  const check = await restored.evaluateAsync(`(and
    (typep (miricl-threads:current-thread) 'miricl-threads:thread)
    (string= (miricl-threads:thread-name (miricl-threads:current-thread)) "Main thread")
    (eq (class-of (miricl-threads:current-thread)) (find-class 'miricl-threads:thread))
    (eq *package* (find-package :core-test))
    (eq (find-package :kernel) (find-package :miricl-core))
    (= (example-value *example*) (ash 1 80))
    (= (handler-case (error 'saved-error)
         (saved-error (condition) (saved-error-value condition))) 42)
    (eq (car *ring*) *example*) (eq (cdr *ring*) *ring*)
    (eq (gethash *table* *table*) *ring*)
    (equal *values* '(-536870912 536870911 -9223372036854775808 9223372036854775807 3/5 -0.0d0))
    (minusp (float-sign (sixth *values*)))
    (every (lambda (value index) (= value index)) *octets* (loop for i below 256 collect i))
    (equalp (map 'vector #'char-code *byte-string*) *octets*)
    (equal (map 'list #'char-code *wide-string*) '(0 127 128 255 256 955 128512))
    (equal (coerce *signed-array* 'list) '(-2147483648 -1 0 255 2147483647))
    (= (length *deep*) 100000) (= *total* 1) (= (funcall *counter*) 41)
    (null (function-lambda-expression (first *hir-counters*)))
    (= (funcall (first *hir-counters*)) 41)
    (= (funcall (second *hir-counters*)) 41)
    (equal *saved-hashes* (mapcar #'sxhash (list *counter* *table* *example*))))`, {eof: true});
  assert.equal(check.output, 'T\n');
  const second = await Miricl.instantiate(restored.saveImage(), options);
  try {
    assert.deepEqual(restarts, [true]);
    assert.equal(second.evaluate('(funcall *counter*)').output, '42\n');
    assert.equal(second.evaluate('(values (funcall (first *hir-counters*)) (funcall (second *hir-counters*)))').output,
      '42\n42\n');
  } finally {second.close();}
  const module = runInNewContext('new WebAssembly.Module(saved)', {saved});
  const compiled = await Miricl.instantiate(module, options);
  try {assert.equal(compiled.evaluate('(funcall *counter*)').output, '41\n');}
  finally {compiled.close();}
  assert.deepEqual(restarts, [true, true]);
  assert.throws(() => {
    restored.evaluate('(defparameter *foreign* (js:global "Math"))');
    restored.saveImage();
  }, /host object/);
} finally {restored.close();}

await assert.rejects(Miricl.instantiate(saved.subarray(0, -1)), /Truncated Miricl image/);
// Corrupt the packed octet fixture inside an otherwise valid Wasm module.
// Reject its mode and truncated body before allocating/restoring its array.
const payload = new Uint8Array(WebAssembly.Module.customSections(new WebAssembly.Module(saved), 'miricl.core')[0]);
const fixture = Uint8Array.from([128, 2, 1, ...Array.from({length: 256}, (_, i) => i)]);
const packed = Buffer.from(payload).indexOf(fixture);
assert.ok(packed >= 0, 'Octet arrays must be stored as raw bytes');
const word = value => {
  const bytes = [];
  do {bytes.push((value & 127) | (value >= 128 ? 128 : 0)); value >>>= 7;} while (value);
  return Uint8Array.from(bytes);
};
const concat = (...parts) => {
  const bytes = new Uint8Array(parts.reduce((n, part) => n + part.length, 0));
  let offset = 0;
  for (const part of parts) {bytes.set(part, offset); offset += part.length;}
  return bytes;
};
const snapshot = data => {
  const name = new TextEncoder().encode('miricl.core');
  const section = concat(word(name.length), name, data);
  return concat(runtime, Uint8Array.of(0), word(section.length), section);
};
for (const format of [1, 3]) {
  const previousFormat = payload.slice();
  previousFormat[0] = format;
  await assert.rejects(Miricl.instantiate(snapshot(previousFormat)), /Unsupported Miricl image format/);
}
const invalidMode = payload.slice();
invalidMode[packed + 2] = 2;
await assert.rejects(Miricl.instantiate(snapshot(invalidMode)), /Invalid array encoding/);
await assert.rejects(Miricl.instantiate(snapshot(payload.subarray(0, packed + 3 + 128))), /Invalid array length/);
const exports = WebAssembly.Module.exports(new WebAssembly.Module(runtime)).map(entry => entry.name);
const rootSignature = exports.find(name => name.startsWith('core-roots:')).split(':')[1];
const layouts = new Map();
for (const name of exports) {
  const [prefix, id, signature] = name.split(':');
  if (signature && (prefix === 'core-array' || prefix === 'core-read'))
    layouts.set(Number(id), {signature, array: prefix === 'core-array'});
}
let cursor = 0;
const readWord = () => {
  let value = 0, shift = 0, byte;
  do {byte = payload[cursor++]; value += (byte & 127) * 2 ** shift; shift += 7;} while (byte & 128);
  return value;
};
const skipField = kind => {
  const value = readWord();
  if (kind.toLowerCase() === 'l' && value) cursor += 8;
};
assert.equal(readWord(), 2);
for (const kind of rootSignature) skipField(kind);
cursor += 4; // Object count.
const unknownHeader = payload.slice();
assert.equal(unknownHeader[cursor], 0, 'The first object must define its header');
unknownHeader[cursor] = 1;
await assert.rejects(Miricl.instantiate(snapshot(unknownHeader)), /Invalid object header/);
const headers = [null];
for (;;) {
  let header = readWord();
  if (!header) {
    const type = layouts.get(readWord());
    if (!type.array) {
      const start = cursor;
      const mask = readWord();
      const corrupt = concat(payload.subarray(0, start), word(2 ** type.signature.length), payload.subarray(cursor));
      await assert.rejects(Miricl.instantiate(snapshot(corrupt)), /Invalid object field mask/);
      assert.ok((mask & 1) && type.signature[0].toLowerCase() === 'i', 'Fixture includes a scalar prefix');
      const prefixStart = cursor;
      assert.ok(readWord() < 128);
      const badPrefix = concat(payload.subarray(0, prefixStart), word(128), payload.subarray(cursor));
      await assert.rejects(Miricl.instantiate(snapshot(badPrefix)), /Invalid scalar prefix/);
      const scalarStart = cursor;
      readWord();
      const badScalar = concat(payload.subarray(0, scalarStart), word(0x2000000), payload.subarray(cursor));
      await assert.rejects(Miricl.instantiate(snapshot(badScalar)), /Invalid scalar field/);
      break;
    }
    header = headers.length;
    headers.push(type);
  }
  const {signature} = headers[header];
  const length = readWord(), packed = signature === 'i' && readWord();
  if (packed) cursor += length;
  else for (let i = 0; i < length; i++) skipField(signature);
}
const compressed = gzipSync(saved, {level: 9});
await assert.rejects(Miricl.instantiate(compressed.subarray(0, -4)));
// Exercise gzip snapshots, including a byte view with a nonzero buffer offset.
const padded = new Uint8Array(compressed.length + 2);
padded.set(compressed, 1);
const recovered = await Miricl.instantiate(padded.subarray(1, -1), options);
try {
  assert.equal(recovered.evaluate('(values (funcall (first *hir-counters*)) (funcall (second *hir-counters*)))').output,
    '41\n41\n');
} finally {recovered.close();}
console.log('Miricl images restore packages, cyclic state, native and HIR closures, and the restart function.');
