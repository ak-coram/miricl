// Compare object serialization with identical workloads and fresh loader VMs.
// node scripts/benchmark-saved-slots.mjs IMAGE OUTPUT [COUNT=1000] [COALTON-CORE]
import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import {gzipSync, brotliCompressSync, constants} from 'node:zlib';
import {Miricl} from './miricl.mjs';

const sharedOnly = process.argv.includes('--shared');
const [imagePath, output, countText = '1000', coaltonCore] = process.argv.slice(2).filter(arg => arg !== '--shared');
if (!imagePath || !output) throw new Error('Expected IMAGE OUTPUT [COUNT] [COALTON-CORE]');
const count = Number(countText);
if (!Number.isSafeInteger(count) || count < 2 || count % 2) throw new Error('COUNT must be a positive even integer');
await fs.mkdir(output, {recursive: true});
const image = await fs.readFile(imagePath);
const definitions = `(defpackage :saved-slots-benchmark (:use :cl))
  (in-package :saved-slots-benchmark)
  (defvar *initforms* 0)
  (defclass node () ((number :initarg :number) (next) (payload :initarg :payload)
    (a :initarg :a) (b :initarg :b) (c :initarg :c) (d :initarg :d)
    (unbound) (default :initform (incf *initforms*))))
  (defmethod make-load-form ((object node) &optional environment)
    (make-load-form-saving-slots object :environment environment))
  (values)`;

function recordCounts(bytes) {
  let at = 8, units = 0, records = 0;
  const uleb = () => {let n = 0, shift = 0, byte;
    do {byte = bytes[at++]; n += (byte & 127) * 2 ** shift; shift += 7;} while (byte & 128);
    return n;};
  while (at < bytes.length) {
    const kind = bytes[at++], size = uleb(), end = at + size;
    if (kind !== 0) throw new Error('Expected FASL custom sections');
    const nameLength = uleb();
    assert.equal(bytes.subarray(at, at + nameLength).toString(), 'miricl.fasl');
    at += nameLength;
    assert.equal(bytes.subarray(at, at + 4).toString(), 'MIRF');
    records += bytes.readUInt32LE(at + 8); units++;
    at = end;
  }
  return {units, records};
}
function sizes(bytes) {
  return {bytes: bytes.length, gzipBytes: gzipSync(bytes, {level: 9}).length,
    brotliBytes: brotliCompressSync(bytes, {params: {[constants.BROTLI_PARAM_QUALITY]: 9}}).length,
    ...recordCounts(bytes)};
}
async function timed(vm, source) {
  const cpu = process.cpuUsage(), start = performance.now();
  await vm.evaluateAsync(source, {eof: true});
  const used = process.cpuUsage(cpu);
  return {milliseconds: performance.now() - start, cpuMilliseconds: (used.user + used.system) / 1000};
}
async function prepare(vm, workload) {
  if (workload === 'objects') await vm.evaluateAsync(definitions, {eof: true});
  else {
    vm.mountFile('/dependencies.wasm', await fs.readFile(coaltonCore));
    await vm.evaluateAsync('(load "/dependencies.wasm") (cl:in-package :cl-user) (values)', {eof: true});
  }
}
const report = {image: imagePath, runtimeBytes: image.length, count, sharedOnly, workloads: []};
for (const workload of ['objects', ...(coaltonCore ? ['coalton-metadata'] : [])]) {
  const vm = await Miricl.instantiate(image);
  try {
    await prepare(vm, workload);
    if (sharedOnly) await vm.evaluateAsync(`(setf (symbol-function
        (miricl-compiler::compiler-primitive 10 1 "%FASL-SAVED-SLOTS"))
      (lambda (object creation initialization) (declare (ignore object creation initialization)) nil))
      (values)`, {eof: true});
    if (workload === 'objects') {
      await vm.evaluateAsync(`(defparameter *graph* (make-array ${count}))
        (let ((payload (vector :shared 1 2 3)))
          (dotimes (i ${count})
            (setf (aref *graph* i) (make-instance 'node :number i :payload payload :a i :b (+ i 1) :c (+ i 2) :d (+ i 3))))
          (dotimes (i ${count})
            (setf (slot-value (aref *graph* i) 'next) (aref *graph* (logxor i 1)))))
        (values)`, {eof: true});
      vm.mountFile('/graph.lisp', '(in-package :saved-slots-benchmark) (defparameter *loaded* (quote #.*graph*))');
    } else {
      vm.mountFile('/metadata.wasm', await fs.readFile('build/coalton/fast/cache/coalton/library/classes.wasm'));
      await vm.evaluateAsync(`(defparameter *metadata*
        (let* ((unit (first (miricl-compiler::read-units "/metadata.wasm")))
               (records (miricl-compiler::ir-unit-records unit))
               (objects nil))
          (progv (mapcar (lambda (name) (miricl-compiler::compiler-primitive 10 1 name))
                   '("%FASL-RECORDS" "%FASL-VALUES" "%FASL-STATES" "%FASL-READTABLE" "%FASL-PENDING"))
                 (list records (make-array (length records))
                   (make-array (length records) :initial-element 0) (copy-readtable nil) nil)
            (loop for record across records for i from 0
              when (= (first record) 9) do
                (push (funcall (miricl-compiler::compiler-primitive 10 1 "%FASL-OBJECT") i) objects)
              until (= (length objects) ${Math.min(count, 200)}))
            (funcall (miricl-compiler::compiler-primitive 10 1 "%FASL-FINISH-INITIALIZATIONS")))
          (coerce (nreverse objects) 'vector))) (values)`, {eof: true});
      vm.mountFile('/graph.lisp', '(in-package :cl-user) (defparameter *loaded-metadata* (quote #.*metadata*))');
    }
    const compile = await timed(vm, '(compile-file "/graph.lisp" :verbose nil) (values)');
    const compact = await timed(vm, '(miricl-compiler:shake-file "/graph.wasm" "/compact.wasm" :strip nil) (values)');
    await vm.evaluateAsync('(load "/compact.wasm") (values)', {eof: true});
    const native = await timed(vm, '(miricl-compiler:compile-native-bundle "/compact.wasm" "/native.wasm") (values)');
    const result = {workload, compile, compact, native, artifacts: []};
    // Coalton dependencies are deliberately outside the object-load timing.
    // Reuse one prepared VM there; full cold startup is profiled separately.
    const prepared = workload === 'coalton-metadata' ? await Miricl.instantiate(image) : null;
    if (prepared) await prepare(prepared, workload);
    try { for (const kind of ['graph', 'compact', 'native']) {
      const bytes = Buffer.from(await vm.readFile(`/${kind}.wasm`));
      const file = path.join(output, `${workload}-${kind}.wasm`);
      await fs.writeFile(file, bytes);
      const loads = [];
      for (let repeat = 0; repeat < 3; repeat++) {
        const fresh = prepared ?? await Miricl.instantiate(image);
        try {
          if (!prepared) await prepare(fresh, workload);
          fresh.mountFile('/artifact.wasm', bytes);
          loads.push(await timed(fresh, '(load "/artifact.wasm") (values)'));
          if (workload === 'objects') assert.equal((await fresh.evaluateAsync(`
            (and (= *initforms* 0) (= (length *loaded*) ${count})
              (loop for i below ${count} always
                (let ((object (aref *loaded* i)))
                  (and (= (slot-value object 'number) i)
                    (eq (slot-value object 'next) (aref *loaded* (logxor i 1)))
                    (eq (slot-value object 'payload) (slot-value (aref *loaded* 0) 'payload))
                    (not (slot-boundp object 'unbound)) (= (slot-value object 'default) (1+ i))))))`, {eof: true})).output, 'T\n');
          else assert.equal((await fresh.evaluateAsync(`(= (length *loaded-metadata*) ${Math.min(count, 200)})`, {eof: true})).output, 'T\n');
        } finally {if (!prepared) fresh.close();}
      }
      result.artifacts.push({kind, file, ...sizes(bytes), loads});
    }} finally {prepared?.close();}
    report.workloads.push(result);
    await fs.writeFile(path.join(output, 'results.json'), JSON.stringify(report, null, 2) + '\n');
    console.log(`${workload}: compile ${Math.round(compile.milliseconds)} ms; native ${Math.round(native.milliseconds)} ms; ` +
      result.artifacts.map(a => `${a.kind} ${a.bytes} bytes`).join(', '));
  } finally {vm.close();}
}
report.peakRSSBytes = process.resourceUsage().maxRSS * 1024;
await fs.writeFile(path.join(output, 'results.json'), JSON.stringify(report, null, 2) + '\n');
