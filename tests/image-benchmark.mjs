#!/usr/bin/env node
// Compare size candidates in one process with alternating execution order.
// node tests/image-benchmark.mjs before.wasm after.wasm [other.wasm ...]
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl, MiriclError} from '../scripts/miricl.mjs';

const binaries = process.argv.slice(2);
assert.ok(binaries.length >= 2, 'Supply at least two interpreter images.');
const modules = await Promise.all(binaries.map(file => WebAssembly.compile(fs.readFileSync(file))));
const warmup = 5, samples = 20;
const median = values => [...values].sort((a, b) => a - b)[Math.floor(values.length / 2)];
const result = {binaries, node: process.version, warmup, samples, workloads: {}};
async function measure(name, run) {
  const times = binaries.map(() => []);
  for (let round = 0; round < warmup + samples; round++) {
    const order = binaries.map((_, i) => i);
    if (round % 2) order.reverse();
    for (const i of order) {
      const start = performance.now();
      await run(i);
      if (round >= warmup) times[i].push(performance.now() - start);
    }
  }
  const medians = times.map(median);
  result.workloads[name] = {medianMs: medians,
    changePercent: medians.map(value => 100 * (value / medians[0] - 1)), samplesMs: times};
}
await measure('initialization', async i => {
  const vm = new Miricl(await WebAssembly.instantiate(modules[i]));
  assert.equal(vm.evaluate('(+ 20 22)').output, '42\n');
  assert.equal(vm.vm.memory.buffer.byteLength, 131072);
});
const vms = await Promise.all(modules.map(async module => new Miricl(await WebAssembly.instantiate(module))));
for (const vm of vms) {
  vm.mountFile('benchmark-compile.lisp', `(defparameter *benchmark-data* '#(${Array(200).fill('"éλ中𝄞"').join(' ')}))`);
  vm.evaluate(`
  (defun image-sum (n) (loop for i below n sum i))
  (compile 'image-sum)
  (defgeneric image-call (x))
  (defmethod image-call ((x integer)) (+ x 1))`, {eof: true});
}
for (const [name, source, expected] of [
  ['arithmetic', '(let ((sum 0)) (dotimes (i 10000 sum) (incf sum i)))', '49995000\n'],
  ['compiled-loop', '(image-sum 10000)', '49995000\n'],
  ['typep', "(loop for i below 5000 count (typep i '(integer 0 10000)))", '5000\n'],
  ['generic-call', '(loop for i below 1000 sum (image-call i))', '500500\n'],
  ['unicode-fold', '(dotimes (i 2000) (string-equal "éλж𐐀ǅ" "ÉΛЖ𐐨ǆ"))', 'NIL\n'],
  ['printer', '(dotimes (i 500) (write-to-string (list (find-package :cl) 1.25d0 12345678901234567890)))', 'NIL\n'],
  ['file-characters', `(with-open-file (s "benchmark-chars" :direction :output :if-exists :supersede)
    (dotimes (i 10000) (write-char #\\λ s)) (file-length s))`, '20000\n'],
  ['file-bytes', `(with-open-file (s "benchmark-bytes" :direction :output :if-exists :supersede
      :element-type '(unsigned-byte 8))
    (dotimes (i 10000) (write-byte 65 s)) (file-length s))`, '10000\n'],
  ['bit-reader', `(length (read-from-string "#*${'01'.repeat(5000)}"))`, '10000\n'],
  ['file-compilation', '(nth-value 2 (compile-file "benchmark-compile.lisp" :verbose nil :print nil))', 'NIL\n'],
]) await measure(name, i => {
  for (let repeat = 0; repeat < 3; repeat++) assert.equal(vms[i].evaluate(source, {eof: true}).output, expected);
});
await measure('error-reporting', i => {
  for (let repeat = 0; repeat < 200; repeat++) {
    assert.throws(() => vms[i].evaluate('(/ 1 0)', {eof: true}), error =>
      error instanceof MiriclError && error.message.includes('Division by zero'));
  }
});
console.log(JSON.stringify(result, null, 2));
