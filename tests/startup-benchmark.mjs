#!/usr/bin/env node
// Compile once, then measure guest initialization in fresh instances.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const binary = process.argv[2] ?? 'miricl.wasm';
const bytes = fs.readFileSync(binary);
const started = performance.now();
const module = await WebAssembly.compile(bytes);
const compilationMs = performance.now() - started;
const times = [];
let memoryBytes;
for (let i = 0; i < 7; i++) {
  const start = performance.now();
  const vm = new Miricl(await WebAssembly.instantiate(module));
  const elapsed = performance.now() - start;
  memoryBytes = vm.vm.memory.buffer.byteLength;
  assert.equal(vm.evaluate(`(list (string= (string-upcase "abλ") "ABΛ")
    (let ((n (coerce 7 'double-float))) (and (typep n 'double-float) (= n 7)))
    (string= (subseq "abc" 1) "bc"))`).output, '(T T T)\n');
  if (i >= 2) times.push(elapsed);
}
console.log(JSON.stringify({binary, binaryBytes: bytes.length, compilationMs,
  initializationMs: Math.min(...times), memoryBytes}, null, 2));
