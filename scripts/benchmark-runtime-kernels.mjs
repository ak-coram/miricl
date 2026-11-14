// Separate warmed native drivers call the actual runtime worker in a diagnostic image.
// No guest compiler/bootstrap and no interpreted Lisp evaluation occur inside timing.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';

const [image = 'build/kernel-batch/ports-bench.wasm',
  catalogPath = 'build/kernel-batch/ports-catalog.json', ...options] = process.argv.slice(2);
const option = (name, fallback) => {
  const index = options.indexOf(name);
  return index < 0 ? fallback : options[index + 1];
};
const scale = Number(option('--scale', '1'));
const samples = Number(option('--samples', '5'));
const warmups = Number(option('--warmups', '4'));
const sampleMilliseconds = Number(option('--sample-ms', '10'));
assert.ok(scale > 0 && Number.isFinite(scale));
assert.ok(Number.isInteger(samples) && samples >= 1);
assert.ok(Number.isInteger(warmups) && warmups >= 1);
assert.ok(Number.isFinite(sampleMilliseconds) && sampleMilliseconds >= 0);
const filter = new RegExp(option('--filter', ''));
const cases = JSON.parse(fs.readFileSync(catalogPath, 'utf8')).cases
  .filter(spec => filter.test(spec.name) || filter.test(spec.entry));
assert.ok(cases.length, 'No benchmark cases selected');
const vm = await Miricl.instantiate(fs.readFileSync(image));
const setup = spec => {
  // Evaluate standard CL fixtures only. These are our own trusted source forms.
  try {
    const evaluated = vm.evaluate(`(progn (setq cl-user::*runtime-port-bench-arguments* ${spec.setup}) (values))`, {eof: true});
    assert.equal(evaluated.output, '', `${spec.name}: fixture setup failed`);
  } catch (cause) {
    throw new Error(`${spec.name}: fixture setup failed: ${spec.setup}`, {cause});
  }
};
const calls = spec => {
  const count = Math.max(1, Math.round(spec.count * scale));
  assert.ok(Number.isSafeInteger(count) && count <= 0x7fffffff, 'Count exceeds diagnostic i32 ABI');
  return count;
};
const invoke = (spec, count) => {
  const driver = vm.vm[spec.export];
  assert.equal(typeof driver, 'function', `Missing diagnostic export ${spec.export}`);
  const result = driver(count);
  assert.equal(result, count * spec.truth, `${spec.name}: consumed result mismatch`);
};
const results = [];
try {
  // Warm all fixtures and branch variants BEFORE collecting samples. Each kernel
  // has its own generated native driver; no shared dispatcher competes to inline.
  for (const spec of cases) {
    for (let round = 0; round < warmups; round++) {
      setup(spec);
      invoke(spec, Math.min(calls(spec), 20000));
    }
  }
  for (const spec of cases) {
    let count = calls(spec);
    if (sampleMilliseconds > 0) {
      const probeCount = Math.min(count, 1000);
      setup(spec);
      const start = performance.now();
      invoke(spec, probeCount);
      const elapsed = performance.now() - start;
      // Bound both fixture-specific work and time spent on slow generic calls.
      // JIT warmup already covered every candidate and branch above.
      count = Math.min(count, Math.max(1, Math.round(probeCount * sampleMilliseconds / Math.max(elapsed, 0.001))));
    }
    const durations = [];
    for (let round = 0; round < samples; round++) {
      setup(spec); // fixture allocation/reset is deliberately outside timing
      const start = performance.now();
      const result = vm.vm[spec.export](count);
      const end = performance.now();
      assert.equal(result, count * spec.truth, spec.name);
      durations.push(end - start);
    }
    const milliseconds = [...durations].sort((a, b) => a - b)[Math.floor(samples / 2)];
    results.push({name: spec.name, entry: spec.entry, count,
      milliseconds, samples: durations});
    console.error(`${spec.name}: ${(milliseconds * 1e6 / count).toFixed(3)} ns/call (${count} calls)`);
  }
} finally {
  vm.close();
}
console.log(JSON.stringify({schemaVersion: 1, runtime: 'wasm', node: process.version,
  image, catalog: catalogPath, scale, warmups, sampleCount: samples,
  targetSampleMilliseconds: sampleMilliseconds, size: measureWasm(image), results}, null, 2));
