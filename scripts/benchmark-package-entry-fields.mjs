import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from './miricl.mjs';
const image = process.argv[2] ?? 'build/kernel-migration/package-entry-bench.wasm';
const vm = await Miricl.instantiate(fs.readFileSync(image));
const drivers = ['bench-package-entry-direct-field', 'bench-package-entry-dispatched-field'];
const results = [];
try {
  for (const name of drivers) for (let round = 0; round < 4; round++) assert.equal(vm.vm[name](20000), 20000);
  for (const name of drivers) {
    const probeStart = performance.now();
    assert.equal(vm.vm[name](20000), 20000);
    const elapsed = performance.now() - probeStart;
    const count = Math.min(1000000, Math.max(20000, Math.round(200000 / Math.max(elapsed, 0.001))));
    const samples = [];
    for (let round = 0; round < 5; round++) {
      const start = performance.now();
      assert.equal(vm.vm[name](count), count);
      samples.push(performance.now() - start);
    }
    const median = [...samples].sort((a, b) => a - b)[2];
    results.push({name, count, milliseconds: median, nanoseconds: median * 1e6 / count, samples});
  }
  console.log(JSON.stringify({image, warmups: 4, results}, null, 2));
} finally { vm.close(); }
