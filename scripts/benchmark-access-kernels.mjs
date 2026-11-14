// Run after (miricl::write-access-kernel-benchmark ... ) in the persistent host REPL.
// SBCL half: load scripts/benchmark-access-kernels.lisp in that same REPL.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';
const image = process.argv[2] ?? 'build/kernel-batch/kernels.wasm';
const vm = await Miricl.instantiate(fs.readFileSync(image));
const cases = [
  ['read-path', [1,4,10], 2000000, n=>n-1], ['write-path', [1,4,10], 2000000, n=>n],
  ['pop-argument', [0,1], 2000000, n=>n], ['bounded-index', [0,512,1024], 2000000, n=>n],
  ['keywords', [0,1,8,64], 500000, ()=>1], ['cursor-buffer', [0,1,8,64,512], 100000, n=>n],
  ['property-value', [0,1,8,64], 500000, n=>n ? n-1 : 99],
  ['nthcdr-general', [0,1,8,64], 100000, ()=>1],
  ['generic-arguments', [0,1,8,64], 500000, ()=>1]
];
const results = [];
// Warm every representative branch before timing. Each kernel has its own driver.
for (const [kernel, sizes, count, expected] of cases)
  for (const size of sizes) for(let warm=0;warm<4;warm++)
    assert.equal(vm.vm[`bench-${kernel}`](Math.min(count, 20000),size), expected(size));
for (const [kernel, sizes, count, expected] of cases) for(const size of sizes) {
  const calls = kernel === "cursor-buffer" && size < 64 ? 1000000 : count;
  const samples=[];
  for(let round=0;round<5;round++) {
    const start=performance.now();
    assert.equal(vm.vm[`bench-${kernel}`](calls,size),expected(size));
    samples.push(performance.now()-start);
  }
  results.push({kernel,size,count:calls,samples,milliseconds:[...samples].sort((a,b)=>a-b)[2]});
}
vm.close();
console.log(JSON.stringify({image,results,size:measureWasm(image)},null,2));
