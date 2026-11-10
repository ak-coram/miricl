// Compare identical integer workloads and complete image sizes in two runtimes.
// node scripts/benchmark-integers.mjs BEFORE.wasm AFTER.wasm [report.json]
import fs from 'node:fs';
import path from 'node:path';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {measureFile} from './wasm-size.mjs';

const [before, after, output = 'build/integer-tiers/benchmark.json'] = process.argv.slice(2);
if (!before || !after) throw new Error('Usage: benchmark-integers.mjs BEFORE.wasm AFTER.wasm [report.json]');
const squareSum = n => n * (n + 1n) * (2n * n + 1n) / 6n;
const cases = [
  ['integer-steps', '(loop for i from 100000 below 300000 count (< (- i 100001) 100))', 101n],
  ['integer-products', '(loop for i below 32768 sum (* i i))', squareSum(32767n)],
  ['wide-products', '(loop for i from 1048576 below 1078576 sum (* i i))',
    squareSum(1078575n) - squareSum(1048575n)],
  ['wide-counter', `(let ((x 1099511627776))
     (dotimes (i 100000 x) (setq x (+ x 1))))`, 1099511727776n],
  ['typed-accumulation', `(let ((sum 0))
     (dotimes (i 100000 sum)
       (setq sum (+ (the (signed-byte 63) sum) (the (signed-byte 31) i)))))`, 4999950000n],
  ['bignum-products', `(let ((result 0)) (dotimes (repeat 100 result)
    (setq result (loop for i from 1 to 150 for product = i then (* product i)
                       finally (return product)))))`,
    Array.from({length: 150}, (_, i) => BigInt(i + 1)).reduce((a, b) => a * b, 1n)],
];
const report = {node: process.version, rounds: 3, warmups: 3, samples: 7, images: []};
const variants = [before, after].map(image => {
  const bytes = fs.readFileSync(image);
  return {image, bytes, sha256: createHash('sha256').update(bytes).digest('hex'),
    ...measureFile(image), measurements: []};
});
for (let round = 0; round < report.rounds; round++) {
 for (const variant of round % 2 ? variants.toReversed() : variants) {
  const vm = await Miricl.instantiate(variant.bytes);
  try {
    // Keep compilation outside timing and use the same evaluation policy.
    vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
    for (const [name, body, expected] of cases) {
      const source = `(lambda () ${body})`;
      vm.evaluate(`(defparameter *integer-benchmark*
        (miricl-compiler:compile-native '${source}))
        (multiple-value-bind (code literals)
            (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))
          (declare (ignore literals))
          (with-open-file (s "/integer-code.wasm" :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8)) (write-sequence code s))) (values)`);
      const code = await vm.readFile('/integer-code.wasm');
      const run = () => assert.equal(vm.evaluate('(funcall *integer-benchmark*)', {eof: true}).output,
        `${expected}\n`, name);
      for (let i = 0; i < report.warmups; i++) run();
      const samples = [];
      for (let i = 0; i < report.samples; i++) {
        const start = performance.now(); run(); samples.push(performance.now() - start);
      }
      const milliseconds = samples.toSorted((a, b) => a - b)[Math.floor(samples.length / 2)];
      variant.measurements.push({round, name, codeBytes: code.length, milliseconds, samples});
      console.log(`Round ${round + 1}, ${variant.image}: ${name}: ${milliseconds.toFixed(3)} ms`);
    }
  } finally {vm.close();}
 }
}
const median = values => values.toSorted((a, b) => a - b)[Math.floor(values.length / 2)];
for (const variant of variants) {
  variant.medians = cases.map(([name]) => {
    const measurements = variant.measurements.filter(entry => entry.name === name);
    assert.ok(measurements.every(entry => entry.codeBytes === measurements[0].codeBytes));
    return {name, codeBytes: measurements[0].codeBytes,
      milliseconds: median(measurements.flatMap(entry => entry.samples))};
  });
  delete variant.bytes;
  report.images.push(variant);
}
report.comparison = report.images[0].medians.map((entry, i) => ({name: entry.name,
  speedup: entry.milliseconds / report.images[1].medians[i].milliseconds}));
fs.mkdirSync(path.dirname(output), {recursive: true});
fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
console.log(`Wrote ${output}`);
