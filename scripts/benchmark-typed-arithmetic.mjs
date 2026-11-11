// Paired microbenchmark of type-directed arithmetic; application benchmarks
// remain the evidence for overall compiler and application performance.
// node scripts/benchmark-typed-arithmetic.mjs OUTPUT BEFORE.wasm AFTER.wasm
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';

const [output, before, after] = process.argv.slice(2);
if (!output || !before || !after) throw new Error('Expected OUTPUT BEFORE.wasm AFTER.wasm');
await fs.mkdir(output, {recursive: true});
const variants = [], jobs = [];
const iterations = 1000000, rounds = 5;
const timed = (vm, source) => {
  const cpu = process.cpuUsage(), start = performance.now();
  const result = vm.evaluate(source), usage = process.cpuUsage(cpu);
  return {output: result.output, wallMs: performance.now() - start,
    cpuMs: (usage.user + usage.system) / 1000};
};
try {
  for (const [name, imagePath] of [['before', before], ['after', after]]) {
    const image = await fs.readFile(imagePath), vm = await Miricl.instantiate(image);
    const record = {name, imagePath, imageSha256: createHash('sha256').update(image).digest('hex'),
      sizes: await measureWasm(imagePath), kernels: []};
    variants.push({vm, record});
    vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
    for (const type of ['fixnum', '(signed-byte 16)', 't']) {
      for (const [operator, answer] of [['+', 16], ['-', -2], ['*', 63]]) {
        const index = record.kernels.length, symbol = `typed-benchmark-${index}`;
        const source = `(lambda (a b count) (declare (type ${type} a b))
          (let ((result nil))
            (dotimes (i count result) (setq result (${operator} a b)))))`;
        const compilation = timed(vm, `(progn (multiple-value-bind (bytes literals)
          (emit-native-module (compile-to-hir '${source}))
          (setf (symbol-function '${symbol}) (compiler-primitive 17 bytes literals '${symbol}))
          (with-open-file (out "/kernel.wasm" :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values))`);
        assert.equal(compilation.output, '');
        const bytes = await vm.readFile('/kernel.wasm');
        await fs.writeFile(path.join(output, `${name}-${index}.wasm`), bytes);
        const imports = WebAssembly.Module.imports(await WebAssembly.compile(bytes)).map(entry => entry.name);
        const call = `(${symbol} 7 9 ${iterations})`, expected = `${answer}\n`;
        assert.equal(timed(vm, call).output, expected); // Warmup, excluded.
        const kernel = {operator, type, source, bytes: bytes.length, imports, compilation, samples: []};
        record.kernels.push(kernel);
        jobs.push({vm, kernel, call, expected});
      }
    }
  }
  for (let round = 0; round < rounds; round++) {
    // Alternate images to reduce drift from JIT warmup and other local jobs.
    for (let index = 0; index < 9; index++) {
      for (const variant of round % 2 ? [1, 0] : [0, 1]) {
        const {vm, kernel, call, expected} = jobs[index + 9 * variant];
        const sample = timed(vm, call);
        assert.equal(sample.output, expected); delete sample.output;
        kernel.samples.push(sample);
      }
    }
    console.log(`Typed arithmetic round ${round + 1}/${rounds} passed.`);
  }
  const median = values => [...values].sort((a, b) => a - b)[Math.floor(values.length / 2)];
  for (const {record} of variants) for (const kernel of record.kernels)
    kernel.median = {wallMs: median(kernel.samples.map(sample => sample.wallMs)),
      cpuMs: median(kernel.samples.map(sample => sample.cpuMs))};
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify({
    node: process.version, platform: process.platform, architecture: process.arch,
    scope: 'Arithmetic microbenchmark; does not establish application-level improvements',
    iterations, rounds, variants: variants.map(({record}) => record)}, null, 2) + '\n');
  for (let index = 0; index < 9; index++) {
    const left = variants[0].record.kernels[index], right = variants[1].record.kernels[index];
    console.log(`${right.operator} ${right.type}: ${left.median.cpuMs.toFixed(1)} -> ${right.median.cpuMs.toFixed(1)} ms CPU; ${left.bytes} -> ${right.bytes} bytes`);
  }
} finally {for (const {vm} of variants) vm.close();}
