// Compare scalar predicate lowering with unbounded/untyped controls.
// node scripts/benchmark-typed-predicates.mjs OUTPUT BEFORE.wasm AFTER.wasm
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';

const [output, before, after] = process.argv.slice(2);
if (!output || !before || !after) throw new Error('Expected OUTPUT BEFORE.wasm AFTER.wasm');
await fs.mkdir(output, {recursive: true});
const variants = [], iterations = 1000000, rounds = 5;
const timed = (vm, source) => {
  const cpu = process.cpuUsage(), start = performance.now();
  const result = vm.evaluate(source), usage = process.cpuUsage(cpu);
  return {output: result.output, wallMs: performance.now() - start, cpuMs: (usage.user + usage.system) / 1000};
};
try {
  for (const [name, imagePath] of [['before', before], ['after', after]]) {
    const image = await fs.readFile(imagePath), vm = await Miricl.instantiate(image);
    const record = {name, imagePath, imageSha256: createHash('sha256').update(image).digest('hex'),
      image: measureWasm(imagePath), kernels: []};
    variants.push({vm, record});
    vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
    for (const type of ['fixnum', 'integer', 't']) {
      for (const [operator, value] of [['zerop', 0], ['minusp', -1], ['plusp', 1], ['evenp', 8], ['oddp', 7]]) {
        const index = record.kernels.length, symbol = `predicate-benchmark-${index}`;
        const source = `(lambda (x count)
          (declare (type ${type} x) (type (unsigned-byte 32) count) (optimize (safety 0))
            (inline ${operator} 1+ + <))
          (let ((answer 0)) (declare (type (unsigned-byte 32) answer))
            (dotimes (i count answer) (declare (type (unsigned-byte 32) i))
              (when (${operator} x) (incf answer)))))`;
        const compilation = timed(vm, `(progn (multiple-value-bind (bytes literals)
          (emit-native-module (compile-to-hir '${source}))
          (setf (symbol-function '${symbol}) (compiler-primitive 17 bytes literals '${symbol}))
          (with-open-file (out "/predicate-kernel.wasm" :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values))`);
        assert.equal(compilation.output, '');
        const bytes = await vm.readFile('/predicate-kernel.wasm');
        assert.ok(WebAssembly.validate(bytes));
        await fs.writeFile(path.join(output, `${name}-${index}.wasm`), bytes);
        const imports = WebAssembly.Module.imports(new WebAssembly.Module(bytes)).map(entry => entry.name);
        const call = `(${symbol} ${value} ${iterations})`;
        assert.equal(timed(vm, call).output, `${iterations}\n`);
        record.kernels.push({operator, type, source, call, bytes: bytes.length, imports, compilation, samples: []});
      }
    }
  }
  for (let round = 0; round < rounds; round++) {
    for (let index = 0; index < variants[0].record.kernels.length; index++) {
      for (const variant of round % 2 ? variants.toReversed() : variants) {
        const kernel = variant.record.kernels[index];
        const result = timed(variant.vm, kernel.call);
        assert.equal(result.output, `${iterations}\n`);
        kernel.samples.push({round, wallMs: result.wallMs, cpuMs: result.cpuMs});
      }
    }
  }
  const median = numbers => numbers.toSorted((a, b) => a - b)[Math.floor(numbers.length / 2)];
  for (const {record} of variants) {
    for (const kernel of record.kernels)
      kernel.medians = Object.fromEntries(['wallMs', 'cpuMs'].map(key => [key, median(kernel.samples.map(s => s[key]))]));
  }
  const results = {iterations, rounds, node: process.version, variants: variants.map(v => v.record)};
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify(results, null, 2) + '\n');
  console.log(`Wrote ${path.join(output, 'results.json')}`);
} finally {for (const {vm} of variants) vm.close();}
