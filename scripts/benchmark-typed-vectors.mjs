// Compare vector dispatch with identical kernels and inputs in two images.
// node scripts/benchmark-typed-vectors.mjs OUTPUT BEFORE.wasm AFTER.wasm
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';

const [output, before, after] = process.argv.slice(2);
if (!output || !before || !after) throw new Error('Expected OUTPUT BEFORE.wasm AFTER.wasm');
await fs.mkdir(output, {recursive: true});
const iterations = 500000, rounds = 5, variants = [];
const median = values => [...values].sort((a, b) => a - b)[Math.floor(values.length / 2)];
function timed(vm, source) {
  const cpu = process.cpuUsage(), start = performance.now();
  const result = vm.evaluate(source), usage = process.cpuUsage(cpu);
  return {output: result.output, wallMs: performance.now() - start,
    cpuMs: (usage.user + usage.system) / 1000};
}
try {
  for (const [name, imagePath] of [['before', before], ['after', after]]) {
    const image = await fs.readFile(imagePath), vm = await Miricl.instantiate(image);
    const record = {name, imagePath, imageSha256: createHash('sha256').update(image).digest('hex'),
      sizes: measureWasm(imagePath), kernels: []};
    const jobs = [];
    variants.push({vm, record, jobs});
    vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
    for (const policy of ['untyped', 'typed', 'typed-inline']) {
      for (const operation of ['read', 'write', 'copy']) {
        const index = jobs.length, symbol = `vector-benchmark-${index}`;
        const body = operation === 'read' ? '(setq result (svref v 0))'
          : operation === 'write' ? '(setq result (setf (svref v 0) i))'
          : '(setq result (setf (svref v 0) (svref v 1)))';
        const source = `(lambda (v count) (declare (optimize (safety 0))
          (type (unsigned-byte 30) count) ${policy === 'untyped' ? '' : '(type simple-vector v)'}
          ${policy === 'typed-inline' ? '(inline svref (setf svref))' : ''})
          (let ((result nil)) (dotimes (i count result) ${body})))`;
        const compilation = timed(vm, `(progn (multiple-value-bind (bytes literals)
          (emit-native-module (compile-to-hir '${source}))
          (setf (symbol-function '${symbol}) (compiler-primitive 17 bytes literals '${symbol}))
          (with-open-file (out "/vector-kernel.wasm" :direction :output :if-exists :supersede
            :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values))`);
        assert.equal(compilation.output, '');
        const bytes = await vm.readFile('/vector-kernel.wasm');
        await fs.writeFile(path.join(output, `${name}-${index}.wasm`), bytes);
        const imports = WebAssembly.Module.imports(await WebAssembly.compile(bytes)).map(entry => entry.name);
        const call = `(${symbol} (vector 42 43) ${iterations})`;
        const expected = `${operation === 'read' ? 42 : operation === 'write' ? iterations - 1 : 43}\n`;
        const kernel = {operation, policy, source, bytes: bytes.length, imports, compilation, samples: []};
        record.kernels.push(kernel); jobs.push({kernel, call, expected});
        assert.equal(timed(vm, call).output, expected);
      }
    }
  }
  for (let round = 0; round < rounds; round++) {
    for (let index = 0; index < variants[0].jobs.length; index++) {
      for (const variant of round % 2 ? [...variants].reverse() : variants) {
        const {kernel, call, expected} = variant.jobs[index];
        const sample = timed(variant.vm, call);
        assert.equal(sample.output, expected); delete sample.output; kernel.samples.push(sample);
      }
    }
    console.log(`Typed vector round ${round + 1}/${rounds} passed.`);
  }
  for (const {record} of variants) for (const kernel of record.kernels)
    kernel.median = {wallMs: median(kernel.samples.map(sample => sample.wallMs)),
      cpuMs: median(kernel.samples.map(sample => sample.cpuMs))};
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify({
    node: process.version, execArgv: process.execArgv, platform: process.platform, architecture: process.arch,
    scope: 'Vector access microbenchmark; does not establish application-level improvements',
    iterations, rounds, variants: variants.map(({record}) => record)}, null, 2) + '\n');
  for (let index = 0; index < variants[0].jobs.length; index++) {
    const left = variants[0].record.kernels[index], right = variants[1].record.kernels[index];
    console.log(`${right.operation} ${right.policy}: ${left.median.cpuMs.toFixed(1)} -> ${right.median.cpuMs.toFixed(1)} ms CPU; ${left.bytes} -> ${right.bytes} bytes`);
  }
} finally {for (const {vm} of variants) vm.close();}
