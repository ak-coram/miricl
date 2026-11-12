// Compare invariant-loop performance between two compiler images. Both use
// their ordinary compilation pipeline; no optimization pass is switched off.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';

const [output, baselinePath, currentPath] = process.argv.slice(2);
if (!output || !baselinePath || !currentPath) throw new Error('Expected OUTPUT BASELINE-IMAGE CURRENT-IMAGE');
await fs.mkdir(output, {recursive: true});
const images = [], kernels = [], iterations = 1_000_000, rounds = 5;
const timed = (vm, source) => {
  const start = performance.now(), cpu = process.cpuUsage();
  const result = vm.evaluate(source), elapsed = process.cpuUsage(cpu);
  return {output: result.output, wallMs: performance.now() - start, cpuMs: (elapsed.user + elapsed.system) / 1000};
};
try {
  for (const [name, imagePath] of [['baseline', baselinePath], ['current', currentPath]]) {
    const image = await fs.readFile(imagePath), vm = await Miricl.instantiate(image);
    images.push({name, imagePath, vm, sha256: createHash('sha256').update(image).digest('hex')});
    vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  }
  for (const [name, binding, step, value, expected] of [
    ['tag true', 'x', '(when (consp a) (incf sum))', '(list 1)', iterations],
    ['tag false', 'x', '(when (consp a) (incf sum))', 'nil', 0],
    ['integer true', '(the fixnum x)', '(when (minusp a) (incf sum))', '-1', iterations],
    ['integer false', '(the fixnum x)', '(when (minusp a) (incf sum))', '1', 0],
    ['immediate arithmetic', '(the (integer -100 100) x)', '(incf sum (* a 3))', '10', 30 * iterations],
    ['wide integer mask', '(the fixnum x)', '(incf sum (logand a 7))', '9223372036854775807', 7 * iterations],
  ]) {
    const variants = [];
    const source = `(lambda (x n)
      (declare (type (unsigned-byte 32) n) (optimize (safety 0)) (inline consp minusp + * logand 1+ <))
      (let ((a ${binding}) (sum 0)) (declare (type (unsigned-byte 32) sum))
        (dotimes (i n sum) (declare (type (unsigned-byte 32) i)) ${step})))`;
    for (const {name: version, vm} of images) {
      const compilation = timed(vm, `(progn
        (multiple-value-bind (bytes literals) (emit-native-module (compile-to-hir '${source}))
          (defparameter *licm-benchmark-function* (compiler-primitive 17 bytes literals nil))
          (with-open-file (out "/licm-kernel.wasm" :direction :output :if-exists :supersede
              :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values))`);
      assert.equal(compilation.output, '');
      const bytes = await vm.readFile('/licm-kernel.wasm'); assert.ok(WebAssembly.validate(bytes));
      await fs.writeFile(path.join(output, `${kernels.length}-${version}.wasm`), bytes);
      // Give each kernel its own binding so later compilation cannot replace it.
      const symbol = `licm-benchmark-${kernels.length}`;
      vm.evaluate(`(setf (symbol-function '${symbol}) *licm-benchmark-function*) (values)`);
      const call = `(${symbol} ${value} ${iterations})`;
      assert.equal(timed(vm, call).output, `${expected}\n`);
      variants.push({version, bytes: bytes.length, call, compilation, samples: []});
    }
    kernels.push({name, source, expected, variants});
  }
  for (let round = 0; round < rounds; round++)
    for (const kernel of kernels)
      for (const variant of round % 2 ? kernel.variants.toReversed() : kernel.variants) {
        const vm = images.find(image => image.name === variant.version).vm;
        const sample = timed(vm, variant.call); assert.equal(sample.output, `${kernel.expected}\n`);
        variant.samples.push({round, wallMs: sample.wallMs, cpuMs: sample.cpuMs});
      }
  const median = values => values.toSorted((a, b) => a - b)[Math.floor(values.length / 2)];
  for (const kernel of kernels)
    for (const variant of kernel.variants)
      variant.medians = Object.fromEntries(['wallMs', 'cpuMs'].map(key => [key, median(variant.samples.map(sample => sample[key]))]));
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify({images: images.map(({vm, ...image}) =>
    ({...image, size: measureWasm(image.imagePath)})), iterations, rounds, node: process.version, kernels}, null, 2) + '\n');
  console.log(`Wrote ${path.join(output, 'results.json')}`);
} finally {for (const {vm} of images) vm.close();}
