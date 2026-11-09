// Compare the real algebra packaging pipeline, excluding startup and caches.
// Usage: node scripts/benchmark-fricas-compiler.mjs IMAGE [COMPARISON_IMAGE ...]
// MIRICL_FRICAS_NATIVE and MIRICL_FRICAS_CORE override the prepared inputs.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {gzipSync, brotliCompressSync} from 'node:zlib';
import {fricasFiles} from './fricas-files.mjs';
import {createFriCAS, lispString} from './fricas.mjs';

const {files} = fricasFiles(path.resolve(process.env.MIRICL_FRICAS_NATIVE ?? 'build/fricas/native'));
const core = await fs.readFile(process.env.MIRICL_FRICAS_CORE ?? 'build/fricas/fricas-optimized.wasm');
// Small, medium and large generated sources, all using the ordinary compiler.
const libraries = ['A1AGG-', 'SMP', 'RDEEFX'];
const variants = [];
for (const file of process.argv.slice(2).length ? process.argv.slice(2) : ['miricl-precompiled.wasm']) {
  const image = await fs.readFile(file);
  variants.push({file, image, imageSha256: createHash('sha256').update(image).digest('hex'), imageBytes: image.length,
    imageGzipBytes: gzipSync(image, {level: 9}).length,
    imageBrotliBytes: brotliCompressSync(image).length, measurements: []});
}
const expected = new Map();
for (let round = 0; round < 3; round++) {
  for (const variant of round % 2 ? variants.toReversed() : variants) {
    const session = await createFriCAS(variant.image, core, files);
    try {
      const vm = session.vm;
      await vm.evaluateAsync(`(miricl-compiler::ensure-compiler)
        (miricl-compiler:compile-native '(lambda (x) (+ x 1))) (values)`, {eof: true});
      for (const name of libraries) {
        const measurements = {round, name};
        const source = lispString(`/fricas/algebra/${name}.lsp`);
        for (const [stage, form] of [
          ['compile', `(compile-file ${source} :output-file "/benchmark.wasm")`],
          ['shake', '(miricl-compiler:shake-file "/benchmark.wasm" "/benchmark.wasm" :strip nil)'],
          ['load', '(load "/benchmark.wasm")'],
          ['native', '(miricl-compiler:compile-native-bundle "/benchmark.wasm" "/benchmark.wasm")'],
        ]) {
          const cpu = process.cpuUsage(), start = performance.now();
          await vm.evaluateAsync(`${form} (values)`, {eof: true});
          const elapsed = process.cpuUsage(cpu);
          measurements[stage + 'WallMs'] = Math.round(performance.now() - start);
          measurements[stage + 'CpuMs'] = Math.round((elapsed.user + elapsed.system) / 1000);
        }
        const bytes = await vm.readFile('/benchmark.wasm');
        assert.ok(WebAssembly.validate(bytes), name);
        const hash = createHash('sha256').update(bytes).digest('hex');
        if (expected.has(name)) assert.equal(hash, expected.get(name), `${name}: emitted bytes differ`);
        else expected.set(name, hash);
        variant.measurements.push({...measurements, bytes: bytes.length, sha256: hash});
        console.error(`${variant.file}: round ${round + 1}, ${name}: ${measurements.nativeWallMs} ms native, ${bytes.length} bytes`);
      }
    } finally {session.close();}
  }
}
for (const variant of variants) {
  delete variant.image;
  variant.medians = libraries.map(name => {
    const samples = variant.measurements.filter(sample => sample.name === name);
    const result = {name};
    for (const field of Object.keys(samples[0]).filter(field => field.endsWith('Ms')))
      result[field] = samples.map(sample => sample[field]).toSorted((a, b) => a - b)[1];
    return result;
  });
}
console.log(JSON.stringify(variants, null, 2));
