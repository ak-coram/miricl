// Compare native emission and frontend+emission, excluding bootstrap and loading.
// Comparisons require byte-identical output; warm up before each timed batch.
// Usage: node scripts/benchmark-native-compiler.mjs [--compare-size] IMAGE [COMPARISON_IMAGE ...]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';

const bindings = Array.from({length: 140}, (_, i) => `(v${i} (+ n ${i}))`).join(' ');
const variables = Array.from({length: 140}, (_, i) => `v${i}`).join(' ');
const cases = [
  ['small', '(lambda (x y) (if x (+ y 3) (- y 2)))', 100],
  ['wide-branch', `(lambda (n) (let (${bindings})
    (if (minusp n) (list ${variables}) (list ${variables}))))`, 3],
  ['wide-handler', `(lambda (n flag) (block done (let (${bindings})
    (funcall (lambda () (when flag (return-from done (values v0 v139)))))
    (list ${variables}))))`, 3],
  ['wide-temporaries', `(lambda (n) (list
    ${Array.from({length: 280}, (_, i) => `(+ n ${i})`).join(' ')}))`, 3],
  ['loop', `(lambda (n) (let ((a 1) (b 2))
    (dotimes (i n) (if (evenp i) (incf a b) (incf b a))) (values a b)))`, 50],
];
const compareSize = process.argv.includes('--compare-size');
const paths = process.argv.slice(2).filter(argument => argument !== '--compare-size');
if (!paths.length) paths.push('miricl.wasm');
const variants = await Promise.all(paths.map(async file => ({file,
  image: await fs.readFile(file), measurements: []})));
const expected = new Map();
for (let round = 0; round < 3; round++) {
  for (const variant of round % 2 ? variants.toReversed() : variants) {
    const vm = await Miricl.instantiate(variant.image);
    try {
      vm.evaluate('(miricl-compiler::ensure-compiler) (values)');
      for (const [name, source, repeats] of cases) {
        vm.evaluate(`(defparameter *benchmark-unit* (miricl-compiler:compile-to-hir '${source}))
          (multiple-value-bind (bytes literals) (miricl-compiler:emit-native-module *benchmark-unit*)
            (declare (ignore literals))
            (with-open-file (out "/benchmark.wasm" :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values)`);
        const bytes = await vm.readFile('/benchmark.wasm');
        assert.ok(WebAssembly.validate(bytes), name);
        const hash = createHash('sha256').update(bytes).digest('hex');
        const key = compareSize ? `${variant.file}:${name}` : name;
        if (expected.has(key)) assert.equal(hash, expected.get(key), `${name}: emitted bytes differ`);
        else expected.set(key, hash);
        vm.evaluate(`(dotimes (i ${repeats})
          (miricl-compiler:emit-native-module *benchmark-unit*)) (values)`);
        const cpu = process.cpuUsage(), start = performance.now();
        vm.evaluate(`(dotimes (i ${repeats})
          (miricl-compiler:emit-native-module *benchmark-unit*)) (values)`);
        const wallMs = performance.now() - start, elapsed = process.cpuUsage(cpu);
        const compilation = `(dotimes (i ${repeats})
          (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))) (values)`;
        vm.evaluate(compilation);
        const compileCpu = process.cpuUsage(), compileStart = performance.now();
        vm.evaluate(compilation);
        const compileWallMs = performance.now() - compileStart, compileElapsed = process.cpuUsage(compileCpu);
        variant.measurements.push({round, name, repeats, bytes: bytes.length, sha256: hash,
          wallMs: Math.round(wallMs), cpuMs: Math.round((elapsed.user + elapsed.system) / 1000),
          compileWallMs: Math.round(compileWallMs),
          compileCpuMs: Math.round((compileElapsed.user + compileElapsed.system) / 1000)});
      }
    } finally {vm.close();}
  }
}
const median = values => values.toSorted((a, b) => a - b)[1];
for (const variant of variants) {
  variant.medians = cases.map(([name]) => {
    const samples = variant.measurements.filter(value => value.name === name);
    return {name, wallMs: median(samples.map(value => value.wallMs)),
      cpuMs: median(samples.map(value => value.cpuMs)),
      compileWallMs: median(samples.map(value => value.compileWallMs)),
      compileCpuMs: median(samples.map(value => value.compileCpuMs))};
  });
  delete variant.image;
}
console.log(JSON.stringify(variants, null, 2));
