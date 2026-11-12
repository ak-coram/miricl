// Compare snapshot size and save/restore time with fresh heaps. Compression
// runs after timed samples. An optional HIR payload exercises compiler code.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {pathToFileURL} from 'node:url';
import {measureWasm, measureFile} from './wasm-size.mjs';

const [output, imagePath, ...options] = process.argv.slice(2);
if (!output || !imagePath) throw new Error('Expected OUTPUT IMAGE [--compare ADAPTER.mjs] [--hir PAYLOAD.wasm] [--compare-image IMAGE] [--compare-hir PAYLOAD.wasm]');
let baseline, hirPath, baselineImagePath, baselineHirPath;
for (let i = 0; i < options.length; i += 2) {
  assert.ok(options[i + 1], 'Missing option value');
  if (options[i] === '--compare') baseline = path.resolve(options[i + 1]);
  else if (options[i] === '--hir') hirPath = options[i + 1];
  else if (options[i] === '--compare-image') baselineImagePath = options[i + 1];
  else if (options[i] === '--compare-hir') baselineHirPath = options[i + 1];
  else throw new Error(`Unknown option ${options[i]}`);
}
const adapters = [...(baseline ? [baseline] : []), path.resolve('scripts/miricl.mjs')];
assert.ok(baseline || (!baselineImagePath && !baselineHirPath), 'Comparison inputs require --compare');
assert.ok(!baselineHirPath || hirPath, '--compare-hir requires --hir');
const variants = await Promise.all(adapters.map(async (file, i) => {
  const inputImage = i === 0 && baselineImagePath ? baselineImagePath : imagePath;
  const inputHir = i === 0 && baselineHirPath ? baselineHirPath : hirPath;
  return {file, imagePath: inputImage, hirPath: inputHir,
    image: await fs.readFile(inputImage), hir: inputHir && await fs.readFile(inputHir),
    bytes: await fs.readFile(file), Miricl: (await import(pathToFileURL(file))).Miricl};
}));
const rounds = Number(process.env.MIRICL_BENCHMARK_ROUNDS ?? 3);
assert.ok(Number.isSafeInteger(rounds) && rounds > 0, 'Expected positive sample count');
const scenarios = ['empty', ...(hirPath ? ['hir'] : [])];
const evaluate = (vm, forms) => assert.equal(vm.evaluate(`(progn ${forms} (values))`).output, '');
await fs.mkdir(output, {recursive: true});
const samples = [], artifacts = [];
for (let round = 0; round < rounds; round++) {
  const order = variants.map((_, i) => i);
  if (round % 2) order.reverse();
  for (const scenario of scenarios) for (const variant of order) {
    const {Miricl, image, hir} = variants[variant];
    const vm = await Miricl.instantiate(image);
    let restored;
    try {
      if (scenario === 'hir') {
        vm.mountFile('/snapshot-hir.wasm', hir);
        evaluate(vm, `(load "/snapshot-hir.wasm" :verbose nil)
          (setq miricl-compiler::*hir-bootstrap-sources* nil)
          (delete-file "/snapshot-hir.wasm")`);
      }
      let start = performance.now();
      const bytes = vm.saveImage(), saveMs = performance.now() - start;
      start = performance.now();
      restored = await Miricl.instantiate(bytes);
      const restoreMs = performance.now() - start;
      assert.equal(restored.evaluate('(= (+ 40 2) 42)').output, 'T\n');
      if (scenario === 'hir') assert.equal(restored.evaluate(`(every (lambda (name)
          (and (null (function-lambda-expression (symbol-function name)))
               (miricl-compiler::compiler-primitive 43 (symbol-function name))))
        miricl-compiler::*native-compiler-seeds*)`).output, 'T\n');
      samples.push({round, scenario, variant, saveMs, restoreMs, rawBytes: bytes.length});
      console.log(`Round ${round + 1}, ${scenario}, adapter ${variant}: ${bytes.length} bytes, save ${saveMs.toFixed(0)} ms, restore ${restoreMs.toFixed(0)} ms`);
      if (round === 0) {
        const file = path.join(output, `${scenario}-${variant}.wasm`);
        await fs.writeFile(file, bytes);
        artifacts.push({scenario, variant, file});
      }
    } finally {restored?.close(); vm.close();}
  }
}
const adapterSizes = [];
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
for (const [variant, {file, bytes, imagePath, image, hirPath, hir}] of variants.entries()) {
  const copy = path.join(output, `adapter-${variant}.mjs`);
  await fs.writeFile(copy, bytes);
  adapterSizes.push({source: file, imagePath, imageSha256: hash(image), hirPath,
    hirSha256: hir && hash(hir), ...measureFile(copy)});
}
await fs.writeFile(path.join(output, 'results.json'), JSON.stringify({imagePath, hirPath,
  adapters: adapterSizes, samples,
  artifacts: artifacts.map(({file, ...entry}) => ({...entry, sizes: measureWasm(file)})),
}, null, 2) + '\n');
