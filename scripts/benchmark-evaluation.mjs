// Time immediate native evaluation separately from persistent compilation.
// node scripts/benchmark-evaluation.mjs BEFORE.wasm AFTER.wasm
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from './miricl.mjs';

const variants = await Promise.all(process.argv.slice(2).map(async file =>
  ({file, image: await fs.readFile(file), samples: []})));
if (!variants.length) throw new Error('Expected at least one runtime image');
const cases = [
  ['acyclic', '(let ((x 19) (y 23)) (car (list (+ x y) (- y x) (* x y))))', 1000],
  ['cyclic', '(let ((sum 0)) (dotimes (i 4) (incf sum (+ i 9))) sum)', 500],
];
for (let round = 0; round < 3; round++) {
  for (const variant of round % 2 ? variants.toReversed() : variants) {
    const vm = await Miricl.instantiate(variant.image);
    try {
      vm.evaluate('(miricl-compiler::ensure-compiler) (setq miricl-compiler:*compile-evaluations* t) (values)');
      for (const [name, form, count] of cases) {
        vm.evaluate(`(dotimes (i 20) (eval '${form})) (values)`);
        const usage = process.cpuUsage(), before = performance.now();
        const {output} = vm.evaluate(`(let ((answer 0))
          (dotimes (i ${count}) (incf answer (eval '${form}))) answer)`);
        const elapsed = process.cpuUsage(usage), milliseconds = performance.now() - before;
        assert.equal(output, `${count * 42}\n`, `${variant.file}: ${name}`);
        variant.samples.push({round, name, count, milliseconds,
          cpuMilliseconds: (elapsed.user + elapsed.system) / 1000});
      }
    } finally {vm.close();}
  }
}
console.log(JSON.stringify(variants.map(({file, samples}) => ({file, samples})), null, 2));
