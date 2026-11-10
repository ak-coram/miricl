// Compare identical warmed generic calls in precompiled runtime images.
// Usage: node scripts/benchmark-generic-dispatch.mjs BEFORE.wasm AFTER.wasm
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from './miricl.mjs';

const paths = process.argv.slice(2);
if (!paths.length) paths.push('miricl-precompiled.wasm');
const variants = await Promise.all(paths.map(async file => ({file,
  image: await fs.readFile(file), measurements: []})));
const cases = ['single-primary', 'primary-chain', 'call-next', 'around'];
const repeats = 100000;
for (let round = 0; round < 3; round++) {
  for (const variant of round % 2 ? variants.toReversed() : variants) {
    const vm = await Miricl.instantiate(variant.image);
    try {
      vm.evaluate(`(miricl-compiler::ensure-compiler)
        (setq miricl-compiler:*compile-evaluations* t)
        (defstruct dispatch-record (id 42))
        (defparameter *dispatch-a* (make-dispatch-record))
        (defparameter *dispatch-b* (make-dispatch-record))
        (defgeneric single-primary (x y)
          (:method ((x dispatch-record) (y dispatch-record))
            (= (dispatch-record-id x) (dispatch-record-id y))))
        (defgeneric primary-chain (x y)
          (:method ((x t) (y t)) nil)
          (:method ((x dispatch-record) (y dispatch-record))
            (= (dispatch-record-id x) (dispatch-record-id y))))
        (defgeneric call-next (x y)
          (:method ((x t) (y t)) nil)
          (:method ((x dispatch-record) (y dispatch-record))
            (or (call-next-method)
                (= (dispatch-record-id x) (dispatch-record-id y)))))
        (defgeneric around (x y)
          (:method ((x dispatch-record) (y dispatch-record))
            (= (dispatch-record-id x) (dispatch-record-id y)))
          (:method :around ((x dispatch-record) (y dispatch-record))
            (call-next-method)))
        (defun dispatch-loop (function count)
          (let ((a *dispatch-a*) (b *dispatch-b*) (result 0))
            (dotimes (i count result)
              (when (funcall function a b) (incf result))))) (values)`, {eof: true});
      for (const name of cases) {
        vm.evaluate(`(dispatch-loop #'${name} 1000)`);
        const cpu = process.cpuUsage(), started = performance.now();
        assert.equal(vm.evaluate(`(dispatch-loop #'${name} ${repeats})`).output, `${repeats}\n`);
        const wallMs = performance.now() - started, used = process.cpuUsage(cpu);
        variant.measurements.push({round, name, repeats, wallMs: Math.round(wallMs),
          cpuMs: Math.round((used.user + used.system) / 1000)});
      }
    } finally {vm.close();}
  }
}
const median = values => values.toSorted((a, b) => a - b)[1];
for (const variant of variants) {
  delete variant.image;
  variant.medians = cases.map(name => {
    const samples = variant.measurements.filter(value => value.name === name);
    return {name, wallMs: median(samples.map(value => value.wallMs)),
      cpuMs: median(samples.map(value => value.cpuMs))};
  });
}
console.log(JSON.stringify(variants, null, 2));
