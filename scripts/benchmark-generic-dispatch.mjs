// Compare identical warmed generic calls in precompiled runtime images.
// Usage: node scripts/benchmark-generic-dispatch.mjs [--sbcl] BEFORE.wasm AFTER.wasm
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from './miricl.mjs';
import os from 'node:os';
import path from 'node:path';
import {spawnSync} from 'node:child_process';

const compareSBCL = process.argv.includes('--sbcl');
const paths = process.argv.slice(2).filter(argument => argument !== '--sbcl');
if (!paths.length) paths.push('miricl-precompiled.wasm');
const variants = await Promise.all(paths.map(async file => ({file,
  image: await fs.readFile(file), measurements: []})));
const cases = ['loop-only', 'direct-constant', 'direct-slots', 'single-primary', 'primary-chain',
  'call-next', 'around', 'keyword-primary', 'optional-primary', 'keyword-around'];
const repeats = 100000;
// The host comparison loads exactly this corpus too. Direct calls distinguish
// universal call/slot costs from additional generic dispatch costs.
const definitions = `(defstruct dispatch-record (id 42))
        (defparameter *dispatch-a* (make-dispatch-record))
        (defparameter *dispatch-b* (make-dispatch-record))
        (defun direct-constant (x y) (declare (ignore x y)) t)
        (defun direct-slots (x y) (= (dispatch-record-id x) (dispatch-record-id y)))
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
        (defgeneric keyword-primary (x y &key)
          (:method ((x dispatch-record) (y dispatch-record) &key marker)
            (declare (ignore marker))
            (= (dispatch-record-id x) (dispatch-record-id y))))
        (defgeneric optional-primary (x y &optional marker)
          (:method ((x dispatch-record) (y dispatch-record) &optional marker)
            (declare (ignore marker))
            (= (dispatch-record-id x) (dispatch-record-id y))))
        (defgeneric keyword-around (x y &key)
          (:method ((x dispatch-record) (y dispatch-record) &key marker)
            (declare (ignore marker))
            (= (dispatch-record-id x) (dispatch-record-id y)))
          (:method :around ((x dispatch-record) (y dispatch-record) &key marker)
            (declare (ignore marker)) (call-next-method)))
        (defun dispatch-loop (function count)
          (let ((a *dispatch-a*) (b *dispatch-b*) (result 0))
            (dotimes (i count result)
              (when (funcall function a b) (incf result)))))
        (defun dispatch-loop-control (count)
          (let ((result 0)) (dotimes (i count result) (incf result))))
        (defun dispatch-keyword-loop (function count)
          (let ((a *dispatch-a*) (b *dispatch-b*) (result 0))
            (dotimes (i count result)
              (when (funcall function a b :marker :present) (incf result)))))
        (defun dispatch-optional-loop (function count)
          (let ((a *dispatch-a*) (b *dispatch-b*) (result 0))
            (dotimes (i count result)
              (when (funcall function a b :present) (incf result))))) (values)`;
const workload = (name, count) => name === 'loop-only'
  ? `(dispatch-loop-control ${count})`
  : `(${name.startsWith('keyword-') ? 'dispatch-keyword-loop'
    : name === 'optional-primary' ? 'dispatch-optional-loop' : 'dispatch-loop'} #'${name} ${count})`;
for (let round = 0; round < 3; round++) {
  for (const variant of round % 2 ? variants.toReversed() : variants) {
    const vm = await Miricl.instantiate(variant.image);
    try {
      vm.evaluate(`(miricl-compiler::ensure-compiler)
        (setq miricl-compiler:*compile-evaluations* t) ${definitions}`, {eof: true});
      for (const name of cases) {
        vm.evaluate(workload(name, 1000));
        const cpu = process.cpuUsage(), started = performance.now();
        assert.equal(vm.evaluate(workload(name, repeats)).output, `${repeats}\n`);
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
if (compareSBCL) {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-dispatch-'));
  try {
    const source = path.join(directory, 'benchmark.lisp');
    // More iterations avoid SBCL's timer resolution obscuring short calls.
    const count = repeats * 100;
    await fs.writeFile(source, definitions + `
      (dolist (name '(${cases.join(' ')}))
        (flet ((work (count)
                 (cond ((eq name 'loop-only) (dispatch-loop-control count))
                       ((member name '(keyword-primary keyword-around))
                        (dispatch-keyword-loop (symbol-function name) count))
                       ((eq name 'optional-primary) (dispatch-optional-loop (symbol-function name) count))
                       (t (dispatch-loop (symbol-function name) count)))))
        (work 10000)
        (dotimes (round 3)
          (let ((real (get-internal-real-time)) (cpu (get-internal-run-time)))
            (assert (= ${count} (work ${count})))
            (format t "~A ~D ~,6F ~,6F~%" name round
              (* 1000d0 (/ (- (get-internal-real-time) real) internal-time-units-per-second))
              (* 1000d0 (/ (- (get-internal-run-time) cpu) internal-time-units-per-second)))))))`);
    const host = spawnSync(process.env.SBCL ?? 'sbcl', ['--noinform', '--non-interactive', '--load', source],
      {encoding: 'utf8', timeout: 120000});
    if (host.error) throw host.error;
    if (host.status !== 0) throw new Error(host.stderr || host.stdout);
    const measurements = host.stdout.trim().split('\n').map(line => {
      const [name, round, wall, cpu] = line.trim().split(/\s+/);
      return {name: name.toLowerCase(), round: Number(round), repeats: count,
        wallMs: Number(wall), cpuMs: Number(cpu)};
    });
    variants.push({file: 'SBCL', measurements, medians: cases.map(name => {
      const samples = measurements.filter(sample => sample.name === name);
      return {name, wallMs: median(samples.map(sample => sample.wallMs)) * repeats / count,
        cpuMs: median(samples.map(sample => sample.cpuMs)) * repeats / count};
    }), normalizedRepeats: repeats});
  } finally {await fs.rm(directory, {recursive: true, force: true});}
}
console.log(JSON.stringify(variants, null, 2));
