// Direct Wasm kernel calls, bypassing general Lisp application and JS interop.
// Prepare diagnostic images with scripts/prepare-runtime-kernel-benchmark.lisp.
// node scripts/benchmark-runtime-kernels.mjs [--kernel=NAME] [--sbcl] IMAGE ...
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
import assert from 'node:assert/strict';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';

const args = process.argv.slice(2), sbclOnly = args.includes('--sbcl-only');
const compareSBCL = args.includes('--sbcl') || sbclOnly;
const selected = args.find(arg => arg.startsWith('--kernel='))?.slice(9);
const available = ['property-cell', 'member-eq', 'list-eq', 'reverse-copy', 'reverse-mutate'];
const kernels = selected ? [selected] : available;
for (const kernel of kernels) assert.ok(available.includes(kernel), `Unknown kernel: ${kernel}`);
const reversal = kernel => kernel.startsWith('reverse-');
const sampleKeys = (kernel, size) => reversal(kernel) ? [0] : [0, size - 1, -1];
const sampleCount = kernel => kernel === 'reverse-copy' ? 100000 : kernel === 'reverse-mutate' ? 100000 : 1000000;
for (const arg of args.filter(arg => arg.startsWith('--')))
  assert.ok(arg === '--sbcl' || arg === '--sbcl-only' || arg.startsWith('--kernel='), `Unknown option: ${arg}`);
const files = args.filter(arg => !arg.startsWith('--'));
assert.ok(files.length || sbclOnly, 'Supply images prepared by prepare-runtime-kernel-benchmark.lisp');
assert.ok(!sbclOnly || !files.length, '--sbcl-only accepts no Wasm images');
const variants = await Promise.all(files.map(async file => ({file, bytes: await fs.readFile(file), samples: []})));
const sizes = [4, 16, 64], rounds = 7;
for (let round = 0; round < rounds; round++) {
  for (const variant of round % 2 ? variants.toReversed() : variants) {
    const vm = await Miricl.instantiate(variant.bytes);
    try {
      for (const kernel of kernels) {
        const run = vm.vm[`benchmark-${kernel}`];
        assert.equal(typeof run, 'function', `Missing diagnostic export benchmark-${kernel} in ${variant.file}`);
        for (const size of sizes) for (const key of sampleKeys(kernel, size)) {
          const count = sampleCount(kernel);
          const expected = reversal(kernel) ? size : kernel === 'list-eq' ? Number(key < 0) : kernel === 'member-eq' ? Number(key >= 0) : key;
          assert.equal(run(10000, size, key), expected);
          const start = performance.now();
          assert.equal(run(count, size, key), expected);
          variant.samples.push({round, kernel, size, key, count, milliseconds: performance.now() - start});
        }
      }
    } finally {vm.close();}
  }
}
if (compareSBCL) {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-kernel-bench-'));
  const lispString = text => `"${text.replace(/["\\]/g, '\\$&')}"`;
  try {
    const source = fileURLToPath(new URL('../runtime-kernel-library.lisp', import.meta.url));
    const program = path.join(directory, 'benchmark.lisp');
    await fs.writeFile(program, `
      (load ${lispString(source)})
      (defun run-property (items key count)
        (declare (type fixnum count))
        (let ((result nil))
          (dotimes (i count result)
            (setq result (miricl-runtime:property-cell items key)))))
      (defun run-member (items key count)
        (declare (type fixnum count))
        (let ((result nil))
          (dotimes (i count result)
            (setq result (miricl-runtime:member-eq key items)))))
      (defun run-list-eq (items other count)
        (declare (type fixnum count))
        (let ((result nil))
          (dotimes (i count result)
            (setq result (miricl-runtime:list-eq items other)))))
      (defun run-reverse-copy (items key count)
        (declare (ignore key) (type fixnum count))
        (dotimes (i count (length items))
          (setq items (miricl-runtime:reverse-copy-onto
                        (miricl-runtime:reverse-copy-onto items nil) nil))))
      (defun run-reverse-mutate (items key count)
        (declare (ignore key) (type fixnum count))
        (dotimes (i count (length items))
          (setq items (miricl-runtime:reverse-mutate-onto
                        (miricl-runtime:reverse-mutate-onto items nil) nil))))
      (write-string "[")
      (let ((first t))
        (dotimes (round ${rounds})
          (dolist (kernel '(${kernels.map(kernel => `:${kernel}`).join(' ')}))
            (dolist (size '(${sizes.join(' ')}))
              (let ((items (loop for i below size
                                append (if (eq kernel :property-cell) (list i i) (list i))))
                    (run (case kernel (:property-cell #'run-property) (:member-eq #'run-member)
                                      (:list-eq #'run-list-eq) (:reverse-copy #'run-reverse-copy)
                                      (:reverse-mutate #'run-reverse-mutate)))
                    (count (case kernel (:reverse-copy 100000) (:reverse-mutate 100000) (otherwise 1000000))))
                (dolist (key (if (member kernel '(:reverse-copy :reverse-mutate)) '(0) (list 0 (1- size) -1)))
                  (let ((argument (if (eq kernel :list-eq)
                                      (loop for item in items for index from 0
                                            collect (if (= index key) -1 item)) key)))
                  (funcall run items argument 10000)
                  (let* ((start (get-internal-real-time))
                         (result (funcall run items argument count))
                         (elapsed (* 1000d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
                    (assert (if (member kernel '(:reverse-copy :reverse-mutate)) (= result size)
                              (if (eq kernel :list-eq) (eq (minusp key) (not (null result)))
                                (if (minusp key) (null result)
                                    (if (eq kernel :property-cell) (= (car result) key) result)))))
                    (unless first (write-string ",")) (setq first nil)
                    (format t "{~S:~D,~S:~S,~S:~D,~S:~D,~S:~D,~S:~,6F}"
                      "round" round "kernel" (string-downcase (symbol-name kernel)) "size" size "key" key
                      "count" count "milliseconds" elapsed)))))))))
      (write-string "]")`);
    const result = spawnSync(process.env.SBCL ?? 'sbcl', ['--noinform', '--script', program], {encoding: 'utf8'});
    if (result.error) throw result.error;
    assert.equal(result.status, 0, result.stderr);
    variants.push({file: 'SBCL', samples: JSON.parse(result.stdout)});
  } finally {await fs.rm(directory, {recursive: true, force: true});}
}
// Compression runs after timing, outside the measured samples.
for (const variant of variants) {
  delete variant.bytes;
  if (variant.file !== 'SBCL') variant.image = measureWasm(variant.file);
  variant.medians = kernels.flatMap(kernel => sizes.flatMap(size => sampleKeys(kernel, size).map(key => {
    const samples = variant.samples.filter(sample => sample.kernel === kernel && sample.size === size && sample.key === key);
    return {kernel, size, key, count: sampleCount(kernel), milliseconds: samples.map(sample => sample.milliseconds).sort((a, b) => a - b)[Math.floor(samples.length / 2)]};
  })));
}
console.log(JSON.stringify(variants, null, 2));
