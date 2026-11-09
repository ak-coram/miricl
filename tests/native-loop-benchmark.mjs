// Generic compiled loop workloads; no application-specific compiler rules.
// node tests/native-loop-benchmark.mjs [IMAGE ...]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {gzipSync} from 'node:zlib';
import {Miricl} from '../scripts/miricl.mjs';

const cases = [
  ['integer-steps', `(lambda ()
    (loop for i from 100000 below 300000
          count (< (- i 100001) 100)))`, '101\n'],
  ['integer-products', `(lambda ()
    (loop for i below 32768 sum (* i i)))`,
    `${32767n * 32768n * 65535n / 6n}\n`],
  ['bit-vector', `(lambda ()
    (let ((bits (make-array 65536 :element-type 'bit :initial-element 0)))
      (dotimes (i 65536) (setf (sbit bits i) 1))
      (loop for i below 65536 sum (sbit bits i))))`, '65536\n'],
  ['native-calls', `(lambda ()
    (let ((last nil)) (dotimes (i 100000 last)
      (setq last (native-loop-step i)))))`, '100000\n'],
];
const reports = [];
for (const image of process.argv.length > 2 ? process.argv.slice(2) : ['miricl.wasm']) {
  const vm = await Miricl.instantiate(await fs.readFile(image));
  const measurements = [];
  try {
    vm.evaluate(`(setf (symbol-function 'native-loop-step)
      (miricl-compiler:compile-native '(lambda (x) (+ x 1)))) (values)`);
    for (const [name, source, expected] of cases) {
      vm.evaluate(`(multiple-value-bind (bytes literals)
        (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))
        (declare (ignore literals))
        (with-open-file (out "/loop-code.wasm" :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8)) (write-sequence bytes out)))
        (defparameter *native-loop-run* (miricl-compiler:compile-native '${source})) (values)`);
      const bytes = await vm.readFile('/loop-code.wasm');
      const run = () => assert.equal(vm.evaluate('(funcall *native-loop-run*)', {eof: true}).output, expected, name);
      run(); run();
      const samples = [];
      for (let i = 0; i < 5; i++) {
        const start = performance.now(); run(); samples.push(performance.now() - start);
      }
      measurements.push({name, rawBytes: bytes.length, gzipBytes: gzipSync(bytes, {level: 9}).length,
        milliseconds: samples.toSorted((a,b) => a-b)[2], samples});
    }
    reports.push({image, measurements});
  } finally {vm.close();}
}
console.log(JSON.stringify(reports, null, 2));
