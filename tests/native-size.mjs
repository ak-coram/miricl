import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {gzipSync} from 'node:zlib';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  const sizes = [];
  for (const [name, source, limit, calls] of [
    ['branch', '(lambda (flag) (if flag 1 2))', 280, [['t', '1\n'], ['nil', '2\n']]],
    ['nested', '(lambda (x y) (if x (if y 1 2) (if y 3 4)))', 380,
      [['t t', '1\n'], ['t nil', '2\n'], ['nil t', '3\n'], ['nil nil', '4\n']]],
    ['exits', '(lambda (x y) (block done (if x (return-from done (values 1 2))) (if y (return-from done (values))) 3))', 450,
      [['t nil', '1\n2\n'], ['nil t', ''], ['nil nil', '3\n']]],
  ]) {
    vm.evaluate(`(multiple-value-bind (bytes literals)
      (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))
      (declare (ignore literals))
      (with-open-file (out "/code.wasm" :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`);
    const bytes = await vm.readFile('/code.wasm');
    assert.ok(WebAssembly.validate(bytes));
    assert.ok(bytes.length <= limit, `${name}: ${bytes.length} bytes exceeds ${limit}.`);
    vm.evaluate(`(defparameter *size-function* (miricl-compiler:compile-native '${source}))`);
    for (const [args, output] of calls)
      assert.equal(vm.evaluate(`(funcall *size-function* ${args})`).output, output, `${name}: ${args}`);
    sizes.push(`${name} ${bytes.length} raw/${gzipSync(bytes, {level: 9}).length} gzip`);
  }
  console.log(`Native size budgets: ${sizes.join('; ')} bytes; branch and multiple-value paths pass.`);
} finally {vm.close();}
