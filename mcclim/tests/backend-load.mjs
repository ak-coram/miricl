import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? new URL('../build/mcclim-core.wasm', import.meta.url)));
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  for (const name of ['package', 'port', 'input', 'windows', 'medium', 'fonts', 'bitmaps']) {
    const file = `/browser-${name}`;
    vm.mountFile(`${file}.lisp`, await fs.readFile(new URL(`../backend/${name}.lisp`, import.meta.url)));
    console.log(`Compile/load browser backend ${name}`);
    vm.evaluate(`(compile-file "${file}.lisp") (load "${file}.wasm") (values)`, {eof: true});
  }
  assert.equal(vm.evaluate(`(and
    (eq (nth-value 0 (climb:find-port-type :browser)) 'mcclim-browser:browser-port)
    (find-class 'mcclim-browser:browser-medium)
    (fboundp 'mcclim-browser::install-input-handlers))`).output, 'T\n');
  console.log('PASS browser backend definitions compile and load without DOM initialization');
} finally {vm.close();}
