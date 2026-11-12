// Private fixed-call packets must retain Lisp argument identity and bounds.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
try {
  // A native entry returning its private argument packet lets us test the ABI
  // directly, including malformed indices that source/HIR validation rejects.
  const entry = new WebAssembly.Instance(new WebAssembly.Module(Uint8Array.from([
    0,97,115,109,1,0,0,0,
    1,7,1,96,2,111,111,1,111,
    3,2,1,0,
    7,8,1,4,97,114,103,115,0,0,
    10,6,1,4,0,32,1,11,
  ]))).exports.args;
  const parent = vm.vm['native-function'](entry, null, null, 0);
  const values = [null, vm.javascript.makeInteger(42n), vm.javascript.makeText('argument'),
    vm.javascript.makeInteger(-5n)];
  const fails = (action, reason) => assert.throws(action, cause => {
    assert.match(vm.failure(cause).message, reason);
    return true;
  });
  for (let arity = 1; arity <= values.length; arity++) {
    const fn = vm.vm['native-close-fixed'](entry, parent, null, arity);
    const packet = vm.vm[`native-call${arity}`](fn, ...values.slice(0, arity));
    const kind = vm.vm['core-object-type'](packet);
    const length = vm.vm[`core-array:${kind}:r`];
    assert.equal(typeof length, 'function', 'A fixed call uses a compact reference array');
    assert.equal(length(packet), arity);
    for (let index = 0; index < arity; index++)
      assert.equal(vm.vm['native-argument'](packet, index), values[index]);
    assert.equal(vm.vm['native-argument'](packet, arity), null);
    assert.equal(vm.vm['native-argument'](packet, 0x7fffffff), null);
    assert.deepEqual(vm.javascript.list(vm.vm['native-rest-argument'](packet, 0), value => value),
      values.slice(0, arity));
    assert.equal(vm.vm['native-rest-argument'](packet, arity), null);
    vm.vm['native-check-arguments'](packet, arity, arity);
    fails(() => vm.vm['native-check-arguments'](packet, arity + 1, arity + 1), /argument/i);
    fails(() => vm.vm['native-rest-argument'](packet, -1), /malformed/i);
    fails(() => vm.vm['native-rest-argument'](packet, -2147483648), /malformed/i);
    fails(() => vm.vm['native-argument'](packet, -1), /malformed/i);
    fails(() => vm.vm['native-argument'](packet, -2147483648), /malformed/i);
    assert.equal(vm.vm['native-argument'](packet, arity - 1), values[arity - 1]);
    assert.equal(vm.evaluate('(+ 40 2)').output, '42\n');
  }
  console.log('Native argument arrays: fixed calls, identity, REST, bounds, arity and recovery pass.');
} finally {vm.close();}
