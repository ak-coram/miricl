// Behavioral fixture for the host adapter's explicit fixed callback contract.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';

for (const fixture of JSON.parse(await fs.readFile(process.argv[2], 'utf8'))) {
  const {instance: {exports: vm}} = await WebAssembly.instantiate(await fs.readFile(fixture.image));
  for (const [callback, selector, object, extra, result, mode] of fixture.runs) {
    for (const active of [1, 0]) {
      vm.active.value = active;
      const before = Object.fromEntries(['polls', 'clears', 'direct', 'ordinary'].map(name => [name, vm[name].value]));
      if (result === 'transfer') {
        assert.throws(() => vm.run(callback, selector, object, extra), WebAssembly.Exception, fixture.name);
        assert.equal(vm.clears.value, before.clears, 'Nonlocal transfer must bypass normal-return value cleanup.');
      } else {
        assert.equal(vm.run(callback, selector, object, extra), result, fixture.name);
        assert.ok(vm.clears.value > before.clears, 'Successful primary calls clear transient values.');
      }
      if (active) assert.ok(vm.polls.value > before.polls, 'Active threads checkpoint before invoking callbacks.');
      else assert.equal(vm.polls.value, before.polls);
      if (mode === 'direct') {
        assert.ok(vm.direct.value > before.direct, 'The saved callback uses its registered checked implementation.');
        assert.equal(vm.ordinary.value, before.ordinary);
      } else {
        assert.ok(vm.ordinary.value > before.ordinary, 'Replacement and mixed incoming callbacks use ordinary dispatch.');
        if (mode === 'mixed') assert.ok(vm.direct.value > before.direct, 'An earlier callee snapshot survives mutation during argument evaluation.');
      }
    }
  }
}
console.log('Runtime kernel callbacks: copies, replacements, mixed helpers, recursive entry, checkpoints, transfers and recovery pass.');
