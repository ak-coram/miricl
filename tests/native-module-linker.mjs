// Build-time linking must preserve transfer tags and function index spaces.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {linkNativeModules} from '../scripts/native-module-linker.mjs';

const word = value => {
  const bytes = [];
  do {const byte = value & 127; value >>>= 7; bytes.push(byte | (value ? 128 : 0));} while (value);
  return bytes;
};
const name = value => {const bytes = [...new TextEncoder().encode(value)]; return [...word(bytes.length), ...bytes];};
const section = (kind, bytes) => [kind, ...word(bytes.length), ...bytes];
const body = instructions => {const bytes = [0, ...instructions, 0x0b]; return [...word(bytes.length), ...bytes];};
const fixture = ({tagName = 'transfer', payload = false, catchIndex = 0} = {}) => {
  const throwing = [...(payload ? [0xd0, 0x6f] : []), 0x08, 0];
  return Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    ...section(1, [3, 0x60, 1, 0x6f, 1, 0x6f,
      0x60, 2, 0x6f, 0x6f, 1, 0x6f,
      0x60, payload ? 1 : 0, ...(payload ? [0x6f] : []), 0]),
    // The tag sits between two function imports but occupies no function slot.
    ...section(2, [3, ...name('m'), ...name('before'), 0, 0,
      ...name('m'), ...name(tagName), 4, 0, 2,
      ...name('m'), ...name('after'), 0, 0]),
    ...section(3, [2, 1, 1]),
    ...section(7, [1, ...name('entry'), 0, 3]),
    ...section(10, [2,
      ...body([0x20, 0, 0x10, 0, 0x10, 1]),
      ...body([0x20, 1, 0xd1, 0x04, 0x40, ...throwing, 0x0b,
        0x02, payload ? 0x6f : 0x40,
        0x1f, 0x40, 1, 0, catchIndex, 0, ...throwing, 0x0b,
        ...(payload ? [0xd0, 0x6f] : []), 0x0b, ...(payload ? [0x1a] : []),
        0x20, 0, 0x20, 1, 0x10, 2]),
    ]),
  ]);
};
const check = bytes => {
  const transfer = new WebAssembly.Tag({parameters: []}), events = [];
  const original = {original: true}, before = {before: true}, after = {after: true};
  const module = new WebAssembly.Instance(new WebAssembly.Module(bytes), {m: {
    transfer,
    before(value) {events.push('before'); assert.equal(value, original); return before;},
    after(value) {events.push('after'); assert.equal(value, before); return after;},
  }});
  assert.equal(module.exports.entry(original, original), after);
  assert.deepEqual(events, ['before', 'after']);
  assert.throws(() => module.exports.entry(original, null),
    error => error instanceof WebAssembly.Exception && error.is(transfer));
  assert.deepEqual(events, ['before', 'after'], 'An uncaught transfer skips subsequent calls.');
  assert.equal(module.exports.entry(original, original), after, 'A caught call still works after propagation.');
};

// Exercise the shared body optimizer through real module decoding and writing.
// Copies have repeated consumers, so keeping a value on the stack alone cannot
// remove their traffic. Joins retain locals used after the first load; other
// entries invalidate copy and join facts deliberately.
const optimizerFixture = `(module
  (type $box (struct (field i32)))
  (import "m" "hook" (func $hook (param externref) (result externref)))
  (import "m" "fail" (func $fail))
  (func (export "copies") (param externref externref) (result externref externref)
    (local externref)
    local.get 0 local.set 2 local.get 2 local.get 2)
  (func (export "source-write") (param externref externref) (result externref externref)
    (local externref)
    local.get 0 local.set 2 local.get 1 local.set 0 local.get 2 local.get 0)
  (func (export "copy-write") (param externref externref) (result externref externref)
    (local externref)
    local.get 0 local.set 2 local.get 1 local.set 2 local.get 2 local.get 2)
  (func (export "branch") (param i32 externref externref) (result externref externref)
    (local externref)
    local.get 1 local.set 3
    local.get 0 if local.get 2 local.set 1 end
    local.get 3 local.get 1)
  (func (export "loop") (param i32 externref externref) (result externref externref)
    (local externref)
    local.get 1 local.set 3
    block loop
      local.get 0 i32.eqz br_if 1
      local.get 2 local.set 1
      local.get 0 i32.const 1 i32.sub local.set 0 br 0
    end end
    local.get 3 local.get 1)
  (func (export "calls") (param externref externref) (result externref externref)
    (local externref)
    local.get 0 local.set 2 local.get 0 call $hook drop local.get 2 local.get 2)
  (func (export "landing") (param externref externref) (result externref)
    (local externref)
    local.get 0 local.set 2
    block $caught
      try_table (catch_all $caught)
        local.get 1 local.set 0 call $fail
      end
    end
    local.get 2)
  (func (export "ref-branch") (param externref externref) (result externref externref)
    (local externref)
    local.get 0 local.set 2
    block
      local.get 1 br_on_null 0 local.set 0
    end
    local.get 2 local.get 0)
  ;; Several exits and a real fallthrough; the local remains live after its
  ;; first postjoin GET, so materializing only the stack value is insufficient.
  (func (export "join") (param i32 externref externref) (result externref externref)
    (local externref)
    block $done
      local.get 0 i32.eqz
      if local.get 1 local.set 3 br $done end
      local.get 0 i32.const 1 i32.eq
      if local.get 2 local.set 3 br $done end
      local.get 1 local.set 3
    end
    local.get 3 local.get 3)
  ;; A branch crosses IF, LOOP and BLOCK frames; another path iterates before
  ;; reaching the same branch, while zero takes the explicit fallthrough.
  (func (export "nested") (param i32 externref externref) (result externref externref)
    (local externref)
    block $done
      block $inner
        loop $again
          local.get 0 i32.eqz if br $inner end
          local.get 0 i32.const 1 i32.eq
          if local.get 1 local.set 3 br $done end
          local.get 0 i32.const 1 i32.sub local.set 0
          br $again
        end
      end
      local.get 2 local.set 3
    end
    local.get 3 local.get 3)
  ;; A conditional edge cannot carry the value left by removing its earlier
  ;; store: its condition is the top operand, and untaken paths still need it.
  (func (export "conditional") (param i32 externref externref) (result externref externref)
    (local externref)
    block $done
      local.get 1 local.set 3
      local.get 0 br_if $done
      local.get 2 local.set 3
    end
    local.get 3 local.get 3)
  (func (export "table") (param i32 externref externref) (result externref externref)
    (local externref)
    block $done
      block $fallthrough
        local.get 1 local.set 3
        local.get 0 br_table $done $fallthrough
      end
      local.get 2 local.set 3
    end
    local.get 3 local.get 3)
  ;; The exception edge enters the join without supplying an operand-stack
  ;; result; it still observes the local assigned before the throwing call.
  (func (export "landing-join") (param externref externref) (result externref)
    (local externref)
    local.get 1 local.set 2
    block $done
      try_table (catch_all $done)
        local.get 0 local.set 2 call $fail
      end
      local.get 1 local.set 2 br $done
    end
    local.get 2)
  ;; Preserve an operand inherited from outside the result block as well as
  ;; the block's newly produced nullable reference result.
  (func (export "inherited-stack") (param i32 externref externref) (result externref externref)
    (local externref)
    local.get 1
    block $done
      local.get 0 if local.get 1 local.set 3 br $done end
      local.get 2 local.set 3
    end
    local.get 3)
  (func (export "initialized") (param i32 i32) (result i32)
    (local (ref $box) (ref $box))
    local.get 0 struct.new $box local.set 2
    local.get 2 local.set 3
    local.get 1 struct.new $box local.set 2
    local.get 3 struct.get $box 0))`;

const checkOptimized = bytes => {
  assert.ok(WebAssembly.validate(bytes), 'The optimized module must validate.');
  const original = {original: true}, replacement = {replacement: true};
  const failure = new Error('optimizer callback'), events = [];
  let reenter = true, throwing = false, entries;
  entries = new WebAssembly.Instance(new WebAssembly.Module(bytes), {m: {
    hook(value) {
      events.push(value);
      if (throwing) throw failure;
      if (reenter) {
        reenter = false;
        assert.deepEqual(entries.calls(replacement, original), [replacement, replacement]);
        reenter = true;
      }
      return replacement;
    },
    fail() {throw failure;},
  }}).exports;
  assert.deepEqual(entries.copies(original, replacement), [original, original]);
  assert.deepEqual(entries['source-write'](original, replacement), [original, replacement]);
  assert.deepEqual(entries['copy-write'](original, replacement), [replacement, replacement]);
  for (const flag of [0, 1]) {
    assert.deepEqual(entries.branch(flag, original, replacement), [original, flag ? replacement : original]);
    assert.deepEqual(entries.loop(flag * 3, original, replacement), [original, flag ? replacement : original]);
  }
  assert.deepEqual(entries.calls(original, replacement), [original, original]);
  assert.deepEqual(events, [original, replacement]);
  throwing = true;
  assert.throws(() => entries.calls(original, replacement), error => error === failure);
  throwing = false;
  assert.deepEqual(entries.calls(original, replacement), [original, original]);
  assert.equal(entries.landing(original, replacement), original, 'A landing pad observes the saved copy.');
  assert.deepEqual(entries['ref-branch'](original, replacement), [original, replacement]);
  assert.deepEqual(entries['ref-branch'](original, null), [original, original]);
  for (const flag of [0, 1, 2]) {
    const joined = flag === 1 ? replacement : original;
    assert.deepEqual(entries.join(flag, original, replacement), [joined, joined]);
    const nullable = flag === 1 ? replacement : null;
    assert.deepEqual(entries.join(flag, null, replacement), [nullable, nullable]);
    const nested = flag ? original : replacement;
    assert.deepEqual(entries.nested(flag, original, replacement), [nested, nested]);
    const conditional = flag ? original : replacement;
    assert.deepEqual(entries.conditional(flag, original, replacement), [conditional, conditional]);
    const table = flag === 0 ? original : replacement;
    assert.deepEqual(entries.table(flag, original, replacement), [table, table]);
    assert.deepEqual(entries['inherited-stack'](flag, original, replacement), [original, flag ? original : replacement]);
  }
  assert.equal(entries['landing-join'](original, replacement), original, 'A landing pad retains the pre-join assignment.');
  assert.equal(entries.initialized(17, 29), 17, 'Non-defaultable references retain their initialization.');
};

// Each fold is a repeated semantic unit whose length changes with its input
// list. Local simplification first removes the saved copy. Sharing must still
// find profitable boundaries and preserve the callback and both return values.
const outlineInputs = [0n, -1n, 0x123456789abcdefn, -(1n << 63n), (1n << 63n) - 1n];
const outlineNames = Array.from({length: 12}, (_, i) => `fold-${i}`);
const outlineOperands = count => Array.from({length: count}, (_, i) =>
  0x123456789abcdefn ^ (BigInt(i + 1) * 0x100000001n));
const outliningFixture = count => `(module
  (import "m" "hook" (func $hook (param i64) (result i64)))
  ${outlineNames.map((name, i) => `
    (func (export "${name}") (param i64) (result i64 i32) (local i64)
      local.get 0 local.set 1 local.get 1
      ${outlineOperands(count).map(value => `i64.const ${value} i64.xor`).join('\n')}
      call $hook i32.const ${i})`).join('\n')})`;

const checkOutlining = (bytes, count) => {
  assert.ok(WebAssembly.validate(bytes), 'Every outlining variant must validate.');
  const folded = input => outlineOperands(count).reduce((value, operand) => value ^ operand, input);
  const returned = input => BigInt.asIntN(64, folded(input) + 17n);
  const failure = new Error('outlining callback'), events = [];
  let entries, reenter = false, throwing = false;
  const exports = new WebAssembly.Instance(new WebAssembly.Module(bytes), {m: {
    hook(value) {
      events.push(value);
      if (throwing) throw failure;
      if (reenter) {
        reenter = false;
        assert.deepEqual(entries.at(-1)[1](-1n), [returned(-1n), entries.length - 1]);
      }
      return BigInt.asIntN(64, value + 17n);
    },
  }}).exports;
  entries = outlineNames.map(name => [name, exports[name]]);
  for (const [i, [, entry]] of entries.entries()) {
    for (const input of outlineInputs) {
      events.length = 0;
      assert.deepEqual(entry(input), [returned(input), i]);
      assert.deepEqual(events, [folded(input)], 'The callback sees the complete fold exactly once.');
    }
    events.length = 0;
    reenter = true;
    assert.deepEqual(entry(0n), [returned(0n), i]);
    assert.deepEqual(events, [folded(0n), folded(-1n)], 'Reentry preserves each activation.');
    events.length = 0;
    throwing = true;
    assert.throws(() => entry(0n), error => error === failure);
    assert.deepEqual(events, [folded(0n)]);
    throwing = false;
    assert.deepEqual(entry(-1n), [returned(-1n), i], 'The entry remains usable after a callback throws.');
  }
};

const [mode, pathname, optimizedPath, sharedPath, operandCount] = process.argv.slice(2);
if (mode === 'prepare') {
  const bytes = fixture();
  check(bytes);
  for (const [options, reason, valid] of [
    [{tagName: 'foreign'}, /Unsupported native exception tag/, true],
    [{payload: true}, /Native transfer tag must have no payload/, true],
    [{catchIndex: 1}, /Invalid compiler module/, false],
  ]) {
    const malformed = fixture(options);
    assert.equal(WebAssembly.validate(malformed), valid);
    assert.throws(() => linkNativeModules([malformed]), reason);
  }
  const {wat, entries} = linkNativeModules([bytes]);
  await fs.writeFile(pathname, `(module
    (type $native-entry (func (param externref externref) (result externref)))
    (type $native-direct-entry (func (param externref externref externref externref externref) (result externref)))
    (import "m" "before" (func $native-before (param externref) (result externref)))
    (import "m" "transfer" (tag $transfer))
    (import "m" "after" (func $native-after (param externref) (result externref)))
    ${wat}
    (export "entry" (func ${entries[0]})))`);
  console.log('Native module linker: original transfer behavior and malformed-input rejection pass.');
} else if (mode === 'verify') {
  check(await fs.readFile(pathname));
  console.log('Native module linker: linked catches, propagation, function calls and recovery pass.');
} else if (mode === 'prepare-optimizer') {
  await fs.writeFile(pathname, optimizerFixture);
} else if (mode === 'verify-optimizer') {
  const original = await fs.readFile(pathname), optimized = await fs.readFile(optimizedPath);
  checkOptimized(original);
  checkOptimized(optimized);
  assert.ok(optimized.length < original.length, 'Shared optimization must reduce the complete copy and join fixture.');
  console.log('Native module optimizer: copies and joins shrink; writes, control, callbacks, landing pads and references retain behavior.');
} else if (mode === 'prepare-outlining') {
  await fs.writeFile(pathname, outliningFixture(Number(optimizedPath)));
} else if (mode === 'verify-outlining') {
  const original = await fs.readFile(pathname), plain = await fs.readFile(optimizedPath);
  const shared = await fs.readFile(sharedPath), count = Number(operandCount);
  for (const bytes of [original, plain, shared]) checkOutlining(bytes, count);
  assert.ok(plain.length < original.length, 'Local simplification must shorten the repeated fold.');
  assert.ok(shared.length < plain.length, 'Sharing must reduce the complete module after local simplification.');
  console.log(`Native module optimizer: ${count}-operand folds share after simplification; values, callbacks, reentry and recovery pass.`);
} else throw new Error('Expected a native linker or optimizer preparation/verification mode.');
