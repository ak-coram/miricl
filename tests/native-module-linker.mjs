// Build-time linking must preserve transfer tags and function index spaces.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';

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
  const comparisons = [...Array.from({length: 10}, (_, index) => [3, 0x46 + index]),
    ...Array.from({length: 10}, (_, index) => [4, 0x51 + index])];
  return Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    ...section(1, [6, 0x60, 1, 0x6f, 1, 0x6f,
      0x60, 2, 0x6f, 0x6f, 1, 0x6f,
      0x60, payload ? 1 : 0, ...(payload ? [0x6f] : []), 0,
      0x60, 2, 0x7f, 0x7f, 1, 0x7f, 0x60, 2, 0x7e, 0x7e, 1, 0x7f,
      0x60, 1, 0x7f, 1, 0x7e]),
    // The tag sits between two function imports but occupies no function slot.
    ...section(2, [3, ...name('m'), ...name('before'), 0, 0,
      ...name('m'), ...name(tagName), 4, 0, 2,
      ...name('m'), ...name('after'), 0, 0]),
    ...section(3, [3 + comparisons.length, 1, 1, ...comparisons.map(([type]) => type), 5]),
    ...section(7, [1, ...name('entry'), 0, 3]),
    ...section(10, [3 + comparisons.length,
      ...body([0x20, 0, 0x10, 0, 0x10, 1]),
      ...body([0x20, 1, 0xd1, 0x04, 0x40, ...throwing, 0x0b,
        0x02, payload ? 0x6f : 0x40,
        0x1f, 0x40, 1, 0, catchIndex, 0, ...throwing, 0x0b,
        ...(payload ? [0xd0, 0x6f] : []), 0x0b, ...(payload ? [0x1a] : []),
        0x20, 0, 0x20, 1, 0x10, 2]),
      ...comparisons.flatMap(([, opcode]) => body([0x20, 0, 0x20, 1, opcode])),
      ...body([0x20, 0, 0xad]),
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
  return module.exports;
};

// The counter emitter uses immediate references, i32 steps and i64-valued
// branches. Exercise those together through the production linker, including
// promotion and a general fallback whose effects must not be speculated.
const counterImports = `
  (import "m" "wide" (func $native-wide (param i64) (result externref)))
  (import "m" "step" (func $native-step (param externref i32) (result externref)))`;
const counterFixture = `(module ${counterImports}
  (func (export "entry") (param externref externref) (result externref)
    (local $small i31ref) (local $value i32) (local $step i32) (local $result i64)
    local.get 1 any.convert_extern ref.cast (ref i31) i31.get_s local.set $step
    local.get 0 any.convert_extern ref.test (ref i31)
    if (result externref)
      local.get 0 any.convert_extern ref.cast (ref i31) local.tee $small
      i31.get_s local.set $value
      local.get $step i32.const 1 i32.eq
      if (result i64)
        local.get $value i32.const 1 i32.add i64.extend_i32_s
      else
        local.get $value i32.const 1 i32.sub i64.extend_i32_s
      end
      local.set $result
      local.get $value
      local.get $step i32.const 1 i32.eq
      if (result i32) i32.const 1073741823 else i32.const -1073741824 end
      i32.eq
      if (result externref)
        local.get $result call $native-wide
      else
        local.get $result i32.wrap_i64 ref.i31 extern.convert_any
      end
    else
      local.get 0 local.get $step call $native-step
    end))`;
const checkCounters = bytes => {
  const events = [], failure = new Error('step callback');
  const entry = new WebAssembly.Instance(new WebAssembly.Module(bytes), {m: {
    wide(value) {events.push(['wide', value]); return value;},
    step(value, direction) {
      events.push(['step', value, direction]);
      if (value === failure) throw failure;
      return value + BigInt(direction);
    },
  }}).exports.entry;
  for (const [input, direction, expected] of [[0, 1, 1], [0, -1, -1],
    [1073741823, 1, 1073741824n], [-1073741824, -1, -1073741825n]]) {
    events.length = 0;
    assert.equal(entry(input, direction), expected);
    assert.deepEqual(events, typeof expected === 'bigint' ? [['wide', expected]] : []);
  }
  for (const input of [1n << 40n, 1n << 100n]) {
    events.length = 0;
    assert.equal(entry(input, 1), input + 1n);
    assert.deepEqual(events, [['step', input, 1]]);
  }
  assert.throws(() => entry(failure, 1), error => error === failure);
  assert.equal(entry(40, 1), 41, 'A failed generic step leaves immediate arithmetic usable.');
};

// Exercise the shared body optimizer through real module decoding and writing.
// Copies have repeated consumers, so keeping a value on the stack alone cannot
// remove their traffic. Joins retain locals used after the first load; other
// entries invalidate copy and join facts deliberately.
const comparisonNames = ['eq', 'ne', 'lt_s', 'lt_u', 'gt_s', 'gt_u', 'le_s', 'le_u', 'ge_s', 'ge_u'];
const wrappingConstants = [0n, -1n, 0x7fffffffn, 0x80000000n, 0xffffffffn,
  0x100000000n, -0x100000001n, -(1n << 63n), (1n << 63n) - 1n];
const optimizerFixture = `(module
  (type $box (struct (field i32)))
  (type $identity (func (param i32) (result i32)))
  (import "m" "hook" (func $hook (param externref) (result externref)))
  (import "m" "fail" (func $fail))
  (import "m" "i32-hook" (func $i32-hook (param i32) (result i32)))
  (import "m" "i64-hook" (func $i64-hook (param i64) (result i64)))
  ;; Only the inner void label is targeted; its surviving alias must remain.
  (func (export "alias-void") (param i32 externref) (result externref)
    block $outer block $inner
      local.get 0 br_if $inner local.get 1 call $hook drop
    end end local.get 1)
  (func (export "alias-chain") (param i32 externref externref) (result externref)
    block $outer (result externref)
      block $middle (result externref)
        block $inner (result externref)
          local.get 1
          local.get 0 i32.eqz br_if $inner
          local.get 0 i32.const 1 i32.eq br_if $middle
          local.get 0 i32.const 2 i32.eq br_if $outer
          drop local.get 2 call $hook
        end
      end
    end)
  (func (export "alias-table") (param i32 externref externref) (result externref externref)
    block $surround (result externref)
      block $outer (result externref)
        block $inner (result externref)
          local.get 1 local.get 0 br_table $inner $outer $surround
        end
      end
    end local.get 2)
  (func (export "alias-function") (param i32) (result i32)
    block $outer (result i32) block $inner (result i32)
      i32.const 17 local.get 0 br_table $inner $outer 2
    end end i32.const 1 i32.add)
  (func (export "alias-trap") (param i32) (result i32)
    block (result i32) block (result i32)
      i32.const 17 call $i32-hook local.get 0 i32.div_s
    end end)
  (func (export "alias-unreachable") (param i32) (result i32)
    block $outer (result i32) block $inner (result i32)
      loop $again
        local.get 0 i32.eqz if unreachable end
        local.get 0 i32.const 1 i32.eq if i32.const 17 br $inner end
        local.get 0 i32.const 2 i32.eq if i32.const 29 br $outer end
        local.get 0 i32.const 1 i32.sub local.set 0 br $again
      end unreachable
    end end)
  ;; Indexed signatures can have parameters or several results. They remain
  ;; checked ordinary control frames rather than supplying literal-type proof.
  (func (export "alias-parameters") (param i32) (result i32)
    local.get 0 block (type $identity) block (type $identity) end end)
  (func (export "alias-multiple") (param i32 externref) (result externref i32)
    block $outer (result externref i32) block $inner (result externref i32)
      local.get 1 i32.const 29 local.get 0 br_table $inner $outer
    end end)
  (func (export "alias-landing") (param externref) (result externref)
    block $outer block $inner
      try_table (catch_all $inner) call $fail end unreachable
    end end local.get 0)
  (func (export "alias-reference") (param externref)
    block $outer block $inner local.get 0 br_on_null $outer drop end end
    local.get 0 call $hook drop)
  ${wrappingConstants.map((value, index) => `(func (export "wrap-${index}") (result i32)
    i64.const ${value} i32.wrap_i64)`).join('\n')}
  ${['i32', 'i64'].flatMap(type => comparisonNames.map(name => `
    (func (export "negate-${type}-${name}") (param ${type} ${type}) (result i32)
      local.get 0 call $${type}-hook local.get 1 call $${type}-hook ${type}.${name} i32.eqz)`)).join('\n')}
  ${['i32', 'i64'].map(type => `
    (func (export "zero-${type}") (param ${type}) (result i32)
      local.get 0 call $${type}-hook ${type}.const 0 ${type}.eq)
    (func (export "zero-branch-${type}") (param ${type}) (result i32)
      block (result i32)
        local.get 0 call $${type}-hook ${type}.const 0 ${type}.eq
        if (result i32) i32.const 17 else i32.const 29 end
      end)`).join('\n')}
  (func (export "zero-pair") (param i32 i64) (result i32 i32)
    local.get 0 call $i32-hook i32.const 0 i32.eq
    local.get 1 call $i64-hook i64.const 0 i64.eq)
  (func (export "opaque-zero") (param i32) (result i32)
    v128.const i32x4 0x00460041 0 0 0 drop
    local.get 0 i32.const 0 i32.eq)
  (func (export "zero-f64") (param f64) (result i32)
    local.get 0 f64.const 0 f64.eq)
  (func (export "zero-trap") (param i32) (result i32)
    i32.const 17 local.get 0 i32.div_s i32.const 0 i32.eq)
  (func (export "extend-signed") (param i32) (result i32)
    local.get 0 call $i32-hook i64.extend_i32_s i32.wrap_i64)
  (func (export "extend-unsigned") (param i32) (result i32)
    local.get 0 call $i32-hook i64.extend_i32_u i32.wrap_i64)
  (func (export "truth-if") (param i32) (result i32)
    local.get 0 i32.eqz i32.eqz if (result i32) i32.const 17 else i32.const 29 end)
  (func (export "truth-branch") (param i32) (result i32)
    block $done (result i32)
      i32.const 17 local.get 0 i32.eqz i32.eqz br_if $done drop i32.const 29
    end)
  (func (export "truth-value") (param i32) (result i32)
    local.get 0 i32.eqz i32.eqz)
  (func (export "negate-float-lt") (param f64 f64) (result i32)
    local.get 0 local.get 1 f64.lt i32.eqz)
  (func (export "trapping-comparison") (param i32 i32) (result i32)
    i32.const 17 local.get 0 i32.div_s local.get 1 i32.eq i32.eqz)
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
    'i32-hook'(value) {events.push(value); if (throwing) throw failure; return value;},
    'i64-hook'(value) {events.push(value); if (throwing) throw failure; return value;},
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
  wrappingConstants.forEach((value, index) =>
    assert.equal(entries[`wrap-${index}`](), Number(BigInt.asIntN(32, value))));
  const compare = (name, left, right, width) => {
    if (name.endsWith('_u')) {
      left = BigInt.asUintN(width, BigInt(left)); right = BigInt.asUintN(width, BigInt(right));
    }
    switch (name.split('_')[0]) {
      case 'eq': return left === right;
      case 'ne': return left !== right;
      case 'lt': return left < right;
      case 'gt': return left > right;
      case 'le': return left <= right;
      case 'ge': return left >= right;
    }
    throw new Error('Unknown comparison');
  };
  for (const [type, width, inputs] of [
    ['i32', 32, [-2147483648, -1, 0, 1, 2, 2147483647]],
    ['i64', 64, [-(1n << 63n), -1n, 0n, 1n, 2n, (1n << 63n) - 1n]],
  ]) {
    for (const input of inputs) {
      events.length = 0;
      assert.equal(entries[`zero-${type}`](input), Number(input === (type === 'i64' ? 0n : 0)));
      assert.deepEqual(events, [input], 'Zero equality retains the operand callback.');
      events.length = 0;
      assert.equal(entries[`zero-branch-${type}`](input), input ? 29 : 17);
      assert.deepEqual(events, [input]);
    }
    throwing = true; events.length = 0;
    assert.throws(() => entries[`zero-${type}`](inputs[0]), error => error === failure);
    assert.deepEqual(events, [inputs[0]]);
    throwing = false;
    for (const name of comparisonNames) for (const left of inputs) for (const right of inputs) {
      events.length = 0;
      assert.equal(entries[`negate-${type}-${name}`](left, right), Number(!compare(name, left, right, width)));
      assert.deepEqual(events, [left, right], 'Comparison operands retain their callback order.');
    }
    throwing = true; events.length = 0;
    assert.throws(() => entries[`negate-${type}-eq`](inputs[0], inputs[1]), error => error === failure);
    assert.deepEqual(events, [inputs[0]], 'A failing operand skips the later callback.');
    throwing = false;
  }
  for (const input of [-2147483648, -1, 0, 1, 2, 2147483647]) {
    assert.equal(entries['truth-if'](input), input ? 17 : 29);
    assert.equal(entries['truth-branch'](input), input ? 17 : 29);
    assert.equal(entries['truth-value'](input), Number(input !== 0), 'A value context retains boolean normalization.');
    for (const kind of ['signed', 'unsigned']) {
      events.length = 0;
      assert.equal(entries[`extend-${kind}`](input), input);
      assert.deepEqual(events, [input]);
    }
  }
  for (const left of [NaN, -Infinity, -0, 0, Infinity]) for (const right of [NaN, -1, 0, 1])
    assert.equal(entries['negate-float-lt'](left, right), Number(!(left < right)));
  for (const input of [-2147483648, -1, 0, 1, 2147483647])
    assert.equal(entries['opaque-zero'](input), Number(input === 0), 'Unsupported bodies retain behavior.');
  for (const [left, right] of [[0, 0n], [-1, 0n], [0, 1n], [1, -1n]]) {
    events.length = 0;
    assert.deepEqual(entries['zero-pair'](left, right), [Number(left === 0), Number(right === 0n)]);
    assert.deepEqual(events, [left, right], 'Several zero comparisons retain values and operand order.');
  }
  for (const input of [NaN, -Infinity, -1, -0, 0, 1, Infinity])
    assert.equal(entries['zero-f64'](input), Number(input === 0), 'Integer folding does not alter float comparisons.');
  assert.throws(() => entries['zero-trap'](0), WebAssembly.RuntimeError);
  assert.equal(entries['zero-trap'](17), 0);
  assert.equal(entries['zero-trap'](18), 1, 'Zero equality remains usable after an operand trap.');
  assert.throws(() => entries['trapping-comparison'](0, 17), WebAssembly.RuntimeError);
  assert.equal(entries['trapping-comparison'](1, 17), 0, 'Comparison remains usable after an operand trap.');
  events.length = 0;
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
  reenter = false;
  for (const flag of [0, 1, 2, 3]) {
    events.length = 0;
    assert.equal(entries['alias-void'](flag, original), original);
    assert.deepEqual(events, flag ? [] : [original]);
    events.length = 0;
    assert.equal(entries['alias-chain'](flag, original, replacement), flag < 3 ? original : replacement);
    assert.deepEqual(events, flag < 3 ? [] : [replacement]);
    assert.deepEqual(entries['alias-table'](flag, original, replacement), [original, replacement]);
    assert.equal(entries['alias-function'](flag), flag < 2 ? 18 : 17);
    assert.deepEqual(entries['alias-multiple'](flag, original), [original, 29]);
    assert.equal(entries['alias-parameters'](flag), flag);
    if (flag) assert.equal(entries['alias-unreachable'](flag), flag === 1 ? 17 : 29);
  }
  assert.throws(() => entries['alias-unreachable'](0), WebAssembly.RuntimeError);
  assert.equal(entries['alias-unreachable'](1), 17);
  events.length = 0;
  assert.throws(() => entries['alias-trap'](0), WebAssembly.RuntimeError);
  assert.deepEqual(events, [17], 'The operand callback precedes a division trap.');
  events.length = 0; throwing = true;
  assert.throws(() => entries['alias-trap'](1), error => error === failure);
  assert.deepEqual(events, [17]);
  throwing = false;
  assert.equal(entries['alias-trap'](1), 17, 'Aliased control remains usable after a failed callback.');
  assert.equal(entries['alias-landing'](original), original);
  for (const value of [original, null]) {
    events.length = 0;
    entries['alias-reference'](value);
    assert.deepEqual(events, [value], 'A reference branch retains its landing depth and following effect.');
  }
};

// Each fold is a repeated semantic unit whose length changes with its input
// list. Local simplification first removes the saved copy. Sharing must still
// find profitable boundaries and preserve the callback and both return values.
const outlineInputs = [0n, -1n, 0x123456789abcdefn, -(1n << 63n), (1n << 63n) - 1n];
// Include enough independent short folds for sharing to repay the complete
// helper cost. The two-operand case exercises short-region discovery; longer
// folds retain the large-region checks after local simplification.
const outlineNames = Array.from({length: 64}, (_, i) => `fold-${i}`);
const outlineOperands = count => Array.from({length: count}, (_, i) =>
  0x123456789abcdefn ^ (BigInt(i + 1) * 0x100000001n));
const outliningFixture = count => `(module
  (import "m" "hook" (func $hook (param i64) (result i64)))
  ${outlineNames.map((name, i) => `
    (func (export "${name}") (param i64) (result i64 i32) (local i64)
      local.get 0 local.set 1 local.get 1
      ${outlineOperands(count).map(value => `i64.const ${value} i64.xor`).join('\n')}
      call $hook i32.const ${i})`).join('\n')})`;

// A host countdown bounds execution even if outlining loses the loop-carried
// store. Its only later dynamic read precedes that store in textual order.
const outliningLoopFixture = count => `(module
  (import "m" "hook" (func $hook (param i64) (result i64)))
  (import "m" "tick" (func $tick (param i32) (result i32)))
  ${outlineNames.map((name, i) => `
    (func (export "${name}") (param i64) (result i64 i32) (local i64 i64)
      (block $exit (loop $again
        local.get 0 local.set 1
        i32.const ${i} call $tick i32.eqz br_if $exit
        local.get 0 local.set 2 local.get 2
        ${outlineOperands(count).map(value => `i64.const ${value} i64.xor`).join('\n')}
        call $hook local.set 0 br $again))
      local.get 1 i32.const ${i})`).join('\n')})`;

const checkOutliningLoops = (bytes, count) => {
  assert.ok(WebAssembly.validate(bytes), 'Every loop outlining variant must validate.');
  const folded = input => outlineOperands(count).reduce((value, operand) => value ^ operand, input);
  const expected = (input, steps, token) => {
    const trace = [];
    let state = input;
    for (let i = 0; i < steps; i++) {
      trace.push(['tick', token, true], ['hook', folded(state)]);
      state = BigInt.asIntN(64, folded(state) + 17n);
    }
    trace.push(['tick', token, false]);
    return {value: [state, token], trace};
  };
  const failure = new Error('loop outlining callback'), events = [], frames = [];
  let entries, reenter = false, throwing = false;
  const run = (token, input, steps) => {
    frames.push({token, remaining: steps, polls: 0, limit: steps + 1});
    try { return entries[token](input); }
    finally { frames.pop(); }
  };
  const exports = new WebAssembly.Instance(new WebAssembly.Module(bytes), {m: {
    tick(token) {
      const frame = frames.at(-1);
      assert.equal(token, frame.token, 'Reentry retains the active loop identity.');
      assert.ok(++frame.polls <= frame.limit, 'The loop obeys its host countdown.');
      const active = frame.remaining-- > 0;
      events.push(['tick', token, active]);
      return Number(active);
    },
    hook(value) {
      events.push(['hook', value]);
      if (throwing) throw failure;
      if (reenter) {
        reenter = false;
        const token = entries.length - 1;
        assert.deepEqual(run(token, -1n, 3), expected(-1n, 3, token).value);
      }
      return BigInt.asIntN(64, value + 17n);
    },
  }}).exports;
  entries = outlineNames.map(name => exports[name]);
  for (const token of entries.keys()) {
    for (const input of outlineInputs) {
      for (const steps of [0, 1, 3]) {
        const reference = expected(input, steps, token);
        events.length = 0;
        assert.deepEqual(run(token, input, steps), reference.value, 'Each backedge observes the preceding loop write.');
        assert.deepEqual(events, reference.trace, 'Loop callback values and order remain observable.');
      }
    }
    const outer = expected(0n, 1, token), inner = expected(-1n, 3, entries.length - 1);
    events.length = 0;
    reenter = true;
    assert.deepEqual(run(token, 0n, 1), outer.value);
    assert.deepEqual(events, [...outer.trace.slice(0, 2), ...inner.trace, ...outer.trace.slice(2)],
      'Reentry preserves the loop-carried state of both activations.');
    events.length = 0;
    throwing = true;
    assert.throws(() => run(token, 0n, 3), error => error === failure);
    assert.deepEqual(events, expected(0n, 3, token).trace.slice(0, 2));
    throwing = false;
    events.length = 0;
    const recovery = expected(-1n, 3, token);
    assert.deepEqual(run(token, -1n, 3), recovery.value, 'The loop remains usable after a callback throws.');
    assert.deepEqual(events, recovery.trace);
  }
};

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
  await fs.writeFile(pathname, bytes);
  for (const [suffix, options, valid] of [
    ['foreign', {tagName: 'foreign'}, true],
    ['payload', {payload: true}, true],
    ['catch', {catchIndex: 1}, false],
  ]) {
    const malformed = fixture(options);
    assert.equal(WebAssembly.validate(malformed), valid);
    await fs.writeFile(pathname + '.' + suffix, malformed);
  }
  console.log('Native linker fixture: original transfer behavior and malformed inputs prepared.');
} else if (mode === 'verify') {
  const entries = check(await fs.readFile(pathname));
  for (const [type, inputs] of [['i32', [-2147483648, -1, 0, 1, 2147483647]],
    ['i64', [-(1n << 63n), -1n, 0n, 1n, (1n << 63n) - 1n]]]) {
    for (const [index, relation] of comparisonNames.entries()) for (const first of inputs) for (const second of inputs) {
      let left = first, right = second;
      const actual = entries[`numeric-${(type === 'i32' ? 0 : comparisonNames.length) + index}`](left, right);
      if (relation.endsWith('_u')) {
        const width = type === 'i32' ? 32 : 64;
        left = BigInt.asUintN(width, BigInt(left)); right = BigInt.asUintN(width, BigInt(right));
      }
      const expected = {eq: left === right, ne: left !== right, lt: left < right,
        gt: left > right, le: left <= right, ge: left >= right}[relation.split('_')[0]];
      assert.equal(actual, Number(expected));
    }
  }
  for (const input of [-2147483648, -1, 0, 1, 2147483647])
    assert.equal(entries[`numeric-${comparisonNames.length * 2}`](input), BigInt(input >>> 0));
  console.log('Native module linker: linked catches, propagation, function calls and recovery pass.');
} else if (mode === 'prepare-counters') {
  await fs.writeFile(pathname, counterFixture);
} else if (mode === 'verify-counters') {
  checkCounters(await fs.readFile(pathname));
  console.log('Native module linker: linked immediate counter steps, promotion, generic effects and recovery pass.');
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
} else if (mode === 'prepare-outlining-loops') {
  await fs.writeFile(pathname, outliningLoopFixture(Number(optimizedPath)));
} else if (mode === 'verify-outlining' || mode === 'verify-outlining-loops') {
  const original = await fs.readFile(pathname), plain = await fs.readFile(optimizedPath);
  const shared = await fs.readFile(sharedPath), count = Number(operandCount);
  const loops = mode === 'verify-outlining-loops';
  for (const bytes of [original, plain, shared]) (loops ? checkOutliningLoops : checkOutlining)(bytes, count);
  assert.ok(plain.length < original.length, 'Local simplification must shorten the repeated fold.');
  assert.ok(shared.length < plain.length, 'Sharing must reduce the complete module after local simplification.');
  console.log(`Native module optimizer: ${count}-operand ${loops ? 'loop ' : ''}folds share after simplification; values, callbacks, reentry and recovery pass.`);
} else throw new Error('Expected a native linker or optimizer preparation/verification mode.');
