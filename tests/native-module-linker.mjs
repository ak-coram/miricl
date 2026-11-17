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

const [mode, pathname] = process.argv.slice(2);
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
} else throw new Error('Usage: node tests/native-module-linker.mjs prepare LINKED.wat | verify LINKED.wasm');
