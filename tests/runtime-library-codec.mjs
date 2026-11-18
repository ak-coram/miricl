// Private test exports wrap the production bootstrap decoder in a temporary image.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl, MiriclError} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2]));
const prepare = bytes => {
  new Uint8Array(vm.vm.memory.buffer).set(bytes);
  vm.vm['test-library-prepare'](bytes.length);
};
const read = () => vm.vm['test-library-read']();
const same = (a, b) => Boolean(vm.vm['test-library-same'](a, b));
const car = value => vm.vm['test-library-car'](value);
const cdr = value => vm.vm['test-library-cdr'](value);
const content = (value, source) => {
  vm.vm['test-library-bind'](value);
  assert.equal(vm.evaluate(`(equal * '${source})`).output, 'T\n');
};
const rejection = operation => {
  assert.throws(() => {
    try {operation();} catch (cause) {throw vm.failure(cause);}
  }, error => error instanceof MiriclError && /Malformed form/i.test(error.message));
  // A new decode resets completed-form IDs, replay state and the work budget.
  prepare(Uint8Array.of(34, 1, 2, 1, 4, 1, 6, 63, 0));
  const first = read(), second = read();
  assert.ok(!same(first, second));
  content(first, '(1 2 3)');
  content(second, '(1 2 3)');
};
const unpack = (offset, size, length) => {
  vm.vm['test-library-unpack'](offset, size, length);
  return new Uint8Array(vm.vm.memory.buffer, 0, length).slice();
};
const unpackNames = (offset, size, length) =>
  vm.vm['test-bootstrap-names-unpack'](offset, size, length);
const namesContent = names => Uint8Array.from(
  {length: vm.vm['test-bootstrap-names-length'](names)},
  (_, index) => vm.vm['test-bootstrap-names-byte'](names, index));
try {
  const compressionCases = JSON.parse(await fs.readFile(process.argv[6], 'utf8'));
  for (const [raw, offset, size] of compressionCases.valid)
    assert.deepEqual(unpack(offset, size, raw.length), Uint8Array.from(raw));
  for (const [raw, offset, size] of compressionCases.valid) {
    const names = unpackNames(offset, size, raw.length);
    assert.deepEqual(vm.vm['test-bootstrap-names-state'](), [37, 91],
      'Name setup restores the cursor and preserves the limit.');
    new Uint8Array(vm.vm.memory.buffer, 0, raw.length).fill(0);
    assert.deepEqual(namesContent(names), Uint8Array.from(raw),
      'Decoded name storage survives reuse of linear scratch memory.');
  }
  const [recoveryRaw, recoveryOffset, recoverySize] = compressionCases.valid.at(-1);
  for (const [offset, size, length] of compressionCases.invalid) {
    rejection(() => unpack(offset, size, length));
    assert.deepEqual(unpack(recoveryOffset, recoverySize, recoveryRaw.length),
      Uint8Array.from(recoveryRaw), 'Byte decompression recovers after rejection.');
    rejection(() => unpackNames(offset, size, length));
    assert.deepEqual(namesContent(unpackNames(recoveryOffset, recoverySize, recoveryRaw.length)),
      Uint8Array.from(recoveryRaw), 'Name setup recovers after malformed packed bytes.');
  }

  prepare(await fs.readFile(process.argv[3]));
  const sources = [
    '(1 2 3)', '(1 2 3)', '(4 5 6)', '(4 5 6)',
    '("marker" "marker")', '("marker" "marker")', '("marker" "marker")',
    '(7 . 8)', '(7 . 8)',
    `(${Array(32).fill('9').join(' ')})`,
    `(${Array(40).fill('10').join(' ')})`,
    '((1 2 3) (4 5 6))', '((1 2 3) (4 5 6))',
  ];
  const boundaryStart = sources.length;
  const boundary = Array.from({length: 128 + 16}, (_, index) =>
    `(${1000 + index} ${2000 + index} ${3000 + index})`);
  sources.push(...boundary, ...boundary);
  const values = sources.map(read);
  sources.forEach((source, index) => content(values[index], source));
  for (const [a, b] of [[0, 1], [2, 3], [4, 5], [5, 6], [7, 8], [11, 12]])
    assert.ok(!same(values[a], values[b]), 'Each decoded source spine is fresh.');
  assert.ok(!same(car(values[4]), car(values[5])), 'String references create fresh strings.');
  assert.ok(!same(car(values[5]), car(values[6])), 'Subform replay creates fresh strings.');
  assert.ok(!same(car(values[11]), car(values[12])), 'Nested source spines stay fresh.');
  boundary.forEach((_, index) => assert.ok(!same(values[boundaryStart + index],
    values[boundaryStart + boundary.length + index]), 'ULEB ID-width changes retain fresh objects.'));

  prepare(await fs.readFile(process.argv[4]));
  const strings = JSON.parse(await fs.readFile(process.argv[5], 'utf8'));
  // EVALUATE reuses the input memory. Decode the complete fixture before
  // evaluating content assertions, just as the source-spine fixture does.
  const stringValues = strings.map(() => Array.from({length: 3}, read));
  const stringContent = (value, codes) => {
    vm.vm['test-library-bind'](value);
    assert.equal(vm.evaluate(`(equal (map 'list #'char-code *) '(${codes.join(' ')}))`).output, 'T\n');
  };
  for (const [caseIndex, codes] of strings.entries()) {
    const copies = stringValues[caseIndex];
    const decodedStrings = copies.flatMap(value => [car(cdr(value)), car(cdr(cdr(value)))]);
    decodedStrings.forEach(value => stringContent(value, codes));
    for (let index = 0; index < decodedStrings.length; index++)
      for (let previous = 0; previous < index; previous++)
        assert.ok(!same(decodedStrings[index], decodedStrings[previous]),
          'String references and completed-form replay allocate independent strings.');
    assert.ok(!same(copies[0], copies[1]) && !same(copies[1], copies[2]));
    if (codes.length) {
      vm.vm['test-library-bind'](decodedStrings[0]);
      const replacement = codes[0] === 0 ? 1 : 0;
      vm.evaluate(`(setf (char * 0) (code-char ${replacement}))`);
      stringContent(decodedStrings[0], [replacement, ...codes.slice(1)]);
      decodedStrings.slice(1).forEach(value => stringContent(value, codes));
    }
  }

  const uleb = value => {
    const bytes = [];
    do {
      const low = value % 128;
      value = Math.floor(value / 128);
      bytes.push(low + (value ? 128 : 0));
    } while (value);
    return bytes;
  };
  // Both bodies use the current string header. Raw strings can include every
  // byte, including zero; ordinary bodies retain full character codes.
  for (const [header, body, codes] of [
    [0x80000000, [], []],
    [0x80000004, [0, 127, 128, 255], [0, 127, 128, 255]],
    [3, [...uleb(0), ...uleb(256), ...uleb(0x10ffff)], [0, 256, 0x10ffff]],
  ]) {
    prepare(Uint8Array.from([10, ...uleb(header), ...body, 127, 0]));
    const first = read(), second = read();
    assert.ok(!same(first, second));
    stringContent(first, codes);
    stringContent(second, codes);
  }

  for (const [valid, bytes] of [
    [0, [63, 0]], // No completed form; ID zero must not mean an unfinished ancestor.
    [0, [33, 63, 0]],
    [1, [34, 1, 2, 1, 4, 1, 6, 63, 1]], // Out-of-range completed ID.
    [1, [33, 34, 1, 2, 1, 4, 1, 6, 63, 0, 63, 1]], // A parent containing a replay has no ID.
    [1, [32, 10, 1, 97, 63, 0]], // A form introducing a string ID cannot be replayed.
    [0, [63, 128, 128, 128, 128, 16]], // Overflowed ID.
    [0, [7, 1]], // A dotted list needs a cons cell to receive the tail.
    [0, [10, 128, 128, 1]], // Truncated string length cannot allocate an unbounded array.
    [0, [10, ...uleb(0x80000005), 0, 128, 255]], // Truncated raw body.
    [0, [10, ...uleb(0xffffffff)]], // Masked huge raw length rejected before allocation.
    [0, [10, ...uleb(0x7fffffff)]], // Huge ordinary length rejected before allocation.
    [0, [10, 128, 128, 128, 128, 16]], // String-header overflow past u32.
    [0, [10, 128, 128, 128, 128, 128]], // Unterminated fifth header byte.
    [0, [10, 1, 128]], // Truncated ordinary character code.
    [0, [10, 1, 128, 128, 128, 128, 16]], // Ordinary character code overflow.
    [0, [127, 0]], // No string header available for a reference.
    [1, [10, ...uleb(0x80000001), 255, 127, 1]], // Raw string reference out of bounds.
  ]) rejection(() => {
    prepare(Uint8Array.from(bytes));
    for (let index = 0; index < valid; index++) read();
    read();
  });
  rejection(() => vm.vm['test-library-prepare'](16777216));
  rejection(() => {prepare(Uint8Array.of(0)); vm.vm['test-library-exhaust'](); read();});
  console.log('Bootstrap library codec: compressed bytes, byte/Unicode strings, fresh-object round trips, completed IDs, malformed input and recovery pass.');
} finally {vm.close();}
