#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const quote = text => `"${text.replaceAll('\\', '\\\\').replaceAll('"', '\\"')}"`;
for (const length of [0, 1, 31, 32, 33, 63, 64, 65, 127, 128, 129, 4097]) {
  const characters = Array.from('aλ🙂"\\|:');
  const content = Array.from({length}, (_, i) => characters[i % characters.length]).join('');
  assert.equal(vm.evaluate(`(equal ${quote(content)} ${quote(content)})`).output, 'T\n');
  assert.equal(vm.evaluate(`(length ${quote(content)})`).output, `${Array.from(content).length}\n`);
  const name = `${'aλ🙂'.repeat(length)}|\\:quote"`;
  const token = `#:|${name.replaceAll('\\', '\\\\').replaceAll('|', '\\|')}|`;
  assert.equal(vm.evaluate(`(equal (symbol-name (read-from-string ${quote(token)})) ${quote(name)})`).output, 'T\n');
  if (length) {
    const digits = '1'.repeat(length);
    assert.equal(vm.evaluate(`(= (read-from-string ${quote(digits)}) ${digits})`).output, 'T\n');
  }
}
for (const length of [31, 32, 33, 63, 64, 65, 4097]) {
  assert.throws(() => vm.evaluate(`#\\${'a'.repeat(length)}`, {eof: true}), {code: 15});
  assert.equal(vm.evaluate('#\\a').output, '#\\a\n');
}
for (const token of ['READER-BUFFER-SYMBOL', '123', '"aλ🙂"', '#\\a']) {
  const source = `#(${`${token} `.repeat(10000)})`;
  assert.equal(vm.evaluate(`(length (read-from-string ${quote(source)}))`).output, '10000\n');
}
console.log('Guest reader buffers: growth boundaries, escaped Unicode, long symbols/numbers, large inputs, and error recovery pass.');
