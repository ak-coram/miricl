// The combined WASI command embeds the unchanged terminal evaluator as its
// second top-level core module. Low-level I/O tests can still exercise that core.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
export function terminalCore(bytes) {
  if (bytes[4] === 1) return bytes;
  let position = 8, index = 0;
  function u32() {
    let value = 0;
    for (let shift = 0; shift < 35; shift += 7) {
      const byte = bytes[position++];
      value += (byte & 127) * 2 ** shift;
      if (byte < 128) return value;
    }
    throw new Error('Invalid component section length.');
  }
  while (position < bytes.length) {
    const id = bytes[position++], length = u32(), end = position + length;
    if (id === 1 && index++ === 1) return bytes.subarray(position, end);
    position = end;
  }
  throw new Error('The WASI image has no embedded terminal evaluator.');
}

export function terminalFixture(image) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'miricl-terminal-core-'));
  const file = path.join(directory, 'terminal.wasm');
  fs.writeFileSync(file, terminalCore(fs.readFileSync(image)));
  return {path: file, close: () => fs.rmSync(directory, {recursive: true, force: true})};
}
