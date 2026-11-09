import assert from 'node:assert/strict';
import {encodeFilesystem, decodeFilesystem} from '../scripts/filesystem-archive.mjs';
const files = [['/λ/🙂.bin', Uint8Array.of(0, 128, 255, 10)], ['/empty', new Uint8Array()]];
const encoded = encodeFilesystem(files);
assert.deepEqual(decodeFilesystem(encoded), files);
assert.deepEqual(decodeFilesystem(encoded.buffer), files);
const offset = new Uint8Array(encoded.length + 6); offset.set(encoded, 3);
assert.deepEqual(decodeFilesystem(offset.subarray(3, -3)), files);
for (let length = 0; length < encoded.length; length++)
  assert.throws(() => decodeFilesystem(encoded.subarray(0, length)), /Invalid|Truncated/);
const extra = new Uint8Array(encoded.length + 1); extra.set(encoded);
assert.throws(() => decodeFilesystem(extra), /Unexpected/);
assert.throws(() => decodeFilesystem(encodeFilesystem([files[0], files[0]])), /duplicate/);
assert.deepEqual(decodeFilesystem(encodeFilesystem([])), []);
console.log('Filesystem archive: binary/Unicode round trips, bounds, duplicate paths, and empty archives pass.');
