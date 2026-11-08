#!/usr/bin/env node
// Count section bytes (including headers) and compressed distribution sizes.
import fs from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import assert from 'node:assert/strict';
import {gzipSync, brotliCompressSync, constants} from 'node:zlib';

const sectionNames = ['custom', 'type', 'import', 'function', 'table', 'memory',
  'global', 'export', 'start', 'element', 'code', 'data', 'data-count', 'tag'];
const componentSectionNames = ['custom', 'core-module', 'core-instance', 'core-type',
  'component', 'component-instance', 'alias', 'type', 'canonical', 'start', 'import', 'export'];

function measureBytes(file, bytes) {
  return {file, rawBytes: bytes.length,
    gzipBytes: gzipSync(bytes, {level: 9}).length,
    brotliBytes: brotliCompressSync(bytes,
      {params: {[constants.BROTLI_PARAM_QUALITY]: 11}}).length};
}

// HTML pages are measured as complete files, including embedded Wasm and JS.
export function measureFile(file) {
  return measureBytes(file, fs.readFileSync(file));
}

export function measureWasm(file) {
  const bytes = fs.readFileSync(file);
  const component = bytes.subarray(0, 8).equals(Buffer.from([0, 97, 115, 109, 13, 0, 1, 0]));
  assert.ok(component || bytes.subarray(0, 8).equals(Buffer.from([0, 97, 115, 109, 1, 0, 0, 0])),
    `${file}: expected a WASM module or component`);
  let position = 8;
  const sections = new Map([['header', {name: 'header', bytes: 8, count: 1}]]);
  const u32 = limit => {
    let value = 0;
    for (let shift = 0; shift < 35; shift += 7) {
      assert.ok(position < limit, `${file}: truncated section length`);
      const byte = bytes[position++];
      assert.ok(shift < 28 || byte < 16, `${file}: invalid section length`);
      value += (byte & 127) * 2 ** shift;
      if (byte < 128) return value;
    }
    throw new Error(`${file}: invalid section length`);
  };
  while (position < bytes.length) {
    const start = position;
    const id = bytes[position++];
    const length = u32(bytes.length);
    const end = position + length;
    assert.ok(end <= bytes.length, `${file}: truncated section`);
    let name = (component ? componentSectionNames : sectionNames)[id] ?? `section-${id}`;
    if (id === 0) {
      const nameLength = u32(end);
      assert.ok(position + nameLength <= end, `${file}: truncated custom section name`);
      name += `:${bytes.toString('utf8', position, position + nameLength)}`;
    }
    const section = sections.get(name) ?? {name, bytes: 0, count: 0};
    section.bytes += end - start;
    section.count++;
    sections.set(name, section);
    position = end;
  }
  return {...measureBytes(file, bytes), sections: [...sections.values()]};
}

// Preserve distribution measurements with results, independently of the images.
export function measureWasmImages(binary) {
  const files = [binary];
  const companion = path.join(path.dirname(binary), 'miricl-repl.wasm');
  if (path.resolve(companion) !== path.resolve(binary) && fs.existsSync(companion)) files.push(companion);
  return files.map(file => {
    const {sections, ...sizes} = measureWasm(file);
    return sizes;
  });
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  for (const file of process.argv.length > 2 ? process.argv.slice(2) : ['miricl.wasm']) {
    console.log(JSON.stringify(measureWasm(file), null, 2));
  }
}
