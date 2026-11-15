#!/usr/bin/env node
// Static size attribution, without loading application definitions or a compiler.
// node scripts/image-composition.mjs IMAGE.wasm [IMAGE.wasm.gz ...]
// Writes one JSON record per input. Embedded native modules are recognized by
// their magic, supported runtime imports and validation through their code section.
// This recognizes MIRICL's current code-last module format, not arbitrary Wasm
// embedded in arbitrary containers. A quoted valid native module also matches;
// these are byte-level diagnostics, not an object-ownership or retention proof.
import fs from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import {createHash} from 'node:crypto';
import {gzipSync, gunzipSync} from 'node:zlib';
import {isRuntimeImport, runtimeImportName} from './native-imports.mjs';

const magic = Buffer.from([0, 97, 115, 109, 1, 0, 0, 0]);
const maximumBytes = 512 * 1024 * 1024;
const sectionNames = ['custom', 'type', 'import', 'function', 'table', 'memory',
  'global', 'export', 'start', 'element', 'code', 'data', 'data-count', 'tag'];

class Reader {
  constructor(bytes, position = 0, end = bytes.length) {
    this.bytes = bytes;
    this.position = position;
    this.end = end;
  }
  byte() {
    if (this.position >= this.end) throw new Error('Truncated byte stream.');
    return this.bytes[this.position++];
  }
  skip(length) {
    if (!Number.isSafeInteger(length) || length < 0 || length > this.end - this.position)
      throw new Error('Byte range exceeds its container.');
    this.position += length;
  }
  unsigned() {
    let value = 0;
    for (let shift = 0; shift <= 28; shift += 7) {
      const byte = this.byte();
      if (shift === 28 && byte > 15) throw new Error('Unsigned operand exceeds 32 bits.');
      value += (byte & 127) * 2 ** shift;
      if (byte < 128) return value;
    }
    throw new Error('Invalid unsigned operand.');
  }
  signed(bits = 32) {
    let value = 0n, shift = 0n;
    for (let index = 0; index < Math.ceil(bits / 7); index++) {
      const byte = this.byte();
      value |= BigInt(byte & 127) << shift;
      shift += 7n;
      if (byte < 128) {
        if (byte & 64) value -= 1n << shift;
        const limit = 1n << BigInt(bits - 1);
        if (value < -limit || value >= limit) throw new Error('Signed operand exceeds its width.');
        return bits > 53 ? value.toString() : Number(value);
      }
    }
    throw new Error('Invalid signed operand.');
  }
  name() {
    const length = this.unsigned(), start = this.position;
    this.skip(length);
    return new TextDecoder('utf-8', {fatal: true}).decode(this.bytes.subarray(start, this.position));
  }
  valueType() {
    const type = this.byte();
    if (type === 0x63 || type === 0x64) this.signed(33);
    else if (![0x7f, 0x7e, 0x7d, 0x7c, 0x7b, 0x70, 0x6f, 0x6e, 0x6d, 0x6c,
      0x6b, 0x6a, 0x69, 0x68, 0x67, 0x66, 0x65].includes(type))
      throw new Error(`Unknown value type 0x${type.toString(16)}.`);
  }
}

function readSections(bytes, start = 0, end = bytes.length, codeLast = false) {
  const reader = new Reader(bytes, start + magic.length, end), sections = [];
  while (reader.position < end) {
    const start = reader.position, id = reader.byte();
    if (id >= sectionNames.length) throw new Error(`Unknown section ${id}.`);
    const length = reader.unsigned(), data = reader.position;
    reader.skip(length);
    const section = {id, name: sectionNames[id], start, data, end: reader.position,
      bytes: reader.position - start};
    if (id === 0) section.name += ':' + new Reader(bytes, data, reader.position).name();
    sections.push(section);
    if (codeLast && id === 10) break;
  }
  return sections;
}

function readNativeModule(bytes, start, sections) {
  const imports = [], bodies = [];
  for (const section of sections) {
    if (section.id !== 2 && section.id !== 10) continue;
    const reader = new Reader(bytes, section.data, section.end), count = reader.unsigned();
    if (count > section.end - reader.position) throw new Error('Invalid section entry count.');
    for (let index = 0; index < count; index++) {
      if (section.id === 2) {
        const module = reader.name(), name = reader.name(), kind = reader.byte();
        if (kind === 0) {reader.unsigned(); imports.push({module, name: runtimeImportName({module, name})});}
        else if (kind === 4) {reader.byte(); reader.unsigned();}
        else throw new Error('Native attribution supports only function and tag imports.');
      } else {
        const length = reader.unsigned(), start = reader.position;
        reader.skip(length);
        bodies.push({index: imports.length + index, start, end: reader.position, bytes: length});
      }
    }
    if (reader.position !== section.end) throw new Error('Unexpected bytes after section entries.');
  }
  return {offset: start, bytes: sections.at(-1).end - start, imports, sections, bodies};
}

const opcodeNames = new Map([
  [0, 'unreachable'], [2, 'block'], [3, 'loop'], [4, 'if'], [5, 'else'], [11, 'end'],
  [12, 'br'], [13, 'br_if'], [14, 'br_table'], [15, 'return'], [16, 'call'],
  [18, 'return_call'], [26, 'drop'], [32, 'local.get'], [33, 'local.set'],
  [34, 'local.tee'], [35, 'global.get'], [36, 'global.set'], [65, 'i32.const'],
  [66, 'i64.const'], [69, 'i32.eqz'], [70, 'i32.eq'], [71, 'i32.ne'],
  [72, 'i32.lt_s'], [74, 'i32.gt_s'], [76, 'i32.le_s'], [78, 'i32.ge_s'],
  [113, 'i32.and'], [124, 'i64.add'], [125, 'i64.sub'], [126, 'i64.mul'],
  [208, 'ref.null'], [209, 'ref.is_null'], [210, 'ref.func'], [211, 'ref.eq'],
]);
const indexedOpcodes = new Set([7, 8, 9, 12, 13, 16, 18, 20, 21, 24,
  32, 33, 34, 35, 36, 37, 38, 210, 213, 214]);

function readOperations(bytes, body, imports) {
  const reader = new Reader(bytes, body.start, body.end), groups = reader.unsigned();
  let locals = 0;
  if (groups > body.end - reader.position) throw new Error('Invalid local group count.');
  for (let index = 0; index < groups; index++) {
    locals += reader.unsigned();
    if (locals > 0xffffffff) throw new Error('Too many locals.');
    reader.valueType();
  }
  const localsBytes = reader.position - body.start, operations = [];
  while (reader.position < body.end) {
    const start = reader.position, code = reader.byte();
    let name = opcodeNames.get(code) ?? `op.0x${code.toString(16)}`, argument;
    if ([2, 3, 4, 6].includes(code)) argument = reader.signed(33);
    else if (indexedOpcodes.has(code)) {
      argument = reader.unsigned();
      if (code === 16 || code === 18) name += ' ' + (imports[argument]?.name ?? 'internal');
    } else if (code === 14) {
      const count = reader.unsigned();
      if (count >= body.end - reader.position) throw new Error('Invalid branch table count.');
      for (let index = 0; index <= count; index++) reader.unsigned();
    } else if (code === 17 || code === 19) {reader.unsigned(); reader.unsigned();}
    else if (code === 28) {
      const count = reader.unsigned();
      if (count > body.end - reader.position) throw new Error('Invalid typed-select count.');
      for (let index = 0; index < count; index++) reader.valueType();
    } else if (code === 31) {
      reader.signed(33);
      const count = reader.unsigned();
      if (count > body.end - reader.position) throw new Error('Invalid catch count.');
      for (let index = 0; index < count; index++) {
        const kind = reader.byte();
        if (kind > 3) throw new Error('Unknown catch kind.');
        if (kind < 2) reader.unsigned();
        reader.unsigned();
      }
    } else if (code >= 40 && code <= 62) {reader.unsigned(); reader.unsigned();}
    else if (code === 63 || code === 64) reader.unsigned();
    else if (code === 65) argument = reader.signed();
    else if (code === 66) argument = reader.signed(64);
    else if (code === 67) reader.skip(4);
    else if (code === 68) reader.skip(8);
    else if (code === 208) reader.signed(33);
    else if (code === 251) {
      const subcode = reader.unsigned();
      name = `gc.${subcode}`;
      if ([0, 1, 6, 7, 11, 12, 13, 14, 16].includes(subcode)) reader.unsigned();
      else if ([2, 3, 4, 5, 8, 9, 10, 17, 18, 19].includes(subcode)) {
        reader.unsigned(); reader.unsigned();
      } else if ([20, 21, 22, 23].includes(subcode)) reader.signed(33);
      else if (subcode === 24 || subcode === 25) {
        reader.byte(); reader.unsigned(); reader.signed(33); reader.signed(33);
      } else if (subcode !== 15 && (subcode < 26 || subcode > 30))
        throw new Error(`Unknown GC opcode ${subcode}.`);
    } else if (code === 252) {
      const subcode = reader.unsigned();
      name = `misc.${subcode}`;
      if ([8, 10, 12, 14].includes(subcode)) {reader.unsigned(); reader.unsigned();}
      else if (subcode >= 9 && subcode <= 17) reader.unsigned();
      else if (subcode > 17) throw new Error(`Unknown miscellaneous opcode ${subcode}.`);
    } else if (!([0, 1, 5, 10, 11, 15, 25, 26, 27, 209, 211, 212].includes(code)
      || (code >= 69 && code <= 196)))
      throw new Error(`Unknown opcode 0x${code.toString(16)}.`);
    operations.push({name, argument, bytes: reader.position - start});
  }
  return {locals, localsBytes, operations};
}

const increment = (table, key, count = 1) => {table[key] = (table[key] ?? 0) + count;};
const ranked = table => Object.entries(table).sort((a, b) =>
  (b[1].bytes ?? b[1]) - (a[1].bytes ?? a[1]) || a[0].localeCompare(b[0]));

// Inspect current embedded native modules without loading their initializers.
export function nativeImageModules(input) {
  if (!(input instanceof Uint8Array)) throw new TypeError('Expected Wasm bytes.');
  const bytes = Buffer.from(input.buffer, input.byteOffset, input.byteLength);
  if (bytes.length > maximumBytes) throw new Error('Input exceeds the 512 MiB diagnostic limit.');
  if (!bytes.subarray(0, magic.length).equals(magic)) throw new Error('Expected a core Wasm module.');
  const outerSections = readSections(bytes);
  if (!WebAssembly.validate(bytes)) throw new Error('Input is not a valid module in this Node runtime.');
  const modules = [], scanFailures = {};
  let candidates = 0;
  for (let offset = -1; (offset = bytes.indexOf(magic, offset + 1)) >= 0;) {
    candidates++;
    try {
      const sections = offset === 0 ? outerSections : readSections(bytes, offset, bytes.length, true);
      if (!sections.some(section => section.id === 10)) continue;
      if (offset !== 0 && !WebAssembly.validate(bytes.subarray(offset, sections.at(-1).end))) {
        increment(scanFailures, 'Invalid embedded module prefix');
        continue;
      }
      const module = readNativeModule(bytes, offset, sections);
      if (module.imports.some(isRuntimeImport)) modules.push(module);
    } catch (error) {increment(scanFailures, error.message);}
  }
  return {outerSections, modules, candidates, scanFailures};
}

export function analyzeImage(file) {
  if (fs.statSync(file).size > maximumBytes) throw new Error('Input exceeds the 512 MiB diagnostic limit.');
  const download = fs.readFileSync(file), bytes = file.endsWith('.gz')
    ? gunzipSync(download, {maxOutputLength: maximumBytes}) : download;
  const {outerSections, modules, candidates, scanFailures} = nativeImageModules(bytes);
  const totals = {moduleCount: modules.length, moduleBytes: 0, bodyCount: 0, bodyBytes: 0,
    decodedBodyCount: 0, decodedBodyBytes: 0, localsBytes: 0, literalFunctionLookupPairs: 0};
  const opcodes = {}, calls = {}, constants = {}, sections = {}, consRuns = {}, decodeFailures = {};
  const literalReuse = {acyclicBodies: 0, bySiteCount: {}};
  const largestBodies = [];
  for (const module of modules) {
    totals.moduleBytes += module.bytes;
    for (const section of module.sections) increment(sections, section.name, section.bytes);
    for (const body of module.bodies) {
      totals.bodyCount++;
      totals.bodyBytes += body.bytes;
      largestBodies.push({moduleOffset: module.offset, functionIndex: body.index, bytes: body.bytes});
      try {
        const {localsBytes, operations} = readOperations(bytes, body, module.imports);
        totals.decodedBodyCount++;
        totals.decodedBodyBytes += body.bytes;
        totals.localsBytes += localsBytes;
        const indices = new Map();
        for (let index = 0; index < operations.length; index++) {
          const op = operations[index], previous = operations[index - 1];
          const row = opcodes[op.name] ??= {count: 0, bytes: 0};
          row.count++;
          row.bytes += op.bytes;
          if (op.name.startsWith('call ') || op.name.startsWith('return_call '))
            increment(calls, op.name.split(' ')[1]);
          if (op.name === 'i32.const' || op.name === 'i64.const')
            increment(constants, op.name + ' ' + op.argument);
          if (op.name === 'call native-literal' && previous?.name === 'i32.const')
            indices.set(previous.argument, (indices.get(previous.argument) ?? 0) + 1);
          if (op.name === 'call native-function-reference' && previous?.name === 'call native-literal')
            totals.literalFunctionLookupPairs++;
        }
        for (let index = 0; index < operations.length; index++) {
          if (operations[index].name !== 'call native-cons') continue;
          let count = 1;
          while (operations[index + count]?.name === 'call native-cons') count++;
          increment(consRuns, count);
          index += count - 1;
        }
        // A Wasm LOOP is a conservative byte-level proxy, not the frontend's
        // exact CFG classification or a prediction of its literal-cache plan.
        if (!operations.some(op => op.name === 'loop')) {
          literalReuse.acyclicBodies++;
          for (const count of indices.values()) if (count >= 2) increment(literalReuse.bySiteCount, count);
        }
      } catch (error) {increment(decodeFailures, error.message);}
    }
  }
  return {schemaVersion: 1, file, sha256: createHash('sha256').update(bytes).digest('hex'),
    node: process.version, rawBytes: bytes.length, downloadBytes: download.length,
    gzipBytes: gzipSync(bytes, {level: 9}).length,
    outerSections: outerSections.map(({name, bytes}) => ({name, bytes})),
    discovery: {method: 'Validated code-last modules importing m; canonical helper names', candidates,
      scanFailures: ranked(scanFailures)},
    native: {...totals, sections: ranked(sections), opcodes: ranked(opcodes), calls: ranked(calls),
      constants: ranked(constants).slice(0, 40), consRuns, literalReuse,
      decodeFailures: ranked(decodeFailures)},
    largestBodies: largestBodies.sort((a, b) => b.bytes - a.bytes).slice(0, 40),
    modules: modules.map(module => ({offset: module.offset, bytes: module.bytes,
      bodies: module.bodies.length, imports: module.imports.map(entry => entry.name)}))};
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  if (process.argv.length < 3) {
    console.error('Usage: node scripts/image-composition.mjs IMAGE.wasm [IMAGE.wasm.gz ...]');
    process.exitCode = 1;
  }
  for (const file of process.argv.slice(2)) {
    try {console.log(JSON.stringify(analyzeImage(file)));}
    catch (error) {console.error(`${file}: ${error.message}`); process.exitCode = 1;}
  }
}
