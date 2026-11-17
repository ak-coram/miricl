// Build-time linking of MIRICL compiler output into the runtime module.
import assert from 'node:assert/strict';
import {isRuntimeImport, runtimeImportName} from './native-imports.mjs';

export function nativeModuleToWat(bytes, moduleIndex, declarations, entries, callCounts, {prefix = 'compiler'} = {}) {
  assert.ok(WebAssembly.validate(bytes), `Invalid compiler module ${moduleIndex}`);
  let position = 8;
  const byte = () => {assert.ok(position < bytes.length, 'Truncated native module'); return bytes[position++];};
  const unsigned = () => {
    let value = 0, shift = 0, next;
    do {next = byte(); value += (next & 127) * 2 ** shift; shift += 7; assert.ok(shift <= 35);} while (next & 128);
    return value;
  };
  const signed = () => {
    let value = 0, shift = 0, next;
    do {next = byte(); value |= (next & 127) << shift; shift += 7; assert.ok(shift <= 35);} while (next & 128);
    return shift < 32 && (next & 64) ? value | (-1 << shift) : value;
  };
  const signed64 = () => {
    let value = 0n, shift = 0n, next;
    do {next = byte(); value |= BigInt(next & 127) << shift; shift += 7n; assert.ok(shift <= 70n);} while (next & 128);
    if (next & 64) value -= 1n << shift;
    assert.ok(value >= -(1n << 63n) && value < (1n << 63n), 'Invalid native i64 constant');
    return value.toString();
  };
  const name = () => {const size = unsigned(); const value = new TextDecoder().decode(bytes.subarray(position, position + size)); position += size; return value;};
  const imports = [], tags = [], bodies = [], types = [], functionTypes = [];
  const functionName = index => index < imports.length ? `$${imports[index]}` : `$${prefix}-${moduleIndex}-${index - imports.length}`;
  const tagName = index => {
    assert.ok(index < tags.length, 'Unsupported native exception tag index');
    return tags[index];
  };
  const valueType = () => {
    const type = byte();
    if (type === 0x7f) return 'i32';
    if (type === 0x7e) return 'i64';
    if (type === 0x6f) return 'externref';
    if (type === 0x70) return 'funcref';
    throw new Error(`Unexpected native local type ${type}`);
  };
  // Keep the compiler's canonical GC types in the index space, while
  // recording function signatures for the checked entries and private workers.
  const storageType = () => {
    const type = byte();
    if (type === 0x63 || type === 0x64) unsigned();
    else assert.ok([0x7f, 0x7e, 0x7d, 0x7c, 0x70, 0x6f, 0x6e, 0x6d, 0x6c, 0x78, 0x77].includes(type),
      'Unsupported native storage type');
  };
  const fieldType = () => {storageType(); assert.ok(byte() <= 1, 'Invalid native field mutability');};
  const subtype = () => {
    let kind = byte();
    if (kind === 0x4f || kind === 0x50) {
      for (let count = unsigned(); count > 0; count--) unsigned();
      kind = byte();
    }
    if (kind === 0x60) {
      const parameters = [], results = [];
      for (let count = unsigned(); count > 0; count--) parameters.push(valueType());
      for (let count = unsigned(); count > 0; count--) results.push(valueType());
      types.push({parameters, results});
    } else {
      if (kind === 0x5e) fieldType();
      else {
        assert.equal(kind, 0x5f, 'Unsupported native composite type');
        for (let count = unsigned(); count > 0; count--) fieldType();
      }
      types.push(null);
    }
  };
  const blockType = () => {
    const type = byte();
    if (type === 0x40) return '';
    if (type === 0x7f) return ' (result i32)';
    if (type === 0x6f) return ' (result externref)';
    throw new Error(`Unexpected native block type ${type}`);
  };
  while (position < bytes.length) {
    const kind = byte(), size = unsigned(), end = position + size;
    if (kind === 1) {
      for (let count = unsigned(); count > 0; count--) {
        if (bytes[position] === 0x4e) {
          byte();
          for (let members = unsigned(); members > 0; members--) subtype();
        } else subtype();
      }
    } else if (kind === 2) {
      const count = unsigned();
      for (let i = 0; i < count; i++) {
        const entry = {module: name(), name: name()};
        assert.ok(isRuntimeImport(entry), 'Unsupported native runtime namespace');
        const importKind = byte();
        if (importKind === 0) {
          const field = runtimeImportName(entry); assert.match(field, /^native-[a-z0-9-]+$/);
          unsigned(); imports.push(field);
        } else {
          assert.equal(importKind, 4, 'Unsupported native import kind');
          assert.equal(entry.name, 'transfer', 'Unsupported native exception tag');
          assert.equal(byte(), 0, 'Unsupported native exception tag attribute');
          assert.deepEqual(types[unsigned()], {parameters: [], results: []},
            'Native transfer tag must have no payload');
          // Tags have their own index space and share the runtime transfer tag.
          // They must never shift function imports or linked entry indices.
          tags.push('$transfer');
        }
      }
    } else if (kind === 3) {
      for (let count = unsigned(); count > 0; count--) functionTypes.push(unsigned());
    } else if (kind === 7) {
      assert.equal(unsigned(), 1); assert.equal(name(), 'entry'); assert.equal(byte(), 0);
      entries.push(functionName(unsigned()));
    } else if (kind === 10) {
      const count = unsigned();
      assert.equal(functionTypes.length, count, 'Native function and code counts differ');
      for (let i = 0; i < count; i++) {
        const size = unsigned(), bodyEnd = position + size, locals = [], instructions = [];
        const localGroups = unsigned();
        for (let group = 0; group < localGroups; group++) {
          const count = unsigned(), type = valueType();
          if (count) locals.push(`(local ${Array(count).fill(type).join(' ')})`);
        }
        while (position < bodyEnd) {
          const op = byte();
          switch (op) {
            case 0x00: instructions.push('unreachable'); break;
            case 0x02: instructions.push(`block${blockType()}`); break;
            case 0x03: instructions.push(`loop${blockType()}`); break;
            case 0x04: instructions.push(`if${blockType()}`); break;
            case 0x05: instructions.push('else'); break;
            case 0x08: instructions.push(`throw ${tagName(unsigned())}`); break;
            case 0x0b: if (position < bodyEnd) instructions.push('end'); break;
            case 0x0c: instructions.push(`br ${unsigned()}`); break;
            case 0x0d: instructions.push(`br_if ${unsigned()}`); break;
            case 0x0e: {
              const count = unsigned(), targets = [];
              for (let j = 0; j <= count; j++) targets.push(unsigned());
              instructions.push(`br_table ${targets.join(' ')}`); break;
            }
            case 0x0f: instructions.push('return'); break;
            case 0x10: {
              const index = unsigned();
              if (index < imports.length) callCounts.set(imports[index], (callCounts.get(imports[index]) ?? 0) + 1);
              instructions.push(`call ${functionName(index)}`); break;
            }
            case 0x12: instructions.push(`return_call ${functionName(unsigned())}`); break;
            case 0x1a: instructions.push('drop'); break;
            case 0x1f: {
              const type = blockType(), catches = [];
              for (let count = unsigned(); count > 0; count--) {
                assert.equal(byte(), 0, 'Unsupported native exception catch kind');
                catches.push(`(catch ${tagName(unsigned())} ${unsigned()})`);
              }
              instructions.push(`try_table${type} ${catches.join(' ')}`); break;
            }
            case 0xd3: instructions.push('ref.eq'); break;
            case 0x20: instructions.push(`local.get ${unsigned()}`); break;
            case 0x21: instructions.push(`local.set ${unsigned()}`); break;
            case 0x22: instructions.push(`local.tee ${unsigned()}`); break;
            case 0x41: instructions.push(`i32.const ${signed()}`); break;
            case 0x42: instructions.push(`i64.const ${signed64()}`); break;
            case 0x45: instructions.push('i32.eqz'); break;
            case 0x46: instructions.push('i32.eq'); break;
            case 0x48: instructions.push('i32.lt_s'); break;
            case 0x4a: instructions.push('i32.gt_s'); break;
            case 0x4c: instructions.push('i32.le_s'); break;
            case 0x4e: instructions.push('i32.ge_s'); break;
            case 0x50: instructions.push('i64.eqz'); break;
            case 0x51: instructions.push('i64.eq'); break;
            case 0x52: instructions.push('i64.ne'); break;
            case 0x53: instructions.push('i64.lt_s'); break;
            case 0x55: instructions.push('i64.gt_s'); break;
            case 0x57: instructions.push('i64.le_s'); break;
            case 0x59: instructions.push('i64.ge_s'); break;
            case 0x71: instructions.push('i32.and'); break;
            case 0x7c: instructions.push('i64.add'); break;
            case 0x7d: instructions.push('i64.sub'); break;
            case 0x7e: instructions.push('i64.mul'); break;
            case 0x80: instructions.push('i64.div_u'); break;
            case 0x82: instructions.push('i64.rem_u'); break;
            case 0x83: instructions.push('i64.and'); break;
            case 0x84: instructions.push('i64.or'); break;
            case 0x85: instructions.push('i64.xor'); break;
            case 0x86: instructions.push('i64.shl'); break;
            case 0x87: instructions.push('i64.shr_s'); break;
            case 0xa7: instructions.push('i32.wrap_i64'); break;
            case 0xac: instructions.push('i64.extend_i32_s'); break;
            case 0xd0: assert.equal(byte(), 0x6f); instructions.push('ref.null extern'); break;
            case 0xd1: instructions.push('ref.is_null'); break;
            case 0xd2: instructions.push(`ref.func ${functionName(unsigned())}`); break;
            case 0xfb: {
              const gc = unsigned();
              if (gc === 2 || gc === 5) {
                assert.equal(unsigned(), 1);
                instructions.push(`struct.${gc === 5 ? 'set' : 'get'} $obj ${unsigned()}`);
              }
              else if (gc === 0x17) {
                const type = unsigned();
                assert.ok(type === 1 || type === 0x6d, `Unsupported nullable native heap type ${type}`);
                instructions.push(`ref.cast (ref null ${type === 1 ? '$obj' : 'eq'})`);
              }
              else if (gc === 0x14) {assert.equal(unsigned(), 1); instructions.push('ref.test (ref $obj)');}
              else if (gc === 0x16) {
                const type = unsigned();
                assert.ok(type === 1 || type === 0x6c, `Unsupported native heap type ${type}`);
                instructions.push(`ref.cast (ref ${type === 1 ? '$obj' : 'i31'})`);
              }
              else if (gc === 0x1a) instructions.push('any.convert_extern');
              else if (gc === 0x1b) instructions.push('extern.convert_any');
              else if (gc === 0x1c) instructions.push('ref.i31');
              else if (gc === 0x1d) instructions.push('i31.get_s');
              else throw new Error(`Unsupported native GC instruction ${gc}`);
              break;
            }
            default: throw new Error(`Unsupported native instruction ${op.toString(16)}`);
          }
        }
        assert.equal(position, bodyEnd);
        const function_ = `$${prefix}-${moduleIndex}-${i}`;
        declarations.push(function_);
        const signature = types[functionTypes[i]];
        assert.ok(signature, 'Missing compiler function signature');
        if (entries.includes(function_)) {
          assert.deepEqual(signature.parameters, ['externref', 'externref'],
            'Compiler factory keeps its checked packet entry');
          assert.deepEqual(signature.results, ['externref'], 'Compiler factory returns one Lisp value');
        }
        // Private outline helpers have exact scalar/reference signatures and
        // may return changed locals alongside their ordinary result.
        const result = signature.results.length ? ` (result ${signature.results.join(' ')})` : '';
        bodies.push(`(func ${function_} (param ${signature.parameters.join(' ')})${result}\n${locals.join('\n')}\n${instructions.join('\n')}\n)`);
      }
    }
    position = end;
  }
  return bodies.join('\n');
}

// Link compiler-produced modules into the ordinary runtime, without dynamic
// instantiation or a host execution service at startup.
export function linkNativeModules(codes, {prefix = 'compiler', tableName = 'compiler-entries', tableExport = 'core-compiler-functions'} = {}) {
  for (const name of [prefix, tableName])
    assert.match(name, /^[a-zA-Z][a-zA-Z0-9-]*$/, 'Invalid native link identifier');
  assert.match(tableExport, /^[a-zA-Z][a-zA-Z0-9-]*$/, 'Invalid native table export');
  const declarations = [], entries = [], callCounts = new Map();
  const bodies = codes.map((code, index) => nativeModuleToWat(Uint8Array.from(code), index, declarations, entries, callCounts, {prefix})).join('\n');
  const functions = [...new Set([...entries, ...declarations])];
  const wat = bodies + '\n(table $' + tableName + ' (export "' + tableExport + '") ' + functions.length + ' ' + functions.length + ' funcref)\n(elem (table $' + tableName + ') (i32.const 0) func ' + functions.join(' ') + ')\n';
  return {wat, entries, declarations, callCounts};
}
