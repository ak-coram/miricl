// Optional integration check: node tests/standalone.mjs interpreter.wasm
import fs from 'node:fs';
import assert from 'node:assert/strict';
const module = await WebAssembly.compile(fs.readFileSync(process.argv[2] ?? 'interpreter.wasm'));
assert.deepEqual(WebAssembly.Module.imports(module), []);
const {exports: vm} = await WebAssembly.instantiate(module);
vm.initialize();
const list = (items) => items.reduceRight((cdr, car) => ({car, cdr}), null);
const unlist = (cell) => {const result = []; while (cell) {result.push(cell.car); cell = cell.cdr;} return result;};
function encode(object) {
  const bytes = [];
  const u32 = (n) => {for (let i = 0; i < 4; i++) bytes.push((n >>> (i * 8)) & 255);};
  function write(x) {
    if (x === null) return u32(0);
    if (Array.isArray(x)) return write(list(x));
    if (typeof x === 'bigint') {
      u32(1); u32(x < 0n ? -1 : x > 0n ? 1 : 0);
      let magnitude = x < 0n ? -x : x; const digits = [];
      do {digits.push(Number(magnitude & 32767n)); magnitude >>= 15n;} while (magnitude);
      u32(digits.length); digits.forEach(u32); return;
    }
    if (typeof x === 'string') {u32(10); u32(0); u32([...x].length); for (const char of x) u32(char.codePointAt(0)); return;}
    if (x.kind === 'symbol') {u32(6); u32(x.id); return;}
    if (x.kind === 'instance') {u32(23); u32(x.id); return;}
    if (x.kind === 'package') {u32(12); u32(x.id); return;}
    u32(7); write(x.car); write(x.cdr);
  }
  write(object); return Uint8Array.from(bytes);
}
function decode(length) {
  const data = new DataView(vm.memory.buffer, 0, length); let position = 0;
  const u32 = () => {const result = data.getUint32(position, true); position += 4; return result;};
  function read() {
    switch (u32()) {
      case 0: return null;
      case 1: {const sign = u32(), length = u32(); let x = 0n; for (let i = 0; i < length; i++) x |= BigInt(u32()) << BigInt(15 * i); return sign === 0xffffffff ? -x : x;}
      case 2: return {numerator: read(), denominator: read()};
      case 6: return {kind: 'symbol', id: u32(), name: read(), package: read()};
      case 23: return {kind: 'instance', id: u32()};
      case 7: return {car: read(), cdr: read()};
      case 10: {u32(); const length = u32(); return Array.from({length}, () => String.fromCodePoint(u32())).join('');}
      case 12: return {kind: 'package', id: u32(), name: read()};
      default: throw Error('Unexpected tag');
    }
  }
  assert.equal(u32(), 14); return unlist(read());
}
function request(operation, form, ...args) {
  const input = encode(form);
  if (input.length > vm.memory.buffer.byteLength) vm.memory.grow(Math.ceil((input.length - vm.memory.buffer.byteLength) / 65536));
  new Uint8Array(vm.memory.buffer).set(input); vm.error.value = 0;
  return decode(vm[operation](input.length, ...args));
}
const symbols = new Map();
function S(name, pkg = 'CL') {
  const key = `${pkg}:${name}`;
  if (!symbols.has(key)) symbols.set(key, request('resolve-symbol', [name, pkg], 0)[0]);
  return symbols.get(key);
}
const evaluate = (form) => request('evaluate', form);
evaluate([S('DEFPACKAGE'), 'NODE-DEMO', [S('USE', 'KEYWORD'), 'CL'], [S('EXPORT', 'KEYWORD'), 'FACT']]);
const fact = S('FACT', 'NODE-DEMO'), n = S('N', 'NODE-DEMO');
evaluate([S('DEFUN'), fact, [n], [S('IF'), [S('ZEROP'), n], 1n, [S('*'), n, [fact, [S('1-'), n]]]]]);
assert.equal(evaluate([fact, 30n])[0], 265252859812191058636308480000000n);
assert.deepEqual(evaluate([S('/'), 2n, 3n])[0], {numerator: 2n, denominator: 3n});
assert.equal(request('resolve-symbol', ['FACT', 'NODE-DEMO'], 1)[0].id, fact.id);
assert.equal(evaluate([S('INTERN'), 'N', 'NODE-DEMO'])[1].name, 'INTERNAL');
// Instance and class handles preserve identity through the binary protocol.
const pointName = S('POINT', 'NODE-DEMO'), xSlot = S('X', 'NODE-DEMO');
const pointReader = S('POINT-X', 'NODE-DEMO');
const pointClass = evaluate([S('DEFCLASS'), pointName, [],
  [[xSlot, S('INITARG', 'KEYWORD'), S('X', 'KEYWORD'), S('ACCESSOR', 'KEYWORD'), pointReader]]])[0];
assert.equal(pointClass.kind, 'instance');
const point = evaluate([S('MAKE-INSTANCE'), [S('QUOTE'), pointName], S('X', 'KEYWORD'), 42n])[0];
assert.equal(point.kind, 'instance');
assert.equal(evaluate([pointReader, point])[0], 42n);
assert.equal(evaluate(point)[0].id, point.id);
// Recursive cdr descent exhausts V8's WASM stack well before this length.
for (const operator of ['EQUAL', 'EQUALP']) {
  const makeLongList = () => [S('MAKE-LIST'), 20000n, S('INITIAL-ELEMENT', 'KEYWORD'), 1n];
  assert.equal(evaluate([S(operator), makeLongList(), makeLongList()])[0].name, 'T');
}
console.log('Standalone Node: no imports; packages, arithmetic, multiple values, instance transport, and long-list equality pass.');
