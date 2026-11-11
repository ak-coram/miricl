// Exercise representation boundaries against JavaScript's independent BigInt
// arithmetic, in interpreted and native compiled functions.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {Miricl} from '../scripts/miricl.mjs';

const image = process.argv[2] ?? 'miricl-precompiled.wasm';
const vm = await Miricl.instantiate(fs.readFileSync(image));
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const check = source => assert.equal(evaluate(source), 'T\n', source);

// A separate Wasm module inspects actual GC types, including immediates.
// These match the runtime's canonical DIGITS, OBJ and boxed scalar subtype.
const section = (id, data) => [id, ...uleb(data.length), ...data];
function uleb(n) {const out = []; do {out.push((n & 127) | (n > 127 ? 128 : 0)); n >>>= 7;} while (n); return out;}
const fields = [0x7f,0, 0x7f,1, 0x7c,0, 0x63,0,0, 0x6d,1, 0x6d,1, 0x6d,1];
const body = [0, 0x20,0, 0xfb,0x1a, 0xfb,0x14,0x6c, 0x04,0x7f, 0x41,0,
  0x05, 0x20,0, 0xfb,0x1a, 0xfb,0x14,2, 0x04,0x7f, 0x41,1,
  0x05, 0x41,2, 0x0b,0x0b,0x0b];
const inspector = new WebAssembly.Instance(new WebAssembly.Module(Uint8Array.from([
  0,97,115,109,1,0,0,0,
  ...section(1, [4, 0x5e,0x7f,1, 0x4e,1,0x50,0,0x5f,7,...fields,
    0x4f,1,1,0x5f,8,...fields,0x7e,0, 0x60,1,0x6f,1,0x7f]),
  ...section(3, [1,3]), ...section(7, [1,4,116,105,101,114,0,0]),
  ...section(10, [1,...uleb(body.length),...body]),
]))).exports.tier;

let checks = 0;
try {
  evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  const values = [...new Set([0n, 1n, -1n,
    ...[15n, 30n, 31n, 32n, 53n, 60n, 63n, 64n, 90n, 130n].flatMap(bits =>
      [-1n, 0n, 1n].flatMap(delta => {const n = (1n << bits) + delta; return [n, -n];}))])];
  for (const n of values) {
    const value = vm.javascript.makeInteger(n);
    const expected = n >= -(1n << 30n) && n < (1n << 30n) ? 0
      : n >= -(1n << 63n) && n < (1n << 63n) ? 1 : 2;
    assert.equal(inspector(value), expected, `representation of ${n}`);
    assert.equal(vm.javascript.integer(value), n, `JS integer round trip ${n}`);
  }
  check(`(and (= most-negative-fixnum (- (expt 2 63)))
    (= most-positive-fixnum (1- (expt 2 63)))
    (equal (multiple-value-list (subtypep 'fixnum '(signed-byte 64))) '(t t))
    (equal (multiple-value-list (subtypep '(signed-byte 64) 'fixnum)) '(t t)))`);
  for (const n of values) {
    check(`(and (typep ${n} '${n >= -(1n << 63n) && n < (1n << 63n) ? 'fixnum' : 'bignum'})
      (eql ${n} (read-from-string (write-to-string ${n})))
      (eql ${n} (car (list ${n}))) (eql ${n} (aref (vector ${n}) 0)))`);
    for (const test of ['eq', 'eql', 'equal', 'equalp'])
      check(`(let ((table (make-hash-table :test '${test})) (key ${n}))
        (setf (gethash key table) :found) (eq (gethash key table) :found))`);
  }
  for (const compiled of [false, true]) {
    for (const [name, body] of [
      ['plus', '(+ a b)'], ['minus', '(- a b)'], ['times', '(* a b)'],
      ['quotient', '(multiple-value-list (truncate a b))'],
      ['floor', '(multiple-value-list (floor a b))'], ['ceiling', '(multiple-value-list (ceiling a b))'],
      ['bits', '(list (logand a b) (logior a b) (logxor a b))'],
      ['logtest', '(not (null (logtest a b)))'],
    ]) {
      const lambda = `(lambda (a b) ${body})`;
      evaluate(`(setf (symbol-function 'tier-${name})
        ${compiled ? `(miricl-compiler:compile-native '${lambda})` : `(function ${lambda})`}) (values)`);
    }
    const forms = [];
    const expect = (expression, expected) => {forms.push(`(unless (equal ${expression} '${expected}) (error "Integer tier mismatch: ~S" '${expression}))`); checks++;};
    for (const a of values) for (const b of values) {
      expect(`(tier-plus ${a} ${b})`, a + b);
      expect(`(tier-minus ${a} ${b})`, a - b);
      expect(`(tier-times ${a} ${b})`, a * b);
      expect(`(tier-bits ${a} ${b})`, `(${a & b} ${a | b} ${a ^ b})`);
      expect(`(tier-logtest ${a} ${b})`, (a & b) !== 0n ? 't' : 'nil');
      expect(`(list (< ${a} ${b}) (= ${a} ${b}) (> ${a} ${b}))`,
        `(${a < b ? 't' : 'nil'} ${a === b ? 't' : 'nil'} ${a > b ? 't' : 'nil'})`);
      if (b) {
        const q = a / b, r = a % b;
        expect(`(tier-quotient ${a} ${b})`, `(${q} ${r})`);
        const fq = r && (a < 0n) !== (b < 0n) ? q - 1n : q;
        const cq = r && (a < 0n) === (b < 0n) ? q + 1n : q;
        expect(`(tier-floor ${a} ${b})`, `(${fq} ${a - fq * b})`);
        expect(`(tier-ceiling ${a} ${b})`, `(${cq} ${a - cq * b})`);
      }
    }
    for (const a of values) for (const bits of [0n, 1n, 15n, 30n, 31n, 32n, 62n, 63n, 64n, 65n, 131n]) {
      expect(`(ash ${a} ${bits})`, a << bits);
      expect(`(ash ${a} ${-bits})`, a >> bits);
    }
    for (let start = 0; start < forms.length; start += 100)
      evaluate(forms.slice(start, start + 100).join('\n') + '\n(values)');
    console.log(`PASS ${compiled ? 'compiled' : 'interpreted'} integer arithmetic, division and bits (${forms.length} checks)`);
  }
  check(`(and (handler-case (/ 1 0) (division-by-zero () t))
    (handler-case (car 17) (type-error () t))
    (= (+ 20 22) 42))`);
  check(`(and
    (handler-case (progn (logtest 0 :invalid) nil) (type-error () t))
    (handler-case (progn (logtest :invalid 0) nil) (type-error () t)))`);
  // Widely separated limbs exercise unequal bitmap lengths and zero tails.
  for (const a of [0n, 1n << 600n, (1n << 4095n) | 1n, -(1n << 600n)])
    for (const b of [0n, 1n, 1n << 600n, 1n << 8190n, -1n])
      check(`(eql (not (null (tier-logtest ${a} ${b}))) ${(a & b) !== 0n ? 't' : 'nil'})`);
  const bitChecks = [];
  const bitExpect = (expression, result) => {
    bitChecks.push(`(unless (equal ${expression} '${result}) (error "Bit boundary mismatch: ~S" '${expression}))`);
    checks++;
  };
  for (const n of values) {
    const magnitude = n < 0n ? ~n : n;
    bitExpect(`(integer-length ${n})`, magnitude ? magnitude.toString(2).length : 0);
    bitExpect(`(logcount ${n})`, [...magnitude.toString(2)].filter(c => c === '1').length);
    for (const width of [0n, 1n, 15n, 30n, 31n, 32n, 60n, 63n, 64n, 65n, 130n])
      for (const position of [0n, 1n, 30n, 63n, 64n, 130n]) {
        const mask = (1n << width) - 1n, shifted = mask << position;
        bitExpect(`(ldb (byte ${width} ${position}) ${n})`, (n >> position) & mask);
        bitExpect(`(dpb ${~n} (byte ${width} ${position}) ${n})`,
          (n & ~shifted) | ((~n & mask) << position));
        bitExpect(`(deposit-field ${~n} (byte ${width} ${position}) ${n})`,
          (n & ~shifted) | (~n & shifted));
      }
    for (const other of [-1n, 0n, 1n << 63n, -(1n << 90n)]) {
      const operations = {clr: 0n, set: -1n, 1: n, 2: other, c1: ~n, c2: ~other,
        and: n & other, ior: n | other, xor: n ^ other, eqv: ~(n ^ other),
        nand: ~(n & other), nor: ~(n | other), andc1: ~n & other, andc2: n & ~other,
        orc1: ~n | other, orc2: n | ~other};
      for (const [op, result] of Object.entries(operations))
        bitExpect(`(boole boole-${op} ${n} ${other})`, result);
    }
  }
  for (let start = 0; start < bitChecks.length; start += 100)
    evaluate(bitChecks.slice(start, start + 100).join('\n') + '\n(values)');
  // Both inline operations and scalar runtime entries are native paths. The
  // shared entries retain redefinition guards without duplicating each guard
  // in the caller; a generic native-call2 does not satisfy this check.
  for (const [operator, width, instruction, entry, a, b, expected] of [
    ['*', 31, 'i64.mul', 'native-mul-int64', 1073741823n, -1073741824n, -1152921503533105152n],
    ['*', 32, 'i64.mul', 'native-mul-int64', -(1n << 31n), -(1n << 31n), 1n << 62n],
    ['+', 63, 'i64.add', 'native-add-int64', (1n << 62n) - 1n, (1n << 62n) - 1n, (1n << 63n) - 2n],
    ['-', 63, 'i64.sub', 'native-sub-int64', -(1n << 62n), (1n << 62n) - 1n, -(1n << 63n) + 1n],
    ['<', 64, 'i64.lt_s', 'native-lt-int64', -(1n << 63n), (1n << 63n) - 1n, 't'],
  ]) {
    evaluate(`(defparameter *tier-typed* (miricl-compiler:compile-native
      '(lambda (a b) (declare (optimize (debug 2)))
        (values (${operator} (the (signed-byte ${width}) a) (the (signed-byte ${width}) b)))))) (values)`);
    check(`(equal (funcall *tier-typed* ${a} ${b}) '${expected})`);
    check(`(let ((text (with-output-to-string (*standard-output*) (disassemble *tier-typed*))))
      (not (null (or (search "${instruction}" text) (search "call $${entry}" text)))))`);
    check(`(let ((original (symbol-function '${operator})))
      (unwind-protect
        (progn (setf (symbol-function '${operator}) (lambda (a b) (declare (ignore a b)) :replacement))
          (eq (funcall *tier-typed* ${a} ${b}) :replacement))
        (setf (symbol-function '${operator}) original)))`);
  }
  check(`(equal (format nil "~,2F|~10,3F|~,2@F" 3/2 -1.25 123456) "1.50|    -1.250|+123456.00")`);
  vm.mountFile('/integer-tiers.lisp', `(defun tier-constants ()
    '(1073741823 1073741824 -1073741824 -1073741825
      9223372036854775807 -9223372036854775808 9223372036854775808 -9223372036854775809))`);
  evaluate('(compile-file "/integer-tiers.lisp") (load "/integer-tiers.wasm") (values)');
  check(`(equal (tier-constants) '(1073741823 1073741824 -1073741824 -1073741825
      9223372036854775807 -9223372036854775808 9223372036854775808 -9223372036854775809))`);
  const fresh = await Miricl.instantiate(fs.readFileSync(image));
  try {
    fresh.mountFile('/integer-tiers.wasm', await vm.readFile('/integer-tiers.wasm'));
    assert.equal(fresh.evaluate('(load "/integer-tiers.wasm") (tier-constants)', {eof: true}).output,
      'T\n' + evaluate('(tier-constants)'));
  } finally {fresh.close();}
  console.log(`Integer tiers: physical GC representations, transport, types, containers, fresh FASL, typed native arithmetic and ${checks} arithmetic checks passed.`);
} finally {vm.close();}
