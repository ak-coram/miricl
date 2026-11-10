// Explicit IEEE construction uses Wasm reinterpretation, including nonfinite
// values; ordinary arithmetic keeps its error checks. Exercise both FASL paths.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const image = await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm');
const vm = await Miricl.instantiate(image);
const check = (vm, source) => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)', {eof: true});
  check(vm, `(and (= (miricl:float-bits 1f0) #x3f800000)
    (= (miricl:float-bits -1d0) #xbff0000000000000)
    (typep (miricl:bits-float 0 0f0) 'single-float)
    (typep (miricl:bits-float 0 0d0) 'double-float)
    (handler-case (miricl:float-bits 1) (type-error () t))
    (handler-case (miricl:bits-float 0 0) (type-error () t))
    (handler-case (miricl:bits-float -1 0f0) (type-error () t))
    (handler-case (miricl:bits-float #x100000000 0f0) (type-error () t))
    (handler-case (miricl:bits-float #x10000000000000000 0d0) (type-error () t)))`);
  let seed = 31;
  const random = () => seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0;
  for (const width of [32, 64]) {
    const bits = width === 32 ? [0n, 0x80000000n, 1n, 0x80000001n, 0x7f800000n, 0xff800000n]
      : [0n, 0x8000000000000000n, 1n, 0x8000000000000001n, 0x7ff0000000000000n, 0xfff0000000000000n];
    for (let i = 0; i < 500; i++) {
      const value = width === 32 ? BigInt(random()) : BigInt(random()) << 32n | BigInt(random());
      if (width === 32 ? (value & 0x7f800000n) !== 0x7f800000n : (value & 0x7ff0000000000000n) !== 0x7ff0000000000000n)
        bits.push(value);
    }
    for (let offset = 0; offset < bits.length; offset += 25)
      check(vm, `(every (lambda (bits) (= bits (miricl:float-bits
        (miricl:bits-float bits ${width === 32 ? '0f0' : '0d0'})))) '(${bits.slice(offset, offset + 25).join(' ')}))`);
  }
  check(vm, `(let ((nan (miricl:bits-float #x7ff8000000000000 0d0))
                  (infinity (miricl:bits-float #x7ff0000000000000 0d0)))
    (and (string= (write-to-string nan) "#<DOUBLE-FLOAT NaN>")
         (string= (write-to-string infinity) "#<DOUBLE-FLOAT +Infinity>")
         (handler-case (write-to-string nan :readably t) (print-not-readable () t))
         (handler-case (rational infinity) (floating-point-invalid-operation () t))
         (handler-case (integer-decode-float nan) (floating-point-invalid-operation () t))
         (handler-case (+ infinity 1d0) (floating-point-invalid-operation () t))))`);
  vm.mountFile('/ieee.lisp', `
    (defmacro ieee-literals ()
      (list 'quote (vector (miricl:bits-float #x7fc00000 0f0)
                          (miricl:bits-float #x7ff8000000000000 0d0)
                          (miricl:bits-float #xfff0000000000000 0d0))))
    (defparameter *ieee-values* (ieee-literals))
    (defun ieee-value (n) (aref *ieee-values* n))`);
  vm.evaluate('(compile-file "/ieee.lisp") (load "/ieee.wasm") (values)', {eof: true});
  const portable = await vm.readFile('/ieee.wasm');
  vm.evaluate('(miricl-compiler:compile-native-bundle "/ieee.wasm" "/ieee-native.wasm") (values)', {eof: true});
  const native = await vm.readFile('/ieee-native.wasm');
  for (const [name, bytes] of [['portable', portable], ['native', native]]) {
    const fresh = await Miricl.instantiate(image);
    try {
      fresh.mountFile('/ieee.wasm', bytes);
      fresh.evaluate('(load "/ieee.wasm") (values)', {eof: true});
      check(fresh, `(and (= (miricl:float-bits (ieee-value 0)) #x7fc00000)
        (= (miricl:float-bits (ieee-value 1)) #x7ff8000000000000)
        (= (miricl:float-bits (ieee-value 2)) #xfff0000000000000))`);
      console.log(`PASS nonfinite literals in fresh ${name} FASL load`);
    } finally {fresh.close();}
  }
  console.log('IEEE float bits: exact finite/infinite round trips, types, printing, error checks and compiled literals pass.');
} finally {vm.close();}
