import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
import {coaltonFiles} from '../scripts/coalton-files.mjs';
const files = await coaltonFiles();
assert.equal(new Set(files.map(([name]) => name)).size, files.length);
const contents = new Map(files);
assert.match(contents.get('/coalton/src/source.lisp').toString(), /make-string-input-stream/);
assert.match(contents.get('/quicklisp/trivial-garbage/trivial-garbage.lisp').toString(), /WasmGC finalizers are unavailable/);
assert.equal(contents.get('/quicklisp/closer-mop/closer-mop.asd').toString(),
  '(asdf:defsystem "closer-mop" :components ((:file "closer-miricl")))');
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
try {
  vm.mountFile('/closer-miricl.lisp', contents.get('/quicklisp/closer-mop/closer-miricl.lisp'));
  assert.equal((await vm.evaluateAsync('(progn (load "/closer-miricl.lisp") (values))', {eof: true})).output, '');
  for (const name of ['/quicklisp/float-features/package.lisp', '/quicklisp/float-features/float-features.lisp'])
    vm.mountFile(name, contents.get(name));
  await vm.evaluateAsync(`(load "/quicklisp/float-features/package.lisp")
    (load "/quicklisp/float-features/float-features.lisp") (values)`, {eof: true});
  const check = async source => assert.equal((await vm.evaluateAsync(source, {eof: true})).output, 'T\n', source);
  await check(`(and (= (float-features:single-float-bits 1.0f0) #x3f800000)
    (= (float-features:double-float-bits -1.0d0) #xbff0000000000000)
    (typep (float-features:bits-single-float #x3f800000) 'single-float)
    (typep (float-features:bits-double-float #x3ff0000000000000) 'double-float)
    (not (float-features:float-infinity-p most-positive-double-float))
    (not (float-features:float-nan-p 0.0f0)))`);
  let state = 42;
  const random = () => state = (Math.imul(state, 1664525) + 1013904223) >>> 0;
  for (const width of [32, 64]) {
    const bits = width === 32 ? [0n, 0x80000000n, 1n, 0x80000001n, 0x7f7fffffn, 0xff7fffffn]
      : [0n, 0x8000000000000000n, 1n, 0x8000000000000001n, 0x7fefffffffffffffn, 0xffefffffffffffffn];
    for (let i = 0; i < 500; i++) {
      const value = width === 32 ? BigInt(random()) : BigInt(random()) << 32n | BigInt(random());
      if (width === 32 ? (value & 0x7f800000n) !== 0x7f800000n : (value & 0x7ff0000000000000n) !== 0x7ff0000000000000n)
        bits.push(value);
    }
    const format = width === 32 ? 'single' : 'double';
    for (let offset = 0; offset < bits.length; offset += 25)
      await check(`(every (lambda (bits) (= bits
        (float-features:${format}-float-bits (float-features:bits-${format}-float bits))))
        '(${bits.slice(offset, offset + 25).join(' ')}))`);
  }
  for (const name of ['infinity', 'nan']) {
    const file = `/quicklisp/float-features/${name}.lisp`;
    vm.mountFile(file, contents.get(file));
    await vm.evaluateAsync(`(load "${file}") (values)`, {eof: true});
  }
  await check(`(and
    (float-features:float-infinity-p float-features:single-float-positive-infinity)
    (float-features:float-infinity-p float-features:double-float-negative-infinity)
    (float-features:float-nan-p float-features:single-float-nan)
    (float-features:float-nan-p float-features:double-float-nan)
    (not (float-features:float-nan-p float-features:single-float-positive-infinity)))`);
  console.log('Coalton host: unique guest paths, native IEEE bit conversions, signed zero/subnormals, infinity and NaN passed.');
} finally {vm.close();}
