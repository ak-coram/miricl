import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

function floatBits(number) {
  const view = new DataView(new ArrayBuffer(8));
  view.setFloat64(0, number);
  return view.getBigInt64(0);
}

function lispValue(runtime, expression) {
  const callback = runtime.javascript.callback;
  let thunk;
  runtime.javascript.callback = function (functionObject) {
    thunk = functionObject;
    return callback.call(this, functionObject);
  };
  try {
    runtime.evaluate(`(progn (js:callback (lambda () ${expression})) (values))`, {eof: true});
    return runtime.vm['native-call0'](thunk);
  } finally {
    runtime.javascript.callback = callback;
  }
}

function exerciseTransport(runtime) {
  const scalarCall = runtime.vm['native-scalar-call'];
  const invoke = (name, kind, ...operands) => scalarCall(
    lispValue(runtime, `#'soundness-transport-${name}`),
    ...operands, ...Array(4 - operands.length).fill(null), operands.length, kind);
  const cases = [
    ['mask', 1, ['#*1', '0'], 1n],
    ['mask', 1, ['#(35184372088839)', '0'], 35184372088839n],
    ['bit', 1, ['1'], 1n],
    ['immediate', 1, ['-20'], -20n],
    ['character', 2, ['"λ"', '0'], 955n],
    ['character-argument', 2, [':boxed', '#\\Q'], 81n],
    ['single', 3, ['-0.0f0', '2.0f0'], floatBits(-0)],
    ['single', 3, ['1.0f-40', '2.0f0'], floatBits(Math.fround(Math.fround(1e-40) * 2))],
    ['double', 4, ['1.5f0', '2.5d0'], floatBits(4)],
    ['mixed', 3, ['-3', '1.5f0'], floatBits(-4.5)],
  ];
  for (const [name, kind, forms, bits] of cases)
    assert.deepEqual(invoke(name, kind, ...forms.map(form => lispValue(runtime, form))),
      [bits, 1, null], name);

  const bignum = lispValue(runtime, '(ash 1 80)');
  assert.deepEqual(invoke('integer', 1, bignum), [0n, 0, bignum]);
  const half = lispValue(runtime, '0.5f0');
  const ordinary = invoke('single', 3, lispValue(runtime, '2.0d0'), half);
  assert.deepEqual(ordinary.slice(0, 2), [0n, 0]);
  assert.notEqual(runtime.vm['native-call2'](lispValue(runtime, "#'eql"),
    ordinary[2], lispValue(runtime, '1.0d0')), null);
  assert.throws(() => invoke('single', 3, lispValue(runtime, '3.0f38'),
    lispValue(runtime, '2.0f0')), WebAssembly.Exception);
  assert.throws(() => invoke('single', 3, runtime.vm['native-arguments'](0), half), WebAssembly.Exception);
  assert.deepEqual(invoke('single', 3, half, half), [floatBits(0.25), 1, null]);
}

export async function checkScalarTransport(image) {
  let runtime = await Miricl.instantiate(image);
  try {
    runtime.mountFile('/scalar-transport.lisp',
      await readFile(new URL('./soundness-scalar-transport.lisp', import.meta.url)));
    runtime.evaluate(`
      (setq miricl-compiler:*compile-evaluations* nil)
      (compile-file "/scalar-transport.lisp" :output-file "/scalar-portable.fasl" :verbose nil :print nil)
      (miricl-compiler:compile-native-bundle "/scalar-portable.fasl" "/scalar-native.fasl")
      (load "/scalar-native.fasl")
      (values)`, {eof: true});
    exerciseTransport(runtime);
    runtime.mountFile('/scalar-consumers.lisp',
      await readFile(new URL('./soundness-scalar-consumers.lisp', import.meta.url)));
    runtime.evaluate('(load "/scalar-consumers.lisp") (values)', {eof: true});
    assert.equal(runtime.evaluate('(soundness-scalar-consumers)', {eof: true}).output, 'T\n');
    const saved = runtime.saveImage();
    runtime.close();
    runtime = await Miricl.instantiate(saved);
    exerciseTransport(runtime);
  } finally {
    runtime.close();
  }
}
