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

function callWithOperands(runtime, generic, arity, result, operands, ...boxed) {
  const kinds = operands.reduce((mask, operand, i) => mask | ((operand?.[0] ?? 0) << (3 * i)), 0);
  const payloads = Array.from({length: 3}, (_, i) => operands[i]?.[1] ?? 0n);
  return runtime.vm['native-scalar-call-with-operands'](
    generic, ...Array.from({length: 4}, (_, i) => boxed[i] ?? null),
    arity, result, ...payloads, kinds);
}

function exerciseGenericTransport(runtime) {
  const evaluate = source => runtime.evaluate(source, {eof: true}).output;
  const generic = lispValue(runtime, "#'soundness-transport-generic");
  const left = lispValue(runtime, '3.0f0');
  const right = lispValue(runtime, '-0.0f0');
  const call = () => runtime.vm['native-scalar-call'](generic, left, right, null, null, 2, 3);
  const callWithFloat = value => callWithOperands(
    runtime, generic, 2, 3, [null, [3, floatBits(value)]], left);
  const warm = () => assert.equal(evaluate(`(and
    (equal (multiple-value-list (soundness-transport-generic 3 4)) '(7 :integer))
    (= (soundness-transport-generic-consumer 3.0f0 4.0f0) 24.0f0)
    (= (soundness-transport-generic-pipeline 3.0f0 4.0f0) 30.0f0)
    (= (soundness-transport-generic-consumer 3.0f0 4.0f0) 24.0f0))`), 'T\n');
  warm();
  assert.deepEqual(call(), [floatBits(-0), 1, null]);
  assert.deepEqual(callWithFloat(-0), [floatBits(-0), 1, null]);
  assert.throws(() => callWithOperands(
    runtime, generic, 2, 3, [null, [3, floatBits(1)]], left, right),
  WebAssembly.Exception);
  // A new EQL specializer makes the raw slot relevant to method selection.
  // The transport must reconstruct it before continuing normal dispatch.
  assert.equal(evaluate(`(unwind-protect
    (progn
      (eval '(defmethod soundness-transport-generic ((left single-float) (right (eql 5.0f0)))
               (declare (ignore left right))
               7.0f0))
      (eql (soundness-transport-generic-pipeline 3.0f0 4.0f0) 14.0f0))
    (remove-method #'soundness-transport-generic
      (find-method #'soundness-transport-generic nil
        (list (find-class 'single-float) (miricl-mop:intern-eql-specializer 5.0f0)))))`), 'T\n');
  try {
    assert.equal(evaluate(`(let ((method (find-method #'soundness-transport-generic
                   nil (list (find-class 'single-float) (find-class t)))))
      (defparameter *soundness-saved-scalar-method* method)
      (eval '(defmethod soundness-transport-generic ((left single-float) right)
               (declare (ignore left right))
               (values 5.0f0 :replacement)))
      (and (= (soundness-transport-generic-consumer 3.0f0 4.0f0) 10.0f0)
           (= (soundness-transport-generic-pipeline 3.0f0 4.0f0) 10.0f0)
           (equal (multiple-value-list (soundness-transport-generic 3.0f0 4.0f0))
                  '(5.0f0 :replacement))))`), 'T\n');
    assert.equal(call()[1], 0);
  } finally {
    evaluate(`(progn
      (add-method #'soundness-transport-generic *soundness-saved-scalar-method*)
      (makunbound '*soundness-saved-scalar-method*)
      (values))`);
  }
  warm();
  assert.deepEqual(call(), [floatBits(-0), 1, null]);
  assert.deepEqual(callWithFloat(-0), [floatBits(-0), 1, null]);
  assert.equal(evaluate(`(let ((original #'soundness-transport-generic))
    (unwind-protect
      (progn
        (setf (fdefinition 'soundness-transport-generic)
              (lambda (left right) (declare (ignore left)) (values right :new-binding)))
        (and (= (soundness-transport-generic-pipeline 3.0f0 4.0f0) 10.0f0)
             (equal (multiple-value-list (soundness-transport-generic 3.0f0 4.0f0))
                    '(4.0f0 :new-binding))
             (= (funcall original 3.0f0 4.0f0) 12.0f0)))
      (setf (fdefinition 'soundness-transport-generic) original)))`), 'T\n');

  const forward = lispValue(runtime, "#'soundness-transport-generic-forward");
  const invokeForward = (key, input, result, bits) => callWithOperands(
    runtime, forward, 2, result, [null, [input, bits]], key);
  for (const [selector, input, result, bits] of [
    [':character', 2, 2, 0x110000n],
    [':immediate', 5, 1, 1073741824n],
    [':bit', 6, 1, 2n],
  ]) {
    const key = lispValue(runtime, selector);
    assert.throws(() => invokeForward(key, input, result, bits), WebAssembly.Exception);
  }
  for (const [selector, value, inputKind, resultKind, bits] of [
    [':integer', '35184372088839', 1, 1, 35184372088839n],
    [':character', '#\\λ', 2, 2, 955n],
    [':double', '-0.0d0', 4, 4, floatBits(-0)],
    [':immediate', '-20', 5, 1, -20n],
    [':bit', '1', 6, 1, 1n],
  ]) {
    const key = lispValue(runtime, selector);
    evaluate(`(progn
      (soundness-transport-generic-forward ${selector} ${value})
      (soundness-transport-generic-forward ${selector} ${value})
      (values))`);
    assert.deepEqual(invokeForward(key, inputKind, resultKind, bits), [bits, 1, null], selector);
  }
  assert.equal(evaluate('(soundness-transport-generic-character "λ" 0)'), '955\n');
  assert.equal(evaluate('(soundness-transport-generic-integer #(35184372088839) 0)'), '7\n');
}

async function observeScalarCalls(records, load) {
  const Instance = WebAssembly.Instance;
  WebAssembly.Instance = new Proxy(Instance, {
    construct(target, [module, imports]) {
      const entry = 'scalar-call-with-operands';
      const invoke = imports?.m?.[entry];
      if (invoke) {
        imports = {...imports, m: {...imports.m, [entry]: (...args) => {
          const result = invoke(...args);
          records.push({kinds: args.at(-1), payloads: args.slice(-4, -1), result: result.slice(0, 2)});
          return result;
        }}};
      }
      return Reflect.construct(target, [module, imports]);
    },
  });
  try {
    return await load();
  } finally {
    WebAssembly.Instance = Instance;
  }
}

function exerciseMultiTransport(runtime, records) {
  const evaluate = source => runtime.evaluate(source, {eof: true}).output;
  const pair = '(soundness-transport-pair-caller 3.0f0 4.0f0)';
  const triple = '(soundness-transport-triple-caller 3.0f0 4.0d0 #(7))';
  const expectRaw = (form, expected, kinds, payloads, result) => {
    const resultBits = typeof result === 'bigint' ? result : floatBits(result);
    assert.equal(evaluate(form), expected); // Populate or refresh the live row.
    assert.equal(evaluate(form), expected);
    records.length = 0;
    assert.equal(evaluate(form), expected);
    assert.ok(records.some(call => call.kinds === kinds &&
      payloads.every((bits, i) => call.payloads[i] === bits) &&
      call.result[0] === resultBits && call.result[1] === 1),
    JSON.stringify(records, (_, value) => typeof value === 'bigint' ? String(value) : value));
  };
  expectRaw(pair, '48.0\n', 3 | (3 << 3), [floatBits(4), floatBits(6)], 24);
  records.length = 0;
  assert.equal(evaluate('(soundness-transport-pair-caller 3.0d0 4.0f0)'), '48.0d0\n');
  assert.ok(records.some(call => call.kinds === (3 << 3) && call.result[1] === 0));
  records.length = 0;
  assert.equal(evaluate(`(let ((original #'soundness-transport-pair))
    (unwind-protect
      (progn
        (setf (fdefinition 'soundness-transport-pair) #'soundness-transport-single)
        ${pair})
      (setf (fdefinition 'soundness-transport-pair) original)))`), '48.0\n');
  assert.ok(records.some(call => call.kinds === (3 | (3 << 3)) && call.result[1] === 1));
  const kinds = 3 | (4 << 3) | (1 << 6);
  const payloads = [floatBits(4), floatBits(6), 7n];
  expectRaw(triple, '62.0d0\n', kinds, payloads, 31);
  const generic = lispValue(runtime, "#'soundness-transport-triple");
  const operands = [[3, payloads[0]], [4, payloads[1]], [1, payloads[2]]];
  const raw = entries => callWithOperands(runtime, generic, 3, 4, entries);
  for (const malformed of [
    [[7, 0n], ...operands.slice(1)],
    [operands[0], [4, floatBits(Infinity)], operands[2]],
    [...operands.slice(0, 2), [2, 0xd800n]],
    [...operands.slice(0, 2), [5, 1073741824n]],
    [...operands.slice(0, 2), [6, 2n]],
    [...operands, [1, 0n]],
  ]) {
    assert.throws(() => raw(malformed), WebAssembly.Exception);
    assert.deepEqual(raw(operands), [floatBits(31), 1, null]);
  }
  assert.throws(() => callWithOperands(runtime, generic, 3, 4, operands,
    null, lispValue(runtime, '6.0d0')), WebAssembly.Exception);
  assert.deepEqual(raw(operands), [floatBits(31), 1, null]);
  assert.throws(() => runtime.vm['native-scalar-call-with-operands'](
    generic, null, null, null, null, 3, 4, 1n, 0n, 0n, 0), WebAssembly.Exception);
  assert.deepEqual(raw(operands), [floatBits(31), 1, null]);

  for (const [type, specializer, expected] of [
    ['integer', "(find-class 'integer)", '82.0d0\n'],
    ['(eql 7)', '(miricl-mop:intern-eql-specializer 7)', '82.0d0\n'],
  ]) {
    try {
      evaluate(`(eval '(defmethod soundness-transport-triple (a b (c ${type}))
        (declare (ignore a b c)) (values 41.0d0 :edited))) (values)`);
      records.length = 0;
      assert.equal(evaluate(triple), expected);
      assert.equal(evaluate(triple), expected);
      assert.ok(records.some(call => call.kinds === kinds && call.result[1] === 0));
      assert.equal(evaluate("(multiple-value-list (soundness-transport-triple 4.0f0 6.0d0 7))"),
        "(41.0d0 :EDITED)\n");
    } finally {
      evaluate(`(remove-method #'soundness-transport-triple
        (find-method #'soundness-transport-triple nil
          (list (find-class t) (find-class t) ${specializer}))) (values)`);
    }
  }
  expectRaw(triple, '62.0d0\n', kinds, payloads, 31);

  const forward = lispValue(runtime, "#'soundness-transport-triple-forward");
  evaluate("(soundness-transport-triple-forward -20 1 #\\Q)");
  evaluate("(soundness-transport-triple-forward -20 1 #\\Q)");
  assert.deepEqual(callWithOperands(runtime, forward, 3, 2, [[5, -20n], [6, 1n], [2, 81n]]),
    [81n, 1, null]);
  expectRaw('(soundness-transport-triple-forward-caller #*1 "AQ")', '81\n',
    5 | (6 << 3) | (2 << 6), [65n, 1n, 81n], 81n);
  const small = lispValue(runtime, "#'soundness-transport-triple-small");
  for (let i = 0; i < 2; ++i)
    assert.equal(evaluate('(soundness-transport-triple-small 4.0f0 1 -20)'), '-15.0\n');
  assert.deepEqual(callWithOperands(runtime, small, 3, 3, [[3, floatBits(4)], [6, 1n], [5, -20n]]),
    [floatBits(-15), 1, null]);
  expectRaw('(soundness-transport-triple-small-caller 3.0f0 #*1 "A")', '140.0\n',
    3 | (6 << 3) | (5 << 6), [floatBits(4), 1n, 65n], 70);
}

export async function checkScalarTransport(image, {sourceCompiler = false} = {}) {
  const records = [];
  let runtime = await Miricl.instantiate(image);
  try {
    runtime.mountFile('/scalar-transport.lisp',
      await readFile(new URL('./soundness-scalar-transport.lisp', import.meta.url)));
    if (sourceCompiler) {
      runtime.mountFile('/wasm-compiler.lisp', await readFile(new URL('../wasm-compiler.lisp', import.meta.url)));
      runtime.evaluate(`(setq miricl-compiler:*compile-evaluations* nil
        miricl-compiler::*compiler-state* :interpreted)
        (load "/wasm-compiler.lisp") (values)`, {eof: true});
    }
    await observeScalarCalls(records, () => runtime.evaluate(`
      (setq miricl-compiler:*compile-evaluations* nil)
      (compile-file "/scalar-transport.lisp" :output-file "/scalar-portable.fasl" :verbose nil :print nil)
      (miricl-compiler:compile-native-bundle "/scalar-portable.fasl" "/scalar-native.fasl")
      (load "/scalar-native.fasl")
      (values)`, {eof: true}));
    exerciseTransport(runtime);
    exerciseGenericTransport(runtime);
    exerciseMultiTransport(runtime, records);
    runtime.mountFile('/scalar-consumers.lisp',
      await readFile(new URL('./soundness-scalar-consumers.lisp', import.meta.url)));
    runtime.evaluate('(load "/scalar-consumers.lisp") (values)', {eof: true});
    assert.equal(runtime.evaluate('(soundness-scalar-consumers)', {eof: true}).output, 'T\n');
    const saved = runtime.saveImage();
    runtime.close();
    runtime = await observeScalarCalls(records, () => Miricl.instantiate(saved));
    exerciseTransport(runtime);
    exerciseGenericTransport(runtime);
    exerciseMultiTransport(runtime, records);
  } finally {
    runtime.close();
  }
}
