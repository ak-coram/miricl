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

function exerciseGenericTransport(runtime) {
  const evaluate = source => runtime.evaluate(source, {eof: true}).output;
  const generic = lispValue(runtime, "#'soundness-transport-generic");
  const left = lispValue(runtime, '3.0f0');
  const right = lispValue(runtime, '-0.0f0');
  const call = () => runtime.vm['native-scalar-call'](generic, left, right, null, null, 2, 3);
  const callWithFloat = value => runtime.vm['native-scalar-call-with-operand'](
    generic, left, null, null, null, 2, 3, floatBits(value), (3 << 2) | 2);
  const warm = () => assert.equal(evaluate(`(and
    (equal (multiple-value-list (soundness-transport-generic 3 4)) '(7 :integer))
    (= (soundness-transport-generic-consumer 3.0f0 4.0f0) 24.0f0)
    (= (soundness-transport-generic-pipeline 3.0f0 4.0f0) 30.0f0)
    (= (soundness-transport-generic-consumer 3.0f0 4.0f0) 24.0f0))`), 'T\n');
  warm();
  assert.deepEqual(call(), [floatBits(-0), 1, null]);
  assert.deepEqual(callWithFloat(-0), [floatBits(-0), 1, null]);
  assert.throws(() => runtime.vm['native-scalar-call-with-operand'](
    generic, left, right, null, null, 2, 3, floatBits(1), (3 << 2) | 2),
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
  const invokeForward = (key, input, result, bits) => runtime.vm['native-scalar-call-with-operand'](
    forward, key, null, null, null, 2, result, bits, (input << 2) | 2);
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
    exerciseGenericTransport(runtime);
    runtime.mountFile('/scalar-consumers.lisp',
      await readFile(new URL('./soundness-scalar-consumers.lisp', import.meta.url)));
    runtime.evaluate('(load "/scalar-consumers.lisp") (values)', {eof: true});
    assert.equal(runtime.evaluate('(soundness-scalar-consumers)', {eof: true}).output, 'T\n');
    const saved = runtime.saveImage();
    runtime.close();
    runtime = await Miricl.instantiate(saved);
    exerciseTransport(runtime);
    exerciseGenericTransport(runtime);
  } finally {
    runtime.close();
  }
}
