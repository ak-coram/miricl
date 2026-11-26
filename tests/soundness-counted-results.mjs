import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {loadMiricl} from '../scripts/miricl-loader.mjs';

export async function checkCountedResults(image) {
  const vm = await loadMiricl(await readFile(image));
  const evaluate = source => vm.evaluate(source, {eof: true}).output;
  const section = (id, content) => [id, content.length, ...content];
  const localGet = index => [0x20, index, 0xfb, 0x1a, 0xfb, 0x17, 0x6d];
  const content = bytes => [bytes.length + 1, 0, ...bytes];
  const foreign = new WebAssembly.Instance(new WebAssembly.Module(Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    // The two canonical eqref storage types differ in element mutability.
    ...section(1, [4, 0x5e, 0x6d, 0, 0x5e, 0x6d, 1,
      0x60, 1, 0x6f, 1, 0x6f, 0x60, 0, 1, 0x6f]),
    ...section(3, [3, 2, 3, 3]),
    ...section(7, [3, 4, 109, 97, 107, 101, 0, 0,
       5, 115, 104, 111, 114, 116, 0, 1, 7, 112, 114, 105, 118, 97, 116, 101, 0, 2]),
    ...section(10, [3,
      ...content([0xd0, 0x6d, 0xd0, 0x6d, ...localGet(0),
        0xd0, 0x6d, 0xd0, 0x6d, 0xd0, 0x6d, 0xd0, 0x6d,
        0xfb, 8, 0, 7, 0xfb, 0x1b, 0x0b]),
      ...content([0x41, 0, 0xfb, 7, 0, 0xfb, 0x1b, 0x0b]),
      ...content([0x41, 0, 0xfb, 7, 1, 0xfb, 0x1b, 0x0b])]),
  ]))).exports;

  const project = vm.vm['native-captured-result-ref'];
  const materialize = vm.vm['native-captured-extra-list'];
  const return8 = vm.vm['native-return-counted8'];
  const token = vm.javascript.makeText('result carrier reference');
  const ordinary = foreign.make(token);
  const malformedSlot = foreign.make(foreign.private());
  const reject = operation => {
    assert.throws(() => {
      try {operation();}
      catch (cause) {
        assert.ok(cause instanceof WebAssembly.Exception, 'Malformed ingress signaled a Lisp condition');
        throw vm.failure(cause);
      }
    }, error => error.code > 0 && error.message.length > 0);
    assert.equal(evaluate('(+ 41 1)'), '42\n');
    assert.equal(project(ordinary, 3, 4), token);
  };

  const cons = vm.vm['native-cons'];
  const list = items => items.reduceRight((tail, item) => cons(item, tail), null);
  const object = expression => {
    const saved = vm.javascript.callback;
    let closure;
    vm.javascript.callback = function (value) {closure = value; return saved.call(this, value);};
    try {evaluate(`(js:callback (lambda () ${expression})) (values)`);}
    finally {vm.javascript.callback = saved;}
    assert.ok(closure);
    return vm.vm['native-call0'](closure);
  };

  try {
    const legacy = list([null, null, token, null, null, null, token, null, null]);
    const cycle = object('(let ((x (list nil nil nil))) (setf (cdddr x) x) x)');
    const dotted = cons(null, cons(null, token));
    const badElement = list([null, null, foreign.private()]);
    const badTail = cons(null, cons(null, foreign.private()));
    for (const malformed of [cycle, dotted, badElement, badTail, null]) {
      reject(() => project(malformed, 3, 4));
      reject(() => project(malformed, 4, 4));
      reject(() => materialize(malformed, 4));
    }
    reject(() => project(legacy, 9, 9));
    reject(() => materialize(legacy, 11));
    assert.equal(project(legacy, 3, 10), token);
    assert.equal(project(legacy, 7, 10), token);
    assert.equal(project(legacy, 10, 10), null);
    assert.equal(materialize(legacy, 10), legacy);

    assert.equal(project(ordinary, 3, 4), token);
    assert.equal(project(ordinary, 4, 4), null);
    assert.equal(vm.vm['native-argument'](materialize(ordinary, 4), 2), token);
    for (const operation of [
      () => project(foreign.short(), 3, 4),
      () => project(malformedSlot, 3, 4),
      () => project(foreign.private(), 3, 4),
      () => project(ordinary, 3, -1),
      () => project(ordinary, 3, 9),
      () => project(ordinary, 2, 4),
      () => materialize(malformedSlot, 4),
      () => materialize(foreign.short(), 4),
      () => materialize(null, 4),
      () => return8(token, null, null, token, null, null, null, foreign.private(), 4),
      () => return8(token, null, null, token, null, null, null, null, 9),
    ]) reject(operation);
    assert.equal(return8(token, null, null, token, null, null, null, null, 4), token);
    assert.equal(evaluate('(multiple-value-list (values 1 nil 3 4 5 6 7 8))'), '(1 NIL 3 4 5 6 7 8)\n');
    // Keep normal dispatch polls inactive. The worker enables scheduling
    // after publishing the carrier and at the exact observation under test.
    evaluate(`(defparameter *carrier-ambient-mark* nil)
      (let* ((primitive (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%THREAD")))
             (thread (funcall primitive 0 nil nil)))
        (funcall primitive 16 nil nil)
        (funcall primitive 12 thread
          (lambda () (setq *carrier-ambient-mark* :ran) (values :clobber :second :third)))
        (funcall primitive 13 0 nil)) (values)`);
    const name = value => [value.length, ...new TextEncoder().encode(value)];
    const imported = (id, type) => [...name('m'), ...name(id), ...type];
    const integer = n => [0x41, n, 0xfb, 0x1c, 0xfb, 0x1b];
    const bytes = Uint8Array.from([
      0, 97, 115, 109, 1, 0, 0, 0,
      ...section(1, [3,
        0x60, 9, ...Array(8).fill(0x6f), 0x7f, 1, 0x6f,
        0x60, 0, 0, 0x60, 2, 0x6f, 0x6f, 1, 0x6f]),
      ...section(2, [3, ...imported('return8', [0, 0]), ...imported('poll', [0, 1]),
        ...imported('active', [3, 0x7f, 1])]),
      ...section(3, [1, 2]),
      ...section(7, [1, ...name('entry'), 0, 2]),
      ...section(10, [1, ...content([
        ...[11, 12, 13, 14, 15, 16, 17, 18].flatMap(integer),
        0x41, 8, 0x10, 0,                 // Publish the eight values.
        0x41, 1, 0x24, 0, 0x10, 1,       // Enable scheduling and service the interrupt.
        0x41, 0, 0x24, 0, 0x0b,          // Leave the original primary on the stack.
      ])]),
    ]);
    const active = vm.vm['thread-active'], poll = vm.vm['native-poll'];
    const {entry} = new WebAssembly.Instance(new WebAssembly.Module(bytes),
      {m: {return8, poll, active}}).exports;
    const worker = vm.vm['native-function'](entry, null, null, bytes.length);
    vm.vm['thread-enter'](0); vm.vm['thread-main-state'](0);
    try {
      const [primary, second, third, storage, count] = vm.vm['native-call-captured1'](worker, null);
      assert.deepEqual([primary, second, third, count], [11, 12, 13, 8]);
      assert.deepEqual([3, 4, 5, 6, 7].map(index => project(storage, index, count)), [14, 15, 16, 17, 18]);
    } finally {
      active.value = 0;
      vm.vm['thread-save']();
      vm.vm['thread-main-state'](1);
    }
    assert.equal(evaluate('*carrier-ambient-mark*'), ':RAN\n');
    console.log('Soundness: raw captured results reject malformed references and retain values across interrupt delivery.');
  } finally {
    vm.close();
  }
}
