import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const name = value => [value.length, ...new TextEncoder().encode(value)];
const section = (id, ...bytes) => [id, bytes.length, ...bytes];
const imported = (scope, field, ...type) => [...name(scope), ...name(field), ...type];
const exported = (field, kind, index) => [...name(field), kind, index];
const body = (...bytes) => [bytes.length + 2, 0, ...bytes, 0x0b];
const token = imported('m', 'poll-token', 0, 0);
const epoch = imported('p', 'epoch', 3, 0x7e, 0);
const active = imported('p', 'active', 3, 0x7f, 0);

function pollModule({imports = [token, epoch, active], exports = [], start = false,
    mutable = false, tokenType = 0} = {}) {
  const functions = imports.filter(entry => entry === token).length;
  const epochIndex = imports.indexOf(active) < imports.indexOf(epoch) ? 1 : 0;
  const readEpoch = [0x23, epochIndex, 0xa7, 0xfb, 0x1c, 0xfb, 0x1b];
  return Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    ...section(1, 3, 0x60, 0, 1, 0x7e, 0x60, 2, 0x6f, 0x6f, 1, 0x6f, 0x60, 0, 0),
    ...section(2, imports.length, ...imports.flatMap(entry => entry === token
      ? [...entry.slice(0, -1), tokenType] : entry)),
    ...section(3, 2, 1, 2),
    ...section(7, 1 + exports.length, ...exported('entry', 0, functions), ...exports.flat()),
    ...(start ? section(8, functions + 1) : []),
    ...section(10, 2, ...(mutable ? body(0x42, 0, 0x24, 0, ...readEpoch) : body(...readEpoch)),
      ...body()),
  ]);
}

function install(bytes) {
  return `(miricl-compiler::compiler-primitive 17 #(${bytes.join(' ')}) nil nil)`;
}

export async function checkPollImports(image) {
  let vm = await Miricl.instantiate(image);
  const evaluate = source => vm.evaluate(source, {eof: true}).output;
  const checkReader = value => {
    let getter;
    const callback = vm.javascript.callback;
    vm.javascript.callback = function (fn) { getter = fn; return callback.call(this, fn); };
    try { evaluate('(progn (js:callback (lambda () *soundness-poll-reader*)) (values))'); }
    finally { vm.javascript.callback = callback; }
    const call = vm.vm['native-call0'], worker = call(getter);
    const epoch = vm.vm['poll-epoch-base'], before = epoch.value;
    // Enter the worker directly so an unrelated Lisp call cannot poll between
    // publishing this test epoch and the worker's read.
    try {
      epoch.value = BigInt(value);
      assert.equal(call(worker), value);
    } finally { epoch.value = before; }
  };
  try {
    evaluate(`(defparameter *soundness-poll-reader* ${install(pollModule())}) (values)`);
    checkReader(41);
    for (const bytes of [
      pollModule({imports: [token, imported('p', 'epoch', 3, 0x7e, 1), active], mutable: true}),
      pollModule({imports: [token, token, epoch, active]}),
      pollModule({imports: [token, active, epoch]}),
      pollModule({tokenType: 2}),
      pollModule({exports: [exported('epoch', 3, 0)]}),
      pollModule({exports: [exported('active', 3, 1)]}),
      pollModule({exports: [exported('token', 0, 0)]}),
      pollModule({start: true}),
    ]) {
      assert.ok(WebAssembly.validate(bytes), 'the malformed poll contract is structurally valid Wasm');
      assert.equal(evaluate(`(handler-case (progn ${install(bytes)} nil)
                                 (error (condition) (plusp (length (princ-to-string condition)))))`),
        'T\n');
      checkReader(42);
    }
    const saved = vm.saveImage();
    vm.close();
    vm = await Miricl.instantiate(saved);
    checkReader(43);
    console.log('Soundness: invalid poll imports are rejected and restored workers read live state.');
  } finally { vm.close(); }
}
