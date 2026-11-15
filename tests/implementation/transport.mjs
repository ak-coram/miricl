// Import-free Wasm, host references and callbacks are MIRICL adapter contracts.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {Miricl, MiriclError} from '../../scripts/miricl.mjs';

const image = process.argv[2] ?? 'miricl.wasm';
const module = await WebAssembly.compile(await fs.readFile(image));
assert.deepEqual(WebAssembly.Module.imports(module), [], 'The browser image must instantiate without host imports.');
new WebAssembly.Instance(module).exports.initialize();
// The native image materializes compiled Lisp functions through the adapter's
// service table. Hostless evaluation belongs to the matching compiler-free
// interpreter, shipped beside it in every standard build directory.
const interpreter = await WebAssembly.compile(await fs.readFile(path.join(path.dirname(image), 'miricl-interpreter.wasm')));
assert.deepEqual(WebAssembly.Module.imports(interpreter), []);
const bare = new WebAssembly.Instance(interpreter).exports;
bare.initialize();
const source = new TextEncoder().encode(`(and (not (js:available-p))
  (handler-case (js:global "Math") (js:javascript-unavailable () t)))`);
new Uint8Array(bare.memory.buffer).set(source);
const length = bare['evaluate-source'](source.length);
assert.equal(new TextDecoder().decode(new Uint8Array(bare.memory.buffer, 0, length)), 'T\n');

const output = [];
const fixture = {
  unicode: 'λ\0🙂\ud800', large: (1n << 100n) + 17n,
  identity(value) { return value; },
  apply(callback, value) { return callback(value); },
  retain(callback) { this.callback = callback; },
  fail() { throw new Error('host fixture failure'); },
};
const vm = await Miricl.instantiate(module, {imports: {transport: fixture}, onOutput: text => output.push(text)});
const other = await Miricl.instantiate(module);
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  // Typed bindings exercise both directions without adding language conformance cases.
  check(`(progn
    (js:define-import transport-identity ("transport" "identity") (:value) :value)
    (js:define-import transport-apply ("transport" "apply") (:value :value) :value)
    (js:define-import transport-retain ("transport" "retain") (:value) :void)
    (js:define-import transport-fail ("transport" "fail") () :void)
    (let ((foreign (js:array 1 "two")))
      (and (js:strict-equal foreign (transport-identity foreign))
        (eq (js:from-js (transport-identity js:+null+)) js:+null+)
        (eq (js:from-js (transport-identity js:+undefined+)) js:+undefined+))))`);
  vm.evaluate('(defparameter *transport-instance-value* 17) (values)', {eof: true});
  assert.equal(other.evaluate("(boundp '*transport-instance-value*)").output, 'NIL\n', 'Wasm instances must not share Lisp state.');
  for (const value of [fixture.unicode, fixture.large, -fixture.large]) {
    // Pass host values through an explicit import so no process-global fixture is needed.
    fixture.current = () => value;
    check(`(progn (js:define-import transport-current ("transport" "current") () :value)
      (let ((original (transport-current)))
        (js:strict-equal original (js:to-js (js:from-js original)))))`);
  }
  check(`(let ((cleaned nil))
    (and (handler-case (unwind-protect (transport-fail) (setf cleaned t))
      (js:javascript-error (condition)
        (string= (js:from-js (js:property (js:javascript-error-value condition) "message"))
          "host fixture failure"))) cleaned))`);
  check(`(progn
    (transport-retain (js:callback (lambda (value)
      (format t "callback output~%") (+ (js:from-js value) 2)))) t)`);
  assert.equal(fixture.callback(40), 42);
  assert.deepEqual(output, ['callback output\n']);
  check(`(handler-case
    (transport-apply (js:callback (lambda (value) (declare (ignore value))
      (error "Lisp callback failure"))) js:+null+)
    (simple-error (condition) (string= (simple-condition-format-control condition) "Lisp callback failure")))`);
  check(`(progn (transport-retain (js:callback (lambda () (error "external callback failure")))) t)`);
  assert.throws(() => fixture.callback(), error => error instanceof MiriclError && /external callback failure/.test(error.message));
  check('(js:available-p)');
  console.log('PASS import-free Wasm, isolated heaps, host value round trips, callback output and exception recovery');
} finally {other.close(); vm.close();}
