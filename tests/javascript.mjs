// node tests/javascript.mjs [miricl-interpreter.wasm]
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Script, createContext} from 'node:vm';
import {Miricl, MiriclError} from '../scripts/miricl.mjs';

const bytes = fs.readFileSync(process.argv[2] ?? 'miricl-interpreter.wasm');
const module = await WebAssembly.compile(bytes);
assert.deepEqual(WebAssembly.Module.imports(module), []);
const fixture = {
  value: 7, nil: null, missing: undefined, no: false, yes: true,
  large: (1n << 100n) + 17n, unicode: 'λ\0🙂\ud800',
  nan: NaN,
  add(value) { return this.value + value; },
  identity(value) { return value; },
  apply(callback, value) { return callback(value); },
  throws() { throw new Error('fixture failure'); },
  throwsNull() { throw null; },
  wrongType() { return 'wrong'; },
  nothing() {},
  resolvedPromise() { return Promise.resolve(11); },
};
Object.defineProperty(fixture, 'badGetter', {get() { throw new Error('getter failure'); }});
Object.defineProperty(fixture, 'readOnly', {value: 3});
globalThis.miriclJSFixture = fixture;
try {
  const callbackOutput = [];
  const vm = await Miricl.instantiate(module, {imports: {fixture}, onOutput: text => callbackOutput.push(text)});
  const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
  check(`(and (js:available-p) (eq (find-package "JS") (find-package "MIRICL-JS"))
    (not (nth-value 1 (find-symbol "INVOKE" "CL"))))`);
  check(`(progn (defparameter *js-fixture* (js:global "miriclJSFixture"))
    (and (js:valuep *js-fixture*) (typep *js-fixture* 'js:value)
      (eq (type-of *js-fixture*) 'js:value) (string= (js:typeof *js-fixture*) "object")
      (js:strict-equal (js:global) (js:property (js:global) "globalThis"))))`);
  check(`(and (= (js:from-js (js:invoke *js-fixture* "add" 5)) 12)
    (= (js:from-js (js:call-with-this (js:property *js-fixture* "add") *js-fixture* 4)) 11)
    (= (js:from-js (js:call (js:property (js:global "Math") "sqrt") 81)) 9))`);
  check(`(let ((value (js:to-js 19)))
    (and (eq (setf (js:property *js-fixture* "written") value) value)
      (= (js:from-js (js:property *js-fixture* "written")) 19)
      (eq (js:to-js value) value)))`);
  check(`(and (eq (js:from-js (js:property *js-fixture* "nil")) js:+null+)
    (eq (js:from-js (js:property *js-fixture* "missing")) js:+undefined+)
    (null (js:from-js (js:property *js-fixture* "no")))
    (eq (js:from-js (js:property *js-fixture* "yes")) t)
    (not (js:strict-equal js:+null+ nil)) (not (js:strict-equal js:+undefined+ js:+null+))
    (js:strict-equal (js:to-js nil) (js:property *js-fixture* "no"))
    (js:strict-equal (js:to-js t) (js:property *js-fixture* "yes")))`);
  check(`(let ((n (+ (expt 2 100) 17)))
    (and (= (js:from-js (js:to-js n)) n)
      (= (js:from-js (js:property *js-fixture* "large")) n)
      (= (js:from-js (js:to-js (- n))) (- n))
      (string= (js:typeof (js:to-js 9007199254740991)) "number")
      (string= (js:typeof (js:to-js 9007199254740992)) "bigint")))`);
  check(`(let* ((original (js:property *js-fixture* "unicode")) (string (js:from-js original)))
    (and (= (length string) 4) (= (char-code (char string 3)) 55296)
      (js:strict-equal original (js:to-js string))
      (progn (setf (char string 0) (code-char 120)) (not (js:strict-equal original (js:to-js string))))))`);
  check(`(let* ((base "--λ🙂--")
      (view (make-array 2 :element-type 'character :displaced-to base :displaced-index-offset 2)))
    (string= (js:from-js (js:to-js view)) "λ🙂"))`);
  check(`(let* ((a (js:array 1 "two" js:+null+)) (m (js:new (js:global "Map"))))
    (js:invoke m "set" "key" a)
    (and (js:strict-equal (js:invoke m "get" "key") a)
      (= (js:from-js (js:property a "length")) 3)
      (string= (js:from-js (js:property a 1)) "two")))`);
  check(`(let ((clean nil))
    (and (handler-case (unwind-protect (js:invoke *js-fixture* "throws") (setq clean t))
      (js:javascript-error (condition)
        (and (js:valuep (js:javascript-error-value condition))
          (string= (js:from-js (js:property (js:javascript-error-value condition) "message")) "fixture failure"))))
      clean))`);
  check(`(and (handler-case (js:property *js-fixture* "badGetter") (js:javascript-error () t))
    (handler-case (setf (js:property *js-fixture* "readOnly") 4) (js:javascript-error () t))
    (handler-case (js:invoke *js-fixture* "throwsNull")
      (js:javascript-error (c) (js:strict-equal (js:javascript-error-value c) js:+null+)))
    (= (+ 2 3) 5))`);
  check(`(and (handler-case (js:from-js 1) (type-error () t))
    (handler-case (js:from-js (js:property *js-fixture* "nan")) (floating-point-invalid-operation () t))
    (not (js:strict-equal (js:property *js-fixture* "nan") (js:property *js-fixture* "nan"))))`);
  check(`(let ((seen nil))
    (let* ((function (lambda (value) (setq seen (js:from-js value)) (values (+ seen 1) :ignored)))
           (callback (js:callback function)))
      (and (js:strict-equal callback (js:callback function))
        (= (js:from-js (js:invoke *js-fixture* "apply" callback 41)) 42)
        (= seen 41))))`);
  check(`(let ((clean nil))
    (and (eq (catch 'callback-exit
      (unwind-protect
        (js:invoke *js-fixture* "apply" (js:callback (lambda (x) (declare (ignore x)) (throw 'callback-exit :done))) 1)
        (setq clean t))) :done) clean))`);
  check(`(handler-case
    (js:invoke *js-fixture* "apply" (js:callback (lambda (x) (declare (ignore x)) (error "callback failure"))) 1)
    (simple-error (condition) (string= (simple-condition-format-control condition) "callback failure")))`);
  check(`(progn (defparameter *js-count* 0)
    (setf (js:property *js-fixture* "retained")
      (js:callback (lambda (x) (incf *js-count* (js:from-js x)) *js-count*))) t)`);
  assert.equal(fixture.retained(3), 3);
  assert.equal(fixture.retained(4), 7);
  check('(= *js-count* 7)');
  check(`(progn
    (setf (js:property *js-fixture* "printCallback")
      (js:callback (lambda () (format t "Deferred callback output.~%") js:+undefined+))) t)`);
  assert.equal(fixture.printCallback(), undefined);
  assert.deepEqual(callbackOutput, ['Deferred callback output.\n']);
  assert.equal(vm.evaluate('(progn (js:invoke *js-fixture* "printCallback") t)').output,
    'Deferred callback output.\nT\n');
  assert.deepEqual(callbackOutput, ['Deferred callback output.\n'], 'Synchronous callback output belongs to evaluate().output');
  check(`(progn
    (setf (js:property *js-fixture* "badCallback") (js:callback (lambda () (error "async callback failure")))) t)`);
  assert.throws(() => fixture.badCallback(), error => error instanceof MiriclError && /async callback failure/.test(error.message));
  check('(= (+ 20 22) 42)');
  check(`(progn (defparameter *js-promise-value* nil)
    (setf (js:property *js-fixture* "pending")
      (js:invoke (js:invoke *js-fixture* "resolvedPromise") "then"
        (js:callback (lambda (value) (setq *js-promise-value* (js:from-js value)) (1+ *js-promise-value*))))) t)`);
  assert.equal(await fixture.pending, 12);
  check('(= *js-promise-value* 11)');
  check(`(progn
    (js:define-import js-sqrt ("Math" "sqrt") (:f64) :f64)
    (js:define-import js-add ("fixture" "add") (:i32) :i32)
    (js:define-import js-big ("fixture" "identity") (:bigint) :bigint)
    (js:define-import js-f32 ("fixture" "identity") (:f32) :f32)
    (js:define-import js-i64 ("fixture" "identity") (:i64) :i64)
    (js:define-import js-string ("fixture" "identity") (:string) :string)
    (js:define-import js-boolean ("fixture" "identity") (:boolean) :boolean)
    (js:define-import js-nothing ("fixture" "nothing") () :void)
    (and (= (js-sqrt 81) 9) (= (js-add 5) 12)
      (= (js-big (+ (expt 2 100) 17)) (+ (expt 2 100) 17))
      (typep (js-f32 1/3) 'single-float)
      (= (js-i64 -9223372036854775808) -9223372036854775808)
      (string= (js-string "λ🙂") "λ🙂") (js-boolean t) (not (js-boolean nil))
      (null (multiple-value-list (js-nothing)))
      (= (funcall (compile nil '(lambda (x) (js-sqrt x))) 16) 4)))`);
  check(`(progn (js:define-import js-wrong ("fixture" "wrongType") () :i32)
    (and (handler-case (js-add 2147483648) (js:javascript-error () t))
      (handler-case (js-i64 9223372036854775808) (js:javascript-error () t))
      (handler-case (js-wrong) (js:javascript-error () t))
      (handler-case (macroexpand-1 '(js:define-import invalid ("x" "y") (:void) :value)) (program-error () t))))`);
  check(`(progn (js:define-import js-later ("fixture" "later") () :string)
    (handler-case (js-later) (js:javascript-error () t)))`);
  fixture.later = () => 'now available';
  check('(string= (js-later) "now available")');
  check(`(let ((*print-base* 16) (foreign (js:to-js nil)))
    (and (= (js:from-js (js:invoke *js-fixture* "apply"
      (js:callback (lambda (value) (declare (ignore value)) *print-base*)) 1)) 16)
      (equal (multiple-value-list (values :first foreign :last)) (list :first foreign :last))))`);
  vm.mountFile('/js-binding.lisp', '(js:define-import js-file-sqrt ("Math" "sqrt") (:f64) :f64)');
  check(`(progn (compile-file "/js-binding.lisp" :output-file "/js-binding.fasl")
    (load "/js-binding.fasl") (= (js-file-sqrt 144) 12))`);
  fixture.long = '🙂λ'.repeat(40000);
  check(`(js:strict-equal (js:property *js-fixture* "long")
    (js:to-js (js:from-js (js:property *js-fixture* "long"))))`);
  assert.equal(vm.evaluate('(progn (write-string "before ") (js:to-js "λ🙂") (write-string "after ") t)').output,
    'before after T\n');
  assert.equal(vm.evaluate('(js:global "Math")').output, '#<JS:VALUE>\n');
  check(`(handler-case (write-to-string *js-fixture* :readably t) (print-not-readable () t))`);

  // VM termination skips JS finally blocks, including active callback frames.
  const context = createContext({evaluate: source => vm.evaluate(source), source: ''});
  const script = new Script('evaluate(source)');
  for (const source of ['(loop)',
    '(js:invoke *js-fixture* "apply" (js:callback (lambda (x) (declare (ignore x)) (loop))) 1)']) {
    context.source = source;
    assert.throws(() => script.runInContext(context, {timeout: 50}), {code: 'ERR_SCRIPT_EXECUTION_TIMEOUT'});
    vm.vm.unwind();
    check('(and (js:available-p) (= (+ 20 22) 42))');
  }

  // A host without the adapter can instantiate and use the same image.
  const bare = new WebAssembly.Instance(module).exports;
  bare.initialize();
  const source = new TextEncoder().encode(`(and (not (js:available-p)) (not (js:valuep nil))
    (handler-case (js:global "Math") (js:javascript-unavailable () t)))`);
  new Uint8Array(bare.memory.buffer).set(source);
  const length = bare['evaluate-source'](source.length);
  assert.equal(new TextDecoder().decode(new Uint8Array(bare.memory.buffer, 0, length)), 'T\n');
  console.log('JavaScript: optional host, references, conversions, methods, bindings, callbacks, cleanup, and compiled files pass.');
} finally {
  delete globalThis.miriclJSFixture;
}
