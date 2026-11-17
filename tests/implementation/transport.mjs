// Import-free Wasm, host references and callbacks are MIRICL adapter contracts.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {Miricl, MiriclError} from '../../scripts/miricl.mjs';

const image = process.argv[2] ?? 'miricl.wasm';
const bytes = await fs.readFile(image);
const module = await WebAssembly.compile(bytes);
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
  (handler-case (js:global "Math") (js:javascript-unavailable () t))
  (not (fboundp 'miricl-compiler:compile-native))
  (null miricl-compiler:*compile-evaluations*)
  (= (eval (read-from-string "(+ 20 22)")) 42)
  (let ((function (compile nil '(lambda (x) (values (+ x 2) :tail)))))
    (and (compiled-function-p function)
      (equal (multiple-value-list (funcall function 40)) '(42 :tail))))
  (handler-case (progn (eval '(if t . :bad)) nil) (program-error () t))
  (= (eval '(+ 20 22)) 42))`);
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
const other = await Miricl.instantiate(bytes);
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  other.evaluate(`(setq miricl-compiler:*compile-evaluations* nil)
    (defparameter *transport-saved-graph*
      (let ((counter 40) (literal (list :shared)))
        (setf (cdr literal) literal)
        (vector literal literal (lambda (delta) (incf counter delta))
          (lambda () counter) #'miricl-compiler::compiler-list-length)))
    (defclass transport-saved-record () ((value :initarg :value))
      (:default-initargs :value 42))
    (defparameter *transport-saved-mop*
      (let* ((class (find-class 'transport-saved-record))
             (slots (miricl-mop:class-slots class))
             (defaults (miricl-mop:class-default-initargs class)))
        (list class slots defaults
          (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%STANDARD-CLASS-SLOTS"))
          (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%STANDARD-CLASS-DEFAULT-INITARGS")))))
    (defgeneric transport-saved-next (a &optional b c d e &key marker))
    (funcall (miricl-compiler:compile-native '(lambda ()
      (defmethod transport-saved-next ((a t) &optional b c d e &key (marker :missing))
        (declare (optimize (debug 0)))
        (values a b c d e marker))
      (defmethod transport-saved-next ((a integer) &optional b c d e &key marker)
        (declare (optimize (debug 0)) (ignore b c d e marker))
        (setq a 999)
        (lambda () (call-next-method))))))
    (defparameter *transport-saved-next*
      (let ((driver (miricl-compiler:compile-native
        '(lambda ()
          (declare (optimize (debug 0)))
          (transport-saved-next 1 nil 3 4 5 :marker nil)))))
        (funcall driver) (funcall driver) (funcall driver)))
    (values)`, {eof: true});
  other.evaluate(`(defparameter *transport-method-key* (intern "TRANSPORT-SAVED-KEY" "KEYWORD"))
    (defgeneric transport-saved-keyword (object &key transport-saved-key))
    (defmethod transport-saved-keyword ((object t) &key transport-saved-key)
      (declare (ignore object)) transport-saved-key)
    (defparameter *transport-method*
      (find-method #'transport-saved-keyword nil (list (find-class 't))))
    (function-keywords *transport-method*)
    (defgeneric transport-saved-metadata (object))
    (defmethod transport-saved-metadata ((object t)) object)
    (dotimes (i 8)
      (funcall (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%GENERIC-DISPATCH-METADATA"))
        4 #'transport-saved-metadata nil))
    (unintern *transport-method-key* "KEYWORD")
    (defparameter *transport-new-method-key* (intern "TRANSPORT-SAVED-KEY" "KEYWORD"))
    (values)`, {eof: true});
  const saved = other.saveImage();
  const restored = await Miricl.instantiate(saved);
  try {
    assert.equal(restored.evaluate(`(let* ((generic #'transport-saved-metadata)
        (metadata (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%GENERIC-DISPATCH-METADATA")))
        (info (funcall (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%GENERIC-INFO")) generic)))
      (dotimes (i 8) (funcall metadata 4 generic nil))
      (let* ((slots (funcall (symbol-function
            (miricl-compiler::compiler-primitive 10 1 "%%RAW-SLOTS")) info))
          (tail (last slots)))
        (and
          (unwind-protect
              (progn (rplacd tail slots)
                (handler-case (progn (funcall metadata 4 generic nil) nil)
                  (error (condition) (plusp (length (princ-to-string condition))))))
            (rplacd tail nil))
          (progn (funcall metadata 4 generic nil)
            (eq (transport-saved-metadata :recovered) :recovered)))))`, {eof: true}).output,
      'T\n', 'Restored generic metadata validates borrowing and recovers after a cyclic spine.');
    assert.equal(restored.evaluate(`(let ((keys (function-keywords *transport-method*)))
      (and (equal keys (list *transport-method-key*))
        (not (eq *transport-method-key* *transport-new-method-key*))
        (progn
          (funcall (symbol-function (miricl-compiler::compiler-primitive 10 1 "%%RAW-SLOTS"))
            *transport-method*)
          (equal (function-keywords *transport-method*) (list *transport-method-key*)))
        (equal (multiple-value-list
          (funcall (miricl-mop:method-function *transport-method*)
            (list nil *transport-method-key* 42) nil)) '(42))))`, {eof: true}).output, 'T\n',
      'Saved and borrowed method metadata retains the keyword identity captured by its executable body.');
    assert.equal(restored.evaluate(`(let ((graph *transport-saved-graph*))
      (and (eq (svref graph 0) (svref graph 1))
        (eq (svref graph 0) (cdr (svref graph 0)))
        (= (funcall (svref graph 2) 2) 42) (= (funcall (svref graph 3)) 42)
        (= (funcall (svref graph 4) '(one two) "restored input") 2)
        (handler-case
          (progn (funcall (svref graph 4) '(one . two) "restored input") nil)
          (program-error () t))
        (= (eval '(+ 20 22)) 42)))`, {eof: true}).output, 'T\n');
    assert.equal(restored.evaluate(`(destructuring-bind (class slots defaults read-slots read-defaults)
        *transport-saved-mop*
      (and (eq class (find-class 'transport-saved-record))
        (eq slots (miricl-mop:class-slots class))
        (eq defaults (miricl-mop:class-default-initargs class))
        (equal (multiple-value-list (funcall read-slots class)) (list slots))
        (equal (multiple-value-list (funcall read-defaults class)) (list defaults))
        (handler-case (progn (funcall read-slots) nil) (program-error () t))
        (handler-case (progn (funcall read-defaults 3) nil) (error () t))
        (= (slot-value (make-instance class) 'value) 42)))`, {eof: true}).output, 'T\n');
    assert.equal(restored.evaluate(`(equal (multiple-value-list (funcall *transport-saved-next*))
      '(1 nil 3 4 5 nil))`, {eof: true}).output, 'T\n',
    'An escaped next-method closure retains its original indexed arguments and complete values through image restore.');
  } finally {restored.close();}
  // Corrupt the Lisp payload inside a structurally valid Wasm custom section.
  // Rejection must not damage the original worker or a subsequent restore.
  const payload = new Uint8Array(WebAssembly.Module.customSections(await WebAssembly.compile(saved), 'miricl.core')[0]);
  const name = new TextEncoder().encode('miricl.core');
  const word = value => {
    const encoded = [];
    do {const byte = value & 127; value = Math.floor(value / 128); encoded.push(byte | (value ? 128 : 0));} while (value);
    return encoded;
  };
  const header = Uint8Array.from([0, ...word(word(name.length).length + name.length + payload.length + 1),
    ...word(name.length), ...name]);
  const corrupt = new Uint8Array(bytes.length + header.length + payload.length + 1);
  corrupt.set(bytes); corrupt.set(header, bytes.length); corrupt.set(payload, bytes.length + header.length);
  await assert.rejects(Miricl.instantiate(corrupt), /Unexpected data after Miricl image/);
  assert.equal(other.evaluate('(funcall (svref *transport-saved-graph* 3))', {eof: true}).output, '40\n');
  const recovered = await Miricl.instantiate(saved);
  try {assert.equal(recovered.evaluate('(+ 20 22)', {eof: true}).output, '42\n');}
  finally {recovered.close();}
  console.log('PASS current image round trips preserve closures and sharing; malformed payloads reject with recovery');

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
