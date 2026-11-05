// node tests/backtrace.mjs [interpreter.wasm]
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Script, createContext} from 'node:vm';
import {Miricl, MiriclError} from '../scripts/miricl.mjs';

const bytes = fs.readFileSync(process.argv[2] ?? 'interpreter.wasm');
const vm = await Miricl.instantiate(bytes);
const evaluate = source => vm.evaluate(source, {eof: true}).output;
const header = 'Backtrace (most recent call first):\n';
const failure = source => {
  try { evaluate(source); }
  catch (error) {
    assert.ok(error instanceof MiriclError);
    assert.ok(error.cause instanceof WebAssembly.RuntimeError);
    assert.match(error.backtrace, /^Backtrace \(most recent call first\):\n/);
    return error;
  }
  assert.fail(`Expected failure: ${source}`);
};
evaluate(`(defun trace-leaf () (error "trace failure"))
  (defun trace-middle () (trace-leaf))
  (defun trace-root () (trace-middle))`);
const basic = failure('(trace-root)');
assert.equal(basic.message, 'trace failure');
assert.equal(basic.backtrace, header + '  COMMON-LISP:ERROR\n  COMMON-LISP-USER::TRACE-LEAF\n  COMMON-LISP-USER::TRACE-MIDDLE\n  COMMON-LISP-USER::TRACE-ROOT\n');
assert.ok(basic.stack.includes(basic.backtrace), 'Console errors also expose the Lisp trace through Error.stack');
assert.equal(failure("(funcall #'trace-root)").backtrace,
  header + '  COMMON-LISP:ERROR\n  COMMON-LISP-USER::TRACE-LEAF\n  COMMON-LISP-USER::TRACE-MIDDLE\n  COMMON-LISP-USER::TRACE-ROOT\n  COMMON-LISP:FUNCALL\n');
evaluate("(compile 'trace-root) (compile 'trace-middle) (compile 'trace-leaf)");
assert.equal(failure('(trace-root)').backtrace, basic.backtrace);
assert.match(failure('(flet ((local-trace () (trace-leaf))) (local-trace))').backtrace,
  /  COMMON-LISP-USER::TRACE-LEAF\n  COMMON-LISP-USER::LOCAL-TRACE\n/);
assert.match(failure('(funcall (lambda () (trace-leaf)))').backtrace, /  COMMON-LISP-USER::TRACE-LEAF\n  COMMON-LISP:LAMBDA\n  COMMON-LISP:FUNCALL\n/);
assert.match(failure('(trace-undefined)').backtrace, /\n  COMMON-LISP-USER::TRACE-UNDEFINED\n/);
assert.match(failure("(funcall #'car 7)").backtrace, /\n  COMMON-LISP:CAR\n  COMMON-LISP:FUNCALL\n/);
evaluate(`(defgeneric trace-generic (x))
  (defmethod trace-generic ((x integer)) (trace-leaf))`);
assert.match(failure("(funcall #'trace-generic 1)").backtrace, /\n  COMMON-LISP-USER::TRACE-GENERIC\n/);

// Always show the symbol's home package, independently of *PACKAGE*, USE-PACKAGE,
// nicknames, and whether the function is exported or internal.
evaluate(`(defpackage :trace-model (:use :cl) (:nicknames :tm) (:export :validate))
  (defpackage :trace-ui (:use :cl :trace-model))
  (in-package :trace-model)
  (defun validate () (error "package trace"))
  (in-package :trace-ui)
  (defun submit () (validate))
  (in-package :cl-user)`);
const packageTrace = header + '  COMMON-LISP:ERROR\n  TRACE-MODEL:VALIDATE\n  TRACE-UI::SUBMIT\n';
assert.equal(failure('(trace-ui::submit)').backtrace, packageTrace);
assert.equal(failure('(let ((*package* (find-package :tm))) (trace-ui::submit))').backtrace, packageTrace);
evaluate('(in-package :trace-ui)');
assert.equal(failure('(submit)').backtrace, packageTrace);
evaluate('(in-package :cl-user)');
assert.match(failure('(funcall (make-symbol "NO-HOME"))').backtrace, /\n  #:NO-HOME\n  COMMON-LISP:FUNCALL\n/);
evaluate(`(defpackage "Trace : package" (:use "CL") (:export "validate task"))
  (in-package "Trace : package")
  (defun |validate task| () (error "escaped name"))
  (in-package :cl-user)`);
assert.equal(failure('(|Trace : package|:|validate task|)').backtrace,
  header + '  COMMON-LISP:ERROR\n  |Trace : package|:|validate task|\n');

// Both success and all ordinary nonlocal exits remove their frames.
assert.equal(evaluate(`(defun trace-handled () (handler-case (trace-root) (error () :handled)))
  (defun trace-throw () (throw 'escape :escaped))
  (defun trace-return () (return-from trace-return :returned))
  (defun trace-restart () (restart-case (error "resumable") (resume () :resumed)))
  (list (trace-handled) (catch 'escape (trace-throw)) (trace-return)
    (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'resume)))) (trace-restart)))`),
  'TRACE-HANDLED\nTRACE-THROW\nTRACE-RETURN\nTRACE-RESTART\n(:HANDLED :ESCAPED :RETURNED :RESUMED)\n');
assert.equal(failure('(error "fresh")').backtrace, header + '  COMMON-LISP:ERROR\n');
assert.equal(evaluate(`(defparameter *trace-cleaned* nil)
  (defun trace-protected () (unwind-protect (trace-root) (setq *trace-cleaned* t)))`),
  '*TRACE-CLEANED*\nTRACE-PROTECTED\n');
assert.match(failure('(trace-protected)').backtrace, /  COMMON-LISP-USER::TRACE-ROOT\n  COMMON-LISP-USER::TRACE-PROTECTED\n/);
assert.equal(evaluate('*trace-cleaned*'), 'T\n');

// The ring keeps the innermost 64 calls, and restores overwritten outer slots
// after a deep call returns or throws into an outer frame.
evaluate(`(defun trace-deep (n) (if (zerop n) (trace-leaf) (trace-deep (1- n))))
  (defun trace-deep-return (n) (if (zerop n) :done (trace-deep-return (1- n))))
  (defun trace-deep-exit (n) (if (zerop n) (throw 'deep :done) (trace-deep-exit (1- n))))
  (defun trace-after-deep () (catch 'deep (trace-deep-exit 90)) (trace-leaf))`);
const deep = failure('(trace-deep 90)').backtrace;
assert.equal(deep.split('\n').filter(line => /^  [^.]/.test(line)).length, 64);
assert.ok(deep.endsWith('  ... older calls omitted\n'));
assert.match(deep, /\n  COMMON-LISP:ERROR\n  COMMON-LISP-USER::TRACE-LEAF\n  COMMON-LISP-USER::TRACE-DEEP\n/);
assert.equal(failure('(trace-after-deep)').backtrace,
  header + '  COMMON-LISP:ERROR\n  COMMON-LISP-USER::TRACE-LEAF\n  COMMON-LISP-USER::TRACE-AFTER-DEEP\n');
evaluate('(defun trace-after-return () (trace-deep-return 90) (trace-leaf))');
assert.equal(failure('(trace-after-return)').backtrace,
  header + '  COMMON-LISP:ERROR\n  COMMON-LISP-USER::TRACE-LEAF\n  COMMON-LISP-USER::TRACE-AFTER-RETURN\n');

// Recover after a host interruption, which bypasses guest exception cleanup.
const context = createContext({vm});
assert.throws(() => new Script('vm.evaluate("(loop)")').runInContext(context, {timeout: 30}), /timed out/);
vm.vm.unwind();
assert.equal(failure('(error "after interruption")').backtrace, header + '  COMMON-LISP:ERROR\n');
assert.equal(evaluate('(values 11 23)'), '11\n23\n');

// Deferred callbacks start a fresh Lisp stack; synchronous callbacks retain
// their Lisp caller across JavaScript reentry.
let retained;
globalThis.miriclTraceFixture = {
  save(callback) { retained = callback; },
  call(callback) { return callback(); },
};
try {
  evaluate(`(js:invoke (js:global "miriclTraceFixture") "save" (js:callback #'trace-root))
    (defun trace-through-js ()
      (js:invoke (js:global "miriclTraceFixture") "call" (js:callback #'trace-root)))`);
  assert.throws(() => retained(), error => error.backtrace === basic.backtrace);
  assert.match(failure('(trace-through-js)').backtrace, /  COMMON-LISP-USER::TRACE-ROOT\n[\s\S]*  COMMON-LISP-USER::TRACE-THROUGH-JS\n/);
  assert.equal(failure('(error "after callback")').backtrace, header + '  COMMON-LISP:ERROR\n');
} finally { delete globalThis.miriclTraceFixture; }

// Trace formatting is diagnostic: even unusable printer/package state must
// preserve the original condition's message and error code.
const brokenPrinter = await Miricl.instantiate(bytes);
assert.throws(() => brokenPrinter.evaluate('(progn (makunbound (quote *package*)) (error "original message"))'),
  error => error instanceof MiriclError && error.message === 'original message' && error.code === 18);
console.log('Lisp backtraces: names, compiled functions, bounded depth, nonlocal exits, interruptions, and JS callbacks passed.');
