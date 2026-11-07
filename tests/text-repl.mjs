// node --experimental-wasm-wasmfx tests/text-repl.mjs miricl-interpreter.wasm [miricl-repl.wasm] [--in-process]
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {Miricl, MiriclError} from '../scripts/miricl.mjs';
import {terminalCore, terminalFixture} from './component-core.mjs';

const options = process.argv.slice(2);
const inProcess = options.includes('--in-process');
const paths = options.filter(option => !option.startsWith('--'));
const binary = paths[0] ?? 'miricl-interpreter.wasm';
const wasiBinary = paths[1];
const module = await WebAssembly.compile(fs.readFileSync(binary));
assert.deepEqual(WebAssembly.Module.imports(module), []);
const vm = new Miricl(await WebAssembly.instantiate(module));
const output = (source) => {
  const result = vm.evaluate(source);
  assert.equal(result.incomplete, false, source);
  assert.equal(result.quit, false, source);
  return result.output;
};
const fails = (source, code) => assert.throws(() => output(source), error => error instanceof MiriclError && error.code === code);
assert.equal(output('(list * ** *** + ++ +++ / // ///)'), '(NIL NIL NIL NIL NIL NIL NIL NIL NIL)\n');
assert.equal(output('(values 17 23) 31 (values 41 43 47)'), '17\n23\n31\n41\n43\n47\n');
assert.equal(output('(list * ** *** / // /// + ++ +++)'),
  '(41 31 17 (41 43 47) (31) (17 23) (VALUES 41 43 47) 31 (VALUES 17 23))\n');
assert.equal(output('(car -)'), 'CAR\n');
assert.equal(output('+'), '(CAR -)\n');
assert.equal(output('101 103 107'), '101\n103\n107\n');
fails('(error "History abort")', 18);
assert.equal(output('(list * ** *** / // ///)'), '(107 103 101 (107) (103) (101))\n');
assert.equal(output('(values)'), '');
assert.equal(output('(list * /)'), '(NIL NIL)\n');
vm.mountFile('history-input.lisp', '(values 1 2) (values 3 4)');
assert.equal(output('101 103 107 (load "history-input.lisp" :verbose nil)'), '101\n103\n107\nT\n');
assert.equal(output('(list * ** *** + ++ +++)'),
  '(T 107 103 (LOAD "history-input.lisp" :VERBOSE NIL) 107 103)\n');
assert.equal(output(`(progn
  (defclass history-print-probe () ())
  (defmethod print-object ((object history-print-probe) stream)
    (assert (and (eq object *) (eq object (car /)) (eq (car +) 'values)))
    (setf (cdr /) nil)
    (write-string "history-probe" stream))
  nil)`), 'NIL\n');
assert.equal(output('(values (make-instance \'history-print-probe) 23)'), 'history-probe\n23\n');
assert.equal(output('(+ 1 2)'), '3\n');
// FORMAT and LOOP also run without the Common Lisp host.
assert.equal(output('(loop for x from 1 to 5 collect (* x x))'), '(1 4 9 16 25)\n');
assert.equal(output('(format nil "~{~5,\'0D~^, ~}" (loop for x below 3 collect x))'), '"00000, 00001, 00002"\n');
assert.equal(output('(loop for (a b) in \'((1 2) (3 4)) sum (+ a b))'), '10\n');
assert.equal(output('(loop for x below 3 and y = 10 then x collect (list x y))'), '((0 10) (1 0) (2 1))\n');
assert.equal(output('(loop for x below 5 when (= x 2) do (loop-finish) finally (return x))'), '2\n');
assert.equal(output('(format t "~{~A~^, ~}~%" \'(1 2 3))'), '1, 2, 3\nNIL\n');
assert.equal(output('(progn (format t "λ") (format t "~&🙂~%"))'), 'λ\n🙂\nNIL\n');
assert.equal(output('(format nil "~:[no~;yes~]: ~,2F" t 3/2)'), '"yes: 1.50"\n');
assert.equal(output('(loop repeat 0 always nil)'), 'T\n');
assert.throws(() => output('(progn (format t "before") (error "after"))'),
  error => error.output === 'before' && error.message === 'after');
assert.throws(() => output('(format t "partial ~Q" 1)'), error => error.output === 'partial ');
assert.equal(output('(handler-case (loop for) (program-error () 7))'), '7\n');
assert.equal(output('nil t (values) (values 1 2 nil)'), 'NIL\nT\n1\n2\nNIL\n');
assert.equal(output("'(a (b) . c)"), '(A (B) . C)\n');
assert.equal(output("'(|lower case| |123| |-1| |.| || |a\\|b| #:foo :bar)"), '(|lower case| |123| |-1| |.| || |a\\|b| #:FOO :BAR)\n');
assert.equal(output('"héllo λ 🙂"'), '"héllo λ 🙂"\n');
assert.equal(output('"a\\\"b\\\\c"'), '"a\\\"b\\\\c"\n');
// Graphic standard characters retain their literal #\\ spelling.
assert.equal(output('#\\Space #\\Newline #\\) #\\λ #\\🙂'), '#\\ \n#\\Newline\n#\\)\n#\\λ\n#\\🙂\n');
assert.equal(output('(+ #xFF #b10 #o10 #36rZ) -4/6'), '300\n-2/3\n');
assert.equal(output("(= 12/3 4) (typep 1.00 'single-float) (= 16777217.0 16777216) (= 0.10000000000000001d0 (float 1/10 1d0))"), 'T\nT\nT\nT\n');
assert.equal(output('(+ #c(1 2) #c(3 4))'), '#C(4 6)\n');
assert.equal(output('; line\n#| nested #| comment |# |# (+ 1 #| x |# 2)'), '3\n');
assert.equal(output("(defun fact (n) (if (zerop n) 1 (* n (fact (1- n))))) (fact 30)"), 'FACT\n265252859812191058636308480000000\n');
assert.equal(output('(defpackage :text-demo (:use :cl) (:export :fact)) (in-package :text-demo) (defun fact (n) (+ n 1)) (fact 41) (in-package :cl-user) (text-demo:fact 99)'),
  '#<PACKAGE "TEXT-DEMO">\n#<PACKAGE "TEXT-DEMO">\nFACT\n42\n#<PACKAGE "COMMON-LISP-USER">\n100\n');
fails('text-demo:missing', 12);
assert.equal(output("'text-demo:fact"), 'TEXT-DEMO:FACT\n');
assert.equal(output("'text-demo::private"), 'TEXT-DEMO::PRIVATE\n');

// Classes, structures, generic dispatch, and custom metaclasses run in WASM.
assert.equal(output(`(progn
  (defclass object-base () ((x :initarg :x :initform 3 :accessor object-x)))
  (defclass object-child (object-base) ())
  (defmethod object-call ((x object-base)) (values (object-x x) 8))
  (defmethod object-call ((x object-child)) (call-next-method))
  (defmethod object-call ((x (eql 'special-object))) 99)
  (list (multiple-value-list (object-call (make-instance 'object-child :x 7)))
        (object-call 'special-object)
        (loop for c in (mop:class-precedence-list (find-class 'object-child)) collect (class-name c))))`),
  '((7 8) 99 (OBJECT-CHILD OBJECT-BASE STANDARD-OBJECT T))\n');
assert.equal(output(`(progn
  (defstruct wasm-point (x 2 :type integer) y)
  (let ((point (make-wasm-point :x 12)))
    (setf (wasm-point-y point) 13)
    (list (wasm-point-x point) (wasm-point-y point) (wasm-point-p (copy-structure point)))))`),
  '(12 13 T)\n');
assert.equal(output('(make-wasm-point :x 4)'), '#S(WASM-POINT :X 4 :Y NIL)\n');
assert.equal(output(`(progn
  (defclass wasm-metaclass (standard-class) ())
  (defparameter *wasm-writes* 0)
  (defmethod (setf mop:slot-value-using-class) :around (value (c wasm-metaclass) object slot)
    (setq *wasm-writes* (1+ *wasm-writes*)) (call-next-method))
  (defclass wasm-instance () ((value :initform 5)) (:metaclass wasm-metaclass))
  (let ((object (make-instance 'wasm-instance)))
    (setf (slot-value object 'value) 6)
    (list (slot-value object 'value) *wasm-writes*)))`), '(6 2)\n');
assert.equal(output(`(progn
  (defmethod print-object ((object wasm-instance) stream)
    (format stream "value=~A" (slot-value object 'value)))
  (make-instance 'wasm-instance))`), 'value=5\n');

// Condition handling and recovery run entirely inside the import-free guest.
assert.equal(output("(handler-case (/ 1 0) (division-by-zero (c) (typep c 'arithmetic-error)))"), 'T\n');
assert.equal(output("(multiple-value-bind (v c) (ignore-errors (/ 1 0)) (list v (typep c 'error)))"), '(NIL T)\n');
assert.equal(output("(restart-case (handler-bind ((error (lambda (c) (invoke-restart 'use 42)))) (error \"bad\")) (use (v) v))"), '42\n');
assert.equal(output("(let ((cleaned nil)) (list (catch 'x (unwind-protect (throw 'x 7) (setq cleaned t))) cleaned))"), '(7 T)\n');
assert.equal(output('(handler-case (values 3 4) (:no-error (a b) (+ a b)))'), '7\n');
assert.equal(output('(warn "hello ~A" "λ")'), 'Warning: hello λ\nNIL\n');
assert.equal(output('(handler-bind ((warning (lambda (c) (muffle-warning c)))) (warn "quiet"))'), 'NIL\n');
assert.equal(output('(handler-bind ((error (lambda (c) (continue c)))) (cerror "Continue" "bad"))'), 'NIL\n');
assert.equal(output('(define-condition text-error (error) ((value :initarg :value :reader text-value)) (:report (lambda (c s) (format s "Bad ~A" (text-value c)))))'), 'TEXT-ERROR\n');
assert.equal(output('(format nil "~A" (make-condition \'text-error :value "λ"))'), '"Bad λ"\n');
assert.throws(() => output('(error \'text-error :value "λ")'), error => error.code === 18 && error.message === 'Bad λ');
fails('(unwind-protect (error "bad") (setq condition-cleaned t))', 18);
assert.equal(output('(list condition-cleaned (compute-restarts))'), '(T NIL)\n');
assert.equal(output("(handler-case (throw 'missing 1) (control-error () 9))"), '9\n');
assert.equal(output('(handler-case (format nil "partial ~Q") (error () 7))'), '7\n');
assert.throws(() => output('(progn (warn "before error") (error "failed"))'),
  error => error.output === 'Warning: before error\n' && error.message === 'failed');
assert.equal(output('(+ 1 2)'), '3\n');

// ANSI symbol cells, keywords, property lists, and function objects in pure WASM.
assert.equal(output("(list (symbol-value nil) (symbol-value t) (eq :key (symbol-value :key)) (typep :key 'keyword))"), '(NIL T T T)\n');
assert.equal(output('(let ((*gensym-counter* 123456789012345678901234567890)) (list (symbol-name (gensym)) *gensym-counter*))'),
  '("G123456789012345678901234567890" 123456789012345678901234567891)\n');
assert.equal(output("(progn (set 'cell 10) (let ((cell 20)) (list cell (symbol-value 'cell) (set 'cell 30) cell)))"), '(20 10 30 20)\n');
assert.equal(output("(progn (setf (get 'cell :p) nil) (list (get 'cell :p 99) (remprop 'cell :p) (get 'cell :p 99)))"), '(NIL T 99)\n');
assert.equal(output("(progn (setf (symbol-function 'cell) #'car) (setq cell-copy (copy-symbol 'cell t)) (fmakunbound 'cell) (list (fboundp 'cell) (symbol-value cell-copy) (funcall cell-copy '(42))))"), '(NIL 30 42)\n');
assert.equal(output("(progn (defvar *dynamic-cell* 1) (defun read-cell () *dynamic-cell*) (let ((*dynamic-cell* 2)) (list (read-cell) (symbol-value '*dynamic-cell*))))"), '(2 2)\n');
const gensymBeforeError = output('*gensym-counter*');
fails('(let ((*dynamic-cell* 8) (*gensym-counter* 999)) (/ 1 0))', 1);
assert.equal(output('*dynamic-cell* *gensym-counter*'), `1\n${gensymBeforeError}`);
fails("(symbol-value '#:unbound)", 3);
fails("(set :key 1)", 7);
fails("(funcall 'cell '(42))", 4);
assert.equal(output("(progn (setq detached :detached) (unintern detached :keyword) (list (keywordp detached) (eq detached (symbol-value detached))))"), '(NIL T)\n');
assert.equal(output("(progn (intern \"TEMP0\") (symbol-name (gentemp \"TEMP\")))"), '"TEMP1"\n');

// Reader/printer round trips include IEEE extremes and exact decimal rounding.
for (const literal of ['0.0', '-0.0', '0d0', '-0d0', '.5', '1.', '1e20', '1d23', '1d-300', '5d-324', '1.7976931348623157d308',
  '1.17549435e-38', '1.40129846e-45', '3.40282346e38', '16777217.0', '0.10000000000000001d0']) {
  const printed = output(literal).trim();
  assert.equal(output(`(eql ${literal} ${printed})`), 'T\n', `${literal} -> ${printed}`);
  if (literal.startsWith('-0')) assert.ok(printed.startsWith('-0.0'), printed);
}
// Deterministic samples across binary64 exponent and mantissa ranges.
let bits = 0x123456789abcdef0n;
const raw = new DataView(new ArrayBuffer(8));
for (let i = 0; i < 40; i++) {
  bits = BigInt.asUintN(64, bits * 6364136223846793005n + 1n);
  raw.setBigUint64(0, bits);
  const value = raw.getFloat64(0);
  if (!Number.isFinite(value)) continue;
  const spelling = value.toString();
  const literal = /e/i.test(spelling) ? spelling.replace(/e/i, 'd') : `${spelling}d0`;
  const printed = output(literal).trim();
  assert.equal(output(`(eql ${literal} ${printed})`), 'T\n', `${literal} -> ${printed}`);
}

for (const source of ['(', "'", '"abc', '|abc', 'abc\\', '#', '#x', '#\\', '#:', '#| unfinished', '(a .', '(a . b']) {
  assert.equal(vm.evaluate(source).incomplete, true, source);
}
assert.equal(vm.evaluate('(defun incomplete () 42)\n(').incomplete, true);
fails('(incomplete)', 4); // Incomplete buffers have no evaluation side effects.
assert.equal(output('(defun incomplete () 42)\n(incOMplete)'), 'INCOMPLETE\n42\n');
assert.throws(() => vm.evaluate('(', {eof: true}), error => error.code === 16);
for (const source of [')', '(a . b c)', '(. a)', '#xGG', '#1r0', '#37r0', '#\\Unknown', "'a:::b", '#:cl::foo', '1d10001']) fails(source, 15);
fails('(/ 1 0)', 1);
fails('(let ((*package* (find-package :text-demo))) (/ 1 0))', 1);
assert.equal(output('(package-name *package*) (+ 20 22)'), '"COMMON-LISP-USER"\n42\n');
assert.throws(() => output('123 (/ 1 0)'), error => error.code === 1 && error.output === '123\n');

// Validate UTF-8 in the guest, independently of the JS encoder.
for (const bytes of [[0xc0, 0x80], [0xed, 0xa0, 0x80], [0xf4, 0x90, 0x80, 0x80], [0xe2, 0x82]]) {
  new Uint8Array(vm.vm.memory.buffer).set(bytes);
  assert.throws(() => vm.vm['evaluate-source'](bytes.length), WebAssembly.RuntimeError);
  assert.equal(vm.vm.error.value, 15);
  vm.vm.unwind();
}
const large = `"${'λ🙂'.repeat(25000)}"`;
assert.equal(output(large), `${large}\n`);
assert.equal(output('(+ 1 2)'), '3\n');
assert.equal(vm.evaluate(':quit (/ 1 0)').quit, true);

const repl = fileURLToPath(new URL('../scripts/repl.mjs', import.meta.url));
const traceHeader = 'Backtrace (most recent call first):\n';
const session = '(defun f (x)\n (+ x 1))\n(f 41)\n(/ 1 0)\n(+ 3 4)\n:quit\n(/ 1 0)\n';
if (!inProcess) {
  const help = spawnSync(process.execPath, [repl, '--help'], {encoding: 'utf8'});
  assert.ifError(help.error);
  assert.equal(help.status, 0, help.stderr);
  assert.match(help.stdout, /--dev-server --repl-bridge/);
  const node = spawnSync(process.execPath, [repl, binary], {input: session, encoding: 'utf8', timeout: 10000});
  assert.ifError(node.error);
  assert.equal(node.status, 0, node.stderr);
  assert.equal(node.stdout, 'F\n42\n7\n');
  assert.equal(node.stderr, `Error: Division by zero.\n${traceHeader}  COMMON-LISP:/\n`);
  const eof = spawnSync(process.execPath, [repl, binary], {input: '(+ 1', encoding: 'utf8', timeout: 10000});
  assert.ifError(eof.error);
  assert.equal(eof.status, 1);
  assert.equal(eof.stderr, 'Error: Incomplete input.\n');
  const final = spawnSync(process.execPath, [repl, binary], {input: '(+ 1 2)', encoding: 'utf8', timeout: 10000});
  assert.ifError(final.error);
  assert.equal(final.stdout, '3\n');
}

if (wasiBinary) {
  const wasiModule = await WebAssembly.compile(terminalCore(fs.readFileSync(wasiBinary)));
  assert.deepEqual(WebAssembly.Module.imports(wasiModule).map(x => `${x.module}.${x.name}`),
    ['wasi_snapshot_preview1.fd_read', 'wasi_snapshot_preview1.fd_write', 'wasi_snapshot_preview1.random_get',
      'wasi_snapshot_preview1.clock_time_get', 'wasi_snapshot_preview1.poll_oneoff']);
  // Force partial fd_write results, including splitting UTF-8 codepoints.
  const runInProcess = async (input, clock = () => 58) => {
    const source = new TextEncoder().encode(input);
    const written = [];
    let exports;
    let position = 0;
    const partial = await WebAssembly.instantiate(wasiModule, {wasi_snapshot_preview1: {
      clock_time_get(id, precision, pointer) {
        return clock(id, precision, pointer, new DataView(exports.memory.buffer));
      },
      poll_oneoff() { return 58; },
      random_get(pointer, length) {
        assert.equal(length, 16);
        globalThis.crypto.getRandomValues(new Uint8Array(exports.memory.buffer, pointer, length));
        return 0;
      },
      fd_read(fd, iov, count, result) {
        assert.equal(fd, 0); assert.equal(count, 1); assert.equal(iov % 4, 0);
        const data = new DataView(exports.memory.buffer);
        const length = Math.min(data.getUint32(iov + 4, true), source.length - position);
        new Uint8Array(exports.memory.buffer).set(source.subarray(position, position + length), data.getUint32(iov, true));
        position += length;
        data.setUint32(result, length, true);
        return 0;
      },
      fd_write(fd, iov, count, result) {
        assert.equal(fd, 1); assert.equal(count, 1); assert.equal(iov % 4, 0);
        const data = new DataView(exports.memory.buffer);
        const length = Math.min(2, data.getUint32(iov + 4, true));
        written.push(...new Uint8Array(exports.memory.buffer, data.getUint32(iov, true), length));
        data.setUint32(result, length, true);
        return 0;
      },
    }});
    exports = partial.exports;
    // Inject terminal faults directly; the real command below also exercises scheduling.
    exports['terminal-start']();
    return new TextDecoder().decode(Uint8Array.from(written));
  };
  assert.equal(await runInProcess(`"${'λ🙂'.repeat(300)}"\n:quit\n`), `miricl> "${'λ🙂'.repeat(300)}"\nmiricl> `);
  assert.equal(await runInProcess(session), `miricl> ...> F\nmiricl> 42\nmiricl> Error: Division by zero.\n${traceHeader}  COMMON-LISP:/\nmiricl> 7\nmiricl> `);
  assert.match(await runInProcess('('), /Error: Incomplete input\./);
  assert.equal(await runInProcess('(+ 1 2)'), 'miricl> 3\n');
  const clockCalls = [];
  assert.equal(await runInProcess(
    '(list (get-universal-time) (get-internal-real-time) (get-internal-run-time))\n:quit\n',
    (id, precision, pointer, data) => {
      clockCalls.push(id);
      assert.equal(precision, 1000000n);
      // EINVAL is even: a bitwise AND of errno and a boolean loses it.
      if (id === 2) return 28;
      data.setBigUint64(pointer, id === 0 ? 1700000000000000000n : 1234567890n, true);
      return 0;
    }), 'miricl> (3908988800 1234 1234)\nmiricl> ');
  assert.deepEqual(clockCalls, [0, 1, 2, 1]);
  assert.equal(await runInProcess(
    '(handler-case (/ 1 0) (error () 42))\n' +
    '(restart-case (invoke-restart \'use 7) (use (v) v))\n' +
    '(error "Bad ~A" "λ")\n(warn "hello")\n:quit\n'),
    `miricl> 42\nmiricl> 7\nmiricl> Error: Bad λ\n${traceHeader}  COMMON-LISP:ERROR\nmiricl> Warning: hello\nNIL\nmiricl> `);
  assert.equal(await runInProcess(
    '(format t "~{~D~^, ~}~%" (loop for i from 1 to 3 collect i))\n' +
    '(loop for i below 5 sum i)\n:quit\n'), 'miricl> 1, 2, 3\nNIL\nmiricl> 10\nmiricl> ');
  assert.equal(await runInProcess(
    '(let ((x 0)) (list (setf (ldb (byte 4 2) x) 31) x))\n' +
    '(remove-duplicates \'(1 2 1 3 2))\n' +
    '(let ((x \'#1=(1 . #1#))) (eq x (cdr x)))\n:quit\n'),
    'miricl> (31 60)\nmiricl> (1 3 2)\nmiricl> T\nmiricl> ');
  if (!inProcess) {
    // Execute native stack switching in Wasmtime. Node exercises the JSPI adapter.
    const fixture = terminalFixture(wasiBinary);
    try {
    const runtimes = [
      [process.env.WASMTIME ?? 'wasmtime', ['run', '-W', 'stack-switching=y', fixture.path]],
      [process.env.WASMTIME ?? 'wasmtime', ['run', '-W', 'stack-switching=y', '-S', 'p3=y', wasiBinary]]
    ];
    for (const [command, args] of runtimes) {
      const wasi = spawnSync(command, args, {input: session, encoding: 'utf8'});
      assert.ifError(wasi.error);
      assert.equal(wasi.status, 0, wasi.stderr);
      assert.equal(wasi.stdout, `miricl> ...> F\nmiricl> 42\nmiricl> Error: Division by zero.\n${traceHeader}  COMMON-LISP:/\nmiricl> 7\nmiricl> `);
      const unfinished = spawnSync(command, args, {input: '(', encoding: 'utf8'});
      assert.ifError(unfinished.error);
      assert.equal(unfinished.status, 0, unfinished.stderr);
      assert.match(unfinished.stdout, /Error: Incomplete input\./);
      const conditions = spawnSync(command, args, {input:
        '(handler-case (/ 1 0) (error () 42))\n' +
        '(restart-case (invoke-restart \'use 7) (use (v) v))\n' +
        '(error "Bad ~A" "λ")\n' +
        '(warn "hello")\n:quit\n', encoding: 'utf8'});
      assert.ifError(conditions.error);
      assert.equal(conditions.status, 0, conditions.stderr);
      assert.equal(conditions.stdout,
        `miricl> 42\nmiricl> 7\nmiricl> Error: Bad λ\n${traceHeader}  COMMON-LISP:ERROR\nmiricl> Warning: hello\nNIL\nmiricl> `);
      const formatting = spawnSync(command, args, {input:
        '(format t "~{~D~^, ~}~%" (loop for i from 1 to 3 collect i))\n' +
        '(loop for i below 5 sum i)\n:quit\n', encoding: 'utf8'});
      assert.ifError(formatting.error);
      assert.equal(formatting.status, 0, formatting.stderr);
      assert.equal(formatting.stdout, 'miricl> 1, 2, 3\nNIL\nmiricl> 10\nmiricl> ');
      const clocks = spawnSync(command, args, {input:
        '(dotimes (i 32) (sleep 1/100000))\n' +
        '(let ((real (get-internal-real-time)) (run (get-internal-run-time)))\n' +
        '  (sleep 1/100)\n' +
        '  (list (integerp (get-universal-time)) (<= real (get-internal-real-time))\n' +
        '        (<= run (get-internal-run-time))))\n:quit\n', encoding: 'utf8', timeout: 10000});
      assert.ifError(clocks.error);
      assert.equal(clocks.status, 0, clocks.stderr);
      assert.equal(clocks.stdout, 'miricl> NIL\nmiricl> ...> ...> ...> (T T T)\nmiricl> ');
    }
    } finally { fixture.close(); }
  }
}
console.log(inProcess ? 'Guest reader/printer, text API, and requested in-process WASI checks pass.' :
  'Guest reader/printer, text API, command help, and requested WASI core checks pass.');
