import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
import {enableCompilationStatistics, disableCompilationStatistics, readCompilationStatistics} from '../scripts/compilation-statistics.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  const compile = async source => {
    vm.evaluate(`(defparameter *unboxed-test-unit* (compile-to-hir '${source}))
      (multiple-value-bind (bytes literals) (emit-native-module *unboxed-test-unit*)
      (defparameter *unboxed-test-function* (compiler-primitive 17 bytes literals nil))
      (with-open-file (out "/unboxed-test.wasm" :direction :output :if-exists :supersede
        :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`);
    const bytes = await vm.readFile('/unboxed-test.wasm');
    await WebAssembly.compile(bytes);
    return bytes;
  };
  for (const [type, samples] of [
    ['(signed-byte 31)', [-1073741824n, -1n, 0n, 1n, 1073741823n]],
    ['(signed-byte 32)', [-2147483648n, -1n, 0n, 1n, 2147483647n]],
    ['(signed-byte 64)', [-(1n << 63n), -1n, 0n, 1n, (1n << 63n) - 1n]],
  ]) {
    enableCompilationStatistics(vm);
    await compile(`(lambda (x) (declare (type ${type} x) (optimize (safety 0) (debug 0))
      (inline minusp logand logxor))
      (if (minusp x) (logand x 255) (logxor x 17)))`);
    const stats = await readCompilationStatistics(vm);
    assert.ok(stats.some(row => row.rule === 'optimization/unboxed-argument-initialize' && row.count > 0));
    assert.ok(stats.some(row => row.rule === 'optimization/unboxed-argument-read' && row.count > 0));
    disableCompilationStatistics(vm);
    for (const x of samples) check(`(equal (multiple-value-list (funcall *unboxed-test-function* ${x}))
      (list (if (minusp ${x}) (logand ${x} 255) (logxor ${x} 17))))`);
  }
  // A return-only narrow result requests its own range proof. Removing the
  // sole general boxing import makes a compact module without an extra call.
  enableCompilationStatistics(vm);
  const maskBytes = await compile(`(lambda (x) (declare (type (signed-byte 64) x)
    (optimize (safety 0) (debug 0)) (inline lognot logand)) (logand (lognot x) 255))`);
  const maskStats = await readCompilationStatistics(vm);
  disableCompilationStatistics(vm);
  assert.ok(maskStats.some(row => row.rule === 'optimization/immediate-integer-box' && row.count > 0));
  assert.ok(maskStats.some(row => row.rule === 'proof/integer-complement-range' && row.count > 0));
  assert.ok(!WebAssembly.Module.imports(new WebAssembly.Module(maskBytes))
    .some(entry => entry.name === 'native-int64'));
  for (const x of [-(1n << 63n), -1n, 0n, 1n, (1n << 63n) - 1n])
    check(`(equal (multiple-value-list (funcall *unboxed-test-function* ${x})) '(${(~x) & 255n}))`);
  // Complement is closed over signed i64 and reverses integer intervals.
  // Its result can feed another bitwise operation without an intermediate box.
  for (const type of ['(signed-byte 31)', '(signed-byte 64)']) {
    const bytes = await compile(`(lambda (x) (declare (type ${type} x)
      (optimize (safety 0) (debug 0)) (inline lognot logand))
      (values (logand (lognot x) 255) (lognot x)))`);
    const imports = WebAssembly.Module.imports(new WebAssembly.Module(bytes)).map(entry => entry.name);
    assert.ok(!imports.includes('native-function-reference'), 'A proven complement needs no function lookup');
    const samples = type === '(signed-byte 31)'
      ? [-1073741824n, -1n, 0n, 1n, 1073741823n]
      : [-(1n << 63n), -1n, 0n, 1n, (1n << 63n) - 1n];
    for (const x of samples) check(`(equal (multiple-value-list (funcall *unboxed-test-function* ${x}))
      '(${(~x) & 255n} ${~x}))`);
  }
  await compile(`(lambda (x) (declare (type (signed-byte 31) x)
    (optimize (safety 0) (debug 0)) (inline lognot +))
    (let ((sum 0)) (declare (type (signed-byte 31) sum))
      (dotimes (i 5 sum) (setq sum (+ sum (lognot x))))))`);
  check(`(= (funcall *unboxed-test-function* 7) -40)`);
  await compile(`(lambda (x) (declare (optimize (safety 3) (debug 0)) (inline lognot)) (lognot x))`);
  check(`(and (= (funcall *unboxed-test-function* (ash 1 100)) (- -1 (ash 1 100)))
    (handler-case (progn (funcall *unboxed-test-function* 1.5) nil) (type-error () t))
    (equal (multiple-value-list (funcall *unboxed-test-function* -1)) '(0)))`);
  await compile(`(lambda (x) (declare (type integer x)
    (optimize (safety 0) (debug 0)) (inline lognot)) (lognot x))`);
  for (const x of [1n << 100n, -(1n << 100n)])
    check(`(equal (multiple-value-list (funcall *unboxed-test-function* ${x})) '(${~x}))`);
  check(`(and (equal (hir-inline-result-type 'lognot '((integer 0 255))) '(integer -256 -1))
    (eq (hir-inline-result-type 'lognot '(t)) t))`);
  check(`(let ((original (symbol-function 'lognot)))
    (unwind-protect
      (let ((fn (compile-native '(lambda (x) (declare (type (signed-byte 64) x)
        (optimize (safety 0) (debug 0)) (notinline lognot)) (lognot x)))))
        (setf (symbol-function 'lognot) (lambda (x) (declare (ignore x)) (values :changed :extra)))
        (equal (multiple-value-list (funcall fn 7)) '(:changed :extra)))
      (setf (symbol-function 'lognot) original)))`);
  // Proven intermediates stay raw until a Lisp reference is consumed.
  enableCompilationStatistics(vm);
  await compile(`(lambda (x) (declare (type (signed-byte 32) x)
    (optimize (safety 0) (debug 0)) (inline + * logxor))
    (logxor (* (+ x 3) 7) 31))`);
  let stats = await readCompilationStatistics(vm);
  const count = rule => stats.filter(row => row.rule === rule).reduce((sum, row) => sum + row.count, 0);
  assert.ok(count('optimization/unboxed-result-read') > 0);
  assert.ok(count('optimization/unboxed-result-box') < count('optimization/unboxed-result'));
  disableCompilationStatistics(vm);
  for (const x of [-2147483648n, -1n, 0n, 2147483647n])
    check(`(equal (multiple-value-list (funcall *unboxed-test-function* ${x}))
      (list (logxor (* (+ ${x} 3) 7) 31)))`);
  // Several reference consumers share a box while numeric users retain i64.
  enableCompilationStatistics(vm);
  await compile(`(lambda (x) (declare (type (signed-byte 32) x)
    (optimize (safety 0) (debug 0)) (inline +))
    (let ((n (+ x 1))) (values (list n n) (+ n 2))))`);
  stats = await readCompilationStatistics(vm);
  assert.ok(count('optimization/unboxed-result-read') > 0);
  assert.ok(count('optimization/unboxed-result-box') <= count('optimization/unboxed-result'));
  disableCompilationStatistics(vm);
  check(`(equal (multiple-value-list (funcall *unboxed-test-function* 2147483647))
    '((2147483648 2147483648) 2147483650))`);
  // Signed byte encoding keeps a shared, aliased low byte live while other
  // temporaries reuse reference slots. This also exercises compiler bootstrap.
  await compile(`(lambda (number) (declare (type (signed-byte 32) number)
    (optimize (safety 0) (debug 0)) (inline logand logior zerop = < >=))
    (let ((bytes (make-array 8 :adjustable t :fill-pointer 0)))
      (loop
        (let ((byte (logand number 127)))
          (declare (type (unsigned-byte 7) byte))
          (setq number (ash number -7))
          (let ((done (or (and (= number 0) (< byte 64))
                          (and (= number -1) (>= byte 64)))))
            (vector-push-extend (logior byte (if done 0 128)) bytes)
            (when done (return bytes)))))))`);
  for (const x of [-2147483648n, -65n, -64n, -1n, 0n, 63n, 64n, 2147483647n]) {
    const bytes = [];
    let n = x;
    for (;;) {
      const byte = n & 127n;
      n >>= 7n;
      const done = (n === 0n && byte < 64n) || (n === -1n && byte >= 64n);
      bytes.push(Number(byte | (done ? 0n : 128n)));
      if (done) break;
    }
    check(`(equalp (funcall *unboxed-test-function* ${x}) #(${bytes.join(' ')}))`);
  }
  // Boxing across a join, storing a mutable value, and operand effects retain
  // normal Lisp semantics even when their producers use scalar registers.
  await compile(`(lambda (x flag) (declare (type (signed-byte 32) x)
    (optimize (safety 0) (debug 0)) (inline + -))
    (let ((events nil) (n (if flag (+ x 1) (- x 1))))
      (values (+ (progn (push :left events) n)
                 (progn (push :right events) 2))
        events (progn (setq n (+ n 3)) n))))`);
  for (const flag of ['t', 'nil']) check(`(equal
    (multiple-value-list (funcall *unboxed-test-function* 7 ${flag}))
    (list ${flag === 't' ? 10 : 8} '(:right :left) ${flag === 't' ? 11 : 9}))`);
  // A bound parameter declaration constrains its initial immutable argument,
  // allowing a mutable parameter itself to retain scalar loop storage.
  enableCompilationStatistics(vm);
  await compile(`(lambda (n) (declare (type (integer 0 1000) n)
    (optimize (safety 0) (debug 0)) (inline zerop 1- +))
    (let ((sum 0)) (declare (type (integer 0 1000000) sum))
      (tagbody next (if (zerop n) (go done))
        (setq sum (the (integer 0 1000000) (+ sum n))
              n (the (integer 0 1000) (1- n)))
        (go next) done) sum))`);
  stats = await readCompilationStatistics(vm);
  assert.ok(count('inference/declared-argument') > 0);
  disableCompilationStatistics(vm);
  check(`(and (= (funcall *unboxed-test-function* 0) 0)
    (= (funcall *unboxed-test-function* 1000) 500500))`);
  check(`(let ((facts (infer-hir-types *unboxed-test-unit*)))
    (some (lambda (function)
      (some (lambda (value)
        (let ((producer (hir-value-producer value)))
          (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :argument)
               (multiple-value-bind (low high known) (hir-integer-bounds (gethash value facts))
                 (and known (= low 0) (= high 1000)))))) (hir-function-values function)))
      (hir-unit-functions *unboxed-test-unit*)))`);
  // Narrow free declarations and shadowed AUX names supply no entry fact
  // for the earlier parameter. An opaque predicate is never executed to prove it.
  for (const source of [
    `(lambda (n) (declare (optimize (safety 0) (debug 0)))
       (locally (declare (type (integer 0 1000) n)) n))`,
    `(lambda (n &aux (n 0)) (declare (type (integer 0 1000) n)
       (optimize (safety 0) (debug 0))) n)`,
  ]) {
    await compile(source);
    check(`(let ((facts (infer-hir-types *unboxed-test-unit*)))
      (every (lambda (function)
        (every (lambda (value)
          (let ((producer (hir-value-producer value)))
            (or (not (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :argument)))
                (not (nth-value 2 (hir-integer-bounds (gethash value facts)))))))
          (hir-function-values function))) (hir-unit-functions *unboxed-test-unit*)))`);
  }
  vm.evaluate(`(defparameter *unboxed-opaque-visits* 0)
    (defun unboxed-opaque-test (x) (incf *unboxed-opaque-visits*) (integerp x)) (values)`);
  for (const type of ['(satisfies unboxed-opaque-test)',
                      '(and (signed-byte 32) (satisfies unboxed-opaque-test))']) {
    await compile(`(lambda (n) (declare (type ${type} n)
      (optimize (safety 0) (debug 0)) (inline +)) (+ n 1))`);
    check(`(and (= *unboxed-opaque-visits* 0) (= (funcall *unboxed-test-function* 7) 8)
      (= *unboxed-opaque-visits* 0))`);
  }
  // A generic operation still promotes overflowing i64 arithmetic to bignums.
  await compile(`(lambda (x) (declare (type (signed-byte 64) x) (optimize (safety 0) (debug 0))
    (inline logand logxor +))
    (values (logand x 255) (logxor x 17) (+ x 1)))`);
  check(`(equal (multiple-value-list (funcall *unboxed-test-function* 9223372036854775807))
    '(255 9223372036854775790 9223372036854775808))`);
  // Reassignment changes the binding, not the immutable argument snapshot.
  await compile(`(lambda (x) (declare (type (signed-byte 32) x) (optimize (safety 0) (debug 0))
    (inline +))
    (let ((a (+ x 1)) (b (+ x 2))) (setq x -5) (values a b (+ x 3))))`);
  check(`(equal (multiple-value-list (funcall *unboxed-test-function* 7)) '(8 9 -2))`);
  await compile(`(lambda (x) (declare (type (signed-byte 32) x) (optimize (safety 0) (debug 0))
    (inline +))
    (let ((change (lambda () (setq x -5))))
      (let ((a (+ x 1))) (funcall change) (values a (+ x 2)))))`);
  check(`(equal (multiple-value-list (funcall *unboxed-test-function* 7)) '(8 -3))`);
  // Planning a child while emitting its caller must use the child's own
  // value IDs, including when captured mutation prevents scalar storage.
  await compile(`(lambda (x n) (declare (optimize (safety 0) (debug 0)) (inline consp))
    (flet ((run (count) (declare (type (unsigned-byte 32) count))
      (let ((sum 0)) (declare (type (unsigned-byte 32) sum))
        (dotimes (i count sum) (declare (type (unsigned-byte 32) i))
          (when (consp x) (incf sum)) (setq x nil)))))
      (declare (notinline run)) (run n)))`);
  check(`(and (= (funcall *unboxed-test-function* nil 0) 0)
    (= (funcall *unboxed-test-function* (list 1) 0) 0)
    (= (funcall *unboxed-test-function* (list 1) 5) 1))`);
  // A read confined to a conditional arm cannot initialize a shared cache.
  enableCompilationStatistics(vm);
  await compile(`(lambda (x flag) (declare (type (signed-byte 32) x)
    (optimize (safety 0) (debug 0)) (inline + -))
    (if flag (values (+ x 1) (- x 1)) (values :skipped)))`);
  assert.ok(!(await readCompilationStatistics(vm)).some(row => row.rule === 'planning/unboxed-argument'));
  disableCompilationStatistics(vm);
  check(`(and (equal (multiple-value-list (funcall *unboxed-test-function* 7 t)) '(8 6))
    (equal (multiple-value-list (funcall *unboxed-test-function* 7 nil)) '(:skipped)))`);
  // Checked boundaries still run before any unchecked numeric internals.
  await compile(`(lambda (x) (declare (optimize (safety 3) (debug 0)))
    (if (typep x '(signed-byte 32))
        (locally (declare (optimize (safety 0)) (inline +))
          (let ((n (the (signed-byte 32) x))) (values (+ n 1) (+ n 2))))
        (error 'type-error :datum x :expected-type '(signed-byte 32))))`);
  check(`(and (handler-case (progn (funcall *unboxed-test-function* :bad) nil)
               (type-error (c) (eq (type-error-datum c) :bad)))
    (equal (multiple-value-list (funcall *unboxed-test-function* 7)) '(8 9)))`);
  enableCompilationStatistics(vm);
  await compile(`(lambda (x) (declare (type (integer 0 1000000) x)
    (optimize (safety 0) (debug 0)) (inline zerop 1- +))
    (let ((n x) (sum 0))
      (declare (type (integer 0 1000000) n) (type (integer 0 1000000000000) sum))
      (tagbody next (if (zerop n) (go done))
        (setq sum (the (integer 0 1000000000000) (+ sum n))
              n (the (integer 0 1000000) (1- n)))
        (go next) done)
      sum))`);
  stats = await readCompilationStatistics(vm);
  assert.ok(count('optimization/unboxed-cell') > 0);
  disableCompilationStatistics(vm);
  for (const x of [0, 1, 5, 1000]) check(`(= (funcall *unboxed-test-function* ${x}) ${x * (x + 1) / 2})`);
  await compile(`(lambda (x) (declare (type (integer 0 1000) x)
    (optimize (safety 0) (debug 0)) (inline zerop 1- +))
    (let ((n x) (sum 0) (seen nil))
      (declare (type (integer 0 1000) n) (type (integer 0 1000000) sum))
      (tagbody next (if (zerop n) (go done))
        (let ((old n))
          (setq n (the (integer 0 1000) (1- n)))
          (push old seen)
          (setq sum (the (integer 0 1000000) (+ sum old))))
        (go next) done)
      (values sum seen n)))`);
  check(`(equal (multiple-value-list (funcall *unboxed-test-function* 5)) '(15 (1 2 3 4 5) 0))`);
  // A write outside scalar range excludes the entire private cell.
  await compile(`(lambda (x) (declare (type (signed-byte 64) x)
    (optimize (safety 0) (debug 0)) (inline +))
    (let ((n x)) (setq n (+ n 9223372036854775808)) (values n n)))`);
  check(`(equal (multiple-value-list (funcall *unboxed-test-function* 7))
    '(9223372036854775815 9223372036854775815))`);

  console.log('Unboxed arithmetic: arguments, intermediate chains, shared boxes, joins, effects, signed boundaries, bignum promotion, mutation, captures and checked recovery pass.');
} finally {vm.close();}
