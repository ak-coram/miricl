import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  const compile = async source => {
    vm.evaluate(`(multiple-value-bind (bytes literals) (emit-native-module (compile-to-hir '${source}))
      (defparameter *shift-test-function* (compiler-primitive 17 bytes literals nil))
      (with-open-file (out "/shift-test.wasm" :direction :output :if-exists :supersede
        :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`);
    const bytes = await vm.readFile('/shift-test.wasm');
    const module = await WebAssembly.compile(bytes);
    return {bytes, imports: WebAssembly.Module.imports(module).map(entry => entry.name)};
  };
  const signed = [-(1n << 63n), -(1n << 31n), -129n, -1n, 0n, 1n, 129n, (1n << 63n) - 1n];
  for (const count of [-1n, -7n, -63n, -64n, -65n, -1000n, -(1n << 100n)]) {
    const {imports} = await compile(`(lambda (x) (declare (type (signed-byte 64) x)
      (optimize (safety 0) (debug 0)) (inline ash)) (ash x ${count}))`);
    assert.ok(!imports.includes('native-function-reference'));
    assert.ok(!imports.includes('native-ash-int64'));
    assert.ok(!imports.includes('native-literal'), 'An encoded constant count needs no literal pool');
    if (count <= -63n)
      assert.ok(!imports.includes('native-int64'), 'A saturated right shift needs only an immediate integer');
    for (const x of signed) {
      const expected = count <= -63n ? (x < 0n ? -1n : 0n) : x >> -count;
      check(`(equal (multiple-value-list (funcall *shift-test-function* ${x})) (list ${expected}))`);
    }
  }
  check(`(and (equal (hir-inline-result-type 'ash '((signed-byte 64) (eql -1267650600228229401496703205376)))
                    '(integer -1 0))
    (equal (hir-inline-result-type 'ash '((integer 0 9223372036854775807) (eql -1000))) '(integer 0 0))
    (every (lambda (count)
      (let ((result (hir-inline-result-type 'ash (list '(signed-byte 64) (list 'eql count)))))
        (and (hir-type-proves-p result 'integer)
             (not (nth-value 2 (hir-integer-bounds result)))
             (null (native-type-integer-width result)))))
      '(1000000 1267650600228229401496703205376)))`);
  for (const [type, count, values] of [
    ['(signed-byte 8)', 3n, [-128n, -1n, 0n, 1n, 127n]],
    ['(integer -1 0)', 63n, [-1n, 0n]],
    ['(unsigned-byte 32)', 31n, [0n, 1n, 4294967295n]],
  ]) {
    const {imports} = await compile(`(lambda (x) (declare (type ${type} x)
      (optimize (safety 0) (debug 0)) (inline ash)) (ash x ${count}))`);
    assert.ok(!imports.includes('native-function-reference'));
    for (const x of values) check(`(= (funcall *shift-test-function* ${x}) ${x << count})`);
  }
  // A possible left-shift overflow and counts beyond Wasm's width retain CL
  // bignum promotion, rather than wrapping or masking the shift count.
  for (const count of [1n, 64n, 100n]) {
    await compile(`(lambda (x) (declare (type (signed-byte 64) x)
      (optimize (safety 0) (debug 0)) (inline ash)) (ash x ${count}))`);
    for (const x of [-(1n << 63n), -1n, 0n, 1n, (1n << 63n) - 1n])
      check(`(= (funcall *shift-test-function* ${x}) ${x << count})`);
  }
  await compile(`(lambda (x) (declare (type (signed-byte 64) x)
    (optimize (safety 0) (debug 0)) (inline ash)) (ash x 1000000))`);
  check(`(and (equal (multiple-value-list (funcall *shift-test-function* 0)) '(0))
    (= (integer-length (funcall *shift-test-function* 1)) 1000001)
    (= (integer-length (funcall *shift-test-function* -1)) 1000000))`);
  await compile(`(lambda (x count) (declare (type (signed-byte 64) x) (type (eql -7) count)
    (optimize (safety 0) (debug 0)) (inline ash +))
    (values (+ (ash x count) (ash x -9)) count))`);
  for (const x of signed) check(`(equal (multiple-value-list (funcall *shift-test-function* ${x} -7))
    '(${(x >> 7n) + (x >> 9n)} -7))`);
  await compile(`(lambda (x) (declare (type (signed-byte 64) x)
    (optimize (safety 0) (debug 0)) (inline ash))
    (let ((events nil))
      (values (ash (progn (push :left events) x)
                   (the (eql -7) (progn (push :right events) (values -7 :ignored)))) events)))`);
  check(`(equal (multiple-value-list (funcall *shift-test-function* -129)) '(-2 (:right :left)))`);
  await compile(`(lambda (x) (declare (type (signed-byte 64) x)
    (optimize (safety 0) (debug 0)) (inline ash))
    (ash x (progn (locally (declare (optimize (safety 3))) (car 7)) -7)))`);
  check(`(handler-case (progn (funcall *shift-test-function* 7) nil) (type-error () t))`);
  await compile(`(lambda (x count) (declare (optimize (safety 3) (debug 0))) (ash x count))`);
  check(`(and (handler-case (progn (funcall *shift-test-function* 1.5 -7) nil) (type-error () t))
    (handler-case (progn (funcall *shift-test-function* 7 :bad) nil) (type-error () t))
    (= (funcall *shift-test-function* -129 -7) -2))`);
  // A local NOTINLINE still observes the current function definition.
  check(`(let ((original (symbol-function 'ash)))
    (unwind-protect
      (let ((fn (compile-native '(lambda (x) (declare (type (signed-byte 32) x)
        (optimize (safety 0) (debug 0)) (notinline ash)) (ash x -7)))))
        (setf (symbol-function 'ash) (lambda (x n) (declare (ignore x n)) (values :changed :extra)))
        (equal (multiple-value-list (funcall fn 7)) '(:changed :extra)))
      (setf (symbol-function 'ash) original)))`);
  console.log('Compiler shifts: signed boundaries, huge right counts, left-shift overflow, effects, value counts, checked errors and NOTINLINE pass.');
} finally {vm.close();}
