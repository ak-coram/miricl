import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
const operators = [
  ['zerop', x => x === 0n], ['minusp', x => x < 0n], ['plusp', x => x > 0n],
  ['evenp', x => x % 2n === 0n], ['oddp', x => x % 2n !== 0n],
];
const scalars = [-(1n << 63n), -(1n << 63n) + 1n, -(1n << 30n) - 1n,
  -2n, -1n, 0n, 1n, 2n, (1n << 30n) - 1n, 1n << 30n, (1n << 63n) - 1n];
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  const compile = async source => {
    vm.evaluate(`(multiple-value-bind (bytes literals) (emit-native-module (compile-to-hir '${source}))
      (defparameter *predicate-function* (compiler-primitive 17 bytes literals nil))
      (with-open-file (out "/predicate.wasm" :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values)`);
    const bytes = await vm.readFile('/predicate.wasm');
    assert.ok(WebAssembly.validate(bytes));
    return WebAssembly.Module.imports(new WebAssembly.Module(bytes)).map(entry => entry.name);
  };
  for (const [operator, expected] of operators) {
    const imports = await compile(`(lambda (x) (declare (optimize (safety 0)) (fixnum x) (inline ${operator}))
      (${operator} x))`);
    assert.ok(!imports.includes('native-function-reference'), operator);
    for (const value of scalars)
      assert.equal(vm.evaluate(`(multiple-value-list (funcall *predicate-function* ${value}))`).output,
        expected(value) ? '(T)\n' : '(NIL)\n', `${operator} ${value}`);
    const branch = await compile(`(lambda (x)
      (declare (optimize (safety 0)) (fixnum x) (inline ${operator}))
      (let ((calls 0))
        (if (${operator} (progn (incf calls) (values x :extra)))
            (values :yes calls) (values :no calls))))`);
    assert.ok(!branch.includes('native-boolean'), `${operator} branch uses its scalar condition`);
    for (const value of scalars)
      assert.equal(vm.evaluate(`(multiple-value-list (funcall *predicate-function* ${value}))`).output,
        `(${expected(value) ? ':YES' : ':NO'} 1)\n`, `${operator} branch ${value}`);
    await compile(`(lambda (x)
      (declare (optimize (safety 0)) (fixnum x) (inline ${operator} not))
      (if (not (${operator} x)) :no :yes))`);
    for (const value of [-1n, 0n, 1n])
      assert.equal(vm.evaluate(`(funcall *predicate-function* ${value})`).output,
        `${expected(value) ? ':YES' : ':NO'}\n`, `${operator} inverted branch ${value}`);
    const shared = await compile(`(lambda (x)
      (declare (optimize (safety 0)) (fixnum x) (inline ${operator}))
      (let ((answer (${operator} x))) (values (if answer :yes :no) answer)))`);
    assert.ok(shared.includes('native-boolean'), `${operator} shared result remains a Lisp boolean`);
    check(`(equal (multiple-value-list (funcall *predicate-function* -1))
      '(${expected(-1n) ? ':yes t' : ':no nil'}))`);
    const intervening = await compile(`(lambda (x)
      (declare (optimize (safety 0)) (fixnum x) (inline ${operator}))
      (let ((answer (${operator} x))) (identity x) (if answer :yes :no)))`);
    assert.ok(intervening.includes('native-boolean'), `${operator} does not cross another call`);
    for (const value of [-1n, 0n, 1n])
      assert.equal(vm.evaluate(`(funcall *predicate-function* ${value})`).output,
        `${expected(value) ? ':YES' : ':NO'}\n`, `${operator} intervening call ${value}`);
    await compile(`(lambda (x) (declare (optimize (safety 0)) (fixnum x) (inline ${operator}))
      (let ((calls 0)) (values (${operator} (progn (incf calls) (values x :extra))) calls)))`);
    check(`(equal (multiple-value-list (funcall *predicate-function* -1)) '(${expected(-1n) ? 't' : 'nil'} 1))`);
    for (const safety of [1, 3]) {
      const safe = await compile(`(lambda (x) (declare (optimize (safety ${safety})) (fixnum x) (inline ${operator}))
        (${operator} x))`);
      assert.ok(safe.includes('native-function-reference'), `${operator} safety ${safety}`);
      check(`(handler-case (progn (funcall *predicate-function* :bad) nil) (type-error () t))`);
    }
    const dynamic = await compile(`(lambda (x) (declare (optimize (safety 0)) (fixnum x) (notinline ${operator}))
      (${operator} x))`);
    assert.ok(dynamic.includes('native-function-reference'), `${operator} NOTINLINE`);
    const unrestricted = await compile(`(lambda (x) (declare (optimize (safety 0)) (inline ${operator})) (${operator} x))`);
    assert.ok(unrestricted.includes('native-function-reference'), `${operator} unknown operands`);
    for (const value of [-(1n << 160n), -(1n << 160n) + 1n, 1n << 160n, (1n << 160n) + 1n])
      assert.equal(vm.evaluate(`(funcall *predicate-function* ${value})`).output,
        expected(value) ? 'T\n' : 'NIL\n', `${operator} bignum`);
    check(`(handler-case (progn (funcall *predicate-function* :bad) nil) (type-error () t))`);
    // An inline call captures the canonical definition. An implementation
    // already replaced before compilation must retain its ordinary behavior.
    await compile(`(lambda (x) (declare (optimize (safety 0)) (fixnum x) (inline ${operator})) (${operator} x))`);
    check(`(let ((original (symbol-function '${operator})) (calls 0))
      (unwind-protect
        (progn (setf (symbol-function '${operator})
                 (lambda (x) (declare (ignore x)) (incf calls) (values :replaced :extra)))
          (and (equal (multiple-value-list (funcall *predicate-function* -1)) '(${expected(-1n) ? 't' : 'nil'}))
            (equal (hir-integer-predicate-type '${operator} '(eql 1)) '${expected(1n) ? '(eql t)' : 'null'})
            (= calls 0)
            (let ((replaced (compile-native '(lambda (x)
                    (declare (optimize (safety 0)) (type (eql 1) x) (inline ${operator})) (${operator} x)))))
              (let ((before calls))
                (and (equal (multiple-value-list (funcall replaced 1)) '(:replaced :extra)) (= calls (1+ before)))))))
        (setf (symbol-function '${operator}) original)))`);
  }
  for (const [operator, expected] of [
    ['<', (x, y) => x < y], ['>', (x, y) => x > y], ['<=', (x, y) => x <= y],
    ['>=', (x, y) => x >= y], ['=', (x, y) => x === y], ['/=', (x, y) => x !== y],
  ]) {
    const imports = await compile(`(lambda (x y)
      (declare (optimize (safety 0)) (fixnum x y) (inline ${operator}))
      (if (${operator} x y) :yes :no))`);
    assert.ok(!imports.includes('native-boolean'), `${operator} branch uses its scalar comparison`);
    for (const [x, y] of [[-(1n << 63n), (1n << 63n) - 1n], [0n, 0n], [1n, -1n]])
      assert.equal(vm.evaluate(`(funcall *predicate-function* ${x} ${y})`).output,
        `${expected(x, y) ? ':YES' : ':NO'}\n`, `${operator} branch ${x} ${y}`);
  }
  await compile('(lambda (fn) (if (funcall fn) :yes :no))');
  check(`(and (eq (funcall *predicate-function* (lambda () (values))) :no)
              (eq (funcall *predicate-function* (lambda () (values nil :extra))) :no)
              (eq (funcall *predicate-function* (lambda () (values 7 nil))) :yes))`);
  for (const [operator, type, value, expected] of [
    ['zerop', '(eql 0)', '0', 'T'], ['zerop', '(integer 1 *)', '1208925819614629174706176', 'NIL'],
    ['minusp', '(integer 0 *)', '1208925819614629174706176', 'NIL'],
    ['plusp', '(integer * -1)', '-1208925819614629174706176', 'NIL'],
    ['evenp', '(eql 1208925819614629174706176)', '1208925819614629174706176', 'T'],
    ['oddp', '(eql -1208925819614629174706177)', '-1208925819614629174706177', 'T'],
  ]) {
    assert.ok(!(await compile(`(lambda (x) (declare (optimize (safety 0)) (type ${type} x) (inline ${operator}))
      (${operator} x))`)).includes('native-function-reference'), `${operator} ${type}`);
    assert.equal(vm.evaluate(`(funcall *predicate-function* ${value})`).output, `${expected}\n`);
    await compile(`(lambda (x) (declare (optimize (safety 0)) (type ${type} x) (inline ${operator}))
      (if (${operator} x) (values :yes 17) (values :no 23)))`);
    check(`(equal (multiple-value-list (funcall *predicate-function* ${value}))
      '${expected === 'T' ? '(:yes 17)' : '(:no 23)'})`);
  }
  await compile('(lambda (x) (declare (optimize (safety 0)) (inline zerop)) (zerop x))');
  for (const value of ['0.0', '-0.0', '#c(0.0 0.0)']) check(`(funcall *predicate-function* ${value})`);
  check('(not (funcall *predicate-function* #c(1.0 0.0)))');
  for (const operator of ['minusp', 'plusp']) {
    await compile(`(lambda (x) (declare (optimize (safety 0)) (inline ${operator})) (${operator} x))`);
    check('(not (funcall *predicate-function* -0.0))');
    check('(handler-case (progn (funcall *predicate-function* #c(0.0 1.0)) nil) (type-error () t))');
  }
  for (const operator of ['evenp', 'oddp']) {
    await compile(`(lambda (x) (declare (optimize (safety 0)) (inline ${operator})) (${operator} x))`);
    check('(handler-case (progn (funcall *predicate-function* 1.0) nil) (type-error () t))');
  }
  for (const [operator] of operators) {
    for (const argumentsText of ['', 'x x']) {
      await compile(`(lambda (x) (declare (optimize (safety 0)) (inline ${operator})) (${operator} ${argumentsText}))`);
      check('(handler-case (progn (funcall *predicate-function* 0) nil) (program-error () t))');
    }
  }
  console.log('Integer predicates: i31/i64 boundaries, direct branches, shared booleans, sign/parity, range proofs, bignum fallback, operand effects, multiple values, safety, redefinition, and float/complex fallback pass.');
} finally {vm.close();}
