import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  const compile = async source => {
    vm.evaluate(`(defparameter *inline-unit* (compile-to-hir '${source}))
      (multiple-value-bind (bytes literals) (emit-native-module *inline-unit*)
        (defparameter *inline-function* (compiler-primitive 17 bytes literals nil))
        (with-open-file (out "/inline.wasm" :direction :output :if-exists :supersede
          :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`);
    const bytes = await vm.readFile('/inline.wasm');
    return {bytes: bytes.length,
      imports: WebAssembly.Module.imports(await WebAssembly.compile(bytes)).map(entry => entry.name)};
  };
  const scalar64 = [-(1n << 63n), -(1n << 31n), -1n, 0n, 1n, 1n << 32n, (1n << 63n) - 1n];
  const scalar32 = [-(1n << 31n), -1n, 0n, 1n, (1n << 31n) - 1n];
  // EQL needs only identity when either literal is outside NUMBER/CHARACTER.
  // This fact comes from the value itself and is valid at every safety level.
  for (const safety of [0, 3]) {
    for (const literal of [':target', 'nil', '"target"', "'(1 2)", "'#(1 2)"]) {
      const shared = ![':target', 'nil'].includes(literal);
      const result = await compile(`(lambda (x) (declare (inline eql) (optimize (safety ${safety})))
        (values (eql x ${shared ? '#1=' : ''}${literal}) ${shared ? '#1#' : literal}))`);
      assert.ok(result.imports.includes('native-eq-inline'), literal);
      assert.ok(!result.imports.includes('native-eql-inline'), literal);
      check(`(multiple-value-bind (match literal) (funcall *inline-function* :missing)
        (and (not match) (funcall *inline-function* literal)
             (or (null literal) (not (typep literal 'sequence))
                 (not (funcall *inline-function* (copy-seq literal))))))`);
    }
  }
  for (const [literal, fresh] of [
    ['1099511627776', '(+ 549755813888 549755813888)'],
    ['1.0f0', '(+ 1.0f0 0.0f0)'], ['1.0d0', '(+ 1.0d0 0.0d0)'],
    ['#\\A', '(code-char 65)'], ['1/2', '(/ 1 2)'], ['#c(3 4)', '(complex 3 4)'],
  ]) {
    const result = await compile(`(lambda (x) (declare (inline eql)) (eql x ${literal}))`);
    assert.ok(result.imports.includes('native-eql-inline'), literal);
    check(`(and (funcall *inline-function* ${fresh}) (not (funcall *inline-function* :missing)))`);
  }
  await compile(`(lambda (x) (declare (inline eql))
    (let ((events nil))
      (values (eql (progn (push :left events) (values x :extra))
                   (progn (push :right events) :target)) (reverse events))))`);
  check(`(and (equal (multiple-value-list (funcall *inline-function* :target)) '(t (:left :right)))
              (equal (multiple-value-list (funcall *inline-function* :other)) '(nil (:left :right))))`);
  // Integer identities apply to bignums as well as scalar representations.
  // Both operand expressions still execute in order and contribute only
  // their primary values, even when the result no longer uses one of them.
  for (const [operator, left, right, constant] of [
    ['+', 'x', '0'], ['+', '0', 'x'], ['-', 'x', '0'],
    ['*', 'x', '1'], ['*', '1', 'x'], ['*', 'x', '0', 0n], ['*', '0', 'x', 0n],
    ['logand', 'x', '-1'], ['logand', '-1', 'x'],
    ['logand', 'x', '0', 0n], ['logand', '0', 'x', 0n],
    ['logior', 'x', '0'], ['logior', '0', 'x'],
    ['logior', 'x', '-1', -1n], ['logior', '-1', 'x', -1n],
    ['logxor', 'x', '0'], ['logxor', '0', 'x'],
    ['ash', 'x', '0'], ['ash', '0', 'x', 0n],
  ]) {
    const expression = `(${operator} (progn (push :left events) (values ${left} :extra))
      (progn (push :right events) (values ${right} :extra)))`;
    const result = await compile(`(lambda (x) (declare (optimize (safety 0))
      (inline ${operator}) (type integer x)) (${operator} ${left} ${right}))`);
    assert.ok(!result.imports.includes('native-function-reference'), expression);
    await compile(`(lambda (x) (declare (optimize (safety 0))
      (inline ${operator}) (type integer x))
      (let ((events nil)) (values ${expression} (reverse events))))`);
    for (const x of [...scalar64, -(1n << 160n), 1n << 160n])
      check(`(equal (multiple-value-list (funcall *inline-function* ${x}))
        '(${constant ?? x} (:left :right)))`);
  }
  // A singleton declaration is evidence even when the operand is not literal.
  const declaredIdentity = await compile(`(lambda (x zero)
    (declare (optimize (safety 0)) (inline +) (type integer x) (type (mod 1) zero))
    (+ x zero))`);
  assert.ok(!declaredIdentity.imports.includes('native-function-reference'));
  check(`(equal (multiple-value-list (funcall *inline-function* 1267650600228229401496703205376 0))
    '(1267650600228229401496703205376))`);
  for (const [operator, expression, declarations] of [
    ['+', '(+ x 0)', ''], ['*', '(* x 0)', ''],
    ['logand', '(logand x 0)', ''], ['logior', '(logior x -1)', ''],
    ['ash', '(ash 0 x)', ''], ['+', '(+ x 0)', '(type number x)'],
  ]) {
    const result = await compile(`(lambda (x) (declare (optimize (safety 0))
      (inline ${operator}) ${declarations}) ${expression})`);
    assert.ok(result.imports.includes('native-function-reference'), expression);
    check(`(handler-case (progn (funcall *inline-function* :invalid) nil) (type-error () t))`);
  }
  await compile(`(lambda (x) (declare (optimize (safety 0)) (inline +) (type number x)) (+ x 0))`);
  check(`(typep (funcall *inline-function* 1.5) 'single-float)`);
  const safeIdentity = await compile(`(lambda (x) (declare (optimize (safety 3))
    (inline logand) (type integer x)) (logand x 0))`);
  assert.ok(safeIdentity.imports.includes('native-function-reference'));
  check(`(handler-case (progn (funcall *inline-function* :invalid) nil) (type-error () t))`);
  for (const [operator, type, arg, answer, entry] of [
    ['simple-vector-p', 'simple-vector', '#(42)', 'T', 'native-simple-vector-p-inline'],
    ['consp', '(cons integer t)', "'(42)", 'T', 'native-consp-inline'],
    ['consp', 'null', 'nil', 'NIL', 'native-consp-inline'],
    ['null', '(mod 256)', '0', 'NIL', 'native-boolean'],
    ['not', 'simple-vector', '#()', 'NIL', 'native-boolean'],
  ]) {
    const result = await compile(`(lambda (x) (declare (optimize (safety 0))
      (type ${type} x) (inline ${operator})) (${operator} x))`);
    assert.ok(!result.imports.includes(entry), `${operator}: predicate must fold`);
    assert.equal(vm.evaluate(`(funcall *inline-function* ${arg})`).output, `${answer}\n`);
  }
  await compile(`(lambda (v) (declare (optimize (safety 0)) (type simple-vector v)
    (inline simple-vector-p))
    (let ((calls 0)) (values (simple-vector-p (progn (incf calls) v)) calls)))`);
  check(`(equal (multiple-value-list (funcall *inline-function* #())) '(t 1))`);
  for (const [operator, delta] of [['1+', 1n], ['1-', -1n]]) {
    const result = await compile(`(lambda (a) (declare (optimize (safety 0))
      (inline ${operator}) (type (or (signed-byte 32) (eql 4294967295)) a)) (${operator} a))`);
    assert.ok(!result.imports.includes('native-function-reference'), operator);
    for (const a of [...scalar32, 4294967295n])
      assert.equal(vm.evaluate(`(funcall *inline-function* ${a})`).output, `${a + delta}\n`);
    const wide = await compile(`(lambda (a) (declare (optimize (safety 0))
      (inline ${operator}) (fixnum a)) (${operator} a))`);
    assert.ok(wide.imports.includes('native-function-reference'));
    for (const a of scalar64)
      assert.equal(vm.evaluate(`(funcall *inline-function* ${a})`).output, `${a + delta}\n`);
  }
  for (const [operator, type, value, answer] of [
    ['1+', '(integer -9223372036854775808 9223372036854775806)', '9223372036854775806', '9223372036854775807'],
    ['1-', '(integer -9223372036854775807 9223372036854775807)', '-9223372036854775807', '-9223372036854775808'],
  ]) {
    const result = await compile(`(lambda (a) (declare (optimize (safety 0))
      (inline ${operator}) (type ${type} a)) (${operator} a))`);
    assert.ok(!result.imports.includes('native-function-reference'), operator);
    assert.equal(vm.evaluate(`(funcall *inline-function* ${value})`).output, `${answer}\n`);
  }
  for (const [operator, left, right, args, answer] of [
    ['+', '(integer -9223372036854775808 0)', '(integer 0 9223372036854775807)', '-9223372036854775808 9223372036854775807', '-1'],
    ['-', '(integer -9223372036854775808 -1)', '(integer -9223372036854775808 -1)', '-9223372036854775808 -1', '-9223372036854775807'],
    ['*', 'fixnum', '(eql 0)', '-9223372036854775808 0', '0'],
  ]) {
    const result = await compile(`(lambda (a b) (declare (optimize (safety 0))
      (inline ${operator}) (type ${left} a) (type ${right} b)) (${operator} a b))`);
    assert.ok(!result.imports.includes('native-function-reference'), operator);
    assert.equal(vm.evaluate(`(funcall *inline-function* ${args})`).output, `${answer}\n`);
  }
  const nullableVector = await compile(`(lambda (v) (declare (optimize (safety 0))
    (inline svref) (type (or null simple-vector) v)) (if v (svref v 0) :empty))`);
  assert.ok(nullableVector.imports.includes('native-vector-ref-inline'));
  check(`(and (eq (funcall *inline-function* nil) :empty)
    (= (funcall *inline-function* #(42)) 42))`);
  // Both paths of the inner conditional carry the outer type premise.
  const joinedVector = await compile(`(lambda (v flag) (declare (optimize (safety 0))
    (inline svref) (type (or null simple-vector) v))
    (if v (let ((i (if flag 0 1))) (svref v i)) :empty))`);
  assert.ok(joinedVector.imports.includes('native-vector-ref-inline'));
  check(`(and (= (funcall *inline-function* #(41 42) t) 41)
    (= (funcall *inline-function* #(41 42) nil) 42)
    (eq (funcall *inline-function* nil t) :empty))`);
  for (const expression of [
    '(cond ((null v)) (t (svref v 0)))',
    '(let ((empty (null v))) (if empty t (svref v 0)))',
    '(let* ((empty (null v)) (alias empty)) (if alias t (svref v 0)))',
  ]) {
    const result = await compile(`(lambda (v) (declare (optimize (safety 0))
      (inline svref null) (type (or null simple-vector) v)) ${expression})`);
    assert.ok(result.imports.includes('native-vector-ref-inline'), expression);
    check(`(and (= (funcall *inline-function* #(42)) 42)
      (eq (funcall *inline-function* nil) t))`);
  }
  const changedPredicate = await compile(`(lambda (v) (declare (optimize (safety 0))
    (inline svref null) (type (or null simple-vector) v))
    (let ((empty (null v))) (setq empty nil) (if empty t (svref v 0))))`);
  assert.ok(!changedPredicate.imports.includes('native-vector-ref-inline'));
  check(`(handler-case (progn (funcall *inline-function* nil) nil) (type-error () t))`);
  const changedSource = await compile(`(lambda (v) (declare (optimize (safety 0))
    (inline svref null) (type (or null simple-vector) v))
    (let ((empty (null v))) (setq v nil) (if empty t (svref v 0))))`);
  assert.ok(!changedSource.imports.includes('native-vector-ref-inline'));
  check(`(handler-case (progn (funcall *inline-function* #(42)) nil) (type-error () t))`);
  const outsideJoin = await compile(`(lambda (v flag) (declare (optimize (safety 0))
    (inline svref) (type (or null simple-vector) v))
    (let ((i (if v (if flag 0 1) 0))) (svref v i)))`);
  assert.ok(!outsideJoin.imports.includes('native-vector-ref-inline'));
  check(`(and (= (funcall *inline-function* #(41 42) nil) 42)
    (handler-case (progn (funcall *inline-function* nil t) nil) (type-error () t)))`);
  for (const test of ['(null v)', '(not v)']) {
    const result = await compile(`(lambda (v) (declare (optimize (safety 0))
      (inline svref null not) (type (or null simple-vector) v))
      (if ${test} :empty (svref v 0)))`);
    assert.ok(result.imports.includes('native-vector-ref-inline'));
    check(`(and (eq (funcall *inline-function* nil) :empty)
      (= (funcall *inline-function* #(42)) 42))`);
  }
  const nullableInteger = await compile(`(lambda (x) (declare (optimize (safety 0))
    (inline 1+) (type (or null (mod 256)) x)) (if x (1+ x) :empty))`);
  assert.ok(!nullableInteger.imports.includes('native-function-reference'));
  check(`(and (eq (funcall *inline-function* nil) :empty)
    (= (funcall *inline-function* 0) 1) (= (funcall *inline-function* 255) 256))`);
  for (const mutation of ['(setq v nil)', '(funcall (lambda () (setq v nil)))']) {
    const result = await compile(`(lambda (v) (declare (optimize (safety 0))
      (inline svref) (type (or null simple-vector) v))
      (if v (progn ${mutation} (svref v 0)) :empty))`);
    assert.ok(!result.imports.includes('native-vector-ref-inline'));
    check(`(handler-case (progn (funcall *inline-function* #(42)) nil) (type-error () t))`);
  }
  for (const body of [
    '(if v (let ((v nil)) (svref v 0)) :empty)',
    '(flet ((null (x) (declare (ignore x)) nil)) (declare (notinline null)) (if (null v) :empty (svref v 0)))',
  ]) {
    const result = await compile(`(lambda (v) (declare (optimize (safety 0))
      (inline svref null) (type (or null simple-vector) v)) ${body})`);
    assert.ok(!result.imports.includes('native-vector-ref-inline'));
    check(`(handler-case (progn (funcall *inline-function* ${body.startsWith('(if') ? '#(42)' : 'nil'}) nil)
      (type-error () t))`);
  }
  for (const [operator, operation, bits, inputs] of [
    ['+', (a, b) => a + b, 32, scalar32], ['-', (a, b) => a - b, 32, scalar32],
    ['*', (a, b) => a * b, 32, scalar32],
    ['logand', (a, b) => a & b, 64, scalar64], ['logior', (a, b) => a | b, 64, scalar64],
    ['logxor', (a, b) => a ^ b, 64, scalar64],
    ['=', (a, b) => a === b, 64, scalar64], ['/=', (a, b) => a !== b, 64, scalar64],
    ['<', (a, b) => a < b, 64, scalar64], ['>', (a, b) => a > b, 64, scalar64],
    ['<=', (a, b) => a <= b, 64, scalar64], ['>=', (a, b) => a >= b, 64, scalar64],
  ]) {
    const result = await compile(`(lambda (a b)
      (declare (optimize (safety 0)) (inline ${operator}) (type (signed-byte ${bits}) a b))
      (${operator} a b))`);
    assert.ok(!result.imports.includes('native-function-reference'), operator);
    for (const a of inputs) for (const b of inputs) {
      const value = operation(a, b), expected = value === true ? 'T' : value === false ? 'NIL' : String(value);
      assert.equal(vm.evaluate(`(funcall *inline-function* ${a} ${b})`).output, `${expected}\n`, operator);
    }
  }
  const checkedSum = await compile(`(lambda (a b)
    (declare (optimize (safety 0)) (inline +) (fixnum a b)) (list (+ a b)))`);
  assert.ok(checkedSum.imports.includes('native-add-int64-checked'));
  check(`(equal (funcall *inline-function* 9223372036854775807 1) '(9223372036854775808))`);
  const unknownBits = await compile(`(lambda (x)
    (declare (optimize (safety 0)) (inline logand)) (logand x 127))`);
  assert.ok(unknownBits.imports.includes('native-function-reference'));
  check(`(= (funcall *inline-function* 1267650600228229401496703205631) 127)`);
  const notinlineSum = await compile(`(lambda (a b)
    (declare (optimize (safety 0)) (inline +) (type (signed-byte 32) a b))
    (locally (declare (notinline +)) (+ a b)))`);
  assert.ok(notinlineSum.imports.includes('native-function-reference'));
  check(`(= (funcall *inline-function* 40 2) 42)`);
  await compile(`(lambda (a b)
    (declare (optimize (safety 0)) (inline +) (type (signed-byte 32) a b))
    (let ((calls 0))
      (values (+ (progn (incf calls) a) (progn (incf calls) b)) calls)))`);
  check(`(equal (multiple-value-list (funcall *inline-function* 40 2)) '(42 2))`);
  for (const [operator, entry] of [['car', 'native-car-list-inline'], ['cdr', 'native-cdr-list-inline']]) {
    const result = await compile(`(lambda (input) (declare (type list input)
      (optimize (safety 0)) (inline ${operator}))
      (let ((copy input)) (${operator} copy)))`);
    assert.ok(result.imports.includes(entry), operator);
    assert.ok(!result.imports.includes('native-function-reference'), operator);
    check(`(and (equal (multiple-value-list (funcall *inline-function* nil)) '(nil))
      (eq (funcall *inline-function* '(42 . :tail)) ${operator === 'car' ? '42' : ':tail'}))`);
  }
  for (const [operator, parameters, expression, args, expected] of [
    ['eq', '(a b)', '(eq a b)', ':same :same', 't'],
    ['eql', '(a b)', '(eql a b)', '9223372036854775808 9223372036854775808', 't'],
    ['null', '(a)', '(null a)', 'nil', 't'],
    ['not', '(a)', '(not a)', '42', 'nil'],
  ]) {
    const result = await compile(`(lambda ${parameters} (declare (inline ${operator})) ${expression})`);
    assert.ok(!result.imports.includes('native-function-reference'), operator);
    check(`(equal (multiple-value-list (funcall *inline-function* ${args})) '(${expected}))`);
  }
  for (const [operator, entry, trueInputs, falseInputs] of [
    ['consp', 'native-consp-inline', ["'(1 . 2)", "'(nil)"], ['nil', 't', '17', '#(1)', '"x"']],
    ['simple-vector-p', 'native-simple-vector-p-inline', ['#()', '#(1 2)', '(make-array 3)'],
      ['nil', '17', "'(1 2)", '"x"', '(make-array 3 :fill-pointer 1)', '(make-array 3 :adjustable t)']],
  ]) {
    const result = await compile(`(lambda (x) (declare (inline ${operator})) (${operator} x))`);
    assert.ok(result.imports.includes(entry), operator);
    assert.ok(!result.imports.includes('native-function-reference'), operator);
    for (const input of trueInputs)
      check(`(equal (multiple-value-list (funcall *inline-function* ${input})) '(t))`);
    for (const input of falseInputs)
      check(`(equal (multiple-value-list (funcall *inline-function* ${input})) '(nil))`);
  }
  for (const [declarations, entry] of [
    ['(optimize (safety 0)) (type simple-vector x)', 'native-vector-length-inline'],
    ['(optimize (safety 3)) (type simple-vector x)', 'native-length-inline'],
    ['', 'native-length-inline'],
  ]) {
    const result = await compile(`(lambda (x) (declare (inline length) ${declarations}) (length x))`);
    assert.ok(result.imports.includes(entry), declarations);
    assert.ok(!result.imports.includes('native-function-reference'), declarations);
    check(`(and (equal (multiple-value-list (funcall *inline-function* #())) '(0))
      (= (funcall *inline-function* #(1 2 3)) 3))`);
    if (!declarations) {
      check(`(let ((v (make-array 9 :initial-element nil :fill-pointer 2)))
        (and (= (funcall *inline-function* v) 2)
          (= (funcall *inline-function* '(1 2 3)) 3)
          (= (funcall *inline-function* "abc") 3)
          (= (funcall *inline-function* nil) 0)))`);
      check(`(let ((cycle (list 1))) (setf (cdr cycle) cycle)
        (every (lambda (input)
          (handler-case (progn (funcall *inline-function* input) nil) (type-error () t)))
          (list 17 '(1 . 2) cycle)))`);
    }
  }
  const consResult = await compile(`(lambda (x y) (declare (inline cons)) (cons x y))`);
  assert.ok(consResult.imports.includes('native-cons'));
  assert.ok(!consResult.imports.includes('native-function-reference'));
  check(`(let* ((tail (list 2)) (a (funcall *inline-function* 1 tail))
               (b (funcall *inline-function* 1 tail)))
    (and (not (eq a b)) (eq (cdr a) tail)
      (equal (multiple-value-list (funcall *inline-function* 3 4)) '((3 . 4)))))`);
  const inferredCons = await compile(`(lambda (x y)
    (declare (optimize (safety 0)) (inline cons cdr))
    (let ((cell (cons x y))) (cdr cell)))`);
  assert.ok(inferredCons.imports.includes('native-cdr-list-inline'));
  assert.ok(!inferredCons.imports.includes('native-function-reference'));
  check(`(and (eq (funcall *inline-function* 17 :tail) :tail)
    (null (funcall *inline-function* 17 nil)))`);
  for (const defaultArgument of ['', ' default']) {
    const lookup = await compile(`(lambda (key table default)
      (declare (inline gethash)) (gethash key table${defaultArgument}))`);
    assert.ok(lookup.imports.includes('native-gethash-inline'));
    assert.ok(!lookup.imports.includes('native-function-reference'));
    check(`(let ((table (make-hash-table)))
      (setf (gethash :present table) nil)
      (and (equal (multiple-value-list (funcall *inline-function* :present table :default)) '(nil t))
        (equal (multiple-value-list (funcall *inline-function* :absent table :default))
          '(${defaultArgument ? ':default' : 'nil'} nil))
        (handler-case (progn (funcall *inline-function* :key 17 nil) nil) (type-error () t))))`);
  }
  await compile(`(lambda (key table) (declare (inline gethash))
    (list (gethash key table :default)))`);
  check(`(let ((table (make-hash-table :test 'equal)))
    (setf (gethash (list 1 2) table) 42)
    (and (equal (funcall *inline-function* (list 1 2) table) '(42))
      (equal (funcall *inline-function* :absent table) '(:default))))`);
  const primaryLookup = await compile(`(lambda (key table) (declare (inline gethash))
    (list (gethash key table)))`);
  assert.ok(primaryLookup.imports.includes('native-gethash-primary-inline'));
  check(`(let ((table (make-hash-table)))
    (setf (gethash :key table) 42)
    (and (equal (funcall *inline-function* :key table) '(42))
      (equal (funcall *inline-function* :absent table) '(nil))))`);
  const store = await compile(`(lambda (value key table)
    (declare (inline (setf gethash)))
    (setf (gethash key table) value))`);
  assert.ok(store.imports.includes('native-puthash-inline'));
  check(`(let ((table (make-hash-table)))
    (and (equal (multiple-value-list (funcall *inline-function* 42 :key table)) '(42))
      (= (gethash :key table) 42)))`);
  await compile(`(lambda (table) (declare (inline gethash (setf gethash)))
    (let ((events nil))
      (setf (gethash (progn (push :key events) :key)
                      (progn (push :table events) table)
                      (progn (push :default events) :ignored))
        (progn (push :value events) 42))
      (values (gethash :key table) events)))`);
  check(`(equal (multiple-value-list (funcall *inline-function* (make-hash-table)))
    '(42 (:value :default :table :key)))`);
  for (const operator of ['svref', 'aref', 'row-major-aref']) {
    const source = `(lambda (v) (declare (type simple-vector v) (optimize (safety 0)) (inline ${operator}))
      (${operator} v 0))`;
    const result = await compile(source);
    assert.ok(result.imports.includes('native-vector-ref-inline'), operator);
    assert.ok(!result.imports.includes('native-function-reference'), operator);
    assert.ok(!result.imports.includes('native-tail-call'), operator);
    check(`(and (equal (multiple-value-list (funcall *inline-function* #(42))) '(42))
                (equal (multiple-value-list (funcall *inline-function* #(nil))) '(nil)))`);
    check(`(let ((original (symbol-function '${operator})))
      (unwind-protect
        (progn (setf (symbol-function '${operator}) (lambda (v i) (declare (ignore v i)) :replacement))
          (= (funcall *inline-function* #(42)) 42))
        (setf (symbol-function '${operator}) original)))`);
    console.log(`PASS inline typed ${operator}: ${result.bytes} bytes, no function lookup or dispatch`);
  }
  for (const type of ['(signed-byte 30)', '(signed-byte 32)', '(signed-byte 64)', 'fixnum']) {
    const result = await compile(`(lambda (v i) (declare (optimize (safety 0))
      (inline svref) (type simple-vector v) (type ${type} i)) (svref v i))`);
    assert.ok(result.imports.includes('native-vector-ref-inline'), type);
    assert.ok(!result.imports.includes('native-function-reference'), type);
    check(`(and (= (funcall *inline-function* #(17 42) 1) 42)
      (equal (multiple-value-list (funcall *inline-function* #(nil) 0)) '(nil)))`);
  }
  for (const source of [
    `(lambda (v) (declare (optimize (safety 0)) (inline svref)) (svref v 0))`,
    `(lambda (v) (declare (type simple-vector v) (optimize (safety 1)) (inline svref)) (svref v 0))`,
    `(lambda (v) (declare (type simple-vector v) (optimize (safety 0)) (inline svref))
       (locally (declare (notinline svref)) (svref v 0)))`,
  ]) {
    assert.ok(!(await compile(source)).imports.includes('native-vector-ref-inline'), source);
    check('(= (funcall *inline-function* #(42)) 42)');
  }
  // A bound declaration starts with its binding, including subsequent
  // initializers. A free declaration starts only at the body's boundary.
  for (const source of [
    `(lambda (input) (declare (optimize (safety 0)) (inline svref))
       (let* ((v input) (answer (svref v 0))) (declare (type simple-vector v)) answer))`,
    `(lambda (v &optional (answer (locally (declare (inline svref)) (svref v 0))))
       (declare (optimize (safety 0)) (inline svref) (type simple-vector v)) answer)`,
    `(lambda (input) (declare (optimize (safety 0)) (inline svref))
       (let* ((v input) (answer (svref v 0)) (v 17))
         (declare (type (or simple-vector integer) v)) (declare (ignore v)) answer))`,
  ]) {
    const result = await compile(source);
    // Only the first two have a sufficiently narrow vector declaration.
    assert.equal(result.imports.includes('native-vector-ref-inline'), !source.includes('(v 17)'), source);
    check('(= (funcall *inline-function* #(42)) 42)');
  }
  for (const source of [
    `(lambda (v) (declare (optimize (safety 0)) (inline svref))
       (let* ((answer (svref v 0))) (declare (type simple-vector v)) answer))`,
    `(lambda (v) (declare (optimize (safety 0)) (inline svref))
       (let ((v (svref v 0))) (declare (type simple-vector v)) v))`,
    `(lambda (v) (declare (optimize (safety 0)) (inline svref) (type simple-vector v))
       (let ((v (car (list v)))) (svref v 0)))`,
  ]) {
    assert.ok(!(await compile(source)).imports.includes('native-vector-ref-inline'), source);
    // The parallel LET declares its resulting binding to be a vector.
    const parallel = source.includes('(let ((v (svref');
    check(parallel ? '(equalp (funcall *inline-function* #(#(42))) #(42))'
                   : '(= (funcall *inline-function* #(42)) 42)');
  }
  // An INLINE request cannot substitute the canonical builtin for a different
  // definition already visible when compilation starts. Use ROW-MAJOR-AREF
  // so the compiler's own SVREF record accessors remain usable during setup.
  check(`(let ((original (symbol-function 'row-major-aref)))
    (unwind-protect
      (progn (setf (symbol-function 'row-major-aref) (lambda (v i) (declare (ignore v i)) :replacement))
        (let ((f (compile-native '(lambda (v) (declare (optimize (safety 0))
          (type simple-vector v) (inline row-major-aref)) (row-major-aref v 0)))))
          (eq (funcall f #(42)) :replacement)))
      (setf (symbol-function 'row-major-aref) original)))`);
  await compile(`(lambda (v) (declare (optimize (safety 0)) (inline svref))
    (flet ((svref (v i) (declare (ignore v i)) :local)) (svref v 0)))`);
  check('(eq (funcall *inline-function* #(42)) :local)');
  await compile(`(lambda (v) (declare (optimize (safety 0)) (inline svref))
    (let ((count 0))
      (values (svref (progn (incf count) (the simple-vector v)) (progn (incf count) 0)) count)))`);
  check(`(equal (multiple-value-list (funcall *inline-function* #(42))) '(42 2))`);
  await compile(`(lambda (v) (declare (type simple-vector v) (optimize (safety 0)) (inline svref))
    (values #'svref (svref v 0)))`);
  check(`(progn
    (let ((references nil))
      (dolist (function (hir-unit-functions *inline-unit*))
        (dolist (block (hir-function-blocks function))
          (dolist (instruction (hir-block-instructions block))
            (when (eq (hir-instruction-opcode instruction) :function-reference)
              (push (hir-instruction-output instruction) references)))))
      (let ((shared (first references)) (other (second references)))
        (dolist (function (hir-unit-functions *inline-unit*))
          (dolist (block (hir-function-blocks function))
            (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
              (setf (hir-instruction-inputs instruction)
                (substitute shared other (hir-instruction-inputs instruction) :test #'eq)))))
        (multiple-value-bind (bytes literals) (emit-native-module *inline-unit*)
          (setq *inline-function* (compiler-primitive 17 bytes literals nil))))) t)`);
  check(`(let ((original (symbol-function 'svref)))
    (unwind-protect
      (progn (setf (symbol-function 'svref) (lambda (v i) (declare (ignore v i)) :replacement))
        (multiple-value-bind (function answer) (funcall *inline-function* #(42))
          (and (eq function (symbol-function 'svref)) (= answer 42))))
      (setf (symbol-function 'svref) original)))`);
  await compile(`(lambda (v) (declare (type simple-vector v) (optimize (safety 0))
    (inline row-major-aref)) (row-major-aref v 0))`);
  check(`(progn
    (dolist (function (hir-unit-functions *inline-unit*))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (hir-block-instructions block))
          (when (eq (hir-instruction-opcode instruction) :function-reference)
            (setf (hir-instruction-attributes instruction) '(row-major-aref notinline))))))
    (let ((original (symbol-function 'row-major-aref)))
      (unwind-protect
        (progn (setf (symbol-function 'row-major-aref) (lambda (v i) (declare (ignore v i)) :replacement))
          (multiple-value-bind (bytes literals) (emit-native-module *inline-unit*)
            (let ((new (compiler-primitive 17 bytes literals nil)))
              (and (= (funcall *inline-function* #(42)) 42) (eq (funcall new #(42)) :replacement)))))
        (setf (symbol-function 'row-major-aref) original))))`);
  console.log('Inline types: canonical definitions, no dynamic lookup, narrow/wide indices, result counts, operand effects, bound/free declaration scope, local shadowing, NOTINLINE, first-class references and fresh emission facts pass.');
} finally {vm.close();}
