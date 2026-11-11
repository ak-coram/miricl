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
