import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate(`(miricl-compiler::ensure-compiler) (in-package :miricl-compiler)
    (defun declared-read-widths (source)
      (let ((unit (compile-to-hir source)))
        (loop for function in (hir-unit-functions unit) append
          (loop for value in (hir-function-values function)
            for producer = (hir-value-producer value)
            when (and (%hir-instruction-p producer)
                      (member (hir-instruction-opcode producer) '(:cell-read :global-read)))
              collect (native-integer-width value))))) (values)`);
  // Free declarations end at their lexical scope, including worklist branches.
  check(`(equal (declared-read-widths '(lambda (x)
    (list (locally (declare (type (unsigned-byte 8) x)) (if x (+ x 1) 0)) (+ x 1))))
    '(9 9 nil))`);
  check(`(equal (declared-read-widths '(lambda (x)
    (declare (type (unsigned-byte 8) x))
    (list (+ x 1) (let ((x 9223372036854775808)) (+ x 1)) (+ x 1)))) '(9 nil 9))`);
  check(`(equal (declared-read-widths '(lambda (x)
    (declare (fixnum x))
    (locally (declare (type (unsigned-byte 8) x)) (+ x 1)))) '(9))`);
  check(`(equal (declared-read-widths '(lambda (x)
    (declare (type (unsigned-byte 8) x)) (lambda () (+ x 1)))) '(9))`);
  check(`(equal (declared-read-widths '(lambda (x)
    (let ((y x)) (declare (type (signed-byte 16) y))
      (setq y 17) (list (+ y 1) (* y 2))))) '(nil 16 16))`);
  check(`(equal (declared-read-widths '(lambda (x)
    (multiple-value-bind (a b) (values x x)
      (declare (type (unsigned-byte 8) a b)) (list (+ a 1) (- b 1))))) '(nil nil 9 9))`);
  // The initializer sees the outer binding; its free declaration covers only
  // the body. Assigning a bignum after that scope is valid and must promote.
  for (const [source, args, expected] of [
    [`(lambda (x) (declare (type (unsigned-byte 8) x))
       (list (+ x 1) (< x 128) (* x x)))`, '255', '(256 NIL 65025)\n'],
    [`(lambda (x) (declare (type (unsigned-byte 8) x))
       (list (+ x 1) (let ((x 9223372036854775808)) (+ x 1)) (+ x 1)))`, '7', '(8 9223372036854775809 8)\n'],
    [`(lambda (x)
       (let ((y (+ x 1))) (declare (type (unsigned-byte 8) x)) (+ x y)))`, '7', '15\n'],
    [`(lambda (x)
       (let ((y (locally (declare (type (unsigned-byte 8) x)) (+ x 1))))
         (setq x 9223372036854775808) (values y (+ x 1))))`, '7', '8\n9223372036854775809\n'],
    [`(lambda (x) (declare (fixnum x)) (list (+ x 1)))`, '9223372036854775807', '(9223372036854775808)\n'],
    [`(lambda (x) (declare (type (unsigned-byte 8) x))
       (let ((f (lambda () (list (+ x 1))))) (setq x 255) (funcall f)))`, '1', '(256)\n'],
    [`(lambda () (the (values) (values)))`, '', ''],
    [`(lambda () (the (values integer keyword) (values 42 :second)))`, '', '42\n:SECOND\n'],
    [`(lambda (x) (list (+ (the (values (unsigned-byte 8)) (values x :extra)) 1)))`, '255', '(256)\n'],
  ]) {
    vm.evaluate(`(defparameter *typed-function* (compile-native '${source}))`);
    assert.equal(vm.evaluate(`(funcall *typed-function* ${args})`).output, expected, source);
  }
  vm.evaluate(`(defparameter *typed-special* 7)`);
  check(`(let ((f (compile-native '(lambda ()
    (locally (declare (special *typed-special*) (type (unsigned-byte 8) *typed-special*))
      (list (+ *typed-special* 1)))))))
    (equal (funcall f) '(8)))`);
  // Inspect actual native imports: a declaration must select native arithmetic,
  // while full-width sums use a scalar entry with bignum promotion.
  const imports = async source => {
    vm.evaluate(`(multiple-value-bind (bytes literals) (emit-native-module (compile-to-hir '${source}))
      (declare (ignore literals))
      (with-open-file (out "/typed.wasm" :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`);
    const module = await WebAssembly.compile(await vm.readFile('/typed.wasm'));
    return WebAssembly.Module.imports(module).map(entry => entry.name);
  };
  assert.ok((await imports(`(lambda (x) (declare (type (unsigned-byte 40) x)) (list (+ x 1)))`)).includes('native-unbox-int64'));
  assert.ok((await imports(`(lambda (x) (declare (fixnum x)) (list (+ x 1)))`)).includes('native-add-int64-checked'));
  assert.ok(!(await imports(`(lambda (x) (declare (type (unsigned-byte 8) x) (notinline +)) (list (+ x 1)))`)).includes('native-add-int64'));
  // FIXNUM spans signed i64 in the target, but its arithmetic is unbounded.
  // Compare scalar entries with independent BigInt arithmetic at both limits,
  // including MIN-I64 * -1, zero divisors in the multiplication fit test,
  // overflow in both directions, and products that fit despite wide operands.
  const boundaryValues = [...new Set([0n, 1n, -1n, 2n, -2n,
    ...[30n, 31n, 32n, 62n, 63n].flatMap(bits => [-1n, 0n, 1n].flatMap(delta => {
      const value = (1n << bits) + delta;
      return [value, -value];
    }))])].filter(value => value >= -(1n << 63n) && value < (1n << 63n));
  for (const [operator, entry, operation] of [
    ['+', 'native-add-int64-checked', (a, b) => a + b],
    ['-', 'native-sub-int64-checked', (a, b) => a - b],
    ['*', 'native-mul-int64-checked', (a, b) => a * b],
  ]) {
    const source = `(lambda (a b) (declare (fixnum a b)) (list (${operator} a b)))`;
    assert.ok((await imports(source)).includes(entry), source);
    vm.evaluate(`(defparameter *typed-function* (compile-native '${source})) (values)`);
    const forms = [];
    for (const a of boundaryValues) for (const b of boundaryValues)
      forms.push(`(assert (equal (funcall *typed-function* ${a} ${b}) '(${operation(a, b)})))`);
    for (let start = 0; start < forms.length; start += 100)
      assert.equal(vm.evaluate(`(progn ${forms.slice(start, start + 100).join('\n')} (values))`).output, '');
    check(`(let ((original (symbol-function '${operator})))
      (unwind-protect
        (progn (setf (symbol-function '${operator}) (lambda (a b) (values (list b a) :extra)))
          (equal (funcall *typed-function* most-negative-fixnum most-positive-fixnum)
                 '((9223372036854775807 -9223372036854775808))))
        (setf (symbol-function '${operator}) original)))`);
    check(`(let ((f (compile-native '(lambda (a b) (declare (fixnum a b)) (${operator} a b))))
                 (original (symbol-function '${operator})))
      (unwind-protect
        (progn (setf (symbol-function '${operator}) (lambda (a b) (values b a)))
          (equal (multiple-value-list (funcall f 7 9)) '(9 7)))
        (setf (symbol-function '${operator}) original)))`);
    assert.ok(!(await imports(`(lambda (a b) (declare (fixnum a b) (notinline ${operator}))
      (list (${operator} a b)))`)).includes(entry));
    assert.ok(!(await imports(`(lambda (a b) (list (${operator} a b)))`)).includes(entry));
  }
  console.log(`PASS typed signed-i64 arithmetic and promotion (${3 * boundaryValues.length ** 2} BigInt checks)`);
  vm.evaluate(`(defun typed-result (x) (values x :extra))`);
  for (const source of [
    `(lambda (x) (declare (ftype (function (t) (values (unsigned-byte 40) keyword)) typed-result))
       (list (+ (typed-result x) 1)))`,
    `(lambda (x) (flet ((helper (y) (values y :extra)))
       (declare (notinline helper) (ftype (function (t) (values (unsigned-byte 40) keyword)) helper))
       (list (+ (helper x) 1))))`,
  ]) {
    assert.ok((await imports(source)).includes('native-unbox-int64'), source);
    vm.evaluate(`(defparameter *typed-function* (compile-native '${source}))`);
    assert.equal(vm.evaluate('(funcall *typed-function* 1099511627775)').output, '(1099511627776)\n');
  }
  check(`(let ((f (compile-native '(lambda ()
    (declare (ftype (function () (unsigned-byte 8)) typed-result))
    (flet ((typed-result () 36893488147419103232)) (list (+ (typed-result) 1)))))))
    (equal (funcall f) '(36893488147419103233)))`);
  const vectorSource = `(lambda (vector index)
    (declare (type simple-vector vector) (type (signed-byte 30) index)) (list (svref vector index)))`;
  assert.ok((await imports(vectorSource)).includes('native-vector-ref'));
  vm.evaluate(`(defparameter *typed-vector-ref* (compile-native '${vectorSource}))`);
  check(`(let* ((value (list :shared)) (vector (vector value)))
    (and (eq (first (funcall *typed-vector-ref* vector 0)) value)
      (handler-case (progn (funcall *typed-vector-ref* vector -1) nil)
        (type-error (c) (= (type-error-datum c) -1)))
      (handler-case (progn (funcall *typed-vector-ref* vector 1) nil)
        (type-error (c) (= (type-error-datum c) 1)))))`);
  check(`(let ((saved (symbol-function 'svref)))
    (unwind-protect
      (progn (setf (symbol-function 'svref) (lambda (v i) (declare (ignore v i)) (values :replacement :extra)))
        (equal (funcall *typed-vector-ref* #(42) 0) '(:replacement)))
      (setf (symbol-function 'svref) saved)))`);
  console.log('Compiler types: lexical scope, shadowing, mutation, captures, special bindings, integer overflow, multiple values, and native arithmetic selection pass.');
} finally {vm.close();}
