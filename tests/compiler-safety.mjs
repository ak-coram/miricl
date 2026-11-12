import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  const compile = async source => {
    vm.evaluate(`(defparameter *safety-unit* (compile-to-hir '${source}))
      (multiple-value-bind (bytes literals) (emit-native-module *safety-unit*)
        (defparameter *safety-function* (compiler-primitive 17 bytes literals nil))
        (with-open-file (out "/safety.wasm" :direction :output :if-exists :supersede
                             :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`);
    return WebAssembly.Module.imports(await WebAssembly.compile(await vm.readFile('/safety.wasm')))
      .map(entry => entry.name);
  };
  const vector = policy => `(lambda (v i) (declare ${policy} (type simple-vector v) (type (signed-byte 30) i))
    (list (svref v i)))`;
  check('(= (compiler-primitive 36 nil) 1)');
  assert.ok((await compile(vector(''))).includes('native-vector-ref'));
  check(`(and (equal (funcall *safety-function* #(42) 0) '(42))
    (handler-case (progn (funcall *safety-function* #(42) 1) nil)
      (type-error (condition) (= (type-error-datum condition) 1))))`);
  for (const safety of [0, 1, 2, 3]) {
    const imports = await compile(vector(`(optimize (safety ${safety}))`));
    assert.ok(imports.includes(safety === 0 ? 'native-vector-ref-trusted' : 'native-vector-ref'));
    check(`(and (equal (funcall *safety-function* #(42) 0) '(42))
      (every (lambda (function)
        (every (lambda (block)
          (every (lambda (instruction) (= (hir-instruction-safety-level instruction) ${safety}))
            (hir-block-instructions block))) (hir-function-blocks function)))
        (hir-unit-functions *safety-unit*)))`);
  }
  // INLINE permits capturing a definition; it does not lower SAFETY or make
  // an out-of-bounds vector access eligible for an unchecked Wasm operation.
  for (const safety of [1, 2, 3]) {
    const read = await compile(`(lambda (v i)
      (declare (inline svref) (optimize (safety ${safety}))
               (type simple-vector v) (type (signed-byte 30) i)) (list (svref v i)))`);
    assert.ok(!read.includes('native-vector-ref-inline'));
    check(`(and (equal (funcall *safety-function* #(42) 0) '(42))
      (every (lambda (i) (handler-case (progn (funcall *safety-function* #(42) i) nil)
        (type-error (condition) (= (type-error-datum condition) i)))) '(-1 1))
      (equal (funcall *safety-function* #(43) 0) '(43)))`);
    const write = await compile(`(lambda (v i value)
      (declare (inline (setf svref)) (optimize (safety ${safety}))
               (type simple-vector v) (type (signed-byte 30) i)) (list (setf (svref v i) value)))`);
    assert.ok(!write.includes('native-vector-set-inline'));
    check(`(let ((v (vector 17)))
      (and (equal (funcall *safety-function* v 0 42) '(42))
        (every (lambda (i) (handler-case (progn (funcall *safety-function* v i 99) nil)
          (type-error (condition) (= (type-error-datum condition) i)))) '(-1 1))
        (= (svref v 0) 42)))`);
  }
  for (const source of [
    `(lambda (v i) (declare (type simple-vector v) (type (signed-byte 30) i))
       (list (locally (declare (optimize (safety 0))) (svref v i)) (svref v i)))`,
    `(lambda (v i) (declare (optimize (safety 0)) (type simple-vector v) (type (signed-byte 30) i))
       (list (locally (declare (optimize (safety 1))) (svref v i)) (svref v i)))`,
    `(lambda (v i) (declare (type simple-vector v) (type (signed-byte 30) i))
       (let ((first (svref v i))) (declare (optimize (safety 0))) (list first (svref v i))))`,
  ]) {
    const imports = await compile(source);
    assert.ok(imports.includes('native-vector-ref') && imports.includes('native-vector-ref-trusted'), source);
    check('(equal (funcall *safety-function* #(42) 0) \'(42 42))');
  }
  // Inlining must use the callee's definition policy, including a locally
  // declared override. The call site's unsafe policy cannot infect its body.
  for (const safety of [0, 1]) {
    const source = `(lambda (v i)
      (flet ((helper (v i) (declare (optimize (safety ${safety}))
                                  (type simple-vector v) (type (signed-byte 30) i)) (list (svref v i))))
        (declare (inline helper))
        (locally (declare (optimize (safety ${1 - safety}))) (helper v i))))`;
    const imports = await compile(source);
    assert.ok(imports.includes(safety ? 'native-vector-ref' : 'native-vector-ref-trusted'));
    assert.ok(!imports.includes(safety ? 'native-vector-ref-trusted' : 'native-vector-ref'));
    check('(equal (funcall *safety-function* #(42) 0) \'(42))');
  }
  for (const operator of ['flet', 'labels']) for (const safety of [0, 1]) {
    const source = `(lambda (v i) (declare (optimize (safety ${safety})))
      (${operator} ((helper (v i) (declare (type simple-vector v) (type (signed-byte 30) i)) (list (svref v i))))
        (declare (inline helper) (optimize (safety ${1 - safety}))) (helper v i)))`;
    const imports = await compile(source);
    assert.ok(imports.includes(safety ? 'native-vector-ref' : 'native-vector-ref-trusted'));
    assert.ok(!imports.includes(safety ? 'native-vector-ref-trusted' : 'native-vector-ref'));
    check('(equal (funcall *safety-function* #(42) 0) \'(42))');
  }
  await compile(`(lambda () (locally (declare (optimize (safety 0)))
    (lambda (v i) (declare (type simple-vector v) (type (signed-byte 30) i)) (list (svref v i)))))`);
  check('(equal (funcall (funcall *safety-function*) #(42) 0) \'(42))');
  // A signed i32 index may be boxed outside i31's range. Trusted access uses
  // scalar unboxing rather than incorrectly casting that value to i31.
  assert.ok((await compile(`(lambda (v i) (declare (optimize (safety 0))
    (type simple-vector v) (type (signed-byte 32) i)) (list (svref v i)))`)).includes('native-unbox-int64'));
  check('(equal (funcall *safety-function* #(42) 0) \'(42))');
  check(`(let ((original (symbol-function 'svref)))
    (unwind-protect
      (progn (setf (symbol-function 'svref) (lambda (v i) (declare (ignore v)) i))
        (equal (funcall *safety-function* #(42) 1073741824) '(1073741824)))
      (setf (symbol-function 'svref) original)))`);
  assert.ok((await compile(`(lambda (v i) (declare (optimize (safety 0))
    (type simple-vector v) (fixnum i)) (list (svref v i)))`)).includes('native-vector-ref-trusted64'));
  check(`(let ((original (symbol-function 'svref)))
    (unwind-protect
      (progn (setf (symbol-function 'svref) (lambda (v i) (declare (ignore v)) i))
        (equal (funcall *safety-function* #(42) 1099511627776) '(1099511627776)))
      (setf (symbol-function 'svref) original)))`);
  for (const [operator, entry, a, b, expected] of [
    ['+', 'native-add-int64', 'most-positive-fixnum', '0', 'most-positive-fixnum'],
    ['-', 'native-sub-int64', 'most-negative-fixnum', '0', 'most-negative-fixnum'],
    ['*', 'native-mul-int64', 'most-negative-fixnum', '1', 'most-negative-fixnum'],
  ]) {
    for (const safety of [0, 1]) {
      const source = `(lambda (a b) (declare (optimize (safety ${safety})) (fixnum a b))
        (list (the fixnum (${operator} a b))))`;
      const imports = await compile(source);
      assert.ok(imports.includes(entry + (safety ? '-checked' : '')), source);
      check(`(equal (funcall *safety-function* ${a} ${b}) (list ${expected}))`);
    }
  }
  await compile(`(lambda (a b) (declare (optimize (safety 0)) (fixnum a b)) (list (+ a b)))`);
  check(`(equal (funcall *safety-function* most-positive-fixnum 1) '(9223372036854775808))`);
  for (const [operator, entry, expected] of [['car', 'native-car-list', '(42)'], ['cdr', 'native-cdr-list', '(:tail)']]) {
    assert.ok((await compile(`(lambda (x) (declare (optimize (safety 0)) (type list x))
      (list (${operator} x)))`)).includes(entry));
    check(`(and (equal (funcall *safety-function* '(42 . :tail)) '${expected})
                (equal (funcall *safety-function* nil) '(nil)))`);
    check(`(let ((original (symbol-function '${operator})))
      (unwind-protect
        (progn (setf (symbol-function '${operator}) (lambda (x) (declare (ignore x)) (values :replacement :extra)))
          (equal (funcall *safety-function* '(42)) '(:replacement)))
        (setf (symbol-function '${operator}) original)))`);
    assert.ok(!(await compile(`(lambda (x) (declare (optimize (safety 0)) (type list x) (notinline ${operator}))
      (list (${operator} x)))`)).includes(entry));
    assert.ok(!(await compile(`(lambda (x) (declare (type list x)) (list (${operator} x)))`)).includes(entry));
    assert.ok(!(await compile(`(lambda (x) (declare (optimize (safety 0))) (list (${operator} x)))`)).includes(entry));
  }
  vm.evaluate('(declaim (optimize (safety 0))) (values)');
  assert.ok((await compile(vector(''))).includes('native-vector-ref-trusted'));
  vm.evaluate('(declaim (optimize (safety 1))) (values)');
  assert.ok((await compile(vector(''))).includes('native-vector-ref'));
  check(`(and (= (compiler-primitive 36 nil) 1)
    (let ((unit (compile-to-hir (compiler-source-policy '(lambda (x) (list x))))))
      (every (lambda (function)
        (every (lambda (block)
          (every (lambda (instruction) (zerop (hir-instruction-safety-level instruction)))
            (hir-block-instructions block))) (hir-function-blocks function)))
        (hir-unit-functions unit))))`);
  for (const source of [
    `(lambda () (declare (optimize (safety 4))) 42)`,
    `(lambda () (locally (declare (optimize (safety -1))) 42))`,
  ]) check(`(handler-case (progn (compile-to-hir '${source}) nil) (program-error () t))`);
  vm.mountFile('/safety-file.lisp', `(in-package :cl-user)
    (defun safety-file-read (v i) (declare (optimize (safety 0) (debug 2))
      (type simple-vector v) (type (signed-byte 30) i)) (list (svref v i)))`);
  check(`(progn (compile-file "/safety-file.lisp") (load "/safety-file.wasm")
    (setf (symbol-function 'cl-user::safety-file-read)
      (compile-native (function-lambda-expression #'cl-user::safety-file-read)))
    (and (equal (cl-user::safety-file-read #(42) 0) '(42))
      (search "native-vector-ref-trusted"
        (with-output-to-string (*standard-output*) (disassemble #'cl-user::safety-file-read))) t))`);
  const fresh = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
  try {
    fresh.mountFile('/safety-file.wasm', await vm.readFile('/safety-file.wasm'));
    assert.equal(fresh.evaluate(`(progn (setq miricl-compiler:*compile-evaluations* nil)
      (load "/safety-file.wasm")
      (setf (symbol-function 'cl-user::safety-file-read)
        (miricl-compiler:compile-native (function-lambda-expression #'cl-user::safety-file-read)))
      (and (equal (cl-user::safety-file-read #(42) 0) '(42))
        (search "native-vector-ref-trusted"
          (with-output-to-string (*standard-output*) (disassemble #'cl-user::safety-file-read))) t))`).output, 'T\n');
  } finally {fresh.close();}
  console.log('Compiler safety: default 1, lexical/global policy, initialization scope, inline definition scope, trusted list/vector access, result assertions, integer promotion, redefinition, and compiler policy isolation pass.');
} finally {vm.close();}
