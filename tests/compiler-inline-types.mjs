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
  // definition already visible when compilation starts.
  check(`(let ((original (symbol-function 'svref)))
    (unwind-protect
      (progn (setf (symbol-function 'svref) (lambda (v i) (declare (ignore v i)) :replacement))
        (let ((f (compile-native '(lambda (v) (declare (optimize (safety 0))
          (type simple-vector v) (inline svref)) (svref v 0)))))
          (eq (funcall f #(42)) :replacement)))
      (setf (symbol-function 'svref) original)))`);
  await compile(`(lambda (v) (declare (optimize (safety 0)) (inline svref))
    (flet ((svref (v i) (declare (ignore v i)) :local)) (svref v 0)))`);
  check('(eq (funcall *inline-function* #(42)) :local)');
  await compile(`(lambda (v) (declare (optimize (safety 0)) (inline svref))
    (let ((count 0))
      (values (svref (progn (incf count) (the simple-vector v)) (progn (incf count) 0)) count)))`);
  check(`(equal (multiple-value-list (funcall *inline-function* #(42))) '(42 2))`);
  await compile(`(lambda (v) (declare (type simple-vector v) (optimize (safety 0)) (inline svref))
    (values #'svref (svref v 0)))`);
  check(`(let ((original (symbol-function 'svref)))
    (unwind-protect
      (progn (setf (symbol-function 'svref) (lambda (v i) (declare (ignore v i)) :replacement))
        (multiple-value-bind (function answer) (funcall *inline-function* #(42))
          (and (eq function (symbol-function 'svref)) (= answer 42))))
      (setf (symbol-function 'svref) original)))`);
  await compile(`(lambda (v) (declare (type simple-vector v) (optimize (safety 0)) (inline svref)) (svref v 0))`);
  check(`(progn
    (dolist (function (hir-unit-functions *inline-unit*))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (hir-block-instructions block))
          (when (eq (hir-instruction-opcode instruction) :function-reference)
            (setf (hir-instruction-attributes instruction) '(svref notinline))))))
    (let ((original (symbol-function 'svref)))
      (unwind-protect
        (progn (setf (symbol-function 'svref) (lambda (v i) (declare (ignore v i)) :replacement))
          (multiple-value-bind (bytes literals) (emit-native-module *inline-unit*)
            (let ((new (compiler-primitive 17 bytes literals nil)))
              (and (= (funcall *inline-function* #(42)) 42) (eq (funcall new #(42)) :replacement)))))
        (setf (symbol-function 'svref) original))))`);
  console.log('Inline types: canonical definitions, no dynamic lookup, narrow/wide indices, result counts, operand effects, bound/free declaration scope, local shadowing, NOTINLINE, first-class references and fresh emission facts pass.');
} finally {vm.close();}
