import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
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
    const source = `(lambda (v x) (declare (type simple-vector v) (optimize (safety 0))
      (inline (setf ${operator}))) (setf (${operator} v 0) x))`;
    const result = await compile(source);
    assert.ok(result.imports.includes('native-vector-set-inline'), operator);
    assert.ok(!result.imports.includes('native-function-reference'), operator);
    check(`(let* ((v (vector nil)) (alias v) (item (list 42))
                  (returned (multiple-value-list (funcall *inline-function* v item))))
      (and (eq v alias) (= (length returned) 1) (eq (car returned) item)
           (eq (svref alias 0) item)
           (equal (multiple-value-list (funcall *inline-function* v nil)) '(nil))
           (null (svref alias 0))))`);
    check(`(let ((original (fdefinition '(setf ${operator}))) (v (vector nil)))
      (unwind-protect
        (progn (setf (fdefinition '(setf ${operator}))
                 (lambda (x v i) (declare (ignore x v i)) :replacement))
          (and (= (funcall *inline-function* v 42) 42) (= (svref v 0) 42)))
        (setf (fdefinition '(setf ${operator})) original)))`);
    if (operator === 'aref') {
      // The emitter itself updates adjustable byte buffers through AREF.
      // Isolate the canonical-definition planner while that setter is replaced.
      check(`(let ((*wasm-inferred-types* (infer-hir-types *inline-unit* :roots (native-type-roots *inline-unit*)))
                   (*wasm-builtin-codes* (make-hash-table :test 'eq))
                   (*wasm-inline-vector-calls* (make-hash-table :test 'eq))
                   (*wasm-inline-references* (make-hash-table :test 'eq))
                   (original (fdefinition '(setf aref))))
        (unwind-protect
          (progn (setf (fdefinition '(setf aref)) (lambda (x v i) (declare (ignore x v i)) :replacement))
            (native-plan-inline-vectors *inline-unit*) (zerop (hash-table-count *wasm-inline-vector-calls*)))
          (setf (fdefinition '(setf aref)) original)))`);
    } else check(`(let ((original (fdefinition '(setf ${operator}))))
      (unwind-protect
        (progn (setf (fdefinition '(setf ${operator}))
                 (lambda (x v i) (declare (ignore x v i)) :replacement))
          (let ((f (compile-native '${source})) (v (vector nil)))
            (and (eq (funcall f v 42) :replacement) (null (svref v 0)))))
        (setf (fdefinition '(setf ${operator})) original)))`);
    console.log(`PASS inline typed (setf ${operator}): ${result.bytes} bytes, shared mutation and single value`);
  }
  for (const type of ['(signed-byte 30)', '(signed-byte 32)', 'fixnum']) {
    const result = await compile(`(lambda (v i x) (declare (optimize (safety 0))
      (inline (setf svref)) (type simple-vector v) (type ${type} i)) (setf (svref v i) x))`);
    assert.ok(result.imports.includes('native-vector-set-inline'), type);
    check('(let ((v (vector 0 0))) (and (= (funcall *inline-function* v 1 42) 42) (= (svref v 1) 42)))');
  }
  for (const source of [
    `(lambda (v x) (declare (optimize (safety 0)) (inline (setf svref))) (setf (svref v 0) x))`,
    `(lambda (v x) (declare (type simple-vector v) (optimize (safety 1)) (inline (setf svref)))
       (setf (svref v 0) x))`,
    `(lambda (v x) (declare (type simple-vector v) (optimize (safety 0)) (inline (setf svref)))
       (locally (declare (notinline (setf svref))) (setf (svref v 0) x)))`,
  ]) {
    assert.ok(!(await compile(source)).imports.includes('native-vector-set-inline'), source);
    check('(let ((v (vector nil))) (and (= (funcall *inline-function* v 42) 42) (= (svref v 0) 42)))');
  }
  await compile(`(lambda (v) (declare (type simple-vector v) (optimize (safety 0)) (inline (setf svref)))
    (let ((seen nil))
      (values (setf (svref (progn (push 1 seen) v) (progn (push 2 seen) 0))
                   (progn (push 3 seen) (values 42 99))) seen)))`);
  check("(equal (multiple-value-list (funcall *inline-function* (vector nil))) '(42 (3 2 1)))");
  const local = await compile(`(lambda (v x) (declare (optimize (safety 0)) (inline (setf svref)))
    (flet (((setf svref) (x v i) (declare (ignore x v i)) :local))
      (funcall #'(setf svref) x v 0)))`);
  assert.ok(!local.imports.includes('native-vector-set-inline'));
  check('(let ((v (vector nil))) (and (eq (funcall *inline-function* v 42) :local) (null (svref v 0))))');
  await compile(`(lambda (v x) (declare (type simple-vector v) (optimize (safety 0)) (inline (setf svref)))
    (values #'(setf svref) (setf (svref v 0) x)))`);
  check(`(let ((original (fdefinition '(setf svref))) (v (vector nil)))
    (unwind-protect
      (progn (setf (fdefinition '(setf svref)) (lambda (x v i) (declare (ignore x v i)) :replacement))
        (multiple-value-bind (function answer) (funcall *inline-function* v 42)
          (and (eq function (fdefinition '(setf svref))) (= answer 42) (= (svref v 0) 42))))
      (setf (fdefinition '(setf svref)) original)))`);
  console.log('Inline vector writes: canonical setters, shared mutation, one returned value, wide indices, declaration controls and operand order pass.');
} finally {vm.close();}
