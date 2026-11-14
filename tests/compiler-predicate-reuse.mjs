import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const cases = [
  ['integer comparison', `(lambda (x y) (declare (fixnum x y) (inline <) (optimize (safety 0)))
    (values (< x y) (< x y)))`, [['1 2', '(T T)'], ['2 1', '(NIL NIL)']], true],
  ['literal identity', `(lambda (x) (declare (inline eq)) (values (eq x :marker) (eq x :marker)))`,
    [[':marker', '(T T)'], [':other', '(NIL NIL)']], true],
  ['stable tag across mutation', `(lambda (x) (declare (inline consp))
    (let ((a (consp x))) (rplaca x 2) (values a (consp x) (car x))))`,
    [['(list 1)', '(T T 2)']], false],
  ['extended result lifetime', `(lambda (x y z)
    (declare (fixnum x y) (inline < consp) (optimize (safety 0)))
    (let ((a (< x y))) (identity a)
      (let ((different (consp z))) (values different (< x y)))))`,
    [['2 1 (list 1)', '(T NIL)'], ['1 2 nil', '(NIL T)']], true],
  ['operand effects', `(lambda (x) (declare (fixnum x) (inline zerop) (optimize (safety 0)))
    (let ((count 0)) (let ((a (zerop (progn (incf count) x))))
      (values a (zerop (progn (incf count) x)) count))))`,
    [['0', '(T T 2)'], ['1', '(NIL NIL 2)']], true],
  ['nested predicates', `(lambda (x) (declare (inline not consp))
    (values (not (consp x)) (not (consp x))))`, [['nil', '(T T)'], ['(list 1)', '(NIL NIL)']], true],
  ['numeric EQL', `(lambda (x y) (declare (inline eql)) (values (eql x y) (eql x y)))`,
    [['1 1.0', '(NIL NIL)'], ['1.0 1.0', '(T T)'],
      ['1208925819614629174706177 1208925819614629174706177', '(T T)']], true],
  ['simple vector tag', `(lambda (x) (declare (inline simple-vector-p))
    (values (simple-vector-p x) (simple-vector-p x)))`, [['#(1 2)', '(T T)'], ['nil', '(NIL NIL)']], true],
  ['loop predicate values', `(lambda (x n)
    (declare (fixnum x) (type (unsigned-byte 32) n) (inline zerop eq + 1+ <) (optimize (safety 0)))
    (let ((sum 0)) (declare (type (unsigned-byte 32) sum))
      (dotimes (i n sum) (declare (type (unsigned-byte 32) i))
        (let ((a (zerop x)) (b (zerop x))) (incf sum (if (eq a b) 1 2))))))`,
    [['0 0', '(0)'], ['0 5', '(5)'], ['1 5', '(5)']], null],
  ['captured predicate values', `(lambda (x) (declare (inline consp))
    (let ((a (consp x)) (b (consp x))) (funcall (lambda () (values a b)))))`,
    [['nil', '(NIL NIL)'], ['(list 1)', '(T T)']], true],
  ['full value groups', `(lambda (x) (declare (inline consp))
    (multiple-value-call #'list (consp x) (consp x)))`,
    [['nil', '((NIL NIL))'], ['(list 1)', '((T T))']], false],
  ['mutable cons contents', `(lambda (x) (declare (list x) (inline car) (optimize (safety 0)))
    (let ((a (car x))) (rplaca x 2) (values a (car x))))`, [['(list 1)', '(1 2)']], false],
  ['mutable hash contents', `(lambda (key table) (declare (inline gethash (setf gethash)))
    (let ((a (gethash key table))) (setf (gethash key table) 2) (values a (gethash key table))))`,
    [[`:key (let ((h (make-hash-table))) (setf (gethash :key h) 1) h)`, '(1 2)']], false],
  ['ordinary effects', `(lambda (x) (setq *reuse-effects* 0)
    (values (reuse-effect x) (reuse-effect x) *reuse-effects*))`, [[':x', '(1 2 2)']], false],
  ['lexical shadowing', `(lambda (x) (let ((calls 0))
    (flet ((consp (z) (declare (ignore z)) (incf calls))) (declare (inline consp))
      (values (consp x) (consp x) calls))))`, [['nil', '(1 2 2)']], false],
  ['checked integer predicate', `(lambda (x) (declare (fixnum x) (inline zerop) (optimize (safety 1)))
    (values (zerop x) (zerop x)))`, [['0', '(T T)'], ['-1', '(NIL NIL)']], false],
  ['debug bindings', `(lambda (x) (declare (inline consp) (optimize (debug 2)))
    (values (consp x) (consp x)))`, [['nil', '(NIL NIL)'], ['(list 1)', '(T T)']], false],
  ['distinct literal identities', `(lambda () (declare (inline eq))
    (let ((a '(marker)) (b '(marker))) (values (eq a a) (eq a b))))`, [['', '(T NIL)']], false],
  ['cyclic literal identities', `(lambda () (declare (inline eq))
    (let ((a '#1=(marker . #1#)) (b '#2=(marker . #2#))) (values (eq a a) (eq a b))))`,
    [['', '(T NIL)']], false],
];
try {
  vm.evaluate(`(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler)
    (defparameter *reuse-effects* 0)
    (defun reuse-effect (x) (incf *reuse-effects*) (values *reuse-effects* x)) (values)`);
  for (const [name, source, calls] of cases) {
    let moduleSize;
    {
      vm.evaluate(`(progn
        (multiple-value-bind (bytes literals) (emit-native-module (compile-to-hir '${source}))
          (defparameter *reuse-function* (compiler-primitive 17 bytes literals nil))
          (with-open-file (out "/reuse.wasm" :direction :output :if-exists :supersede
              :element-type '(unsigned-byte 8)) (write-sequence bytes out)))) (values)`);
      const bytes = await vm.readFile('/reuse.wasm');
      assert.ok(WebAssembly.validate(bytes), name);
      moduleSize = bytes.length;
      for (const [args, expected] of calls)
        assert.equal(vm.evaluate(`(multiple-value-list (funcall *reuse-function* ${args}))`).output,
          `${expected}\n`, `${name}, reuse standard`);
    }
    console.log(`Predicate reuse ${name}: ${moduleSize} bytes`);
  }
  console.log('Predicate reuse: stable canonical results, live ranges, effects, mutation exclusions, shadowing, checked operands and cyclic literal identity pass.');
} finally {vm.close();}
