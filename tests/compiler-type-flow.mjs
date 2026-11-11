import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  const compile = async source => {
    vm.evaluate(`(defparameter *flow-unit* (compile-to-hir '${source}))
      (multiple-value-bind (bytes literals) (emit-native-module *flow-unit*)
        (defparameter *flow-function* (compiler-primitive 17 bytes literals nil))
        (with-open-file (out "/flow.wasm" :direction :output :if-exists :supersede
                             :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`);
    const module = await WebAssembly.compile(await vm.readFile('/flow.wasm'));
    return WebAssembly.Module.imports(module).map(entry => entry.name);
  };
  for (const [source, args, expected, entry = 'native-add-int64'] of [
    [`(lambda (x) (declare (type (unsigned-byte 8) x) (inline +))
       (list (* (+ x 1) 2)))`, '255', '(512)\n', 'native-mul-int64'],
    [`(lambda (x) (declare (type (integer -4 7) x) (inline -))
       (list (* (- x 7) 2)))`, '-4', '(-22)\n', 'native-mul-int64'],
    [`(lambda (x) (declare (type (signed-byte 16) x) (inline *))
       (list (+ (* x 3) 1)))`, '-32768', '(-98303)\n'],
    [`(lambda (v) (declare (type simple-vector v) (inline length)) (list (+ (length v) 1)))`, '#(a b)', '(3)\n'],
    [`(lambda (x) (declare (fixnum x) (inline logand)) (list (+ (logand x 127) 1)))`, '-1', '(128)\n'],
    [`(lambda (x) (declare (inline logand)) (list (+ (logand x 127) 1)))`, '1208925819614629174706303', '(128)\n'],
    [`(lambda (x) (declare (inline logand)) (list (+ (logand 127 x) 1)))`, '-1208925819614629174706177', '(128)\n'],
    [`(lambda (x) (declare (inline logior)) (list (+ (logior x -128) 1)))`, '1208925819614629174706176', '(-127)\n'],
    [`(lambda (x) (declare (inline logior)) (list (+ (logior -128 x) 1)))`, '-1208925819614629174706177', '(0)\n'],
    [`(lambda (x) (declare (type (unsigned-byte 8) x) (inline logior)) (list (+ (logior x 128) 1)))`, '127', '(256)\n'],
    [`(lambda (x) (declare (type (unsigned-byte 8) x) (inline logxor)) (list (+ (logxor x 255) 1)))`, '255', '(1)\n'],
    [`(lambda (x) (declare (type (unsigned-byte 32) x) (inline ash)) (list (+ (ash x -7) 1)))`, '4294967295', '(33554432)\n'],
  ]) {
    const imports = await compile(source);
    assert.ok(imports.includes(entry), source);
    assert.equal(vm.evaluate(`(funcall *flow-function* ${args})`).output, expected, source);
  }
  await compile(`(lambda (x) (declare (fixnum x) (inline +)) (list (+ (+ x 1) 1)))`);
  check(`(equal (funcall *flow-function* 9223372036854775807) '(9223372036854775809))`);
  // Repeated interval growth must terminate without asserting a false bound.
  await compile(`(lambda (n) (declare (inline +))
    (let ((x 0)) (dotimes (i n) (setq x (+ x 1))) (list (+ x 1))))`);
  check(`(and (equal (funcall *flow-function* 0) '(1))
    (equal (funcall *flow-function* 100) '(101)))`);
  assert.ok((await compile(`(lambda (n) (declare (inline +) (type (unsigned-byte 8) n))
    (let ((x 0)) (declare (type (unsigned-byte 16) x))
      (dotimes (i n) (setq x (+ x 1))) (list (+ x 1))))`)).includes('native-add-int64'));
  check(`(equal (funcall *flow-function* 255) '(256))`);
  // Result range inference must keep operand errors and evaluation effects.
  await compile(`(lambda (x) (declare (inline logand))
    (list (+ (logand x 0) 1)))`);
  check(`(handler-case (progn (funcall *flow-function* :bad) nil) (type-error () t))`);
  for (const [source, args, expected] of [
    [`(lambda (flag) (list (+ (if flag 1 2) 3)))`, 't', '(4)\n'],
    [`(lambda () (let ((x 7)) (list (+ x 1))))`, '', '(8)\n'],
    [`(lambda (flag) (let ((x 1)) (when flag (setq x 2)) (list (+ x 3))))`, 't', '(5)\n'],
    [`(lambda (flag) (flet ((helper (x) (if x 4 8))) (declare (notinline helper))
       (list (+ (helper flag) 1))))`, 'nil', '(9)\n'],
  ]) {
    assert.ok((await compile(source)).includes('native-add-int64'), source);
    assert.equal(vm.evaluate(`(funcall *flow-function* ${args})`).output, expected, source);
  }
  check(`(let* ((unit (compile-to-hir '(lambda (flag) (if flag 4 8))))
                (facts (infer-hir-types unit)))
    (equal (gethash (first (hir-unit-functions unit)) facts) '(integer 4 8)))`);
  assert.ok((await compile(`(lambda (n) (declare (type (unsigned-byte 8) n))
    (flet ((helper (x) (list (+ x 1)))) (declare (notinline helper)) (helper n)))`)).includes('native-add-int64'));
  assert.equal(vm.evaluate('(funcall *flow-function* 255)').output, '(256)\n');
  assert.ok(!(await compile(`(lambda (n) (declare (type (unsigned-byte 8) n))
    (flet ((helper (x) (list (+ x 1)))) (values (helper n) #'helper)))`)).includes('native-add-int64'));
  check(`(multiple-value-bind (value helper) (funcall *flow-function* 7)
    (and (equal value '(8)) (equal (funcall helper 1208925819614629174706176) '(1208925819614629174706177))))`);
  for (const [source, args, expected] of [
    [`(lambda (x) (let ((y 1)) (setq y x) (list (+ y 1))))`, '1208925819614629174706176', '(1208925819614629174706177)\n'],
    [`(lambda () (let ((x 1))
       (funcall (lambda () (setq x 1208925819614629174706176))) (list (+ x 1))))`, '', '(1208925819614629174706177)\n'],
    [`(lambda (flag) (let ((x (block done
       (when flag (funcall (lambda () (return-from done 1208925819614629174706176)))) 1)))
       (list (+ x 1))))`, 't', '(1208925819614629174706177)\n'],
  ]) {
    assert.ok(!(await compile(source)).includes('native-add-int64'), source);
    assert.equal(vm.evaluate(`(funcall *flow-function* ${args})`).output, expected, source);
  }
  // A changed graph must produce fresh facts; the original assertion slots
  // remain untouched, so inferred constants cannot survive an input rewrite.
  await compile('(lambda () (let ((x 7)) (list (+ x 1))))');
  check(`(progn
    (dolist (value (hir-function-values (first (hir-unit-functions *flow-unit*))))
      (let ((producer (hir-value-producer value)))
        (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant)
                   (eql (first (hir-instruction-attributes producer)) 7))
          (assert (eq (hir-value-type value) t))
          (setf (hir-instruction-attributes producer) '(1208925819614629174706176)))))
    (multiple-value-bind (bytes literals) (emit-native-module *flow-unit*)
      (let ((new (compiler-primitive 17 bytes literals nil)))
        (and (equal (funcall *flow-function*) '(8))
             (equal (funcall new) '(1208925819614629174706177))))))`);
  check(`(let ((f (compile-native '(lambda (flag)
    (if flag (values) (values 42 :second))))))
    (and (null (multiple-value-list (funcall f t)))
         (equal (multiple-value-list (funcall f nil)) '(42 :second))))`);
  console.log('Compiler type flow: constant ranges, assignments, joins, known local returns, captured mutation, nonlocal exits, fresh facts after HIR rewrites, and multiple-value counts pass.');
} finally {vm.close();}
