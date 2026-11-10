import assert from 'node:assert/strict';
import fs from 'node:fs';
import {Miricl} from '../scripts/miricl.mjs';

const binary = fs.readFileSync(process.argv[2] ?? 'miricl-interpreter.wasm');
const fullBinary = fs.readFileSync(process.argv[3] ?? 'miricl.wasm');
assert.ok(binary.length < fullBinary.length, 'The compiler-free image must be smaller');
assert.equal(WebAssembly.Module.imports(new WebAssembly.Module(binary)).length, 0);
const vm = await Miricl.instantiate(binary);
const check = (source, instance = vm) =>
  assert.equal(instance.evaluate(source, {eof: true}).output, 'T\n', source);
const noCompiler = `(and (not (fboundp 'miricl-compiler::ensure-compiler))
  (not (fboundp 'miricl-compiler:compile-native))
  (not (fboundp 'miricl-compiler:compile-to-hir))
  (not (fboundp 'miricl-compiler:emit-native-module))
  (not (fboundp 'miricl::%compile-closure))
  (not (find-class 'miricl-compiler::hir-constant nil)))`;
try {
  check(noCompiler);
  check(`(let* ((x 40) (fn (lambda () (incf x))) (values (multiple-value-list (compile nil fn))))
    (and (equal values (list fn nil nil)) (not (compiled-function-p fn))
      (= (funcall (first values)) 41) (= (funcall fn) 42)))`);
  check(`(progn (defun nop-named (x &optional (y (+ x 2)) &key (z 3)) (+ x y z))
    (let ((fn #'nop-named))
      (and (equal (multiple-value-list (compile 'nop-named)) '(nop-named nil nil))
        (eq fn #'nop-named) (= (nop-named 1 2 :z 39) 42))))`);
  check(`(let ((fn (lambda (value object) (setf (car object) value))))
    (and (equal (multiple-value-list (compile '(setf nop-place) fn)) '((setf nop-place) nil nil))
      (eq fn (fdefinition '(setf nop-place)))
      (let ((object (list 1))) (setf (nop-place object) 42) (= (car object) 42))))`);
  check(`(progn (defmacro nop-macro (x) (list '+ x 1))
    (let ((fn (macro-function 'nop-macro)))
      (and (eq (compile 'nop-macro) 'nop-macro) (eq fn (macro-function 'nop-macro))
        (= (nop-macro 41) 42))))`);
  check(`(let ((fn (compile nil '(lambda (x) (nop-macro x)))))
    (defmacro nop-macro (x) (list '+ x 2))
    (= (funcall fn 40) 42))`);
  check(`(handler-case (progn (compile nil 42) nil) (type-error () t))`);
  check(`(progn (setq miricl-compiler:*compile-evaluations* t)
    (eval '(defun nop-eval () (values 41 42)))
    (and (not (null (function-lambda-expression #'nop-eval)))
      (equal (multiple-value-list (eval '(nop-eval))) '(41 42))))`);
  check(`(progn (defclass nop-object () ((value :initarg :value :accessor nop-value)))
    (defmethod nop-value :around ((object nop-object)) (+ 2 (call-next-method)))
    (= (nop-value (make-instance 'nop-object :value 40)) 42))`);
  check('(let ((x 42)) (equalp (list `(a ,x) `#(,x)) \'((a 42) #(42))))');

  const source = `#.(progn (incf *nop-reads*) '(defparameter *nop-loaded* "é𝄞"))
    (eval-when (:compile-toplevel) (incf *nop-compile-effects*))
    (eval-when (:load-toplevel :execute) (incf *nop-load-effects*))
    (defun nop-file-value () 42)`;
  vm.mountFile('/nop.lisp', source);
  check(`(progn (defparameter *nop-reads* 0) (defparameter *nop-compile-effects* 0)
    (defparameter *nop-load-effects* 0)
    (equal (multiple-value-list (compile-file "/nop.lisp" :verbose nil))
      (list (truename "/nop.wasm") nil nil)))`);
  assert.deepEqual(await vm.readFile('/nop.wasm'), new TextEncoder().encode(source));
  check(`(and (= *nop-reads* 0) (= *nop-compile-effects* 0) (= *nop-load-effects* 0)
    (not (fboundp 'nop-file-value)) (not (boundp '*nop-loaded*)))`);
  check(`(and (load "/nop.wasm" :verbose nil) (= *nop-reads* 1)
    (= *nop-load-effects* 1) (= *nop-compile-effects* 0) (string= *nop-loaded* "é𝄞")
    (= (nop-file-value) 42) (not (compiled-function-p #'nop-file-value)))`);
  check(`(and (equal (multiple-value-list
      (compile-file "/nop.lisp" :output-file "/copied.lisp" :verbose nil))
      (list (truename "/copied.lisp") nil nil))
    (equal (multiple-value-list (compile-file "/nop.lisp" :output-file "/nop.lisp" :verbose nil))
      (list (truename "/nop.lisp") nil nil)))`);
  assert.deepEqual(await vm.readFile('/copied.lisp'), new TextEncoder().encode(source));
  assert.deepEqual(await vm.readFile('/nop.lisp'), new TextEncoder().encode(source));
  check(`(progn (compile-file "/nop.lisp" :output-file "/sub/../nop.lisp" :verbose nil) t)`);
  assert.deepEqual(await vm.readFile('/nop.lisp'), new TextEncoder().encode(source));

  const full = await Miricl.instantiate(fullBinary);
  try {
    full.mountFile('/portable.lisp', `(defun from-full-compiler (x) (+ x 2))
      (defun from-full-library (n)
        (loop for i below n collect i))`);
    check(`(progn (compile-file "/portable.lisp" :verbose nil) t)`, full);
    vm.mountFile('/portable.wasm', await full.readFile('/portable.wasm'));
    check(`(and (load "/portable.wasm" :verbose nil) (= (from-full-compiler 40) 42))`);
    check(`(progn
      (miricl-compiler:compile-native-bundle "/portable.wasm" "/native-portable.wasm") t)`, full);
    vm.mountFile('/native-portable.wasm', await full.readFile('/native-portable.wasm'));
    check(`(and (load "/native-portable.wasm" :verbose nil)
      (compiled-function-p #'from-full-compiler) (= (from-full-compiler 40) 42)
      (equal (from-full-library 3) '(0 1 2))
      (eq (compile nil #'from-full-compiler) #'from-full-compiler))`);
    check(`(and (search "NATIVE-CALL" (with-output-to-string (*standard-output*)
        (disassemble #'from-full-compiler)))
      (search "INTERPRET" (with-output-to-string (*standard-output*)
        (disassemble #'nop-eval))) t)`);
    // Building the compiler-free variant must not alter the ordinary image.
    check(`(and (fboundp 'miricl-compiler:compile-native)
      (compiled-function-p (compile nil '(lambda () 42))))`, full);
  } finally { full.close(); }
  check(noCompiler);
  const restored = await Miricl.instantiate(vm.saveImage());
  try {
    check(noCompiler, restored);
    check(`(let ((fn #'nop-eval))
      (and (eq fn (compile nil fn)) (not (compiled-function-p fn))
        (= (nop-value (make-instance 'nop-object :value 40)) 42)
        (= (from-full-compiler 40) 42) (= (nop-file-value) 42)))`, restored);
  } finally { restored.close(); }
  if (process.argv[4]) {
    vm.mountFile('/asdf.lisp', fs.readFileSync(process.argv[4]));
    await vm.evaluateAsync('(load "/asdf.lisp" :verbose nil)');
    vm.mountFile('/app/app.asd', '(asdf:defsystem "app" :serial t :components ((:file "package") (:file "main")))');
    vm.mountFile('/app/package.lisp', '(defpackage :app (:use :cl) (:export :answer))');
    vm.mountFile('/app/main.lisp', '(in-package :app) (defun answer () 42)');
    await vm.evaluateAsync(`(progn
      (asdf:initialize-source-registry '(:source-registry (:tree "/app/") :ignore-inherited-configuration))
      (asdf:initialize-output-translations '(:output-translations (t "/compiled/") :ignore-inherited-configuration))
      (asdf:load-system "app"))`);
    check(`(and (= (app:answer) 42) (not (compiled-function-p #'app:answer)))`);
    check(noCompiler);
  }
} finally { vm.close(); }
console.log(`Interpreter-only: no-op compilation, source copies, CLOS, portable FASLs and saved images pass (${binary.length} bytes).`);
