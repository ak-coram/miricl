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
  (fboundp 'miricl-compiler:compile-to-hir)
  (not (fboundp 'miricl-compiler::optimize-hir-checked))
  (not (fboundp 'miricl-compiler::hir-integer-bounds))
  (not (fboundp 'miricl-compiler::hir-code-to-unit))
  (not (fboundp 'miricl-compiler:emit-native-module))
  (fboundp (miricl-compiler::compiler-primitive 10 1 "%COMPILE-CLOSURE"))
  (not (find-class 'miricl-compiler::hir-constant nil)))`;
try {
  check(noCompiler);
  // COMPILE still lowers declared SAFETY 0 functions to HIR without the
  // optional native compiler's type-proof helpers.
  for (const debug of [0, 2]) check(`(let ((fn (compile nil '(lambda (n)
      (declare (type (integer 0 1000) n) (optimize (safety 0) (debug ${debug})))
      (values (1+ n) n)))))
    (and (compiled-function-p fn) (equal (multiple-value-list (funcall fn 7)) '(8 7))))`);
  check(`(let ((fn (compile nil '(lambda (x)
      (declare (type single-float x) (optimize (safety 0) (debug 0))) (+ 0.0 x)))))
    (and (compiled-function-p fn) (= (funcall fn -0.0) 0.0)))`);
  check(`(let* ((x 40) (fn (lambda () (incf x))) (values (multiple-value-list (compile nil fn))))
    (and (= (length values) 3) (not (second values)) (not (third values))
      (compiled-function-p (first values)) (not (compiled-function-p fn))
      (= (funcall (first values)) 41) (= (funcall fn) 42)))`);
  check(`(progn (defun nop-named (x &optional (y (+ x 2)) &key (z 3)) (+ x y z))
    (let ((fn #'nop-named))
      (and (equal (multiple-value-list (compile 'nop-named)) '(nop-named nil nil))
        (compiled-function-p #'nop-named) (= (nop-named 1 2 :z 39) 42))))`);
  check(`(let ((text (with-output-to-string (*standard-output*) (disassemble 'nop-named))))
    (and (search "HIR function NOP-NAMED" text) (search "Packed HIR code:" text)
         (search "entry B0" text) (search "return" text) t))`);
  check(`(progn
    (defparameter *minimal-capture* (funcall (compile nil '(lambda (x) (lambda () (incf x)))) 40))
    (let ((text (with-output-to-string (*standard-output*) (disassemble *minimal-capture*))))
      (and (compiled-function-p *minimal-capture*) (search "Capture 0: X" text)
           (search "capture 0 [X]" text) (search "cell-write" text)
           (= (funcall *minimal-capture*) 41))))`);
  check(`(progn (defun nop-uncompiled (x) (+ x 2))
    (let* ((original #'nop-uncompiled)
           (text (with-output-to-string (*standard-output*) (disassemble 'nop-uncompiled))))
      (and (search "HIR function" text) (eq original #'nop-uncompiled)
           (not (compiled-function-p #'nop-uncompiled)) (= (nop-uncompiled 40) 42))))`);
  check(`(let ((text nil) (result nil))
    (setq text (with-output-to-string (*standard-output*)
                 (setq result (disassemble '(lambda (x) (+ x 2))))))
    (and (null result) (search "HIR function" text) t))`);
  check(`(let ((fn (lambda (value object) (setf (car object) value))))
    (and (equal (multiple-value-list (compile '(setf nop-place) fn)) '((setf nop-place) nil nil))
      (compiled-function-p (fdefinition '(setf nop-place)))
      (let ((object (list 1))) (setf (nop-place object) 42) (= (car object) 42))))`);
  check(`(progn (defmacro nop-macro (x) (list '+ x 1))
    (let ((fn (macro-function 'nop-macro)))
      (and (eq (compile 'nop-macro) 'nop-macro) (compiled-function-p (macro-function 'nop-macro))
        (= (nop-macro 41) 42))))`);
  check(`(let ((fn (compile nil '(lambda (x) (nop-macro x)))))
    (defmacro nop-macro (x) (list '+ x 2))
    (= (funcall fn 41) 42))`);
  check(`(let* ((literal (list 42))
                (fn (compile nil (list 'lambda nil (list 'quote literal)))))
    (and (compiled-function-p fn) (eq literal (funcall fn)) (eq fn (compile nil fn))))`);
  check(`(progn (defparameter *minimal-ltv-count* 0)
    (let ((fn (compile nil '(lambda () (load-time-value (incf *minimal-ltv-count*))))))
      (and (= *minimal-ltv-count* 1) (= (funcall fn) 1) (= (funcall fn) 1)
           (= *minimal-ltv-count* 1))))`);
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
  assert.deepEqual([...(await vm.readFile('/nop.wasm')).subarray(0, 8)], [0,97,115,109,1,0,0,0]);
  check(`(and (= *nop-reads* 1) (= *nop-compile-effects* 1) (= *nop-load-effects* 0)
    (not (fboundp 'nop-file-value)) (not (boundp '*nop-loaded*)))`);
  check(`(and (load "/nop.wasm" :verbose nil) (= *nop-reads* 1)
    (= *nop-load-effects* 1) (= *nop-compile-effects* 1) (string= *nop-loaded* "é𝄞")
    (= (nop-file-value) 42) (compiled-function-p #'nop-file-value))`);
  check(`(not (null (search "HIR function" (with-output-to-string (*standard-output*)
    (disassemble #'nop-file-value)))))`);
  check(`(and (equal (multiple-value-list
      (compile-file "/nop.lisp" :output-file "/copied.lisp" :verbose nil))
      (list (truename "/copied.lisp") nil nil))
    (load "/copied.lisp" :verbose nil) (compiled-function-p #'nop-file-value)
    (not (null (miricl-compiler::compiler-primitive 44 #'nop-file-value))))`);
  assert.deepEqual(await vm.readFile('/nop.lisp'), new TextEncoder().encode(source));
  vm.mountFile('/condition-file.lisp', `(define-condition minimal-file-condition (type-error) ()
    (:default-initargs :expected-type 'symbol)
    (:report (lambda (condition stream)
               (format stream "Expected ~A" (type-error-expected-type condition)))))
    (let ((text "Captured reporter"))
      (define-condition minimal-captured-condition (condition) ()
        (:report (lambda (condition stream) (declare (ignore condition))
                   (write-string text stream)))))`);
  check(`(progn (compile-file "/condition-file.lisp" :verbose nil)
    (load "/condition-file.wasm" :verbose nil)
    (and (string= (princ-to-string (make-condition 'minimal-file-condition)) "Expected SYMBOL")
         (string= (princ-to-string (make-condition 'minimal-captured-condition)) "Captured reporter")))`);
  vm.mountFile('/debug-file.lisp', `(defun minimal-debug-file (x)
    (declare (optimize (debug 3))) (+ x 2))`);
  check(`(progn (compile-file "/debug-file.lisp" :verbose nil)
    (load "/debug-file.wasm" :verbose nil)
    (and (= (minimal-debug-file 40) 42) (compiled-function-p #'minimal-debug-file)
      (function-lambda-expression #'minimal-debug-file)
      (search "INTERPRET" (with-output-to-string (*standard-output*)
        (disassemble #'minimal-debug-file))) t))`);
  // Minimal compilation uses the same checked syntax boundary as the full frontend.
  for (const malformed of [
    "'(lambda #1=(x . #1#) x)", "'(lambda (x x) x)",
    "'(lambda () #1=(progn . #1#))", "'(lambda () (if t))",
  ]) check(`(handler-case (progn (compile nil ${malformed}) nil) (program-error () t))`);
  check(`(= (funcall (compile nil '(lambda () (+ 20 22)))) 42)`);

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
      (search "HIR function" (with-output-to-string (*standard-output*)
        (disassemble #'nop-eval))) t)`);
    // Building the compiler-free variant must not alter the ordinary image.
    check(`(and (fboundp 'miricl-compiler:compile-native)
      (compiled-function-p (compile nil '(lambda () 42))))`, full);
  } finally { full.close(); }
  check(noCompiler);
  const restored = await Miricl.instantiate(vm.saveImage());
  try {
    check(noCompiler, restored);
    check(`(let ((text (with-output-to-string (*standard-output*) (disassemble *minimal-capture*))))
      (and (search "Capture 0: X" text) (= (funcall *minimal-capture*) 42)))`, restored);
    check(`(let ((fn #'nop-eval))
      (and (compiled-function-p (compile nil fn))
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
    check(`(and (= (app:answer) 42) (compiled-function-p #'app:answer))`);
    check(noCompiler);
  }
} finally { vm.close(); }
console.log(`Interpreter-only: minimal compilation, macro snapshots, load-time values, checked input, CLOS, portable FASLs and saved images pass (${binary.length} bytes).`);
