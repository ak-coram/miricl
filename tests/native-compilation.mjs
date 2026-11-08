import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl, MiriclError} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  check("(fboundp 'miricl-compiler:compile-native)");
  const lispDisassembly = vm.evaluate(`(defun disassembly-square (x) (* x x))
    (disassemble #'disassembly-square)`, {eof: true}).output;
  assert.match(lispDisassembly, /Packed Lisp code: [1-9][0-9]* bytes/);
  check(`(eq miricl-compiler::*compiler-state* :cold)`);
  check(`(let ((function #'disassembly-square))
    (with-output-to-string (*standard-output*) (disassemble function))
    (eq function #'disassembly-square))`);
  if (process.argv.includes('--bootstrap')) {
    check(`(multiple-value-bind (installed unsupported) (miricl-compiler::compile-compiler)
      (and (>= (length installed) 93) (null unsupported)
           (null (function-lambda-expression #'miricl-compiler::wasm-byte))
           (null (function-lambda-expression #'miricl-compiler::hir-emit))
           (null (function-lambda-expression #'miricl-compiler::hir-form))
           (null (function-lambda-expression #'miricl-compiler:compile-native))))`);
  }
  check(`(progn
    (defparameter *native* (miricl-compiler:compile-native '(lambda (x) (if x (+ x 2) 0))))
    (and (compiled-function-p *native*) (= (funcall *native* 40) 42) (= (funcall *native* nil) 0)))`);
  const moduleSize = Number(vm.evaluate(`(length (miricl-compiler:emit-native-module
    (miricl-compiler:compile-to-hir '(lambda (x) (if x (+ x 2) 0)))))`, {eof: true}).output.trim());
  assert.ok(moduleSize > 0);
  assert.match(vm.evaluate('(disassemble *native*)', {eof: true}).output,
    new RegExp(`Wasm module: ${moduleSize} bytes`));
  check(`(let* ((maker (miricl-compiler:compile-native '(lambda (x) (lambda () x))))
               (closure (funcall maker 42)))
    (and (= (funcall closure) 42)
      (= (miricl-compiler::compiler-primitive 26 maker)
         (miricl-compiler::compiler-primitive 26 closure))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x &aux (before (lambda () x)) (x (1+ x))
                    (between (lambda () x)) (x (1+ x)))
       (list (funcall before) (funcall between) x)))))
    (equal (funcall function 40) '(40 41 42)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x &rest tail) (values (car tail) (cdr tail) x)))))
    (and (equal (multiple-value-list (funcall function 1 2 3)) '(2 (3) 1))
         (equal (multiple-value-list (funcall function 1)) '(nil nil 1))
         (handler-case (progn (funcall function) nil) (program-error () t))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (let ((x (+ x 1)) (y x))
      (let* ((y (+ y x)) (z y)) (setq x (+ x z) y (+ x y)) (values x y z)))))))
    (equal (multiple-value-list (funcall function 10)) '(32 53 21)))`);
  check(`(let ((zero (miricl-compiler:compile-native '(lambda () (values))))
              (many (miricl-compiler:compile-native '(lambda (flag) (if flag (values nil 2 3) (values))))))
    (and (null (multiple-value-list (funcall zero)))
         (null (multiple-value-list (funcall many nil)))
         (equal (multiple-value-list (funcall many t)) '(nil 2 3))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (multiple-value-prog1 (values 1 2 3) (values 4 5))))))
    (equal (multiple-value-list (funcall function)) '(1 2 3)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (multiple-value-bind (a b c) (if x (values 1 2) (values))
                   (values a b c))))))
    (and (equal (multiple-value-list (funcall function t)) '(1 2 nil))
         (equal (multiple-value-list (funcall function nil)) '(nil nil nil))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (list (if x (values 1 2 3) (values))
                      (if x (values) (values nil 4)))))))
    (and (equal (funcall function t) '(1 nil)) (equal (funcall function nil) '(nil nil))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (case x (nil :never) ((nil) (values 1 2)) ((:a :b) (values)) ((t) 9) (otherwise 11))))))
    (and (equal (multiple-value-list (funcall function nil)) '(1 2))
         (null (multiple-value-list (funcall function :a)))
         (= (funcall function t) 9) (= (funcall function :other) 11)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (ecase x ((nil) 1) (t 2) (otherwise 3))))))
    (and (= (funcall function nil) 1) (= (funcall function t) 2) (= (funcall function 'otherwise) 3)
         (handler-case (progn (funcall function :missing) nil)
           (type-error (c) (eq (type-error-datum c) :missing)))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (flet ((values (x) (list x))) (values 42))))))
    (equal (funcall function) '(42)))`);
  check(`(let* ((literal (list 1 2 2 3))
    (function (miricl-compiler:compile-native (list 'lambda '(x) (list 'member 'x (list 'quote literal))))))
    (and (eq (funcall function 2) (cdr literal)) (eq (funcall function 1) literal)
         (null (funcall function 9)) (null (funcall function 2.0))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (flet ((member (a b) (declare (ignore a b)) :local)) (member x '(1 2)))))))
    (eq (funcall function 1) :local))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (multiple-value-bind (x y) (values (+ x 1) x :discarded)
                   (values x y))))))
    (equal (multiple-value-list (funcall function 41)) '(42 41)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (v) (setf (aref v 0) (values 42 :discarded)) (aref v 0)))))
    (= (funcall function (vector 0)) 42))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (values (car x) (cdr x))))))
    (and (equal (multiple-value-list (funcall function '(1 . 2))) '(1 2))
         (equal (multiple-value-list (funcall function nil)) '(nil nil))
         (handler-case (progn (funcall function 42) nil) (type-error () t))))`);
  check(`(let ((function (miricl-compiler:compile-native '(lambda (x) (list (car x)))))
              (original (symbol-function 'car)))
    (unwind-protect
      (progn (setf (symbol-function 'car) (lambda (x) (declare (ignore x)) :redefined))
             (equal (funcall function '(1)) '(:redefined)))
      (setf (symbol-function 'car) original)))`);
  check(`(let ((unit (miricl-compiler:compile-to-hir '(lambda (v) (setf (aref v 0) 42)))))
    (= (length (miricl-compiler:hir-unit-functions unit)) 1))`);
  check(`(progn (defun native-empty () (values))
    (let ((function (miricl-compiler:compile-native '(lambda () (list (native-empty) (floor 17 5))))))
      (equal (multiple-value-list (funcall function)) '((nil 3)))))`);
  check(`(progn
    (defparameter *native-events* nil)
    (defun native-event (x) (push x *native-events*) (values x :ignored))
    (let ((function (miricl-compiler:compile-native
      '(lambda () (list (native-event 1) (native-event 2) (native-event 3))))))
      (and (equal (funcall function) '(1 2 3)) (equal *native-events* '(3 2 1)))))`);
  check(`(progn
    (defun native-helper (x) (+ x 1))
    (defparameter *native-caller* (miricl-compiler:compile-native '(lambda (x) (native-helper x))))
    (defun native-helper (x) (+ x 2))
    (= (funcall *native-caller* 40) 42))`);
  check(`(let* ((literal (list "é𝄞"))
    (function (miricl-compiler:compile-native (list 'lambda nil (list 'quote literal)))))
    (and (eq (funcall function) literal) (eq (funcall function) (funcall function))))`);
  check(`(let ((function (miricl-compiler:compile-native '(lambda (x) (* x x)))))
    (= (funcall function 12345678901234567890) (* 12345678901234567890 12345678901234567890)))`);
  check(`(let ((function (miricl-compiler:compile-native '(lambda (x) (incf x) x))))
    (= (funcall function 41) 42))`);
  check(`(let ((factory (miricl-compiler:compile-native
    '(lambda (x) (values (lambda () (incf x)) (lambda () x))))))
    (multiple-value-bind (increment read) (funcall factory 41)
      (and (= (funcall increment) 42) (= (funcall read) 42)
           (= (funcall increment) 43) (= (funcall read) 43))))`);
  check(`(let* ((factory (miricl-compiler:compile-native '(lambda (x) (lambda () (lambda () x)))))
    (middle (funcall factory 42)) (inner (funcall middle))) (= (funcall inner) 42))`);
  check(`(let* ((factory (miricl-compiler:compile-native
      '(lambda (x) (values (lambda () x) (lambda () x)))))
      (object (list 1)))
    (multiple-value-bind (first second) (funcall factory object)
      (setf (car object) 42)
      (and (eq (funcall first) object) (eq (funcall second) object)
           (= (car (funcall second)) 42)
           (null (funcall (funcall factory nil))))))`);
  check(`(let* ((factory (miricl-compiler:compile-native
      '(lambda (x) (let ((read (lambda () x))) (setq x 42) read))))
      (read (funcall factory 0)))
    (= (funcall read) 42))`);
  check(`(let ((factory (miricl-compiler:compile-native
      '(lambda (x) (values (lambda () (lambda () (incf x))) (lambda () x))))))
    (multiple-value-bind (middle read) (funcall factory 41)
      (let ((increment (funcall middle)))
        (and (= (funcall increment) 42) (= (funcall read) 42)
             (= (funcall increment) 43) (= (funcall read) 43)))))`);
  check(`(let* ((factory (miricl-compiler:compile-native
      '(lambda (x) (labels ((even (n) (if (zerop n) x (odd (1- n))))
                           (odd (n) (if (zerop n) nil (even (1- n)))))
                    #'even))))
      (even (funcall factory :yes)))
    (and (eq (funcall even 10) :yes) (null (funcall even 9))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (labels ((factorial (n) (if (= n 0) 1 (* n (factorial (1- n)))))) (factorial x))))))
    (= (funcall function 10) 3628800))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (multiple-value-call #'list (values 1 2) (values) (values nil 3))))))
    (equal (funcall function) '(1 2 nil 3)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x &optional (y (+ x 1) supplied) (z (+ y 1)) &rest tail &aux (sum (+ x y z)))
       (values sum supplied tail)))))
    (and (equal (multiple-value-list (funcall function 1)) '(6 nil nil))
         (equal (multiple-value-list (funcall function 1 10)) '(22 t nil))
         (equal (multiple-value-list (funcall function 1 10 20 30 40)) '(31 t (30 40)))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (&optional (x (error "Default must be skipped") supplied)) (list x supplied)))))
    (and (equal (funcall function nil) '(nil t))
         (handler-case (progn (funcall function 1 2) nil) (program-error () t))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (block outer (block inner (return-from outer (values 1 2))) (error "Unreachable"))))))
    (equal (multiple-value-list (funcall function)) '(1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (flet ((local (x) (declare (ignore x)) (return-from local (values 1 2)))) (local 0))))))
    (equal (multiple-value-list (funcall function)) '(1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () ((lambda (x) (the (values integer integer) (values x 2))) 1)))))
    (equal (multiple-value-list (funcall function)) '(1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (the (values) (values))))))
    (null (multiple-value-list (funcall function))))`);
  check(`(progn (defmacro native-shadow (x) (declare (ignore x)) :global)
    (let ((function (miricl-compiler:compile-native
      '(lambda () (flet ((native-shadow (x) (+ x 1))) (native-shadow 41))))))
      (= (funcall function) 42)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (eval-when (:compile-toplevel :load-toplevel) (error "Must not execute"))
                (eval-when (:execute) (values 1 2))))))
    (equal (multiple-value-list (funcall function)) '(1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (let ((sum 0)) (tagbody (go next) (error "Unreachable")
      loop (incf sum) next (if (< sum 10) (go loop))) sum)))))
    (= (funcall function) 10))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (let ((sum 0)) (tagbody out (tagbody (go done) out (error "Shadowed") done)
      (incf sum) (if (< sum 3) (go out))) sum)))))
    (= (funcall function) 3))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (n) (let ((sum 0)) (dotimes (i n sum) (incf sum i)))))))
    (= (funcall function 10) 45))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (macrolet ((twice (form) (list '+ form form))) (twice x))))))
    (= (funcall function 21) 42))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (symbol-macrolet ((first (car x))) (setq first 42) first)))))
    (let ((cell (list 0))) (and (= (funcall function cell) 42) (= (car cell) 42))))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda (x) (symbol-macrolet ((x (error "Shadowed"))) (let ((x 42)) x))))))
    (= (funcall function 0) 42))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () ((lambda (x y) (declare (ignore x)) "doc" (declare (ignore y)) "value") 1 2)))))
    (equal (funcall function) "value"))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (destructuring-bind (&whole (a . b) c . d) '(1 . 2) (list a b c d))))))
    (equal (funcall function) '(1 2 1 2)))`);
  check(`(let ((function (miricl-compiler:compile-native
    '(lambda () (let ((x 0)) (macrolet ((local-tag () 'tag))
      (tagbody (tagbody (go tag) (local-tag) (setq x 1)) tag)) x)))))
    (= (funcall function) 0))`);
  check(`(= (funcall (miricl-compiler:compile-native
    '(lambda () (block escaped (funcall (lambda () (return-from escaped 1))))))) 1)`);
  check(`(handler-case (progn (funcall *native* 1 2) nil) (program-error () t))`);
  check(`(progn
    (defparameter *native-error* (miricl-compiler:compile-native '(lambda () (/ 1 0)) :name 'native-error))
    (handler-case (progn (funcall *native-error*) nil) (division-by-zero () t)))`);
  check(`(multiple-value-bind (expression closure name) (function-lambda-expression *native-error*)
    (and (null expression) closure (eq name 'native-error)))`);
  assert.throws(() => vm.evaluate('(funcall *native-error*)'), error =>
    error instanceof MiriclError && error.backtrace.includes('NATIVE-ERROR'));
  check(`(let ((function (miricl-compiler:compile-native '(lambda () (/ 1 0) 42))))
    (handler-case (progn (funcall function) nil) (division-by-zero () t)))`);
  check(`(let ((function (miricl-compiler:compile-native '(lambda () native-unbound-variable 42))))
    (handler-case (progn (funcall function) nil) (unbound-variable () t)))`);
  check(`(multiple-value-bind (bytes literals)
    (miricl-compiler:emit-native-module
      (miricl-compiler:compile-to-hir '(lambda () "Discarded literal" 42)))
    (and (< (length bytes) 250) (equalp literals #(42))))`);
  check('(= (funcall *native* 40) 42)');
  check(`(multiple-value-bind (bytes literals)
    (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '(lambda (x) (+ x 2))))
    (with-open-file (s "/native-code.wasm" :direction :output :element-type '(unsigned-byte 8))
      (write-sequence bytes s)) (and (vectorp bytes) (vectorp literals)))`);
  const bytes = await vm.readFile('/native-code.wasm');
  assert.ok(WebAssembly.validate(bytes));
  const imports = WebAssembly.Module.imports(await WebAssembly.compile(bytes));
  assert.ok(imports.length > 0 && imports.every(x => x.module === 'miricl.runtime'));
  assert.equal(imports.some(x => /native-cell|native-close|native-capture/.test(x.name)), false,
    'Uncaptured variables must not import heap-cell or closure operations.');
  assert.ok(bytes.length < 600, `Small native module grew to ${bytes.length} bytes.`);
  for (const source of ['(lambda (x) (lambda () x))', '(lambda (x) (lambda () (lambda () x)))']) {
    check(`(multiple-value-bind (bytes literals)
      (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))
      (declare (ignore literals))
      (with-open-file (s "/native-captures.wasm" :direction :output :if-exists :supersede
                         :element-type '(unsigned-byte 8)) (write-sequence bytes s)) t)`);
    const module = await WebAssembly.compile(await vm.readFile('/native-captures.wasm'));
    assert.equal(WebAssembly.Module.imports(module).some(x => /^native-cell/.test(x.name)), false,
      'Unassigned captures must carry the original value without allocating or reading binding cells.');
  }
  assert.equal((await vm.evaluateAsync(`(progn (defparameter *native-loop-started* nil)
    (let ((worker (miricl-threads:make-thread (miricl-compiler:compile-native
        '(lambda () (setq *native-loop-started* t) (loop))))))
      (loop until *native-loop-started* do (miricl-threads:thread-yield))
      (miricl-threads:destroy-thread worker)
      (handler-case (progn (miricl-threads:join-thread worker) nil)
        (miricl-threads:abnormal-exit () (not (miricl-threads:thread-alive-p worker))))))`)).output, 'T\n',
    'Compiled loops without calls must retain a scheduling safepoint.');
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda (x &optional (y 2) &rest rest &key (z (+ x y) supplied) &allow-other-keys &aux (sum (+ x y z)))
         (values sum supplied rest)))))
    (and (equal (multiple-value-list (funcall function 10)) '(24 nil nil))
         (equal (multiple-value-list (funcall function 10 3 :z 4 :ignored 5))
                '(17 t (:z 4 :ignored 5)))))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda (&key ((custom-key x) 7 xp) (y (+ x 1))) (values x y xp)))))
    (and (equal (multiple-value-list (funcall function)) '(7 8 nil))
         (equal (multiple-value-list (funcall function 'custom-key nil :y 9)) '(nil 9 t))
         (equal (multiple-value-list (funcall function 'custom-key 1 'custom-key 2)) '(1 2 t))
         (handler-case (progn (funcall function :unknown 1) nil) (program-error () t))
         (handler-case (progn (funcall function :y) nil) (program-error () t))
         (equal (multiple-value-list (funcall function :unknown 1 :allow-other-keys t)) '(7 8 nil))))`);
  check(`(progn (defparameter *native-special* 1) (defun native-read-special () *native-special*)
    (let ((function (miricl-compiler:compile-native
      '(lambda () (values (let ((*native-special* 7))
                            (list *native-special* (native-read-special) (symbol-value '*native-special*)))
                          *native-special*)))))
      (and (equal (multiple-value-list (funcall function)) '((7 7 7) 1)) (= *native-special* 1))))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda (*native-special*)
        (values *native-special* (let ((*native-special* 3) (previous *native-special*)) previous)
          (let* ((*native-special* 4) (previous *native-special*)) previous) *native-special*)))))
    (and (equal (multiple-value-list (funcall function 2)) '(2 2 4 2)) (= *native-special* 1)))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda () (values (block done (let ((*native-special* 8)) (return-from done *native-special*)))
                    (progn (tagbody (let ((*native-special* 9)) (go done)) done) *native-special*))))))
    (and (equal (multiple-value-list (funcall function)) '(8 1)) (= *native-special* 1)))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((native-local-special 42)) (declare (special native-local-special))
                    (symbol-value 'native-local-special))))))
    (and (= (funcall function) 42) (not (boundp 'native-local-special))))`);
  check(`(let ((function (miricl-compiler:compile-native
      '(lambda () (let ((*native-special* 9)) (error "Unwind native specials"))))))
    (and (handler-case (progn (funcall function) nil) (error () (= *native-special* 1)))
         (= *native-special* 1)))`);
  check(`(let ((package *package*) (function (miricl-compiler:compile-native
      '(lambda () (let ((*package* (find-package "KEYWORD"))) (symbol-package (intern "NATIVE-KEYWORD")))))))
    (and (eq (funcall function) (find-package "KEYWORD")) (eq *package* package)))`);
  console.log(`Native compilation: ${bytes.length}-byte module; arithmetic, branches, lexical cells, rest arguments, macros, literal identity, multiple values, redefinition, arity and conditions pass.`);
} finally {vm.close();}
