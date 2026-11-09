import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
import {compilerCases} from './compiler-cases.mjs';

const binary = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const builder = await Miricl.instantiate(binary);
const fresh = await Miricl.instantiate(binary);
const check = (vm, form) => assert.equal(vm.evaluate(form, {eof: true}).output, 'T\n', form);
try {
  // Generated algebra often expands into deeply nested conditional trees.
  // Exercise conversion through lexical scopes without a larger Wasm stack.
  builder.mountFile('/deep.lisp', '(defun deep-file-code (x) (let ((y 17)) ' +
    '(if x '.repeat(180) + 'y' + ' 23)'.repeat(180) + '))');
  builder.evaluate('(compile-file "/deep.lisp")', {eof: true});
  fresh.mountFile('/deep.wasm', await builder.readFile('/deep.wasm'));
  fresh.evaluate('(load "/deep.wasm")', {eof: true});
  check(fresh, '(and (= (deep-file-code t) 17) (= (deep-file-code nil) 23))');
  builder.mountFile('/scope.lisp', `
    (defpackage :file-scope-test (:use :cl))
    (in-package :file-scope-test)
    (defvar *dynamic* :outside)
    (declaim (special *proclaimed*))
    (defun proclaimed-reader () *proclaimed*)
    (defun proclaimed-binding () (let ((*proclaimed* :dynamic)) (proclaimed-reader)))
    (defun outer-call (x) (+ x 10))
    (defun scope (x &optional (y (+ x 1) supplied) &rest rest &key (z 7) &allow-other-keys &aux (a (+ y z)))
      (list x y supplied rest z a))
    (defun captures (x)
      (values (lambda () (incf x)) (lambda () x)
        (let ((x (+ x 100))) (lambda () x))))
    (defun locals (x)
      (flet ((outer-call (y) (outer-call y)))
        (labels ((even-step (n) (if (zerop n) x (odd-step (1- n))))
                 (odd-step (n) (if (zerop n) (outer-call x) (even-step (1- n)))))
          (values (even-step 3) (funcall #'odd-step 2)))))
    (defun local-exits ()
      (list (flet ((exit () (return-from exit 17) 99)) (exit))
        (labels ((exit () (block exit (return-from exit 19)) (return-from exit 23))) (exit))
        (flet (((setf cell) (new object) (declare (ignore object)) (return-from cell new)))
          (funcall #'(setf cell) 29 nil))))
    (defun specials (x)
      (declare (special x))
      (let ((x :lexical))
        (list x (locally (declare (special x)) x)
          (let ((x :inner)) (declare (special x)) (symbol-value 'x)))))
    (defun binding-order (x)
      (list (let ((x 11) (y x)) (list x y))
        (let* ((x 13) (y x)) (list x y))))
    (defun special-init (x)
      (let ((old x) (x 31)) (declare (special x)) (list old x)))
    (defun dynamic-closure (*dynamic*)
      (let ((reader (lambda () *dynamic*)))
        (list (funcall reader) (let ((*dynamic* :nested)) (funcall reader)))))
    (defun nested-definitions (value)
      (let ((local-default :lexical) (local-parameter :lexical))
        (defvar local-default value)
        (defparameter local-parameter value)
        (list local-default local-parameter (symbol-value 'local-default) (symbol-value 'local-parameter))))
    (defun export-names (package)
      (let ((names (make-hash-table :test 'equal)))
        (do-external-symbols (symbol package)
          (setf (gethash (symbol-name symbol) names) t))
        (loop for name being the hash-keys of names collect name)))
    (defun local-setters ()
      (let ((left (list 1)) (right (list 2)))
        (flet ((head (object) (car object))
               ((setf head) (value object) (setf (car object) value)))
          (psetf (head left) (head right) (head right) (head left))
          (list (head left) (head right)))))
    (defun condition-forms (x)
      (list (handler-case (if x (error "test") (values 19 23))
              (error (condition) (typep condition 'error)) (:no-error (a b) (+ a b)))
        (restart-case (invoke-restart 'supply 42) (supply (value) value))))
    (defun restart-options (value)
      (flet ((report-value (stream) (princ value stream)))
        (let ((report nil))
          (list (restart-case
                    (let ((restart (find-restart 'supply)))
                      (setq report (princ-to-string restart)) (invoke-restart restart 42))
                  (supply (value) :report report-value :test (lambda (condition) (declare (ignore condition)) value)
                    value))
            report))))
    (defun declared-condition-body (x)
      (declare (special x))
      (let ((x :lexical))
        (list (handler-bind () (declare (special x)) x)
          (restart-bind () (declare (special x)) x)
          (ignore-errors (declare (special x)) x)
          (with-simple-restart (continue "Continue") (declare (special x)) x))))
    (defun loop-forms ()
      (list (loop for x in '(1 2 3) collect (* x 2))
        (loop for x below 4 sum x)
        (loop for x in '(1 2 3) always (plusp x))
        (loop for x in '(1 2 3) thereis (and (evenp x) x))
        (loop for x from 1 to 3 collect x finally (return :finished))
        (let ((x (list 1 2))) (psetf (car x) (cadr x) (cadr x) (car x)) x)))
    (defun loop-syntax ()
      (let ((fixnum :not-a-type) (it :outer))
        (list fixnum it (loop for i fixnum from 1 to 3 collect i)
          (loop for x in '(1 2 3) when (and (evenp x) (+ x 10)) collect it))))
    (defun quoted-names (x) (list x 'x '#:separate '#:separate '#1=(:cycle . #1#)))
  `);
  check(builder, '(pathnamep (compile-file "/scope.lisp"))');
  check(builder, '(not (boundp (quote file-scope-test::*dynamic*)))');
  check(builder, '(not (boundp (quote file-scope-test::*proclaimed*)))');
  fresh.mountFile('/scope.wasm', await builder.readFile('/scope.wasm'));
  check(fresh, '(load "/scope.wasm")');
  for (const form of [
    "(equal (file-scope-test::scope 3) '(3 4 nil nil 7 11))",
    "(equal (file-scope-test::scope 3 5 :z 9) '(3 5 t (:z 9) 9 14))",
    "(equal (multiple-value-list (file-scope-test::locals 5)) '(15 15))",
    "(equal (file-scope-test::local-exits) '(17 23 29))",
    "(equal (file-scope-test::specials :dynamic) '(:lexical :dynamic :inner))",
    "(equal (file-scope-test::binding-order 7) '((11 7) (13 13)))",
    "(equal (file-scope-test::special-init 7) '(7 31))",
    "(equal (file-scope-test::dynamic-closure :inside) '(:inside :nested))",
    "(eq file-scope-test::*dynamic* :outside)",
    "(and (eq (file-scope-test::proclaimed-binding) :dynamic) (not (boundp 'file-scope-test::*proclaimed*)))",
    "(equal (file-scope-test::nested-definitions 42) '(:lexical :lexical 42 42))",
    "(equal (file-scope-test::local-setters) '(2 1))",
    "(equal (file-scope-test::condition-forms nil) '(42 42))",
    "(equal (file-scope-test::condition-forms t) '(t 42))",
    "(equal (file-scope-test::restart-options 17) '(42 \"17\"))",
    "(equal (file-scope-test::declared-condition-body :dynamic) '(:dynamic :dynamic :dynamic :dynamic))",
    "(equal (file-scope-test::loop-forms) '((2 4 6) 6 t 2 :finished (2 1)))",
    "(equal (file-scope-test::loop-syntax) '(:not-a-type :outer (1 2 3) (12)))",
  ]) check(fresh, form);
  check(fresh, `(let ((package (make-package "FILE-SCOPE-EXPORTS" :use nil)))
    (unwind-protect
      (progn (export (list (intern "ONE" package) (intern "TWO" package)) package)
        (null (set-exclusive-or (file-scope-test::export-names package) '("ONE" "TWO") :test #'equal)))
      (delete-package package)))`);
  check(fresh, `(multiple-value-bind (increment read other) (file-scope-test::captures 1)
    (and (= (funcall increment) 2) (= (funcall read) 2) (= (funcall other) 101)
         (= (funcall increment) 3) (= (funcall read) 3)))`);
  check(fresh, `(let ((a (file-scope-test::quoted-names 7)) (b (file-scope-test::quoted-names 8)))
    (and (= (first a) 7) (eq (second a) 'file-scope-test::x)
      (not (eq (third a) (fourth a))) (eq (third a) (third b))
      (eq (fifth a) (cdr (fifth a)))))`);

  // File compilation must also leave code passed to COMPILE-NATIVE as quoted
  // source. Exercise the shared compiler semantics through a fresh FASL load.
  builder.mountFile('/compiler-cases.lisp', compilerCases.map(([, form], i) =>
    `(defun file-compiler-case-${i} () ${form})`).join('\n'));
  check(builder, '(pathnamep (compile-file "/compiler-cases.lisp"))');
  fresh.mountFile('/compiler-cases.wasm', await builder.readFile('/compiler-cases.wasm'));
  check(fresh, '(load "/compiler-cases.wasm")');
  for (let i = 0; i < compilerCases.length; i++)
    check(fresh, `(file-compiler-case-${i})`);

  // Retain every function in an unrelated generated program. The comparison
  // measures sharing of equivalent executable syntax, without dead-code gains.
  const names = Array.from({length: 100}, (_, i) => `SHARING-TEST:FUNCTION-${i}`);
  builder.mountFile('/sharing.lisp', `
    (defpackage :sharing-test (:use :cl) (:export ${names.map(n => ':' + n.split(':')[1]).join(' ')}))
    (in-package :sharing-test)
    (eval-when (:compile-toplevel)
      (defmacro shared-computation (argument)
        (let ((temporary (gensym)))
          \`(let ((,temporary (+ ,argument 7)))
             (list (* ,temporary ,temporary) (- ,temporary 1) (1+ ,temporary))))))
    ${names.map((_, i) => `(defun function-${i} (argument-${i}) (shared-computation argument-${i}))`).join('\n')}
  `);
  check(builder, `(progn (miricl-compiler::compile-linker) t)`);
  check(builder, `(let* ((name (find "%FILE-LEXICAL-FORM" miricl-compiler::*native-compiler-seeds*
                                :key #'symbol-name :test #'string=))
                        (original (symbol-function name)))
    (unwind-protect
      (progn (setf (symbol-function name) (lambda (form &rest environment) (declare (ignore environment)) form))
        (compile-file "/sharing.lisp" :output-file "/unshared.wasm"))
      (setf (symbol-function name) original))
    (pathnamep (compile-file "/sharing.lisp" :output-file "/shared.wasm")))`);
  const keep = names.map(n => `"${n}"`).join(' ');
  for (const variant of ['unshared', 'shared'])
    check(builder, `(pathnamep (miricl-compiler:shake-file "/${variant}.wasm" "/${variant}-small.wasm"
      :entry-points '(${keep})))`);
  const unshared = await builder.readFile('/unshared-small.wasm');
  const shared = await builder.readFile('/shared-small.wasm');
  assert.ok(shared.length < unshared.length * 0.75, `${shared.length} / ${unshared.length}`);
  fresh.mountFile('/shared.wasm', shared);
  check(fresh, '(load "/shared.wasm")');
  for (let i = 0; i < names.length; i++)
    check(fresh, `(equal (${names[i]} ${i}) '(${(i + 7) ** 2} ${i + 6} ${i + 8}))`);
  console.log(`Generic file code sharing: ${unshared.length} -> ${shared.length} bytes with all 100 functions retained; scopes, captures, dynamic bindings, exits and literal identities pass.`);
} finally {builder.close(); fresh.close();}
