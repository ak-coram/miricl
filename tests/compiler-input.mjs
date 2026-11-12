// Malformed source/IR must signal Lisp conditions and leave compilation usable.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  const reject = (source, reason) => {
    const {output} = vm.evaluate(`(handler-case
      (progn ${source} (error "Malformed compiler input was accepted"))
      (compiler-input-error (condition) (princ-to-string condition)))`, {eof: true});
    assert.match(output, reason, source);
    assert.match(output, /Input:/, source);
    assert.ok(output.length < 1500, 'Diagnostics must bound printing of malformed graphs');
    assert.equal(vm.evaluate("(funcall (compile-native '(lambda (x) (+ x 1))) 41)").output, '42\n', source);
  };
  for (const count of [0, 1, 2, 3, 4, 1000]) {
    assert.equal(vm.evaluate(`(compiler-list-length (make-list ${count}) "list spine")`).output,
      `${count}\n`);
  }
  for (const reader of ['native-read-uleb', 'native-read-sleb']) {
    for (const bytes of ['#()', '#(128)', '#(128 128 128 128 128 128 128 128 128 128 0)'])
      reject(`(${reader} ${bytes} 0)`, /truncated|exceeds 64 bits/);
  }
  for (const bytes of ['#(0 65)', '#(0 66)', '#(0 66 128)'])
    reject(`(native-read-ops ${bytes})`, /missing operand|truncated/);
  assert.equal(vm.evaluate(`(let ((function (compile-native
    '(lambda (x &aux (before (lambda () x)) (x (1+ x))
                    (between (lambda () x)) (x (1+ x)))
       (list (funcall before) (funcall between) x)))))
    (equal (funcall function 40) '(40 41 42)))`).output, 'T\n');
  assert.equal(vm.evaluate(`(let ((function (compile-native
    '(lambda (x &aux x (x (if x :bad :rebound))) x))))
    (eq (funcall function :parameter) :rebound))`).output, 'T\n');
  for (const input of ['17', "'(1 . 2)", "'(1 2 . 3)", '#(1 2)'])
    reject(`(compiler-list-length ${input} "list spine")`, /dotted tail/);
  for (const prefix of [0, 1, 2, 3]) {
    for (const size of [1, 2, 3, 4]) {
      reject(`(let ((cycle (make-list ${size})))
        (setf (cdr (last cycle)) cycle)
        (compiler-list-length (append (make-list ${prefix}) cycle) "list spine"))`, /circular list/);
    }
  }
  for (const [form, reason] of [
    ['42', /lambda expression/], ['#()', /lambda expression/],
    ["'(lambda)", /missing lambda list/],
    ["'(lambda (x . y) x)", /dotted tail/],
    ["'(lambda (x x) x)", /duplicate parameter/],
    ["'(lambda (x &optional x) x)", /duplicate parameter/],
    ["'(lambda (&optional (x nil x)) x)", /duplicate parameter/],
    ["'(lambda (x &key ((:other x))) x)", /duplicate parameter/],
    ["'(lambda (t) t)", /nonconstant variable/],
    ["'(lambda (&rest) nil)", /REST requires/],
    ["'(lambda (&optional &optional) nil)", /repeated lambda-list keyword/],
    ["'(lambda (&rest x y) x)", /unexpected parameter/],
    ["'(lambda (&key ((:x 9))) nil)", /nonconstant variable/],
    ["'(lambda (&optional (x nil nil)) x)", /nonconstant variable/],
    ["'(lambda (&aux (:bad 1)) nil)", /nonconstant variable/],
    ["'(lambda (&aux (x nil supplied)) x)", /invalid parameter specification/],
    ["'(lambda () (let ((x 1 2)) x))", /binding/],
    ["'(lambda () (let ((x 1) (x 2)) x))", /duplicate variable/],
    ["'(lambda () (let ((:x 1)) :x))", /nonconstant variable/],
    ["'(lambda () (declare bad) 1)", /declaration/],
    ["'(lambda () (declare (special 7)) 1)", /SPECIAL declaration/],
    ["'(lambda () (declare (type integer :bad)) 1)", /TYPE declaration variable/],
    ["'(lambda () (declare (optimize (safety 4))) 1)", /level from 0 through 3/],
    ["'(lambda (x) (declare (type (integer (0 1) 9) x)) x)", /integer type bound/],
    ["'(lambda (x) (declare (type (signed-byte -1) x)) x)", /byte width/],
    ["'(lambda () (the (mod 0) 0))", /positive integer modulus/],
    ["'(lambda () (if t))", /IF form/],
    ["'(lambda () (quote 1 2))", /exactly one argument/],
    ["'(lambda () (setq x))", /variable\/value pairs/],
    ["'(lambda () (flet ((f)) nil))", /local function definition/],
    ["'(lambda () (return-from 4))", /RETURN-FROM/],
    ["'(lambda () (load-time-value 1 :bad))", /read-only flag/],
  ]) reject(`(compile-native ${form})`, reason);

  reject(`(let ((parameters (list 'x)))
    (setf (cdr parameters) parameters)
    (compile-to-hir (list 'lambda parameters 'x)))`, /circular list/);
  reject(`(let ((form (list 'progn nil)))
    (setf (second form) form)
    (compile-to-hir (list 'lambda nil form)))`, /circular executable form/);
  reject(`(let ((bindings (list '(x 1))))
    (setf (cdr bindings) bindings)
    (compile-to-hir (list 'lambda nil (list 'let bindings 'x))))`, /circular list/);
  reject(`(compile-module-to-hir '((defun f)))`, /module definition/);
  reject(`(compile-native-module 4)`, /module definitions/);
  reject(String.raw`(compile-to-hir (make-string 100000 :initial-element #\x))`, /lambda expression/);
  reject(`(compile-module-to-hir '((defun f () 1)) :entry-points '(f . g))`, /entry points/);
  reject(`(compile-to-hir '(lambda () 1) :global-specials '(x . y))`, /global special variables/);
  vm.evaluate(`(defmacro input-bad-expansion () '(let ((x 1 2)) x)) (values)`);
  reject(`(compile-native '(lambda () (input-bad-expansion)))`, /binding/);
  reject(`(let ((*compile-evaluations* t)) (eval '(let ((x 1 2)) x)))`, /binding/);
  reject(`(let ((*compile-evaluations* t)) (eval '(input-bad-expansion)))`, /binding/);
  vm.mountFile('/bad-source.lisp', '(defun bad-boundary-file () (let ((x 1 2)) x))');
  reject(`(compile-file "/bad-source.lisp")`, /binding/);

  for (const source of ['(verify-hir 4)', '(emit-native-module #())', '(infer-hir-types nil)'])
    reject(source, /HIR unit/);
  reject(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 1))))
      (type (list 'and 'integer))
      (value (first (hir-function-values (first (hir-unit-functions unit))))))
    (setf (second type) type (hir-value-type value) type) (infer-hir-types unit))`, /circular type/);
  reject(`(let* ((type (list 'or 'integer)))
    (setf (cdr (last type)) type)
    (compile-to-hir (list 'lambda '(x) (list 'declare (list 'type type 'x)) 'x)))`, /circular list/);
  for (const [mutation, reason] of [
    ['(setf (first (hir-unit-functions unit)) #())', /function record/],
    ['(setf (hir-function-value-count function) -1)', /counts/],
    ['(setf (hir-function-value-count function) #xffffffff)', /counts/],
    ['(setf (hir-function-parent function) function)', /parent chain/],
    ['(setf (hir-block-id block) -1)', /block record or ID/],
    ['(setf (hir-block-id block) 1)', /dense and ordered/],
    ['(setf (hir-block-id block) #xffffffff)', /dense and ordered/],
    ['(setf (hir-block-instructions block) 7)', /block instructions/],
    ['(setf (hir-block-terminator block) #())', /instruction record/],
    ['(setf (hir-instruction-inputs instruction) #(1))', /instruction operands/],
    ['(setf (hir-instruction-opcode instruction) :unknown)', /unknown opcode/],
    ['(setf (hir-instruction-safety-level instruction) -1)', /safety level/],
    ['(setf (hir-instruction-attributes instruction) nil)', /attribute count/],
    ['(setf (hir-value-kind (first (hir-function-values function))) 4)', /kind/],
    ['(setf (hir-function-environment function) (copy-hir-value (hir-function-environment function)))', /value record/],
  ]) reject(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 1))))
      (function (first (hir-unit-functions unit)))
      (block (first (hir-function-blocks function)))
      (instruction (first (hir-block-instructions block))))
    ${mutation} (emit-native-module unit))`, reason);
  reject(`(let ((unit (compile-to-hir '(lambda () 1))))
    (emit-native-module unit :entries (list #())))`, /native entry point/);
  reject(`(let ((unit (compile-to-hir '(lambda () 1))))
    (infer-hir-types unit :entries (list #())))`, /type-inference entry point/);
  reject(`(let* ((body (list 'progn nil))
                 (source (list 'lambda nil
                   (list 'labels (list (list 'helper nil body))
                     '(declare (inline helper)) '(helper)))))
    (setf (second body) body) (compile-native source))`, /circular executable/);

  // Literal data and macro arguments have their own syntax. Do not mistake
  // dotted/circular data or a shared executable DAG for malformed code.
  vm.evaluate(`(defmacro input-dotted ((head . tail)) (list 'quote (cons head tail))) (values)`);
  reject(`(compile-native '(lambda () (if (member 1 (quote)) :yes :no)))`, /exactly one argument/);
  reject(`(compile-native '(lambda () (if (member 1 . bad) :yes :no)))`, /proper list|dotted/);
  reject(`(let* ((test (list 'member 1 '(quote (1))))
                  (source (list 'lambda nil (list 'if test :yes :no))))
    (setf (cdr (last test)) test) (compile-native source))`, /circular/);
  for (const literal of ["'(1 . 2)", "'#1=(1 . #1#)"])
    assert.equal(vm.evaluate(`(compiled-function-p
      (compile-native '(lambda (x) (if (member x ${literal}) :yes :no))))`).output, 'T\n');
  assert.equal(vm.evaluate(`(equal (funcall (compile-native '(lambda () (input-dotted (1 . 2))))) '(1 . 2))`).output, 'T\n');
  assert.equal(vm.evaluate(`(let ((object (list :cycle)))
    (setf (cdr object) object)
    (eq object (funcall (compile-native (list 'lambda nil (list 'quote object))))))`).output, 'T\n');
  assert.equal(vm.evaluate(`(let ((shared '(+ x 1)))
    (equal (funcall (compile-native (list 'lambda '(x) (list 'list shared shared))) 1) '(2 2)))`).output, 'T\n');
  // Diagnostic strings remain fresh even when their image bytes are pooled.
  // Mutating one condition must not damage subsequent errors or output text.
  assert.equal(vm.evaluate(`(flet ((reason ()
      (handler-case (read-hir-code-instruction (string (code-char 63)) 0 (make-array 8))
        (hir-code-input-error (condition) (hir-code-error-reason condition)))))
    (let ((first (reason)) (second (reason)))
      (and (string= first "unknown HIR opcode") (string= second first) (not (eq first second))
           (progn (setf (char first 0) #\\X)
             (string= second (reason))))))`).output, 'T\n');
  console.log('Compiler boundaries: malformed source, macroexpansions and HIR produce bounded Lisp diagnostics; compilation recovers.');
} finally {vm.close();}
