// node tests/evaluation-policy.mjs [interpreter] [precompiled-image] [wasi-command]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {spawnSync} from 'node:child_process';
import {Miricl} from '../scripts/miricl.mjs';

for (const [file, compiled] of [[process.argv[2] ?? 'miricl.wasm', false],
  [process.argv[3] ?? 'miricl-precompiled.wasm', true]]) {
  const vm = await Miricl.instantiate(await fs.readFile(file));
  const output = source => vm.evaluate(source, {eof: true}).output;
  const check = source => assert.equal(output(source), 'T\n', `${file}: ${source}`);
  const mode = value => output(`(setq miricl-compiler:*compile-evaluations* ${value ? 't' : 'nil'})`);
  try {
    assert.equal(output('miricl-compiler:*compile-evaluations*'), compiled ? 'T\n' : 'NIL\n');
    check(`(eq (nth-value 1 (find-symbol "*COMPILE-EVALUATIONS*" "MIRICL-COMPILER")) :external)`);
    output('(defun policy-default (x) (+ x 2))');
    check(`(and (= (policy-default 40) 42)
      (eq (null (function-lambda-expression #'policy-default)) ${compiled ? 't' : 'nil'}))`);

    // A dynamic binding controls a nested EVAL without changing the global
    // preference, even when the enclosing form was compiled.
    mode(true);
    check(`(let ((miricl-compiler:*compile-evaluations* nil))
      (eval '(defun policy-interpreted (x) (+ x 3)))
      (not (null (function-lambda-expression #'policy-interpreted))))`);
    check('miricl-compiler:*compile-evaluations*');
    mode(false);
    check(`(let ((miricl-compiler:*compile-evaluations* t))
      (eval '(defun policy-compiled (x) (+ x 4)))
      (and (null (function-lambda-expression #'policy-compiled)) (= (policy-compiled 38) 42)))`);
    check('(not miricl-compiler:*compile-evaluations*)');
    output(`(defvar *policy-special* :outside)
      (setf (symbol-value 'policy-global) 42)`);
    check(`(let ((miricl-compiler:*compile-evaluations* t)
                (*policy-special* 19) (policy-global :lexical))
      (declare (ignore policy-global))
      (and (= (eval 'policy-global) 42)
           (equal (multiple-value-list (eval '(values *policy-special* 23))) '(19 23))
           (null (multiple-value-list (eval '(values))))))`);
    check(`(let* ((object (list 42)) (miricl-compiler:*compile-evaluations* t))
      (setf (cdr object) object)
      (eq object (eval (list 'identity (list 'quote object)))))`);

    mode(true);
    output(`(defvar *policy-expansions* 0) (defvar *policy-effects* 0)
      (defmacro policy-once () (incf *policy-expansions*) '(incf *policy-effects*))`);
    assert.equal(output('(policy-once)'), '1\n');
    check('(and (= *policy-expansions* 1) (= *policy-effects* 1))');
    output(`(defmacro policy-expansion-mode ()
      (list 'quote miricl-compiler:*compile-evaluations*))`);
    check('(policy-expansion-mode)');
    check('(load-time-value miricl-compiler:*compile-evaluations*)');
    check(`(handler-case (progn (eval '(eval-when)) nil) (program-error () t))`);
    check(`(handler-case (eval '(progn (incf *policy-effects*) (error "Expected policy error")))
      (error () (= *policy-effects* 2)))`);
    check('miricl-compiler:*compile-evaluations*');

    // A failed compilation never executes a prefix or retries application
    // execution through the interpreter. Bind NIL for unsupported lowering.
    mode(false);
    check(`(let ((miricl-compiler:*compile-evaluations* t))
      (handler-case
        (progn (eval '(let () (incf *policy-effects*) (if))) nil)
        (error () (= *policy-effects* 2))))`);
    check('(not miricl-compiler:*compile-evaluations*)');

    // Top-level forms execute sequentially, including a policy change within
    // PROGN and macros introduced by an earlier form in the same source file.
    mode(true);
    output(`(progn (setq miricl-compiler:*compile-evaluations* nil)
                   (defun policy-after-change () 42))`);
    check(`(and (not miricl-compiler:*compile-evaluations*)
      (not (null (function-lambda-expression #'policy-after-change))))`);
    const source = `(defpackage :evaluation-policy-loaded (:use :cl))
      (in-package :evaluation-policy-loaded)
      (eval-when (:execute)
      (defmacro policy-loaded-plus (x) (list '+ x 2))
      (defun policy-loaded (x) (policy-loaded-plus x)))
      (setq miricl-compiler:*compile-evaluations* nil)
      (defun policy-loaded-interpreted () 42)`;
    output(`(register-source-file "/evaluation-policy.lisp"
      "${source.replaceAll('\\', '\\\\').replaceAll('"', '\\"')}")`);
    mode(true);
    assert.equal(output('(load "/evaluation-policy.lisp")'), 'T\n');
    check(`(and (not miricl-compiler:*compile-evaluations*)
      (eq *package* (find-package "CL-USER"))
      (= (evaluation-policy-loaded::policy-loaded 40) 42)
      (null (function-lambda-expression #'evaluation-policy-loaded::policy-loaded))
      (not (null (function-lambda-expression #'evaluation-policy-loaded::policy-loaded-interpreted))))`);
    mode(true);
    // Stream LOAD follows the same policy and retains its source-load scope.
    check(`(with-input-from-string (input "(defun policy-stream-loaded () 42)")
      (and (load input) (= (policy-stream-loaded) 42)
           (null (function-lambda-expression #'policy-stream-loaded))))`);
    output(`(register-source-file "/policy-fasl.lisp" "(defun policy-fasl () 42)")
      (compile-file "/policy-fasl.lisp" :output-file "/policy-fasl.wasm") (values)`);
    assert.equal(output('(load "/policy-fasl.wasm")'), 'T\n');
    check(`(and miricl-compiler:*compile-evaluations* (= (policy-fasl) 42)
      (not (null (function-lambda-expression #'policy-fasl))))`);
    assert.equal((await vm.evaluateAsync(`(defun policy-async (x) (+ x 2))
      (and (= (policy-async 40) 42) (null (function-lambda-expression #'policy-async)))`)).output,
      'POLICY-ASYNC\nT\n');
    console.log(`PASS ${file}: evaluation default, dynamic bindings, native definitions, EVAL values/identity, source and stream LOAD, sequential effects and errors`);
  } finally {vm.close();}
}

if (process.argv[4]) {
  const terminal = spawnSync(process.execPath, ['scripts/repl.mjs', process.argv[2] ?? 'miricl.wasm'], {
    input: 'miricl-compiler:*compile-evaluations*\n:quit\n', encoding: 'utf8', timeout: 30000,
  });
  assert.ifError(terminal.error);
  assert.equal(terminal.status, 0, terminal.stderr);
  assert.match(terminal.stdout, /NIL\n/);
  const result = spawnSync(process.env.WASMTIME ?? 'wasmtime', [
    'run', '-W', 'stack-switching=y', '-S', 'p3=y', process.argv[4],
  ], {input: 'miricl-compiler:*compile-evaluations*\n(+ 20 22)\n:quit\n', encoding: 'utf8', timeout: 30000});
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /miricl> NIL\nmiricl> 42\n/);
  console.log('PASS WASI: evaluation defaults to interpretation with the native compiler image installed');
}
