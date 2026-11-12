import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createCompilerTestVM} from './compiler-test-vm.mjs';
const vm = await createCompilerTestVM(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  check(`(progn
    (defun hir-instructions (function)
      (mapcan (lambda (block) (copy-list (miricl-compiler::hir-block-instructions block)))
        (miricl-compiler::hir-function-blocks function)))
    (defun hir-operation-p (instruction opcode) (eq (miricl-compiler::hir-instruction-opcode instruction) opcode))
    t)`);
  check(`(let* ((unit (miricl-compiler:compile-to-hir
    '(lambda (args) (apply (lambda (x y) (+ x y)) args)))))
    (and (miricl-compiler:verify-hir unit) (= (length (miricl-compiler:hir-unit-functions unit)) 1)))`);
  check(`(let* ((unit (miricl-compiler:compile-to-hir
    '(lambda (x) (multiple-value-call (lambda (&rest args) args) (values x 19))))))
    (and (miricl-compiler:verify-hir unit) (= (length (miricl-compiler:hir-unit-functions unit)) 1)))`);
  check(`(let* ((unit (miricl-compiler:compile-to-hir
    '(lambda (x) (values (lambda () (incf x)) (lambda () x)))))
    (functions (miricl-compiler:hir-unit-functions unit))
    (binding (first (miricl-compiler::hir-function-bindings (first functions)))))
    (and (miricl-compiler:verify-hir unit) (= (length functions) 3)
      (miricl-compiler::hir-binding-captured-p binding)
      (miricl-compiler::hir-binding-assigned-p binding)
      (eq (first (miricl-compiler::hir-function-captures (second functions))) binding)
      (eq (first (miricl-compiler::hir-function-captures (third functions))) binding)))`);
  check(`(let* ((unit (miricl-compiler:compile-to-hir
    '(lambda (x) (lambda () (lambda () x)))))
    (functions (miricl-compiler:hir-unit-functions unit)))
    (and (= (length functions) 3)
      (eq (first (miricl-compiler::hir-function-captures (second functions)))
          (first (miricl-compiler::hir-function-captures (third functions))))))`);
  check(`(let* ((unit (miricl-compiler:compile-to-hir
    '(lambda (flag) (if flag (values 1 2) (values)))))
    (function (first (miricl-compiler:hir-unit-functions unit)))
    (join (find-if (lambda (block) (miricl-compiler::hir-block-parameters block))
            (miricl-compiler::hir-function-blocks function))))
    (and (= (length (miricl-compiler::hir-block-predecessors join)) 2)
      (eq (miricl-compiler::hir-value-kind (first (miricl-compiler::hir-block-parameters join))) :multiple)
      (eq (miricl-compiler::hir-value-kind (miricl-compiler::hir-function-environment function)) :dynamic)))`);
  check(`(let* ((unit (miricl-compiler:compile-to-hir
    '(lambda () (block escape (funcall (lambda () (return-from escape (values 1 2))))))))
    (functions (miricl-compiler:hir-unit-functions unit))
    (child (second functions))
    (exit (miricl-compiler::hir-block-terminator (miricl-compiler::hir-function-entry child))))
    (and (eq (miricl-compiler::hir-instruction-opcode exit) :block-return)
      (null (miricl-compiler::hir-instruction-attributes exit))
      (typep (first (miricl-compiler::hir-function-captures child)) 'miricl-compiler::hir-target)))`);
  check(`(let* ((unit (miricl-compiler:compile-to-hir
    '(lambda () (block escape (if (return-from escape 1) 2 3)))))
    (function (first (miricl-compiler:hir-unit-functions unit))))
    (and (miricl-compiler:verify-hir unit)
      (eq (miricl-compiler::hir-instruction-opcode
            (miricl-compiler::hir-block-terminator (miricl-compiler::hir-function-entry function))) :block-return)))`);
  check(`(progn (defvar *hir-special* 0)
    (let* ((unit (miricl-compiler:compile-to-hir '(lambda (*hir-special*) *hir-special*)))
      (function (first (miricl-compiler:hir-unit-functions unit))))
      (and (find-if (lambda (i) (hir-operation-p i :dynamic-bind)) (hir-instructions function))
           (find-if (lambda (i) (hir-operation-p i :global-read)) (hir-instructions function)) t)))`);
  check(`(let ((unit (miricl-compiler:compile-to-hir '(lambda () (error "Must not execute at compile time.")))))
    (miricl-compiler:verify-hir unit))`);
  console.log('HIR: shared mutable captures, transitive capture, multiple-value joins, dynamic state, lexical nonlocal targets and unreachable paths pass.');
} finally {vm.close();}
