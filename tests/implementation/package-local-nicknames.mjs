// The optional API and native reader share one checked, saved registry.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = (instance, source) => assert.equal(
  instance.evaluate(source, {eof: true}).output, 'T\n', source);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)', {eof: true});
  vm.mountFile('/package-local-nicknames.lisp', await fs.readFile(
    new URL('../../compatibility/package-local-nicknames.lisp', import.meta.url), 'utf8'));
  vm.evaluate('(load "/package-local-nicknames.lisp") (values)', {eof: true});
  check(vm, `(progn
    (defpackage :local-names-a (:use) (:export :sentinel))
    (defpackage :local-names-b (:use) (:export :sentinel))
    (defpackage :local-names-owner-a (:use)
      (:local-nicknames (:scoped :local-names-a)))
    (defpackage :local-names-owner-b (:use)
      (:local-nicknames (:scoped :local-names-b)))
    (and (null (find-package :scoped))
      (every (lambda (pair)
        (let ((*package* (find-package (car pair))))
          (and (eq (find-package :scoped) (find-package (cdr pair)))
            (eq (read-from-string "SCOPED:SENTINEL")
              (find-symbol "SENTINEL" (cdr pair))))))
        '((:local-names-owner-a . :local-names-a)
          (:local-names-owner-b . :local-names-b)))))`);
  check(vm, `(let* ((owner (find-package :local-names-owner-a))
      (target (find-package :local-names-a)) (name (copy-seq "OTHER")))
    (and (eq (miricl-packages:add-package-local-nickname name target owner) owner)
      (eq (miricl-packages:add-package-local-nickname name target owner) owner)
      (progn (setf (char name 0) #\\X)
        (let ((copy (miricl-packages:package-local-nicknames owner)))
          (dolist (entry copy) (setf (char (car entry) 0) #\\X))
          (rplacd (car copy) owner) (rplacd copy copy))
        (eq (let ((*package* owner)) (find-package "OTHER")) target))
      (handler-case
          (progn (miricl-packages:add-package-local-nickname
            "OTHER" :local-names-b owner) nil)
        (package-error () t))
      (miricl-packages:remove-package-local-nickname "OTHER" owner)
      (not (miricl-packages:remove-package-local-nickname "OTHER" owner))))`);
  check(vm, `(let* ((owner (find-package :local-names-owner-a))
      (bridge (miricl-compiler::compiler-primitive 10 1 "%%PACKAGE-NODE-REF"))
      (cycle (list (cons "BAD" (find-package :local-names-b)))))
    (setf (cdr cycle) cycle)
    (and (handler-case (progn (funcall bridge owner 14 cycle) nil) (error () t))
      (handler-case
          (progn (eval '(defpackage :local-names-invalid (:use)
            (:local-nicknames (:good :local-names-a) (:bad)))) nil)
        (error () t))
      (not (find-package :local-names-invalid))
      (eq (let ((*package* owner)) (find-package :scoped))
        (find-package :local-names-a)) (= (+ 20 22) 42)))`);
  check(vm, `(let ((pair (list :bad :local-names-a)))
    (setf (cdr pair) pair)
    (and (handler-case
        (progn (eval (list 'defpackage :local-names-cyclic
          (list :local-nicknames pair))) nil) (error () t))
      (null (find-package :local-names-cyclic)) (= (+ 20 22) 42)))`);
  check(vm, `(progn
    (miricl-packages:add-package-local-nickname
      "LOCAL-NAMES-A" :local-names-b :local-names-owner-a)
    (let* ((a (find-symbol "SENTINEL" :local-names-a))
           (b (find-symbol "SENTINEL" :local-names-b))
           (*package* (find-package :local-names-owner-a)) (*print-readably* t))
      (every (lambda (symbol)
        (eq symbol (read-from-string (write-to-string symbol)))) (list a b))))`);
  check(vm, `(let ((symbol (find-symbol "SENTINEL" :local-names-a))
      (owner (find-package :local-names-owner-a)))
    (miricl-packages:remove-package-local-nickname :scoped owner)
    (unwind-protect
        (let ((*package* owner) (*print-readably* t))
          (handler-case (progn (write-to-string symbol) nil)
            (print-not-readable () t)))
      (miricl-packages:add-package-local-nickname :scoped :local-names-a owner)))`);
  // Finish with no host values in retained history before saving the state.
  vm.evaluate('(values)', {eof: true});
  const restored = await Miricl.instantiate(vm.saveImage());
  try {
    check(restored, `(let* ((target (find-package :local-names-a))
        (*package* (find-package :local-names-owner-a)))
      (and (eq (find-package :scoped) target)
        (eq target (symbol-package (read-from-string "SCOPED:SENTINEL")))
        (miricl-packages:remove-package-local-nickname :scoped)
        (null (find-package :scoped))))`);
    check(restored, `(progn (delete-package :local-names-b)
      (and (null (miricl-packages:package-local-nicknames :local-names-owner-b))
        (let ((target (find-package :local-names-a))
              (*package* (find-package :local-names-owner-a)))
          (eq (find-package "LOCAL-NAMES-A") target))))`);
  } finally { restored.close(); }
} finally { vm.close(); }
console.log('Package-local nickname checks passed.');
