import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
let checks = 0;
const check = source => { try { assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source); checks++; } catch (error) { error.message += `\n${source}`; throw error; } };
try {
  for (const types of [':internal', ':external', ':inherited', ':internal :external', ':internal :inherited', ':external :inherited', ':internal :external :inherited']) {
    check(`(let ((a (make-package "ENUM-A" :use nil)) (b (make-package "ENUM-B" :use nil))
                 (p (make-package "ENUM-P" :use nil)))
      (unwind-protect
        (let ((shared (intern "SHARED" a)) (results nil))
          (export shared a) (import shared b) (export shared b)
          (import (list nil) a) (export (list nil) a)
          (use-package (list a b) p) (shadow "SHADOWED" p)
          (export (intern "EXTERNAL" p) p)
          (with-package-iterator (next p ${types})
            (loop (multiple-value-bind (more symbol status package) (next)
              (unless more (return))
              (assert (eq p package))
              (assert (eq status (nth-value 1 (find-symbol (symbol-name symbol) p))))
              (push (list symbol status) results)))
            (assert (equal '(nil) (multiple-value-list (next)))))
          (every (lambda (type)
            (let ((expected (case type (:internal (list (find-symbol "SHADOWED" p)))
                                     (:external (list (find-symbol "EXTERNAL" p)))
                                     (:inherited (list shared nil)))))
              (every (lambda (symbol) (find symbol results :test #'eq :key #'car)) expected)))
            '(${types})))
        (delete-package p) (delete-package b) (delete-package a)))`);
  }
  check(`(let ((p (make-package "ENUM-REMOVE" :use nil)))
    (unwind-protect
      (progn (dolist (name '("ONE" "TWO" "THREE")) (intern name p))
        (let ((seen nil))
          (with-package-iterator (next p :internal)
            (loop (multiple-value-bind (more symbol status package) (next)
              (unless more (return)) (assert (eq status :internal)) (assert (eq p package))
              (push (symbol-name symbol) seen) (unintern symbol p))))
          (and (= 3 (length seen)) (let ((left nil)) (do-symbols (s p) (push s left)) (null left)))))
      (delete-package p)))`);
  check(`(let ((p (make-package "ENUM-NESTED" :use nil)) (calls 0))
    (unwind-protect
      (progn (import (list nil) p)
        (with-package-iterator (outer (progn (incf calls) p) :internal)
          (with-package-iterator (inner (list p) :internal)
            (and (= calls 1) (equal (multiple-value-list (outer)) (list t nil :internal p))
                 (equal (multiple-value-list (inner)) (list t nil :internal p))
                 (equal '(nil) (multiple-value-list (outer))) (null (inner))))))
      (delete-package p)))`);
  check(`(let ((a (make-package "ENUM-ALL-A" :use nil)) (b (make-package "ENUM-ALL-B" :use nil)))
    (unwind-protect
      (let ((symbol (intern "ENUM-UNIQUE-ALL" a)) (seen nil))
        (import symbol b) (do-all-symbols (s) (when (eq s symbol) (push s seen)))
        (= 1 (length seen)))
      (delete-package b) (delete-package a)))`);
  check(`(let ((a (make-package "ENUM-ALIAS-A" :use nil)) (p (make-package "ENUM-ALIAS-P" :use nil))
               (fresh nil))
    (unwind-protect
      (let ((*package* p))
        (eval '(defpackage "ENUM-ALIAS-P" (:use)
          (:local-nicknames ("ENUM-FREE-GLOBAL" "ENUM-ALIAS-A")
                            ("ENUM-FREE-RENAME" "ENUM-ALIAS-A"))))
        (setq fresh (make-package "ENUM-FREE-GLOBAL" :use nil))
        (and (not (eq fresh a)) (eq p (rename-package p "ENUM-FREE-RENAME"))
             (eq a (find-package "ENUM-FREE-GLOBAL"))
             (eq fresh (let ((*package* (find-package :cl-user))) (find-package "ENUM-FREE-GLOBAL")))))
      (when fresh (delete-package fresh)) (delete-package p) (delete-package a)))`);
  check(`(let ((a (make-package "ENUM-RENAME-A" :use nil)) (p (make-package "ENUM-RENAME-P" :use nil)))
    (unwind-protect
      (and (handler-case (rename-package p "ENUM-RENAME-A") (package-error () t))
           (string= "ENUM-RENAME-P" (package-name p))
           (eq p (rename-package p "ENUM-RENAME-FREE" '("ENUM-RENAME-NICK")))
           (eq p (find-package "ENUM-RENAME-NICK")))
      (delete-package p) (delete-package a)))`);
  for (const source of [
    `(let ((cycle (list :cl))) (setf (cdr cycle) cycle)
       (handler-case (with-package-iterator (next cycle :external) (next)) (type-error () t)))`,
    `(handler-case (with-package-iterator (next '(:cl . :bad) :external) (next)) (type-error () t))`,
    `(handler-case (with-package-iterator (next "ENUM-MISSING" :external) (next)) (package-error () t))`,
    `(equal (multiple-value-list (multiple-value-prog1 (values :kept :second)
       (with-package-iterator (next nil :internal) (next)))) '(:kept :second))`,
  ]) check(source);
  if (vm.vm['test-package-enumeration-install']) {
    vm.vm['test-package-enumeration-install']();
    for (const state of ['nil', '42', '#(1)', '#(0 nil nil nil nil 0 nil 0 0 0)'])
      check(`(handler-case (funcall *package-enumeration-next* ${state}) (error () t))`);
    check(`(let* ((p (make-package "ENUM-CALLBACKS" :use nil))
                  (state (funcall *package-enumeration-state* p 1)) (callbacks (svref state 6)))
      (unwind-protect
        (progn (setf (svref state 6) (vector (lambda (&rest args) (declare (ignore args)) :bad)
                                           #'list #'list #'symbol-name))
          (and (handler-case (funcall *package-enumeration-next* state) (type-error () t))
               (progn (setf (svref state 6) callbacks) (null (funcall *package-enumeration-next* state)))))
        (delete-package p)))`);
    check(`(let* ((p (make-package "ENUM-CALLBACK-SLOTS" :use nil))
                  (state (funcall *package-enumeration-state* p 1))
                  (callbacks (svref state 6)) (field (svref callbacks 0)))
      (unwind-protect
        (progn (setf (svref callbacks 0) (lambda (&rest args) (declare (ignore args)) :bad))
          (and (handler-case (funcall *package-enumeration-next* state) (type-error () t))
               (progn (setf (svref callbacks 0) field) (null (funcall *package-enumeration-next* state)))
               (null (funcall *package-enumeration-next* (funcall *package-enumeration-state* p 1)))))
        (setf (svref callbacks 0) field) (delete-package p)))`);
    check(`(let* ((p (make-package "ENUM-CORRUPT" :use nil))
                  (state (funcall *package-enumeration-state* p 1))
                  (cycle (list p)))
      (unwind-protect
        (progn (setf (cdr cycle) cycle (svref state 1) cycle)
          (and (handler-case (funcall *package-enumeration-next* state) (type-error () t))
               (progn (setf (svref state 1) nil) (null (funcall *package-enumeration-next* state)))))
        (delete-package p)))`);
    check(`(let* ((p (make-package "ENUM-REDEFINED-LENGTH" :use nil))
                  (state (funcall *package-enumeration-state* p 1))
                  (cycle (list p)) (original (symbol-function 'list-length)) (calls 0))
      (unwind-protect
        (progn (setf (cdr cycle) cycle (svref state 1) cycle)
          (setf (symbol-function 'list-length)
            (lambda (value) (if (eq value cycle) (progn (incf calls) (values 0 :replaced))
                                (funcall original value))))
          (and (equal '(0 :replaced) (multiple-value-list (list-length cycle)))
               (handler-case (funcall *package-enumeration-next* state) (type-error () t))
               (= calls 1)))
        (setf (symbol-function 'list-length) original (svref state 1) nil)
        (delete-package p)))`);
  }
  check('(= 42 (+ 40 2))');
  console.log(`Package enumeration checks passed (${checks} cases).`);
} finally { vm.close(); }
