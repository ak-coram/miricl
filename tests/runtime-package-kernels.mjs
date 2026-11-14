import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
let checks = 0;
const check = (source, expected = 'T') => {
  assert.equal(vm.evaluate(source).output, `${expected}\n`, source);
  checks++;
};
try {
  check(`(and (eq (find-package :cl) (find-package :common-lisp))
              (member (find-package :cl-user) (package-used-by-list :cl))
              (not (null (member (find-package :cl) (list-all-packages)))))`);
  check(`(let* ((names (list :port-nick-one "PORT-NICK-TWO" #\\Z))
                (p (make-package "PORT-NAMED" :use nil :nicknames names)))
           (unwind-protect
             (and (equal (package-nicknames p) '("PORT-NICK-ONE" "PORT-NICK-TWO" "Z"))
                  (eq p (find-package :port-nick-one)) (equal names '(:port-nick-one "PORT-NICK-TWO" #\\Z)))
             (delete-package p)))`);
  check(`(let ((names (list "CIRCULAR-NICK")))
           (setf (cdr names) names)
           (handler-case (make-package "PORT-CIRCULAR" :use nil :nicknames names)
             (type-error () t)))`);
  check(`(handler-case (make-package "PORT-DOTTED" :use nil :nicknames '("NICK" . :bad))
            (type-error () t))`);
  check(`(null (find-package "PORT-CIRCULAR"))`);
  check(`(let* ((backing (copy-seq "xxVIEW-NICKyy"))
                (view (make-array 9 :element-type 'character :displaced-to backing :displaced-index-offset 2))
                (p (make-package "PORT-VIEW" :use nil :nicknames (list view))))
           (unwind-protect
             (progn (setf (char backing 2) #\\X)
                    (and (string= (car (package-nicknames p)) "VIEW-NICK")
                         (eq p (find-package "VIEW-NICK"))
                         (null (find-package "XIEW-NICK"))))
             (delete-package p)))`);
  check(`(let ((a (make-package "PORT-USED" :use nil))
                (b (make-package "PORT-USER" :use nil)))
           (unwind-protect
             (progn (use-package a b)
                    (and (equal (package-used-by-list a) (list b))
                         (progn (unuse-package a b) (null (package-used-by-list a)))))
             (delete-package b) (delete-package a)))`);
  check(`(let ((p (make-package "PORT-DELETED" :use nil)))
           (delete-package p) (and (null (package-name p)) (not (member p (list-all-packages)))))`);
  check(`(progn (defpackage port-options (:use) (:shadow a b) (:export a b) (:intern c) (:size 16)
                           (:documentation "Package worker documentation"))
                (let ((p (find-package :port-options)))
                  (unwind-protect
                    (and (eq (nth-value 1 (find-symbol "A" p)) :external)
                         (eq (nth-value 1 (find-symbol "C" p)) :internal)
                         (string= (documentation p t) "Package worker documentation"))
                    (delete-package p))))`);
  for (const options of ['(:shadow a) (:intern a)', '(:intern a) (:export a)',
                         '(:export a) (:intern a)', '(:size 1) (:size 2)',
                         '(:documentation "A") (:documentation "B")', '(:not-an-option)']) {
    check(`(handler-case (eval '(defpackage port-invalid (:use) ${options})) (program-error () t))`);
  }
  check(`(null (find-package :port-invalid))`);
  check(`(let* ((a (make-package "PORT-LOOKUP-A" :use nil :nicknames '("PORT-LOOKUP-NICK")))
                (b (make-package "PORT-LOOKUP-B" :use nil)))
           (unwind-protect
             (progn (import (list nil) a)
                    (let ((symbol (intern "PORT-SHARED" a)))
                      (import symbol b)
                      (and (eq a (find-package "PORT-LOOKUP-NICK"))
                           (null (find-package "port-lookup-nick"))
                           (equal (find-all-symbols "PORT-SHARED") (list symbol))
                           (member nil (find-all-symbols "NIL"))
                           (progn (delete-package b)
                                  (equal (find-all-symbols "PORT-SHARED") (list symbol))))))
             (when (package-name b) (delete-package b)) (delete-package a)))`);
  check(`(let ((source (make-package "PORT-LOCAL-SOURCE" :use nil)))
           (unwind-protect
             (progn (eval '(defpackage port-local-target (:use)
                             (:local-nicknames ("LOCAL-SOURCE" "PORT-LOCAL-SOURCE"))))
                    (let ((*package* (find-package :port-local-target)))
                      (and (eq source (find-package "LOCAL-SOURCE"))
                           (null (find-package "local-source")))))
             (when (find-package :port-local-target) (delete-package :port-local-target))
             (delete-package source)))`);
  check(`(let ((original (symbol-function 'package-name)))
           (unwind-protect
             (progn (setf (symbol-function 'package-name) (lambda (p) (declare (ignore p)) nil))
                    (and (eq (find-package :cl) (find-package "COMMON-LISP"))
                         (not (null (member (find-package :cl) (list-all-packages))))))
             (setf (symbol-function 'package-name) original)))`);
  check(`(let* ((p (make-package "PORT-APROPOS" :use nil))
                (names '("ALPHA" "alphabet" "BETA" "Αλφα" "µ-symbol" "Μ-SYMBOL" "🙂")))
           (unwind-protect
             (progn (dolist (name names) (intern name p))
                    (and (= 2 (length (apropos-list "alpha" p)))
                         (= 1 (length (apropos-list "αλ" p)))
                         (= (if (char-equal #\\µ #\\Μ) 2 1) (length (apropos-list "µ" p)))
                         (= (length names) (length (apropos-list "" p)))
                         (null (apropos-list "not-present" p))
                         (= 1 (length (apropos-list #\\🙂 p)))))
             (delete-package p)))`);
  check(`(= 42 (+ 40 2))`);
  console.log(`Package kernel checks passed (${checks} cases).`);
} finally { vm.close(); }
