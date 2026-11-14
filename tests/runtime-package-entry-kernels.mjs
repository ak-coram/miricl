import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
let checks = 0;
const check = source => {
  assert.equal(vm.evaluate(`(not (null ${source}))`).output, 'T\n', source);
  checks++;
};
try {
  check(`(let ((p (make-package "ENTRY-INDEX" :use nil)) (names nil))
    (unwind-protect
      (progn
        (dotimes (i 160) (push (format nil "ENTRY-~D-αλ🙂" i) names))
        (dolist (name names) (intern name p))
        (and (every (lambda (name) (eq (nth-value 1 (find-symbol name p)) :internal)) names)
             (every (lambda (name) (null (find-symbol (string-downcase name) p))) names)
             (progn (dolist (name names) (unintern (find-symbol name p) p))
                    (every (lambda (name) (null (nth-value 1 (find-symbol name p)))) names))))
      (delete-package p)))`);
  check(`(let ((a (make-package "ENTRY-A" :use nil)) (b (make-package "ENTRY-B" :use nil))
               (p (make-package "ENTRY-P" :use nil)))
    (unwind-protect
      (let ((symbol (intern "SHARED" a)))
        (export symbol a) (import symbol b) (export symbol b)
        (use-package (list a b) p)
        (and (eq symbol (find-symbol "SHARED" p))
             (eq :inherited (nth-value 1 (find-symbol "SHARED" p)))
             (progn (import symbol p) (eq :internal (nth-value 1 (find-symbol "SHARED" p))))
             (progn (export symbol p) (eq :external (nth-value 1 (find-symbol "SHARED" p))))
             (progn (unexport symbol p) (eq :internal (nth-value 1 (find-symbol "SHARED" p))))
             (progn (unintern symbol p) (eq :inherited (nth-value 1 (find-symbol "SHARED" p))))))
      (delete-package p) (delete-package b) (delete-package a)))`);
  check(`(let ((a (make-package "ENTRY-CONFLICT-A" :use nil)) (b (make-package "ENTRY-CONFLICT-B" :use nil))
               (p (make-package "ENTRY-CONFLICT-P" :use nil)))
    (unwind-protect
      (let ((x (intern "SAME" a)) (y (intern "SAME" b)))
        (export x a) (export y b) (use-package a p)
        (and (handler-case (use-package b p) (package-error () t))
             (not (member b (package-use-list p)))
             (eq x (find-symbol "SAME" p))
             (progn (shadowing-import y p) (use-package b p)
                    (and (eq y (find-symbol "SAME" p)) (member y (package-shadowing-symbols p))))))
      (delete-package p) (delete-package b) (delete-package a)))`);
  check(`(let ((a (make-package "ENTRY-NIL-A" :use nil)) (p (make-package "ENTRY-NIL-P" :use nil)))
    (unwind-protect
      (progn (import (list nil) a) (export (list nil) a) (use-package a p)
        (and (null (find-symbol "NIL" p)) (eq :inherited (nth-value 1 (find-symbol "NIL" p)))
             (progn (shadow "NIL" p) (and (find-symbol "NIL" p)
                                          (eq :internal (nth-value 1 (find-symbol "NIL" p)))))
             (progn (shadowing-import (list nil) p)
                    (and (null (find-symbol "NIL" p)) (eq :internal (nth-value 1 (find-symbol "NIL" p)))
                         (member nil (package-shadowing-symbols p))))))
      (delete-package p) (delete-package a)))`);
  check(`(let ((p (make-package "ENTRY-REDEFINED" :use nil)) (original (symbol-function 'symbol-name)))
    (unwind-protect
      (let ((symbol (intern "CANONICAL" p)))
        (setf (symbol-function 'symbol-name) (lambda (symbol) (declare (ignore symbol)) "WRONG"))
        (eq symbol (find-symbol "CANONICAL" p)))
      (setf (symbol-function 'symbol-name) original) (delete-package p)))`);
  if (vm.vm['test-package-entry-install']) {
    vm.vm['test-package-entry-install']();
    for (const args of [':bad 0 nil', '(find-package :cl) 11 nil', '(find-package :cl) -1 nil',
                        '(find-package :cl) (ash 1 100) nil', '(find-package :cl) 1 -1',
                        '(find-package :cl) 1 (ash 1 100)', '(find-package :cl) 4 nil']) {
      check(`(handler-case (funcall *package-entry-fields* ${args}) (package-error () t))`);
    }
    for (const args of ['', 'nil', 'nil 0', 'nil 0 nil nil']) {
      check(`(handler-case (funcall *package-entry-fields* ${args}) (program-error () t))`);
    }
    check(`(let* ((a (make-package "ENTRY-CYCLE-A" :use nil))
                  (p (make-package "ENTRY-CYCLE-P" :use (list a)))
                  (uses (funcall *package-entry-fields* p 2 nil)) (tail (cdr uses)))
      (unwind-protect
        (progn (setf (cdr uses) uses)
          (and (handler-case (find-symbol "MISSING" p) (type-error () t))
               (progn (setf (cdr uses) tail) (null (find-symbol "MISSING" p)))))
        (setf (cdr uses) tail) (delete-package p) (delete-package a)))`);
  }
  check('(= 42 (+ 40 2))');
  console.log(`Package entry checks passed (${checks} cases).`);
} finally { vm.close(); }
