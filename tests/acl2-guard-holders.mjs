import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {portAcl2Source} from '../acl2/host.mjs';
import {iterativeGuardHolders, iterativeGuardHolderLists} from '../acl2/guard-holders.mjs';
import {loadAcl2TestCore} from './acl2-test-core.mjs';
const imagePath = process.argv[2] ?? 'miricl.wasm';
const corePath = process.argv[3] ?? 'build/acl2/acl2-native.wasm';
const original = await fs.readFile('vendor/acl2/translate.lisp', 'utf8');
const start = original.indexOf('(mutual-recursion\n\n(defun remove-guard-holders1 (');
const end = original.indexOf('\n(defun remove-guard-holders-weak (', start);
const listsStart = original.indexOf('(defun remove-guard-holders1-lst-lst (');
const listsEnd = original.indexOf('\n(defun remove-guard-holders-weak-lst-lst (', listsStart);
const oracle = (original.slice(start, end) + '\n' + original.slice(listsStart, listsEnd))
  .replaceAll('remove-guard-holders1', 'recursive-guard-holders1');
assert.throws(() => portAcl2Source('translate.lisp', Buffer.from(original.replace(
  '(defun remove-guard-holders1 (changedp0 term lamp)',
  '(defun remove-guard-holders1 (changedp0 term lamp)\n; changed'))), /source changed/u);
const {vm} = await loadAcl2TestCore(imagePath, corePath);
const evaluate = source => vm.evaluateAsync(source, {eof: true});
try {
  console.log('ACL2 guard holders: real native core loaded.');
  vm.mountFile('/oracle.lisp', '(in-package "ACL2")\n' + oracle);
  const adapted = portAcl2Source('translate.lisp', Buffer.from(original));
  const adaptedStart = adapted.lastIndexOf('#+(and miricl (not acl2-loop-only))',
    adapted.indexOf('(defun miricl-remove-guard-holders-work'));
  vm.mountFile('/logical-guard-holders.lisp', adapted.slice(adaptedStart,
    adapted.indexOf('\n(defun remove-guard-holders-weak (', adaptedStart)));
  vm.mountFile('/guard-holders.lisp', `(in-package "ACL2")\n${iterativeGuardHolders}\n${iterativeGuardHolderLists}`);
  await evaluate(`(load "/oracle.lisp") (load "/guard-holders.lisp")
    (compile-file "/guard-holders.lisp" :output-file "/guard-holders.wasm")
    (miricl-compiler:compile-native-bundle "/guard-holders.wasm" "/guard-holders-native.wasm" :load t)
    (in-package :acl2)
    (dolist (features (list (cons :acl2-loop-only *features*) (remove :miricl *features*)))
      (let ((*features* features))
        (with-open-file (stream "/logical-guard-holders.lisp")
          (assert (equal (read stream) '${original.slice(start, end)}))
          (assert (eq (read stream nil :eof) :eof)))))
    (defparameter *miricl-block-hide* t)
    (setf (symbol-function 'remove-guard-holders-blocked-by-hide-p) (lambda () *miricl-block-hide*))
    (defun compare-guard-holders (term)
      (dolist (lamp '(nil t))
        (dolist (*miricl-block-hide* '(nil t))
          (dolist (force '(nil t :sentinel))
            (let ((expected (multiple-value-list (recursive-guard-holders1 force term lamp)))
                  (actual (multiple-value-list (remove-guard-holders1 force term lamp))))
              (assert (equal expected actual) () "Term mismatch: ~S ~S ~S ~S" term lamp expected actual)
              (when (eq (second expected) term) (assert (eq (second actual) term)))))
          (dolist (pair '((recursive-guard-holders1-lst remove-guard-holders1-lst)
                         (recursive-guard-holders1-lst-lst remove-guard-holders1-lst-lst)))
            (let* ((items (if (eq (first pair) 'recursive-guard-holders1-lst) (list term 'x) (list (list term 'x) nil)))
                   (expected (multiple-value-list (funcall (first pair) items lamp)))
                   (actual (multiple-value-list (funcall (second pair) items lamp))))
              (assert (equal expected actual))
              (when (eq (second expected) items) (assert (eq (second actual) items))))))))
    (values)`);
  console.log('ACL2 guard holders: iterative functions compiled; comparing oracle.');
  let state = 91;
  const random = n => {state = (Math.imul(state, 1664525) + 1013904223) >>> 0; return state % n;};
  const term = depth => {
    if (!depth) return ['x', 'y', "'nil", "'1", "'(a b)"][random(5)];
    const a = () => term(depth - 1);
    switch (random(10)) {
      case 0: return `(return-last 'mbe1-raw (ignored x) ${a()})`;
      case 1: return `(cons-with-hint ${a()} ${a()} (ignored x))`;
      case 2: return `(hide ${a()})`;
      case 3: return `(from-df ${a()})`;
      case 4: return `(to-df ${a()})`;
      case 5: return `((lambda (x y) ${a()}) ${a()} ${a()})`;
      default: return `(f ${a()} ${a()})`;
    }
  };
  const cases = ['x', "'(a b)", '(df0)', '(df1)', "(to-df '1d0)", "(to-df '1)",
    '(the-check x y (from-df z))', '(mv-list x (return-last x y z))',
    '((lambda (var) (the-check x y var)) (from-df z))',
    "((lambda (x y) (return-last 'mbe1-raw (exec x) (logic x y))) (a z) (b z))",
    '((lambda (x) x) (from-df y))', '((lambda (x y) (f x y)) x y)',
    "(do$ a b c d e 'payload)", "(do$ a b c d e 'nil)", '(do$ a b c d e x)',
    "(cons 'a '(b c))", ...Array.from({length: 200}, () => term(3))];
  for (let i = 0; i < cases.length; i += 10)
    await evaluate(`(dolist (term '(${cases.slice(i, i + 10).join('\n')})) (compare-guard-holders term)) (values)`);
  console.log(`ACL2 guard holders: ${cases.length * 20} oracle comparisons pass.`);
  assert.equal((await evaluate(`
    (let ((deep 'x))
      (dotimes (i 100000) (setq deep (list 'f deep)))
      (multiple-value-bind (changed result) (remove-guard-holders1 nil deep nil)
        (assert (not changed)) (assert (eq result deep))))
    (let ((deep '(from-df x)))
      (dotimes (i 100000) (setq deep (list 'f deep)))
      (multiple-value-bind (changed result) (remove-guard-holders1 nil deep nil)
        (assert changed)
        (dotimes (i 100000) (assert (eq (car result) 'f)) (setq result (cadr result)))
        (assert (eq result 'x))))
    (let* ((wide (make-list 100000 :initial-element 'x)) (term (cons 'f wide)))
      (multiple-value-bind (changed result) (remove-guard-holders1 nil term nil)
        (assert (not changed)) (assert (eq result term))))
    (let* ((suffix (list 'x 'y)) (items (cons '(from-df z) suffix)))
      (multiple-value-bind (changed result) (remove-guard-holders1-lst items nil)
        (assert changed) (assert (eq (cdr result) suffix)) (assert (eq (car result) 'z))))
    t`)).output, 'NIL\nNIL\nNIL\nNIL\nT\n');
  await fs.writeFile('build/acl2/guard-holders-patch.wasm', await vm.readFile('/guard-holders-native.wasm'));
  await fs.writeFile('build/acl2/guard-holders-patch-portable.wasm', await vm.readFile('/guard-holders.wasm'));
  console.log('ACL2 guard holders: 100,000-level terms, 100,000 arguments, unchanged identity and shared suffixes pass.');
} finally {vm.close();}
