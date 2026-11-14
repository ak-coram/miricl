import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
import {portAcl2Source} from '../acl2/host.mjs';
const bytes = await fs.readFile('vendor/acl2/translate.lisp'), source = bytes.toString();
const start = source.indexOf('(defun all-fnnames1 (');
const end = source.indexOf('\n(defmacro all-fnnames (', start);
const original = source.slice(start, end), adapted = portAcl2Source('translate.lisp', bytes);
const portStart = adapted.lastIndexOf('#+(and miricl (not acl2-loop-only))',
  adapted.indexOf('(defun all-fnnames1 ('));
const definitions = adapted.slice(portStart, adapted.indexOf('\n(defmacro all-fnnames (', portStart));
const execBytes = await fs.readFile('vendor/acl2/defuns.lisp'), execSource = execBytes.toString();
const execStart = execSource.indexOf('(defun all-fnnames1-exec (');
const execOriginal = execSource.slice(execStart, execSource.indexOf('\n(defmacro all-fnnames-exec (', execStart));
const execAdapted = portAcl2Source('defuns.lisp', execBytes);
const execPortStart = execAdapted.lastIndexOf('#+(and miricl (not acl2-loop-only))',
  execAdapted.indexOf('(defun all-fnnames1-exec ('));
const execDefinitions = execAdapted.slice(execPortStart,
  execAdapted.indexOf('\n(defmacro all-fnnames-exec (', execPortStart));
assert.ok(portStart > 0);
assert.throws(() => portAcl2Source('translate.lisp', Buffer.from(source.replace(
  '(defun all-fnnames1 (flg x acc)', '(defun all-fnnames1 (flg x acc)\n; changed'))), /source changed/);
const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const evaluate = async text => vm.evaluateAsync(text, {eof: true});
try {
  await evaluate(`
    (defpackage :acl2 (:use :cl)) (in-package :acl2)
    (defmacro variablep (x) (list 'atom x))
    (defmacro nvariablep (x) (list 'consp x))
    (defmacro flambdap (x) (list 'consp x))
    (defmacro fargn (x n) (list 'nth n x))
    (defmacro fquotep (x) (list 'eq ''quote (list 'car x)))
    (defmacro flambda-applicationp (x) (list 'consp (list 'car x)))
    (defmacro lambda-body (x) (list 'caddr x))
    (defmacro ffn-symb (x) (list 'car x))
    (defmacro fargs (x) (list 'cdr x))
    (defun add-to-set-eq (item acc) (if (member item acc :test #'eq) acc (cons item acc)))
    ${original.replaceAll('all-fnnames1', 'recursive-fnnames')}
    ${execOriginal.replaceAll('all-fnnames1-exec', 'recursive-exec-fnnames')}
    ${definitions}
    ${execDefinitions}
    (defun compare-walkers (flg term seed)
      (let* ((saved (copy-tree term)) (expected (recursive-fnnames flg term seed))
             (actual (all-fnnames1 flg term seed)))
        (assert (equal expected actual)) (assert (equal saved term))
        (assert (eq (last actual (length seed)) seed)))
      (let* ((saved (copy-tree term)) (expected (recursive-exec-fnnames flg term seed))
             (actual (all-fnnames1-exec flg term seed)))
        (assert (equal expected actual)) (assert (equal saved term))
        (assert (eq (last actual (length seed)) seed))))
    (values)`);
  vm.mountFile('/definitions.lisp', definitions);
  vm.mountFile('/exec-definitions.lisp', execDefinitions);
  await evaluate(`
    (dolist (features (list (cons :acl2-loop-only *features*) (remove :miricl *features*)))
      (let ((*features* features)) (with-open-file (s "/definitions.lisp")
        (assert (equal (read s) '${original})) (assert (eq (read s nil :eof) :eof)))))
    (compile-file "/definitions.lisp" :output-file "/walker.wasm")
    (load "/walker.wasm")
    (compile-file "/exec-definitions.lisp" :output-file "/exec-walker.wasm")
    (load "/exec-walker.wasm") (values)`);
  let state = 19;
  const random = n => {state = (Math.imul(state, 1664525) + 1013904223) >>> 0; return state % n;};
  const term = depth => !depth ? ['x', 'nil', "'(quoted (ignored x))"][random(3)]
    : random(4) ? `(f${random(8)} ${Array.from({length: random(5)}, () => term(depth - 1)).join(' ')})`
      : `((lambda (x) ${term(depth - 1)}) ${term(depth - 1)} ${term(depth - 1)})`;
  const terms = ['nil', '(foo (bar x) (baz y) (bar z))', '((lambda (x) (body x)) (a x) (b x))',
    "(return-last 'mbe1-raw (exec x) (logical y))",
    "(return-last 'ec-call1-raw nil (guarded (arg x)))",
    "(return-last 'ec-call1-raw nil ((lambda (x) (body x)) (arg y)))",
    "(return-last 'ec-call1-raw nil 'constant)",
    ...Array.from({length: 500}, () => term(4))];
  for (let offset = 0; offset < terms.length; offset += 25)
    await evaluate(`(dolist (term '(${terms.slice(offset, offset + 25).join('\n')}))
      (dolist (seed '(nil (existing f1)))
        (compare-walkers nil term seed)
        (compare-walkers t (list term '(sibling x)) seed))) (values)`);
  await evaluate(`(let ((deep 'x) (wide nil))
    (dotimes (n 100000) (setq deep (list 'deep deep)) (push '(wide x) wide))
    (assert (equal (all-fnnames1 nil deep nil) '(deep)))
    (assert (equal (all-fnnames1 t wide nil) '(wide)))
    (assert (equal (all-fnnames1-exec nil deep nil) '(deep)))
    (assert (equal (all-fnnames1-exec t wide nil) '(wide)))) (values)`);
  console.log(`ACL2 function-name walkers: ${terms.length * 8} oracle comparisons; logical definition, order, sharing, FASL and 100,000-level/argument traversal passed.`);
} finally {vm.close();}
