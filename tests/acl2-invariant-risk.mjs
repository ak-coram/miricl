// Compare the actual upstream walker and the port using controlled world-query
// fixtures. This checks traversal semantics, not ACL2 theorem proving.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
import {portAcl2Source} from '../acl2/host.mjs';

const bytes = fs.readFileSync('vendor/acl2/defuns.lisp');
const source = bytes.toString();
const start = source.indexOf('(defun all-fnnames1-invariant-risk (');
const end = source.indexOf('\n(defun put-invariant-risk (', start);
const original = source.slice(start, end);
const adapted = portAcl2Source('defuns.lisp', bytes);
const adaptedStart = adapted.lastIndexOf('#+(and miricl (not acl2-loop-only))',
  adapted.indexOf('(defun all-fnnames1-invariant-risk ('));
const definitions = adapted.slice(adaptedStart, adapted.indexOf('\n(defun put-invariant-risk (', adaptedStart));
assert.ok(adaptedStart > 0);
assert.throws(() => portAcl2Source('defuns.lisp', Buffer.from(source.replace(
  '(defun all-fnnames1-invariant-risk (flg x i wrld acc)',
  '(defun all-fnnames1-invariant-risk (flg x i wrld acc)\n; changed upstream'))), /source changed/);
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl-precompiled.wasm'));
const evaluate = async text => (await vm.evaluateAsync(text, {eof: true})).output;
try {
  await evaluate(`
    (defpackage :acl2 (:use :cl)) (in-package :acl2)
    (defvar *query-trace* nil)
    (defmacro variablep (x) (list 'atom x))
    (defmacro nvariablep (x) (list 'consp x))
    (defmacro fquotep (x) (list 'eq ''quote (list 'car x)))
    (defmacro unquote (x) (list 'cadr x))
    (defmacro flambda-applicationp (x) (list 'consp (list 'car x)))
    (defmacro flambdap (x) (list 'consp x))
    (defmacro lambda-body (x) (list 'caddr x))
    (defmacro ffn-symb (x) (list 'car x))
    (defmacro fargs (x) (list 'cdr x))
    (defmacro fargn (x n) (list 'nth n x))
    (defmacro access (type object field)
      (assert (and (eq type 'apply$-badge) (eq field :ilks))) object)
    (defun add-to-set-eq (item acc) (if (member item acc :test #'eq) acc (cons item acc)))
    (defun special-loop$-scion-callp (x wrld)
      (push (list :special (car x)) *query-trace*) (assoc (car x) wrld))
    (defun executable-badge (fn wrld)
      (push (list :badge fn) *query-trace*) (cdr (assoc fn wrld)))
    (defun well-formed-lambda-objectp (x wrld)
      (declare (ignore wrld)) (push :well-formed *query-trace*)
      (and (consp x) (eq (car x) 'lambda)))
    (defun lambda-object-body (x) (push :body *query-trace*) (car (last x)))
    (defun lambda-object-guard (x) (push :guard *query-trace*)
      (if (consp (cdddr x)) (caddr x) '(quote t)))
    ${original.replaceAll('all-fnnames1-invariant-risk', 'recursive-invariant-risk')}
    ${definitions}
    (defun compare-walkers (flg term ilk world seed)
      (let* ((saved (copy-tree term)) (*query-trace* nil)
             (expected (recursive-invariant-risk flg term ilk world seed))
             (trace *query-trace*))
        (setq *query-trace* nil)
        (let ((actual (all-fnnames1-invariant-risk flg term ilk world seed)))
          (assert (equal expected actual))
          (assert (equal trace *query-trace*))
          (assert (equal saved term))
          (assert (eq (last actual (length seed)) seed)))))
    (values)`);
  // Raw MIRICL selects PROG; ACL2's logical reader and other hosts retain the
  // byte-for-byte original DEFUN, without non-ACL2 PROG/GO syntax.
  vm.mountFile('/definitions.lisp', Buffer.from(definitions));
  await evaluate(`
    (dolist (features (list (cons :acl2-loop-only *features*) (remove :miricl *features*)))
      (let ((*features* features))
        (with-open-file (s "/definitions.lisp")
          (assert (equal (read s) '${original}))
          (assert (eq (read s nil :eof) :eof)))))
    (multiple-value-bind (file warnings failure)
        (compile-file "/definitions.lisp" :output-file "/walker.wasm")
      (declare (ignore file warnings)) (assert (not failure)))
    (setf (symbol-function 'all-fnnames1-invariant-risk)
          (lambda (&rest args) (declare (ignore args)) (error "FASL did not replace the walker.")))
    (load "/walker.wasm")
    (values)`);
  const cases = [
    'x', 'nil', "'(ignored x)", '(foo (bar x) (baz y) (bar z))',
    '((lambda (x) (body x)) (arg-one a) (arg-two b))',
    "(return-last 'mbe1-raw (exec x) (logic y))",
    "(return-last 'ec-call1-raw ignored (protected (arg x)))",
    "(return-last 'ec-call1-raw ignored x)",
    "(return-last 'ec-call1-raw ignored '(quoted))",
    "(return-last 'ec-call1-raw ignored ((lambda (x) (body x)) (arg y)))",
    "(return-last 'other (first x) (second y))",
    "(scion '(lambda (x) (guard x) (body x)) (arg a))",
    "(scion 'invalid (arg a))",
    "(ordinary '(lambda (x) (guard x) (body x)))",
  ];
  let randomState = 73;
  const random = n => {randomState = (Math.imul(randomState, 1664525) + 1013904223) >>> 0; return randomState % n;};
  const term = depth => {
    if (!depth) return ['x', 'nil', "'(data (ignored x))"][random(3)];
    switch (random(5)) {
      case 0: return `(f${random(7)} ${Array.from({length: random(4)}, () => term(depth - 1)).join(' ')})`;
      case 1: return `((lambda (x) ${term(depth - 1)}) ${term(depth - 1)})`;
      case 2: return `(return-last 'mbe1-raw ${term(depth - 1)} ${term(depth - 1)})`;
      case 3: return `(return-last 'ec-call1-raw x ${term(depth - 1)})`;
      default: return `(scion '(lambda (x) ${term(depth - 1)} ${term(depth - 1)}) ${term(depth - 1)})`;
    }
  };
  for (let i = 0; i < 500; i++) cases.push(term(4));
  for (let offset = 0; offset < cases.length; offset += 25) {
    await evaluate(`(dolist (term '(${cases.slice(offset, offset + 25).join('\n')}))
      (dolist (ilk '(nil :fn :expr))
        (compare-walkers nil term ilk '((scion :fn nil)) '(existing f1))
        (compare-walkers t (list term '(sibling x)) (list ilk) '((scion :fn nil)) nil)))
      (values)`);
  }
  await evaluate(`
    (let ((deep 'x) (wide nil))
      (dotimes (n 100000)
        (setq deep (list 'deep deep))
        (push '(wide x) wide))
      (assert (equal (all-fnnames1-invariant-risk nil deep nil nil nil) '(deep)))
      (assert (equal (all-fnnames1-invariant-risk t wide nil nil nil) '(wide))))
    (values)`);
  console.log(`ACL2 invariant-risk walker: ${cases.length * 6} comparisons preserve results, query order, sharing and inputs; raw/logical feature selection, portable FASL loading and 100,000-level/argument traversals pass with default stacks.`);
} finally {vm.close();}
