import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {portAcl2Source} from '../acl2/host.mjs';
import {iterativeOneify} from '../acl2/oneify.mjs';
import {loadAcl2TestCore} from './acl2-test-core.mjs';

const bytes = await fs.readFile('vendor/acl2/interface-raw.lisp'), source = bytes.toString();
const start = source.indexOf('(mutual-recursion\n\n(defun-one-output oneify-flet-bindings (');
const end = source.indexOf('\n(defun-one-output select-stobj', start);
const original = source.slice(start, end);
const adapted = portAcl2Source('interface-raw.lisp', bytes);
const portStart = adapted.lastIndexOf('#+(and miricl (not acl2-loop-only))',
  adapted.indexOf('(defun miricl-oneify-work'));
assert.throws(() => portAcl2Source('interface-raw.lisp', Buffer.from(source.replace(
  '(defun-one-output oneify (x fns w program-p)', '(defun-one-output oneify (x fns w program-p)\n; changed'))), /source changed/u);
const {vm} = await loadAcl2TestCore(process.argv[2] ?? 'build/acl2/runtime.wasm',
  process.argv[3] ?? 'build/acl2/acl2-native.wasm');
const evaluate = source => vm.evaluateAsync(source, {eof: true});
try {
  vm.mountFile('/original-oneify.lisp', `(in-package "ACL2")\n${original.replace(
    /(?<![\w*-])oneify(?:-flet-bindings|-lst)?(?![\w*-])/gu, name => 'recursive-' + name)}`);
  vm.mountFile('/oneify.lisp', `(in-package "ACL2")\n${iterativeOneify(original)}`);
  vm.mountFile('/logical-oneify.lisp', adapted.slice(portStart,
    adapted.indexOf('\n(defun-one-output select-stobj', portStart)));
  await evaluate(`(load "/original-oneify.lisp") (load "/oneify.lisp")
    (compile-file "/oneify.lisp" :output-file "/oneify.wasm")
    (miricl-compiler:compile-native-bundle "/oneify.wasm" "/oneify-native.wasm" :load t)
    (in-package :acl2)
    (dolist (features (list (cons :acl2-loop-only *features*) (remove :miricl *features*)))
      (let ((*features* features))
        (with-open-file (stream "/logical-oneify.lisp")
          (assert (equal (read stream) '${original}))
          (assert (eq (read stream nil :eof) :eof)))))
    (defparameter *oneify-test-seed* 0)
    (defparameter *oneify-test-gentemp* (symbol-function 'acl2-gentemp))
    (setf (symbol-function 'acl2-gentemp)
      (lambda (prefix) (declare (ignore prefix))
        (intern (format nil "ONEIFY-TEST-~D" (incf *oneify-test-seed*)) "ACL2")))
    (defun compare-oneify (term)
      (dolist (program-p '(nil t invariant-risk))
        (dolist (world '(nil ((f invariant-risk . t))
                            ((f stobjs-in :df nil) (f stobjs-out :df))))
          (setq *oneify-test-seed* 0)
          (let ((expected (recursive-oneify term nil world program-p)))
            (setq *oneify-test-seed* 0)
            (assert (equal expected (oneify term nil world program-p)) ()
                    "ONEIFY mismatch: ~S program ~S world ~S" term program-p world))))) (values)`);
  let seed = 31;
  const random = n => {seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0; return seed % n;};
  const term = depth => !depth ? ['x', 'y', ':key', '17', "'(f x)"][random(5)]
    : random(5) ? `(${['f', 'g', 'if', 'cons'][random(4)]} ${term(depth - 1)} ${term(depth - 1)})`
      : `(let ((x ${term(depth - 1)})) ${term(depth - 1)})`;
  const cases = ['x', ':key', '17', 'nil', "'(f x)", '(f x y)', '(if x y z)',
    '((lambda (x) (f x)) y)', '(let ((x (g y))) (declare (ignore x)) (f y))',
    '(let ((x (g y))) (declare (type integer x)) (f x))',
    '(mv-let (x y) (f z) (declare (ignore y)) (g x))',
    '(flet ((local (x) (f x))) (local y))',
    '(flet ((local (x y) (declare (ignore y)) (f x))) (local y z))',
    '(macrolet ((local (x) (list (quote f) x))) (local y))',
    '(macrolet ((local (x) x)) (local nil))',
    '(translate-and-test ignored (f x))',
    '(with-global-stobj st (f x))', '(with-global-stobj st raw (f x))',
    "(wormhole-eval 'name '(lambda (x) (f x)) 'ignored)",
    "(return-last 'unknown (f x) (g y))", "(return-last 'mbe1-raw (f x) (g y))",
    ...Array.from({length: 200}, () => term(4))];
  for (let i = 0; i < cases.length; i += 10)
    await evaluate(`(dolist (term '(${cases.slice(i, i + 10).join('\n')})) (compare-oneify term)) (values)`);
  assert.equal((await evaluate(`
    (setf (symbol-function 'acl2-gentemp) *oneify-test-gentemp*)
    (let ((deep 'x))
      (dotimes (i 100000) (setq deep (list 'cons deep 'nil)))
      (setq deep (oneify deep nil nil nil))
      (dotimes (i 100000) (assert (eq (car deep) 'cons)) (setq deep (cadr deep)))
      (assert (eq deep 'x)))
    (assert (= (length (oneify-lst (make-list 100000 :initial-element 'x) nil nil nil)) 100000))
    (let ((deep 'x))
      (dotimes (i 100000) (setq deep (list 'let '((x y)) deep)))
      (setq deep (oneify deep nil nil nil))
      (dotimes (i 100000) (assert (eq (car deep) 'let)) (setq deep (caddr deep)))
      (assert (eq deep 'x))) t`)).output.endsWith('T\n'), true);
  await fs.writeFile('build/acl2/oneify-patch.wasm', await vm.readFile('/oneify-native.wasm'));
  await fs.writeFile('build/acl2/oneify-patch-portable.wasm', await vm.readFile('/oneify.wasm'));
  const initializers = Number((await evaluate(`
    (length (miricl-compiler::ir-unit-roots
      (car (miricl-compiler::ir-program-units (miricl-compiler:analyze-file "/oneify-native.wasm")))))`)).output.trim());
  assert.ok(Number.isSafeInteger(initializers) && initializers > 0);
  await fs.writeFile('build/acl2/oneify-patch.json', JSON.stringify({initializers}) + '\n');
  console.log(`ACL2 ONEIFY: ${cases.length * 9} oracle comparisons; preserved logical source and 100,000-level/list/LET traversals pass.`);
} finally {vm.close();}
