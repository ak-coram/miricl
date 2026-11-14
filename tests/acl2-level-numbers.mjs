import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {portAcl2Source} from '../acl2/host.mjs';
import {iterativeLevelNumbers} from '../acl2/level-numbers.mjs';
import {loadAcl2TestCore} from './acl2-test-core.mjs';
const bytes = await fs.readFile('vendor/acl2/simplify.lisp'), source = bytes.toString();
const start = source.indexOf('(mutual-recursion\n\n(defun max-level-no (');
const end = source.indexOf('\n(mutual-recursion\n\n(defun sort-fcds1-rating1', start);
const original = source.slice(start, end);
const adapted = portAcl2Source('simplify.lisp', bytes);
const portStart = adapted.lastIndexOf('#+(and miricl (not acl2-loop-only))',
  adapted.indexOf('(defun miricl-max-level-no-work'));
assert.throws(() => portAcl2Source('simplify.lisp', Buffer.from(source.replace(
  '(defun max-level-no (term wrld)', '(defun max-level-no (term wrld)\n; changed'))), /source changed/u);
const {vm} = await loadAcl2TestCore(process.argv[2] ?? 'miricl.wasm',
  process.argv[3] ?? 'build/acl2/acl2-native.wasm');
const evaluate = source => vm.evaluateAsync(source, {eof: true});
try {
  vm.mountFile('/original-levels.lisp', `(in-package "ACL2")\n${original
    .replaceAll('max-level-no', 'recursive-max-level-no').replaceAll('get-level-no', 'recursive-get-level-no')}`);
  vm.mountFile('/levels.lisp', `(in-package "ACL2")\n${iterativeLevelNumbers}`);
  vm.mountFile('/logical-levels.lisp', adapted.slice(portStart,
    adapted.indexOf('\n(mutual-recursion\n\n(defun sort-fcds1-rating1', portStart)));
  await evaluate(`(load "/original-levels.lisp") (load "/levels.lisp")
    (compile-file "/levels.lisp" :output-file "/levels.wasm")
    (miricl-compiler:compile-native-bundle "/levels.wasm" "/levels-native.wasm" :load t)
    (in-package :acl2)
    (dolist (features (list (cons :acl2-loop-only *features*) (remove :miricl *features*)))
      (let ((*features* features))
        (with-open-file (stream "/logical-levels.lisp")
          (assert (equal (read stream) '${original}))
          (assert (eq (read stream nil :eof) :eof)))))
    (defun compare-level-numbers (term)
      (dolist (world '(nil ((f level-no . 3) (g level-no . 7) (h level-no . 2))
                          ((f level-no . 0) (g level-no . 1267650600228229401496703205376))))
        (assert (= (max-level-no term world) (recursive-max-level-no term world)))
        (assert (= (max-level-no-lst (list term '(g x)) world)
                   (recursive-max-level-no-lst (list term '(g x)) world)))
        (when (consp term)
          (assert (= (get-level-no (ffn-symb term) world)
                     (recursive-get-level-no (ffn-symb term) world)))))) (values)`);
  let seed = 31;
  const random = n => {seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0; return seed % n;};
  const term = depth => !depth ? ['x', 'y', "'(g x)"][random(3)]
    : random(4) ? `(${['f', 'g', 'h', 'absent'][random(4)]} ${term(depth - 1)} ${term(depth - 1)})`
      : `((lambda (x) ${term(depth - 1)}) ${term(depth - 1)})`;
  const cases = ['x', "'(f (g x))", '(absent x)', '((lambda (x) (g x)) (f y))',
    ...Array.from({length: 500}, () => term(4))];
  for (let i = 0; i < cases.length; i += 20)
    await evaluate(`(dolist (term '(${cases.slice(i, i + 20).join('\n')})) (compare-level-numbers term)) (values)`);
  assert.equal((await evaluate(`
    (let ((deep '(g x)) (world '((f level-no . 3) (g level-no . 7))))
      (dotimes (i 100000) (setq deep (list 'f deep)))
      (assert (= (max-level-no deep world) 7))
      (assert (= (max-level-no-lst (make-list 100000 :initial-element '(g x)) world) 7)))
    (let ((deep 'x) (world '((g level-no . 7))))
      (dotimes (i 100000) (setq deep (list (list 'lambda '(x) deep) '(g x))))
      (assert (= (max-level-no deep world) 7))) t`)).output, 'NIL\nNIL\nT\n');
  await fs.writeFile('build/acl2/levels-patch.wasm', await vm.readFile('/levels-native.wasm'));
  await fs.writeFile('build/acl2/levels-patch-portable.wasm', await vm.readFile('/levels.wasm'));
  console.log(`ACL2 level numbers: ${cases.length * 9} oracle comparisons; logical definitions, missing levels, bignum levels and 100,000-level/argument/lambda traversals pass.`);
} finally {vm.close();}
