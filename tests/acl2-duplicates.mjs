import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';
import {portAcl2Source} from '../acl2/host.mjs';

const bytes = await fs.readFile('vendor/acl2/induct.lisp'), source = bytes.toString();
const start = source.indexOf('(defun remove-adjacent-duplicates (x)');
const middle = source.indexOf('\n(defun remove-adjacent-duplicates-eq (x)', start);
const end = source.indexOf('\n#+non-standard-analysis\n(defun non-standard-induction-vars', middle);
const originals = [source.slice(start, middle), source.slice(middle + 1, end)];
const adapted = portAcl2Source('induct.lisp', bytes);
const portStart = adapted.lastIndexOf('#+(and miricl (not acl2-loop-only))',
  adapted.indexOf('(defun remove-adjacent-duplicates (x)'));
const definitions = adapted.slice(portStart,
  adapted.indexOf('\n#+non-standard-analysis\n(defun non-standard-induction-vars', portStart));
assert.throws(() => portAcl2Source('induct.lisp', Buffer.from(source.replace(
  '(defun remove-adjacent-duplicates (x)', '(defun remove-adjacent-duplicates (x)\n; changed'))), /source changed/u);
const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
let vm = await Miricl.instantiate(image);
const evaluate = source => vm.evaluateAsync(source, {eof: true});
try {
  vm.mountFile('/duplicates.lisp', definitions);
  await evaluate(`
    ${originals.join('\n').replaceAll('remove-adjacent-duplicates', 'recursive-duplicates')}
    (load "/duplicates.lisp")
    (dolist (features (list (cons :acl2-loop-only *features*) (remove :miricl *features*)))
      (let ((*features* features))
        (with-open-file (stream "/duplicates.lisp")
          (assert (equal (read stream) '${originals[0]}))
          (assert (equal (read stream) '${originals[1]}))
          (assert (eq (read stream nil :eof) :eof)))))
    (defun compare-duplicates (input)
      (let ((saved (copy-tree input)))
        (dolist (pair '((remove-adjacent-duplicates recursive-duplicates)
                       (remove-adjacent-duplicates-eq recursive-duplicates-eq)))
          (let ((result (funcall (first pair) input)))
            (assert (equal result (funcall (second pair) input)))
            (loop for tail on result do
              (loop for original = input then (cdr original) while (consp original) do
                (assert (not (eq tail original)))))))
        (assert (equal input saved))))
    (compile-file "/duplicates.lisp" :output-file "/duplicates.wasm")
    (miricl-compiler:compile-native-bundle "/duplicates.wasm" "/duplicates-native.wasm")
    (load "/duplicates-native.wasm") (values)`);
  let seed = 83;
  const random = n => {seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0; return seed % n;};
  const cases = ['nil', ':atom', '(a a b a)', '(a a . tail)', '(nil nil b b . tail)',
    ...Array.from({length: 500}, () => {
      const values = Array.from({length: 1 + random(30)}, () => ['a', 'b', 'nil', '(a)', '(a)'][random(5)]);
      return `(${values.join(' ')}${random(2) ? ' . tail' : ''})`;
    })];
  for (let index = 0; index < cases.length; index += 25)
    await evaluate(`(dolist (input '(${cases.slice(index, index + 25).join('\n')}))
      (compare-duplicates input)) (values)`);
  await evaluate(`
    (let* ((a (list 'x)) (b (list 'x)) (c (list 'x)) (input (list a b c)))
      (assert (eq (car (remove-adjacent-duplicates input)) c))
      (assert (equal (remove-adjacent-duplicates-eq input) input)))
    (let ((input (loop for i below 100000 collect (floor i 2))))
      (dolist (fn '(remove-adjacent-duplicates remove-adjacent-duplicates-eq))
        (let ((result (funcall fn input)))
          (assert (= (length result) 50000))
          (assert (loop for value in result for i from 0 always (= value i)))
          (assert (not (eq (last result) (last input))))))) (values)`);
  const native = await vm.readFile('/duplicates-native.wasm');
  vm.close(); vm = await Miricl.instantiate(image); vm.mountFile('/native.wasm', native);
  assert.equal((await evaluate(`(load "/native.wasm")
    (equal (remove-adjacent-duplicates '(a a b b a . tail)) '(a b a))`)).output, 'T\nT\n');
  console.log(`ACL2 adjacent duplicates: ${cases.length * 2} oracle comparisons, logical definitions, final-member identity, fresh output, native FASL and 100,000-element scans pass.`);
} finally {vm.close();}
