// node --expose-gc tests/weak-hash-tables.mjs [miricl.wasm]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
const bytes = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const vm = await Miricl.instantiate(bytes);
async function evaluate(form) {return (await vm.evaluateAsync(form, {eof: true})).output.trim();}
async function check(name, form) {
  assert.equal(await evaluate(form), 'T', name);
  console.log(`PASS ${name}`);
}
try {
  for (const weakness of ['key', 'value']) for (const test of ['eq', 'eql', 'equal', 'equalp']) {
    await check(`${weakness} / ${test}: standard operations and multiple values`, `
      (let* ((table (js:make-weak-hash-table :weakness :${weakness} :test '${test} :size 1))
             (key (list :key)) (value (vector 42)))
        (and (hash-table-p table) (typep table 'hash-table)
             (eq (js:hash-table-weakness table) :${weakness})
             (eq (hash-table-test table) '${test})
             (eq (setf (gethash key table) value) value)
             (equal (multiple-value-list (gethash key table)) (list value t))
             (equal (multiple-value-list (gethash :absent table :default)) '(:default nil))
             (progn (setf (gethash :nil table) nil)
                    (equal (multiple-value-list (gethash :nil table :default)) '(nil t)))
             (= (hash-table-count table) 2) (>= (hash-table-size table) 2)
             (equal (sort (loop for k being the hash-keys of table using (hash-value v)
                               collect (if (eq k key) (aref v 0) 7)) #'<) '(7 42))
             (progn (maphash (lambda (k v) (declare (ignore v)) (remhash k table)) table)
                    (zerop (hash-table-count table)))
             (not (remhash key table)) (eq (clrhash table) table)))`);
  }
  await check('target EQUAL/EQUALP semantics and hash-table equality', `
    (let ((a (js:make-weak-hash-table :weakness :key :test 'equal))
          (b (make-hash-table :test 'equal))
          (c (js:make-weak-hash-table :weakness :value :test 'equalp)))
      (setf (gethash (copy-seq "λ") a) 17 (gethash (copy-seq "λ") b) 17
            (gethash "Hello" c) '(1 2))
      (and (= (gethash (copy-seq "λ") a) 17) (equalp a b) (equalp b a)
           (equal (gethash "HELLO" c) '(1 2))
           (null (js:hash-table-weakness b))))`);
  await check('validation rejects malformed options and recovers', `
    (and (handler-case (js:make-weak-hash-table :weakness :invalid) (error () t))
         (handler-case (js:make-weak-hash-table :weakness :key :size -1) (error () t))
         (handler-case (js:make-weak-hash-table :weakness :key :test 'identity) (error () t))
         (handler-case (js:hash-table-weakness 'bad) (type-error () t))
         (eq :key (js:hash-table-weakness (js:make-weak-hash-table :weakness :key))))`);
  await check('native calls use weak storage and preserve all values', `
    (let* ((f (miricl-compiler:compile-native '(lambda (k table)
                 (setf (gethash k table) 42) (gethash k table))))
           (table (js:make-weak-hash-table :weakness :key)))
      (equal (multiple-value-list (funcall f :key table)) '(42 t)))`);
  await evaluate(`
    (defparameter *image-key* (list :root))
    (defparameter *image-value* (list :value))
    (defparameter *image-weak-key* (js:make-weak-hash-table :weakness :key :test 'eq))
    (defparameter *image-weak-value* (js:make-weak-hash-table :weakness :value :test 'equal))
    (setf (gethash *image-key* *image-weak-key*) (list 42)
          (gethash "retained" *image-weak-value*) *image-value*)
    ;; This cycle has no strong application root and must not be resurrected.
    (let* ((key (list :dead)) (value (list key)))
      (setf (gethash key *image-weak-key*) value))
    ;; Saving reaches the second weak table's key through the first one's value.
    (defparameter *image-chain* (js:make-weak-hash-table :weakness :key))
    (let ((next (list :next)))
      (setf (gethash :first *image-chain*) next (gethash next *image-chain*) :last))
    (values)`);
  vm.mountFile('/weak-literal.lisp', `
    (eval-when (:compile-toplevel)
      (defmacro weak-literal ()
        (let ((table (js:make-weak-hash-table :weakness :key)))
          (setf (gethash :literal table) (vector 42))
          (list 'quote table))))
    (defun weak-literal-table () (weak-literal))`);
  await evaluate('(compile-file "/weak-literal.lisp") (values)');
  const literalTarget = await Miricl.instantiate(bytes);
  try {
    literalTarget.mountFile('/weak-literal.wasm', await vm.readFile('/weak-literal.wasm'));
    assert.equal((await literalTarget.evaluateAsync(`(load "/weak-literal.wasm")
      (let ((table (weak-literal-table)))
        (and (eq (js:hash-table-weakness table) :key)
             (equalp (gethash :literal table) #(42))))`, {eof: true})).output.trim(), 'T\nT');
    console.log('PASS compiled literals preserve table weakness and entries');
  } finally {literalTarget.close();}
  const image = vm.saveImage();
  const restored = await Miricl.instantiate(image);
  try {
    assert.equal((await restored.evaluateAsync(`(and
      (eq (js:hash-table-weakness *image-weak-key*) :key)
      (eq (js:hash-table-weakness *image-weak-value*) :value)
      (equal (gethash *image-key* *image-weak-key*) '(42))
      (eq (gethash "retained" *image-weak-value*) *image-value*)
      (= (hash-table-count *image-weak-key*) 1)
      (eq (gethash (gethash :first *image-chain*) *image-chain*) :last))`, {eof: true})).output.trim(), 'T');
    console.log('PASS saved images preserve strongly reachable entries and ephemeron chains');
  } finally {restored.close();}
  if (global.gc) {
    for (const weakness of ['key', 'value']) {
      await evaluate(`(defparameter *gc-table* (js:make-weak-hash-table :weakness :${weakness}))
        (let* ((target (list :target)) (backlink (list target)))
          (setf (gethash ${weakness === 'key' ? 'target' : 'backlink'} *gc-table*)
            ${weakness === 'key' ? 'backlink' : 'target'})) (values)`);
      let collected = false;
      for (let i = 0; i < 50; i++) {
        await new Promise(resolve => setImmediate(resolve)); global.gc();
        if (await evaluate('(zerop (hash-table-count *gc-table*))') === 'T') {collected = true; break;}
      }
      assert.ok(collected, `${weakness}: weak-only cycle retained`);
      console.log(`PASS ${weakness}: weak-only cycle collected`);
    }
  } else console.log('SKIP collection checks: run Node with --expose-gc');
  vm.vm['javascript-services'].set(0, null);
  await check('unavailable host signals a Lisp condition; ordinary tables still work', `
    (and (handler-case (js:make-weak-hash-table :weakness :key)
           (js:javascript-unavailable () t))
         (let ((table (make-hash-table))) (setf (gethash :key table) 42) (= (gethash :key table) 42)))`);
} finally {vm.close();}
