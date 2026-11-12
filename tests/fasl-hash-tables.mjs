import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';
const image = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const vm = await createCompilerTestVM(image);
try {
  vm.mountFile('/hash-literals.lisp', `
    (defmacro hash-literals ()
      (let* ((shared (list :shared))
             (eq (make-hash-table :test 'eq :size 37 :rehash-size 1.5 :rehash-threshold 0.75))
             (eql (make-hash-table :test 'eql))
             (equal (make-hash-table :test 'equal))
             (equalp (make-hash-table :test 'equalp))
             (empty (make-hash-table :test 'equal :size 0)))
        (setf (gethash shared eq) shared (gethash eq eq) eq
              (gethash :self eq) eq (gethash :other eq) eql
              (gethash :other eql) eq
              (gethash #x10000000000000000 eql) shared
              (gethash 1f0 eql) :single (gethash 1d0 eql) :double
              (gethash (copy-seq "same") equal) shared
              (gethash '(:nested :key) equal) eql
              (gethash "Case" equalp) shared (gethash 42 equalp) eql)
        (list 'quote (vector eq eql equal equalp shared empty eq))))
    (defparameter *hash-literals* (hash-literals))
    (defparameter *load-value* (load-time-value (list :ready)))
    (defun get-hash-literals () *hash-literals*)`);
  await vm.evaluateAsync(`(compile-file "/hash-literals.lisp")
    (load "/hash-literals.wasm")
    (miricl-compiler:shake-file "/hash-literals.wasm" "/hash-compacted.wasm" :strip nil)
    (miricl-compiler:compile-native-bundle "/hash-compacted.wasm" "/hash-native.wasm") (values)`, {eof: true});
  for (const file of ['/hash-literals.wasm', '/hash-compacted.wasm', '/hash-native.wasm']) {
    const fresh = await createCompilerTestVM(image);
    try {
      fresh.mountFile('/hash.wasm', await vm.readFile(file));
      await fresh.evaluateAsync(`(let ((miricl-compiler:*compile-evaluations* t)
          (compiler (symbol-function 'miricl-compiler:compile-native)))
        (unwind-protect
            (progn
              (setf (symbol-function 'miricl-compiler:compile-native)
                    (lambda (&rest arguments)
                      (declare (ignore arguments)) (error "FASL load recompiled a recipe.")))
              (load "/hash.wasm"))
          (setf (symbol-function 'miricl-compiler:compile-native) compiler))
        (values))`, {eof: true});
      assert.equal((await fresh.evaluateAsync(`
        (destructuring-bind (eq eql equal equalp shared empty alias) (coerce (get-hash-literals) 'list)
          (and (equal *load-value* '(:ready))
            (eq eq alias) (eq (gethash shared eq) shared) (eq (gethash eq eq) eq)
            (eq (gethash :self eq) eq) (eq (gethash :other eq) eql) (eq (gethash :other eql) eq)
            (eq (gethash #x10000000000000000 eql) shared)
            (eq (gethash 1f0 eql) :single) (eq (gethash 1d0 eql) :double)
            (eq (gethash "same" equal) shared) (eq (gethash (list :nested :key) equal) eql)
            (eq (gethash "cASE" equalp) shared) (eq (gethash 42d0 equalp) eql)
            (equal (mapcar #'hash-table-test (list eq eql equal equalp empty)) '(eq eql equal equalp equal))
            (zerop (hash-table-count empty))
            (>= (hash-table-size eq) 37) (= (hash-table-rehash-size eq) 1.5)
            (= (hash-table-rehash-threshold eq) 0.75)))`, {eof: true})).output, 'T\n');
      console.log(`PASS hash table literals, tests, numeric keys, aliases, cycles and hints in ${file}`);
    } finally {fresh.close();}
  }
} finally {vm.close();}
