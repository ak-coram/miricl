import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const binary = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const first = await Miricl.instantiate(binary);
const second = await Miricl.instantiate(binary);
try {
  // A disabled depth check used to hash every list identically, making compiler
  // tables and FASL coalescing quadratic. Check distribution without timing CI.
  assert.equal(first.evaluate(`(let ((codes (make-hash-table)))
    (dotimes (i 1024) (setf (gethash (sxhash (list 5 i 0 0)) codes) t))
    (> (hash-table-count codes) 1000))`).output, 'T\n');
  assert.equal(first.evaluate(`(let ((table (make-hash-table :test 'equal)))
    (dotimes (i 2048) (setf (gethash (list 'key (list i) "text") table) i))
    (and (= (hash-table-count table) 2048)
      (loop for i below 2048 always
        (multiple-value-bind (value present) (gethash (list 'key (list i) (copy-seq "text")) table)
          (and present (= value i))))))`).output, 'T\n');
  assert.equal(first.evaluate(`(let ((table (make-hash-table :test 'equalp)))
    (setf (gethash '("Mixed" (1 2.0d0 #\\a)) table) :found)
    (eq (gethash '("mIXED" (1.0d0 2 #\\A)) table) :found))`).output, 'T\n');
  const hashes = `(list (sxhash '(1 (2 (3 (4 5))) "é𝄞"))
    (sxhash '#1=(a . #1#)) (sxhash '#2=(#2# . #2#)))`;
  const output = first.evaluate(hashes).output;
  assert.match(output, /^\(\d+ \d+ \d+\)\n$/);
  assert.equal(second.evaluate(hashes).output, output);
  console.log('Structural hashing: distributed list keys, EQUAL/EQUALP lookup, bounded cycles and stable cross-image SXHASH.');
} finally { first.close(); second.close(); }
