import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  // Region-relative bits straddle both 32-bit word boundaries in the shared
  // native marker storage, independently of the sequence's nonzero start.
  check(`(let* ((indices '(33 34 65 66))
                (source (coerce (loop for i below 70 collect i) 'vector))
                (removed (remove-if (lambda (i) (member i indices)) source :start 2 :end 67))
                (substituted (substitute-if :new (lambda (i) (member i indices)) source :start 2 :end 67)))
    (and (= (length removed) (- (length source) (length indices)))
         (every (lambda (i) (not (find i removed))) indices)
         (loop for i below (length source)
           always (and (= i (aref source i))
                       (eql (aref substituted i) (if (member i indices) :new i))))))`);
  check(`(let* ((source (list 1 2 1 3)) (second (cdr source)) (fourth (cdddr source))
                (result (delete 1 source)))
    (and (equal result '(2 3)) (eq second result) (eq fourth (cdr result))))`);
  check(`(let* ((source (list 1 2 1 3)) (tails (loop for cell on source collect cell)))
    (and (eq source (nsubstitute 9 1 source)) (equal source '(9 2 9 3))
         (every #'eq tails (loop for cell on source collect cell))))`);
  check(`(let ((source (list 1 2 3)))
    (and (eq source (remove 42 source)) (eq source (substitute :new 42 source))
         (equal (multiple-value-list (remove 1 source)) '((2 3)))
         (equal (multiple-value-list (substitute 9 1 source)) '((9 2 3)))))`);
  check(`(let ((backing (vector 0 1 2 1 3 5)))
    (let ((view (make-array 4 :displaced-to backing :displaced-index-offset 1 :fill-pointer 4)))
      (and (eq view (nsubstitute 9 1 view))
           (equalp backing #(0 9 2 9 3 5)) (= 4 (fill-pointer view)))))`);
  check(`(and (handler-case (progn (remove 1 '(1 . :bad)) nil) (type-error () t))
       (handler-case (let ((source (list 1 2))) (setf (cddr source) source)
                       (substitute 9 1 source) nil) (type-error () t))
       (equal (remove 1 '(1 2 3)) '(2 3)))`);
  console.log('Marked sequence editing: packed boundaries, sharing, spines, views, values and recovery pass.');
} finally {vm.close();}
