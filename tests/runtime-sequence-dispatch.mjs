import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-interpreter.wasm'));
const cases = [
  `(equal (multiple-value-list (values-list '(1 2 3))) '(1 2 3))`,
  `(null (multiple-value-list (values-list nil)))`,
  `(equal (multiple-value-list (funcall (complement #'identity) nil)) '(t))`,
  `(let* ((value (list :shared)) (f (constantly value)))
     (and (eq value (funcall f :ignored))
          (equal (multiple-value-list (funcall f)) (list value))))`,
  `(and (constantp '(quote (1 2))) (not (constantp '(quote 1 2)))
        (not (constantp 'sequence-dispatch-unbound)))`,
  `(let ((table (make-hash-table :test 'eql)))
     (and (funcall (hash-table-test table) 1 1) (plusp (hash-table-size table))))`,
  `(and (equal (multiple-value-list (floor 17 5)) '(3 2))
        (equal (multiple-value-list (truncate -17 5)) '(-3 -2))
        (= (isqrt 49) 7) (string= (string-trim '(#\\Space #\\a) "aa bb aa") "bb"))`,
  `(let ((v (vector 1 2)))
     (and (eq v (map-into v #'1+ v)) (equalp v #(2 3))
          (= (svref v 1) 3) (= (array-rank v) 1)))`,
  `(let ((v (make-array 3 :fill-pointer 0 :initial-element nil)))
     (and (= (vector-push :one v) 0) (= (vector-push :two v) 1)
          (eq (vector-pop v) :two) (= (fill-pointer v) 1)))`,
  `(and (simple-string-p "text") (bit-vector-p #*01) (simple-bit-vector-p #*01)
        (equal (concatenate 'list #(1 2) '(3)) '(1 2 3))
        (equalp (subseq #(1 2 3) 1) #(2 3)))`,
  `(and (= (search #(1 2) #(0 1 2 1 2) :from-end t) 3)
        (= (mismatch "abc" "abd") 2)
        (= (search nil '(1 2 3) :start2 1) 1))`,
  `(let ((events nil))
     (and (= (search (progn (push :pattern events) #(1 2))
                     (progn (push :source events) #(0 1 2))
                     :key (lambda (x) (push :key events) (values x :extra))) 1)
          (equal (subseq (reverse events) 0 2) '(:pattern :source))))`,
  `(and (equal (sort (list 3 1 2) #'<) '(1 2 3))
        (equal (subst :x :a '(:a (:b :a))) '(:x (:b :x)))
        (equal (substitute :x :a '(:a :b :a)) '(:x :b :x))
        (equal (remove-duplicates '(1 2 1)) '(2 1))
        (= (ash 3 2) 12) (equalp (bit-xor #*011 #*110) #*101))`,
  `(with-input-from-string (stream "ab")
     (and (char= (read-char stream) #\\a) (char= (peek-char nil stream) #\\b)))`,
  `(let ((cell (cons :head :tail)))
     (and (eq (identity cell) cell) (consp cell)
          (eq (rplaca cell :changed) cell) (eq (car cell) :changed)
          (= (length '(1 2 3)) 3)))`,
];
try {
  for (const source of cases) {
    let output;
    try { output = vm.evaluate(source).output; }
    catch (cause) { throw new Error(`Sequence dispatch case failed: ${source}`, {cause}); }
    assert.equal(output, 'T\n', source);
  }
  for (const [source, condition] of [
    ['(values-list)', 'program-error'], ['(floor)', 'program-error'],
    ['(svref)', 'program-error'], ['(constantp)', 'program-error'],
    ["(values-list '(1 . :bad))", 'type-error'],
    ["(search '(1) '(1 . :bad))", 'type-error'],
    ['(svref :bad 0)', 'type-error'],
    ["(apply #'subseq '(#(1 2) . :bad))", 'type-error'],
  ]) {
    assert.equal(vm.evaluate(`(handler-case (progn ${source} nil) (${condition} () :caught))`).output,
      ':CAUGHT\n', source);
    assert.equal(vm.evaluate('(search #(2) #(1 2 3))').output, '1\n', `${source}: recovery`);
  }
  console.log(`Sequence dispatch: ${cases.length} mixed-family cases, checked failures and recovery pass.`);
} finally {vm.close();}
