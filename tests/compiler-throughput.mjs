import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(miricl-compiler::ensure-compiler) (in-package :miricl-compiler) (values)');
  check(`(every (lambda (entry)
    (let ((bytes (coerce (append '(99) (second entry) '(99)) 'vector)))
      (multiple-value-bind (value next) (funcall (first entry) bytes 1)
        (and (= value (third entry)) (= next (1+ (length (second entry))))))))
    (list (list #'native-read-uleb '(0) 0)
          (list #'native-read-uleb '(127) 127)
          (list #'native-read-uleb '(128 1) 128)
          (list #'native-read-uleb '(128 128 1) 16384)
          (list #'native-read-sleb '(64) -64)
          (list #'native-read-sleb '(127) -1)
          (list #'native-read-sleb '(0) 0)
          (list #'native-read-sleb '(63) 63)
          (list #'native-read-sleb '(192 0) 64)
          (list #'native-read-sleb '(191 127) -65)
          (list #'native-read-sleb '(128 128 128 128 120) -2147483648)
          (list #'native-read-sleb '(255 255 255 255 7) 2147483647)))`);
  // Decode the emitter's complete instruction subset with both readers. Keep
  // local layouts, signed constants, GC operands, catch tables and byte
  // positions identical, including displaced and adjustable byte vectors.
  check(`(let* ((bytes (coerce
      '(2 3 127 5 111 2 64 3 64 4 64 5 11 11 11
        8 128 1 12 0 13 1 16 128 1 18 2 32 3 33 4 34 5
        65 64 65 192 0 65 128 128 128 128 120 65 255 255 255 255 7
        208 111 210 255 1 14 2 0 128 1 2
        31 64 4 0 1 2 1 3 4 2 5 3 6
        251 2 0 1 251 20 111 251 22 111 251 23 111
        251 26 251 27 251 28 251 29
        0 15 26 69 70 72 74 76 78 81 82 83 85 87 89 113 124 125 126 131 132 133 167 172 209 11)
      '(vector (unsigned-byte 8))))
      (adjustable (make-array (+ 8 (length bytes)) :element-type '(unsigned-byte 8)
        :adjustable t :fill-pointer (length bytes) :initial-element 0))
      (storage (concatenate '(vector (unsigned-byte 8)) #(99) bytes #(99)))
      (displaced (make-array (length bytes) :element-type '(unsigned-byte 8)
        :displaced-to storage :displaced-index-offset 1)))
    (replace adjustable bytes)
    (every (lambda (source)
      (every (lambda (references)
        (let ((native (compiler-primitive 38 source references)))
          (and native
            (equalp (coerce native 'list)
              (multiple-value-list (native-read-ops-portable source references))))))
        '(nil t))) (list bytes adjustable displaced)))`);
  check(`(and (null (compiler-primitive 38 #(0 16 128) nil))
    (null (compiler-primitive 38 #(0 255) nil))
    (null (compiler-primitive 38 #(1 1 124 11) nil))
    (let ((bytes #(0 16 128 128 128 128 128 1 11)))
      (and (null (compiler-primitive 38 bytes nil))
        (equalp (multiple-value-list (native-read-ops bytes))
          (multiple-value-list (native-read-ops-portable bytes))))))`);
  // The direct simplifier must match the Lisp reference, including mutations
  // to removed instructions, modular arithmetic, traps and scope cleanup.
  check(`(labels ((same (ops bytes)
    (every (lambda (clean)
      (let* ((reference (map 'vector #'copy-seq ops))
             (candidate (map 'vector #'copy-seq ops))
             (expected (native-simplify-ops-lisp reference bytes clean))
             (actual (native-simplify-ops candidate bytes clean)))
        (and (equalp actual expected) (equalp candidate reference)))) '(nil t)))
    (records (entries)
      (map 'vector (lambda (entry) (make-native-op (first entry) (second entry) 0 0)) entries)))
    (and
      (every (lambda (code)
        (every (lambda (left)
          (every (lambda (right)
            (same (records (list (list 65 left) (list 65 right) (list code nil) '(11 nil))) #()))
            '(-2147483648 -65 -1 0 1 31 32 33 2147483647)))
          '(-2147483648 -65 -1 0 1 31 32 33 2147483647)))
        '(70 71 72 73 74 75 76 77 78 79 106 107 108 113 114 115 116 117 118))
      (every (lambda (entries) (same (records entries) #()))
        '(((65 0) (69 nil) (26 nil) (11 nil))
          ((65 -1) (69 nil) (26 nil) (11 nil))
          ((208 111) (209 nil) (26 nil) (11 nil))
          ((210 0) (209 nil) (26 nil) (11 nil))
          ((32 0) (65 0) (106 nil) (26 nil) (11 nil))
          ((32 0) (65 1) (108 nil) (26 nil) (11 nil))
          ((32 0) (65 -1) (113 nil) (26 nil) (11 nil))
          ((35 0) (36 0) (11 nil))
          ((35 0) (36 1) (11 nil))
          ((32 0) (34 1) (26 nil) (11 nil))
          ((65 1) (103 nil) (104 nil) (26 nil) (11 nil))
          ((66 1) (80 nil) (26 nil) (11 nil))
          ((32 0) (251 22) (26 nil) (11 nil))
          ((65 1) (65 0) (109 nil) (26 nil) (11 nil))
          ((16 0) (26 nil) (11 nil))
          ((0 nil) (65 99) (26 nil) (11 nil))))
      (every (lambda (bytes) (same (native-read-ops bytes) bytes))
        '(#(0 65 0 4 64 65 1 26 5 65 2 26 11 65 7 11)
          #(0 65 1 4 64 65 1 26 5 65 2 26 11 65 7 11)
          #(0 65 0 4 64 65 8 26 11 65 7 11)
          #(0 65 1 4 64 65 8 26 11 65 7 11)
          #(0 2 64 65 0 13 0 65 1 26 11 65 7 11)
          #(0 2 64 65 1 13 0 65 9 26 11 65 7 11)
          #(0 65 0 4 64 65 1 4 64 65 8 26 11 5 65 2 26 11 65 7 11)
          #(0 31 64 0 65 0 4 64 65 8 26 11 11 65 7 11)))))`);
  // Compare direct and portable liveness, including backedges, exceptional
  // reads of all locals, deleted instructions, and bitsets wider than i64.
  for (const [bytes, total] of [
    [[1, 2, 111, 65, 1, 33, 2, 32, 2, 15, 11], 4],
    [[1, 2, 111, 2, 64, 32, 0, 4, 64, 32, 1, 33, 2, 5, 65, 3, 33, 2, 11, 32, 2, 26, 11, 11], 4],
    [[1, 2, 111, 65, 0, 33, 2, 3, 64, 32, 2, 32, 0, 70, 13, 0, 11, 32, 2, 15, 11], 4],
    [[1, 2, 111, 31, 64, 0, 65, 7, 33, 2, 16, 0, 26, 11, 32, 2, 15, 11], 4],
    [[1, 152, 2, 111, 31, 64, 0, 32, 151, 2, 33, 150, 2, 16, 0, 26, 11, 32, 150, 2, 15, 11], 282],
  ]) {
    check(`(let* ((ops (native-read-ops #(${bytes.join(' ')})))
                  (exceptions (native-op-flow ops))
                  (reference (map 'vector #'copy-seq ops))
                  (expected (native-op-liveness-lisp reference ${total} exceptions))
                  (actual (native-op-liveness ops ${total} exceptions)))
      (and (equalp expected actual) (equalp reference ops)
        (progn
          (native-stackify-ops ops exceptions nil ${total})
          (setq reference (map 'vector #'copy-seq ops))
          (equalp (native-op-liveness-lisp reference ${total} exceptions expected)
                  (native-op-liveness ops ${total} exceptions actual)))
        (equalp reference ops)))`);
  }
  // Sibling stores may be consumed in creation order. Eliminating the older
  // store inserts a real operand below the younger one; both must stackify.
  // A temporary real operand must block them only until it is consumed.
  for (const barrier of ['', '(65 99) (26 nil)']) {
    check(`(let* ((ops (map 'vector (lambda (entry)
                    (make-native-op (first entry) (second entry) 0 0))
                  '((65 1) (33 2) (65 2) (33 3) ${barrier}
                    (32 2) (32 3) (70 nil) (15 nil) (11 nil)))))
      (native-op-flow ops) (native-op-liveness ops 4 nil)
      (native-stackify-ops ops)
      (equal (loop for op across ops when (native-op-code op) collect (native-op-code op))
        '(65 65 ${barrier ? '65 26' : ''} 70 15 11)))`);
  }
  // Compare the direct Wasm kernel with the general pass across local widths,
  // dead stores/tees, operand barriers, branches, imports and throwing calls.
  // A deterministic generator also exercises empty imports and wide bitsets.
  check(`(let ((seed 1729))
    (labels ((next (limit)
      (setq seed (mod (+ (* seed 1664525) 1013904223) 4294967296))
      (mod (ash seed -8) limit)))
      (loop for trial below 48 always
        (let* ((total (if (evenp trial) 8 280))
               (*native-imports* (if (zerop (mod trial 3)) nil
                 '(("zero" nil nil) ("unary" (object) object)
                   ("binary" (object object) object))))
               (*native-import-stack-effects* nil)
               (exceptions (oddp trial))
               (codes #(nil 0 2 3 4 5 8 11 12 13 14 15 16 18 26 31
                        32 32 32 33 33 34 65 69 70 72 74 76 78
                        83 85 87 89 113 124 125 126 167 172 208 209 210 251))
               (ops (make-array 300)))
          (dotimes (i (length ops))
            (let* ((code (svref codes (next (length codes))))
                   (op (make-native-op code
                     (if (member code '(32 33 34)) (next total) (next 6)) i (1+ i))))
              (setf (native-op-live-out op)
                (logior (ash 1 (next total)) (ash 1 (next total))))
              (setf (svref ops i) op)))
          (let ((reference (map 'vector #'copy-seq ops)))
            (native-stackify-ops-lisp reference exceptions nil total)
            (native-stackify-ops ops exceptions nil total)
            (equalp reference ops))))))`);
  // The named records retain callable predicates, type tests and shallow copy
  // semantics as well as the field macros used by the self-compiled kernels.
  check(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 19))))
                (function (first (hir-unit-functions unit)))
                (value (first (hir-function-values function)))
                (copy (copy-hir-value value)) (calls 0))
    (setf (hir-value-type (progn (incf calls) copy)) 'integer)
    (and (typep unit 'hir-unit) (funcall #'hir-value-p value)
         (typep copy 'hir-value) (not (eq copy value))
         (eq (hir-value-producer copy) (hir-value-producer value))
         (= calls 1) (eq (hir-value-type copy) 'integer) (eq (hir-value-type value) t)))`);
  check(`(let* ((calls 0)
                (value (make-hir-value-record (incf calls) :single nil nil))
                (instruction (make-hir-instruction-record :return nil nil nil nil :control)))
    (and (= calls 1) (hir-value-p value)
         (equalp value (make-hir-value :id 1 :kind :single))
         (equalp instruction (make-hir-instruction :opcode :return :effect :control))
         (= (hir-instruction-safety-level instruction) 1)))`);
  check(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 19))))
                (function (first (hir-unit-functions unit)))
                (value (first (hir-function-values function))))
    (setf (hir-value-id value) (hir-function-value-count function))
    (handler-case (progn (verify-hir unit) nil) (error () t)))`);
  check(`(let* ((unit (compile-to-hir '(lambda (x) (+ x 19))))
                (function (first (hir-unit-functions unit)))
                (instruction (loop for block in (hir-function-blocks function)
                  thereis (find-if (lambda (op) (hir-instruction-inputs op))
                            (hir-block-instructions block))))
                (input (first (hir-instruction-inputs instruction))))
    (setf (first (hir-instruction-inputs instruction)) (copy-hir-value input))
    (handler-case (progn (verify-hir unit) nil) (error () t)))`);
  vm.evaluate(`(defun throughput-rewrite (unit)
    (let ((left nil) (right nil) (function (first (hir-unit-functions unit))))
      (dolist (value (hir-function-values function))
        (let ((producer (hir-value-producer value)))
          (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :constant))
            (case (first (hir-instruction-attributes producer)) (19 (setq left value)) (37 (setq right value))))))
      (assert (and left right))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (setf (hir-instruction-inputs instruction)
            (substitute left right (hir-instruction-inputs instruction) :test #'eq)))))
    unit)
    (defparameter *throughput-unit* (compile-to-hir '(lambda (x) (list (+ x 19) (+ x 37)))))
    (progn (emit-native-module *throughput-unit*)
      (throughput-rewrite *throughput-unit*) (values))`);
  // Reusing an HIR object after its uses change must give the same module as
  // a fresh, identically rewritten graph. Disable final size passes here so
  // they cannot mask stale lowering metadata by simplifying both results.
  for (const [name, unit] of [
    ['reused', '*throughput-unit*'],
    ['fresh', `(throughput-rewrite (compile-to-hir '(lambda (x) (list (+ x 19) (+ x 37)))))`],
  ]) {
    vm.evaluate(`(let ((*wasm-optimize* nil))
      (multiple-value-bind (bytes literals) (emit-native-module ${unit})
        (with-open-file (out "/${name}.wasm" :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8)) (write-sequence bytes out))
        (assert (equal (funcall (compiler-primitive 17 bytes literals nil) 3) '(22 22))))) (values)`);
  }
  assert.deepEqual(await vm.readFile('/reused.wasm'), await vm.readFile('/fresh.wasm'));
  const expressions = Array.from({length: 280}, (_, i) => `(+ n ${i})`).join(' ');
  vm.evaluate(`(defparameter *throughput-function* (compile-native '(lambda (n) (list ${expressions})))) (values)`);
  for (const n of [-5, 7]) check(`(equal (funcall *throughput-function* ${n})
    '(${Array.from({length: 280}, (_, i) => n + i).join(' ')}))`);
  // Construct the tree in Lisp so this tests frontend recursion independently
  // of the text reader's nesting bound. Large dispatcher frames used to trap.
  check(`(let ((form '(values x :deep)))
    (dotimes (i 1000)
      (setq form (list 'let '((y x)) (list 'if 'y (list 'progn form) :absent))))
    (let ((function (compile-native (list 'lambda '(x) form))))
      (and (equal (multiple-value-list (funcall function 17)) '(17 :deep))
           (eq (funcall function nil) :absent))))`);
  // Each of these formerly overflowed at 1,000 levels on the default stack.
  // Calls must keep left-to-right effects; THE must preserve multiple values.
  check(`(let ((form 'x))
    (dotimes (i 2000) (setq form (list 'identity form)))
    (let ((function (compile-native (list 'lambda '(x) form))))
      (and (= (funcall function 42) 42) (null (funcall function nil)))))`);
  check(`(let ((form '(values x :second)))
    (dotimes (i 2000) (setq form (list 'the '(values integer keyword) form)))
    (equal (multiple-value-list (funcall (compile-native (list 'lambda '(x) form)) 42))
      '(42 :second)))`);
  check(`(let ((form '(values)) (zero '(values)))
    (dotimes (i 1000)
      (setq form (list 'if 'x form zero)))
    (let ((function (compile-native (list 'lambda '(x) form))))
      (and (null (multiple-value-list (funcall function t)))
           (null (multiple-value-list (funcall function nil))))))`);
  console.log('Compiler throughput: LEB boundaries, operand barriers, record APIs, dense value validation, changed HIR reuse, 280 live temporaries and deeply nested scopes pass.');
} finally {vm.close();}
