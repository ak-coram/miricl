import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  check(`(progn
    (defun optimization-operations (unit)
      (loop for function in (hir-unit-functions unit) append
        (loop for block in (hir-function-blocks function) append
          (append (hir-block-instructions block) (list (hir-block-terminator block))))))
    (defun optimization-count (opcode unit)
      (count opcode (optimization-operations unit) :key (lambda (instruction) (hir-instruction-opcode instruction)))) t)`);
  check(`(let* ((source '(lambda (x) (let ((y x)) (setq y (cons y y)) y)))
                (before (lower-source-to-hir source))
                (after (compile-to-hir source)))
    (and (plusp (optimization-count :cell-write before))
      (zerop (optimization-count :cell-write after))
      (zerop (optimization-count :cell-read after))
      (plusp (optimization-count :copy after)) (verify-hir after)
      (verify-hir-code-dominance after)))`);
  check(`(let* ((source '(lambda (flag x) (let ((y nil))
                           (setq y x) (if flag (identity flag) (identity flag)) y)))
                (unit (compile-to-hir source)))
    (and (zerop (optimization-count :cell-read unit))
         (zerop (optimization-count :cell-write unit))
         (verify-hir-code-dominance unit)
         (let ((x (list :identity)) (f (compile-native source)))
           (and (eq (funcall f t x) x) (eq (funcall f nil x) x)))))`);
  check(`(let* ((source '(lambda (n) (let ((x 0))
                  (tagbody again (when (>= x n) (go done))
                    (let ((a x) (b x)) (setq x (1+ (if (= a b) x 99)))) (go again) done) x)))
                (unit (compile-to-hir source)) (function (first (hir-unit-functions unit)))
                (binding (find 'x (hir-function-bindings function)
                           :key (lambda (binding) (hir-binding-name binding))))
                (cell (hir-binding-cell binding)))
    (and (loop for block in (hir-function-blocks function) always
           (<= (count-if (lambda (op) (and (eq (hir-instruction-opcode op) :cell-read)
                         (eq (first (hir-instruction-inputs op)) cell))) (hir-block-instructions block)) 1))
      (let ((f (compile-native source))) (and (= (funcall f 0) 0) (= (funcall f 5) 5)))))`);
  // A constant primary value can select an arm while preserving its entire
  // result group, including zero values. The two engines share this HIR.
  const cases = [
    [`(lambda () (let ((flag nil)) (setq flag t) (if flag (values 19 23) (error "dead"))))`, '', '(19 23)'],
    [`(lambda () (if (values) (error "dead") (values)))`, '', 'NIL'],
    [`(lambda () (let ((x :initial)) (setq x (values 19 23)) (values x)))`, '', '(19)'],
    [`(lambda (flag) (let ((x 0)) (if flag (setq x 19) (setq x 23)) x))`, 't', '(19)'],
    [`(lambda (flag) (let ((x 0)) (if flag (setq x 19) (setq x 23)) x))`, 'nil', '(23)'],
    [`(lambda (n) (let ((x 0)) (dotimes (i n) (setq x (+ x i))) x))`, '10', '(45)'],
    [`(lambda (n) (let ((x 1)) (dotimes (i n) (setq x (* x 2))) x))`, '70', '(1180591620717411303424)'],
    [`(lambda (n) (let ((x 0)) (dotimes (i n x)
       (if (let ((flag nil)) flag) (error "dead loop arm") (incf x)))))`, '5', '(5)'],
    [`(lambda (n) (let ((x 0)) (dotimes (i n x)
       (if (let ((flag t)) flag) (incf x) (error "dead loop arm")))))`, '5', '(5)'],
    [`(lambda (n) (dotimes (i n :done) (/ 1 0)))`, '0', '(:DONE)'],
    [`(lambda () (let ((x 1)) (let ((f (lambda () (incf x)))) (funcall f) (values x (funcall f) x))))`, '', '(2 3 3)'],
    [`(lambda () (let ((x 1)) (block out (funcall (lambda () (setq x 42) (return-from out)))) x))`, '', '(42)'],
    [`(lambda () (let ((x (list :fresh))) (values (eq x x) (eq (car (cons x nil)) x))))`, '', '(T T)'],
    [`(lambda () (let ((events nil) (x 0))
       (setq x (progn (push :first events) 1) x (progn (push :second events) 2))
       (if x (values x (reverse events)) (error "dead"))))`, '', '(2 (:FIRST :SECOND))'],
    [`(lambda () (let ((x nil)) (setq x (if t (values :present :extra) (values)))
       (values x (if x :yes :no))))`, '', '(:PRESENT :YES)'],
    [`(lambda () (let ((x nil)) (setq x (nth-value 2 (values 1 2))) (if x :bad :good)))`, '', '(:GOOD)'],
    [`(lambda (flag) (tagbody (if flag (go done) (go done)) done) :done)`, 't', '(:DONE)'],
    [`(lambda (flag) (tagbody (if flag (go done) (go done)) done) :done)`, 'nil', '(:DONE)'],
    [`(lambda (flag) (let ((events nil))
       (tagbody (if (progn (push :test events) flag) (go done) (go done)) done)
       (values :done events)))`, 'nil', '(:DONE (:TEST))'],
    [`(lambda (n flag) (let ((count 0))
       (tagbody start (when (zerop n) (go done)) (incf count) (decf n)
         (if flag (go start) (go start)) done) count))`, '5 nil', '(5)'],
    [`(lambda (n flag) (let ((count 0))
       (tagbody start (when (zerop n) (go done)) (incf count) (decf n)
         (if flag (go start) (go start)) done) count))`, '0 t', '(0)'],
    [`(lambda (n flag) (let ((count 0))
       (tagbody start (when (zerop n) (go done)) (incf count) (decf n)
         (if flag (go start) (go skip)) skip (incf count 10) (go start) done) count))`, '5 t', '(5)'],
    [`(lambda (n flag) (let ((count 0))
       (tagbody start (when (zerop n) (go done)) (incf count) (decf n)
         (if flag (go start) (go skip)) skip (incf count 10) (go start) done) count))`, '5 nil', '(55)'],
    [`(lambda (n flag) (let ((count 0))
       (tagbody start (when (zerop n) (go done)) (incf count) (decf n)
         (if flag (go skip) (go start)) skip (incf count 10) (go start) done) count))`, '5 nil', '(5)'],
    [`(lambda (n flag) (let ((count 0))
       (tagbody start (when (zerop n) (go done)) (incf count) (decf n)
         (if flag (go skip) (go start)) skip (incf count 10) (go start) done) count))`, '5 t', '(55)'],
    [`(lambda (x) (declare (type (unsigned-byte 8) x)) (let ((y x)) (+ y 1)))`, '255', '(256)'],
    [`(lambda () (declare (optimize (debug 2))) (let ((x nil)) (setq x t) (if x :yes :no)))`, '', '(:YES)'],
  ];
  for (const [source, args, expected] of cases) {
    {
      for (const compiler of ['compile-native', 'compile-hir']) {
        const result = vm.evaluate(`(progn
          (multiple-value-list (funcall (${compiler} '${source}) ${args})))`).output;
        assert.equal(result, `${expected}\n`, `${compiler}: ${source}`);
      }
    }
  }
  check(`(let* ((source '(lambda () (let ((flag t)) (if flag (values 1 2) (values)))))
                (before (lower-source-to-hir source))
                (after (compile-to-hir source)))
    (and (plusp (optimization-count :branch before))
      (zerop (optimization-count :branch after))))`);
  // Nested joins are allocated before their arms. Analyze them in topological
  // order rather than exhausting a sweep budget before the outer proof arrives.
  check(`(let ((form '(values t :tail)))
    (dotimes (i 24) (setq form (list 'if 'flag form '(values t :other))))
    (let* ((source (list 'lambda '(flag)
                    (list 'if (list 'let (list (list 'value form)) 'value)
                      '(values 19 23) '(error "unreachable nested join"))))
           (unit (compile-to-hir source)))
      (multiple-value-bind (bytes literals) (emit-native-module unit)
        (let ((function (compiler-primitive 17 bytes literals nil)))
          (and (not (find "unreachable nested join" literals :test #'equal))
            (equal (multiple-value-list (funcall function t)) '(19 23))
            (equal (multiple-value-list (funcall function nil)) '(19 23)))))))`);
  check(`(let* ((unit (compile-to-hir '(lambda () (tagbody start (go start)))))
                (function (first (hir-unit-functions unit))))
    (null (hir-forward-blocks function)))`);
  check(`(let* ((unit (compile-to-hir '(lambda (n) (dotimes (i n :done)
                   (if (let ((flag nil)) flag) (optimization-dead-loop-call))))))
                (function (first (hir-unit-functions unit))))
    (multiple-value-bind (bytes literals) (emit-native-module unit)
      (declare (ignore bytes))
      (and (< (length (native-reachable-blocks function (native-block-targets function)))
              (hir-function-block-count function))
        (not (find 'optimization-dead-loop-call literals)))))`);
  // Every optimization invocation reestablishes its facts after client edits.
  check(`(let* ((unit (progn (lower-source-to-hir '(lambda (x) (let ((y x)) y)))))
                (instructions (optimization-operations unit))
                (read (find :cell-read instructions :key (lambda (instruction) (hir-instruction-opcode instruction)))))
    (setf (hir-instruction-inputs read) (list (hir-instruction-output read)))
    (handler-case (progn (optimize-hir unit) nil) (error () t)))`);
  check(`(let* ((unit (progn
                  (lower-source-to-hir '(lambda (x) (values (lambda () (incf x)) (lambda () x))))))
                (owner (first (hir-unit-functions unit)))
                (binding (first (hir-function-bindings owner))))
    (setf (hir-binding-captured-p binding) nil (hir-binding-assigned-p binding) nil)
    (optimize-hir unit)
    (not (gethash (hir-binding-cell binding) (hir-private-cells owner (hir-optimization-barriers unit)))))`);
  check(`(let* ((unit (compile-to-hir '(lambda (x) (let ((y x)) y))))
                (copy (find :copy (optimization-operations unit) :key (lambda (op) (hir-instruction-opcode op))))
                (group (find-if (lambda (value) (eq (hir-value-kind value) :multiple))
                         (hir-function-values (first (hir-unit-functions unit))))))
    (setf (hir-instruction-inputs copy) (list group))
    (handler-case (progn (emit-native-module unit) nil) (compiler-input-error () t)))`);
  check(`(= (funcall (compile-native '(lambda (x) (let ((y x)) (+ y 1)))) 41) 42)`);
  check(`(let* ((unit (progn (lower-source-to-hir '(lambda (flag) (if flag :yes :no)))))
                (branch (find :branch (optimization-operations unit) :key (lambda (op) (hir-instruction-opcode op)))))
    (setf (second (hir-instruction-attributes branch)) (first (hir-instruction-attributes branch)))
    (multiple-value-bind (bytes literals) (emit-native-module unit)
      (let ((f (compiler-primitive 17 bytes literals nil)))
        (and (eq (funcall f nil) :yes) (eq (funcall f t) :yes)))))`);
  // A conditional backedge and its fallthrough can become adjacent after
  // pruning/reordering. The fallthrough has no label of its own.
  for (const typed of [false, true])
  for (const condition of ['(if (< i n) (go start))', '(if (>= i n) nil (go start))'])
  check(`(let* ((unit (compile-to-hir '(lambda (n)
                  ${typed ? '(declare (fixnum n) (inline < >= 1+) (optimize (safety 0)))' : ''}
                  (let ((i 0)) ${typed ? '(declare (type (unsigned-byte 32) i))' : ''}
                  (tagbody start (incf i) ${condition} done) i))))
                (f (first (hir-unit-functions unit)))
                (branch-block (find-if (lambda (block)
                  (eq (hir-instruction-opcode (hir-block-terminator block)) :branch)) (hir-function-blocks f)))
                (next (find-if (lambda (target) (> (hir-block-id target) (hir-block-id branch-block)))
                  (hir-instruction-attributes (hir-block-terminator branch-block)))))
    (setf (hir-function-blocks f)
      (loop for block in (hir-function-blocks f) unless (eq block next)
        append (if (eq block branch-block) (list block next) (list block))))
    (loop for block in (hir-function-blocks f) for id from 0 do (setf (hir-block-id block) id))
    (verify-hir unit) (verify-hir-code-dominance unit)
    (multiple-value-bind (bytes literals) (emit-native-module unit)
      (let ((function (compiler-primitive 17 bytes literals nil)))
        (and (= (funcall function 5) 5) (= (funcall function 0) 1)))))`);
  check(`(let ((*wasm-bytes* (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (handler-case (progn (wasm-u32 -1) nil) (compiler-input-error () t)))`);
  check(`(let ((*wasm-bytes* (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (wasm-u32 #xffffffff) (equal (coerce *wasm-bytes* 'list) '(255 255 255 255 15)))`);
  check(`(= (funcall (compile-native '(lambda (x) (+ x 1))) 41) 42)`);
  for (const [operator, expression] of [
    ['consp', '(consp x)'], ['simple-vector-p', '(simple-vector-p x)'],
    ['eq', '(eq x x)'], ['eql', '(eql x x)'], ['null', '(null x)'],
    ['cons', '(cons x x)'],
  ]) {
    vm.evaluate(`(multiple-value-bind (bytes literals)
      (emit-native-module (compile-to-hir '(lambda (x) (declare (inline ${operator}))
        ${expression} :done)))
      (with-open-file (out "/pure-call.wasm" :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values)`);
    const imports = WebAssembly.Module.imports(new WebAssembly.Module(await vm.readFile('/pure-call.wasm')));
    assert.ok(!imports.some(entry => entry.name === `native-${operator}-inline`
      || operator === 'cons' && entry.name === 'native-cons'), expression);
  }
  check(`(equal (funcall (compile-native '(lambda () (declare (inline consp cons))
    (let ((events nil))
      (consp (cons (progn (push :left events) 1) (progn (push :right events) 2)))
      (reverse events))))) '(:left :right))`);
  check(`(handler-case (progn (funcall (compile-native '(lambda () (declare (inline length))
    (length (cons 1 2)) :done))) nil) (type-error () t))`);
  console.log('HIR optimization: SSA forwarding, private stores, constants, branches, identity, effects, joins, loops, captures, multiple values, and malformed-input recovery pass.');
} finally {vm.close();}
