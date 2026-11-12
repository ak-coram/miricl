import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
const cases = [
  ['typed sum', `(lambda (n) (declare (type (unsigned-byte 32) n) (optimize (safety 0)) (inline < + 1+))
    (let ((sum 0)) (declare (type (unsigned-byte 64) sum))
      (dotimes (i n sum) (declare (type (unsigned-byte 32) i)) (incf sum i))))`, [['0', '(0)'], ['10', '(45)']], true],
  ['multiple exits', `(lambda (n) (block done
    (dotimes (i n) (when (= i 3) (return-from done (values i :early)))) (values n :end)))`,
    [['0', '(0 :END)'], ['6', '(3 :EARLY)']], true],
  ['do while', `(lambda (n) (let ((i 0)) (tagbody again (incf i) (when (< i n) (go again))) i))`,
    [['0', '(1)'], ['5', '(5)']], true],
  ['empty body', '(lambda (n) (dotimes (i n n)))', [['0', '(0)'], ['10', '(10)']], true],
  ['object mutation', `(lambda (n) (let ((pair (cons 0 nil)))
    (dotimes (i n) (incf (car pair) i)) (values (car pair) pair)))`,
    [['0', '(0 (0))'], ['5', '(10 (10))']], true],
  ['dynamic scopes', `(lambda (n) (let ((events nil) (*loop-special* :outer))
    (declare (special *loop-special*))
    (dotimes (i n) (let ((*loop-special* i)) (declare (special *loop-special*)) (push *loop-special* events)))
    (values (reverse events) *loop-special*)))`, [['0', '(NIL :OUTER)'], ['4', '((0 1 2 3) :OUTER)']], true],
  ['bignum promotion', '(lambda (n) (let ((x 1)) (dotimes (i n x) (setq x (* x 2)))))',
    [['0', '(1)'], ['70', '(1180591620717411303424)']], true],
  ['zero values', '(lambda (n) (dotimes (i n) (values)) (values))', [['0', 'NIL'], ['4', 'NIL']], true],
  ['value joins', `(lambda (n) (let ((answer nil))
    (dotimes (i n answer) (setq answer (multiple-value-list (if (oddp i) (values i :odd) (values)))))))`,
    [['0', '(NIL)'], ['3', '(NIL)'], ['4', '((3 :ODD))']], true],
  ['captured mutation', `(lambda (n) (let ((x 0)) (let ((step (lambda () (incf x))))
    (dotimes (i n) (funcall step)) x)))`, [['0', '(0)'], ['5', '(5)']], true],
  ['multiple backedges', `(lambda (n) (let ((i 0) (answer 0))
    (tagbody start (when (>= i n) (go done)) (incf i)
      (if (oddp i) (progn (incf answer) (go start)) (progn (incf answer 10) (go start))) done) answer))`,
    [['0', '(0)'], ['5', '(23)'], ['6', '(33)']], true],
  ['nested loops', '(lambda (n) (let ((sum 0)) (dotimes (i n) (dotimes (j i) (incf sum j))) sum))',
    [['0', '(0)'], ['6', '(20)']], true],
  ['separate loops', '(lambda (n) (let ((sum 0)) (dotimes (i n) (incf sum i)) (dotimes (j n) (incf sum j)) sum))',
    [['0', '(0)'], ['6', '(30)']], true],
  ['three nested loops', `(lambda (n) (let ((sum 0))
    (dotimes (i n) (dotimes (j i) (dotimes (k j) (incf sum k)))) sum))`,
    [['0', '(0)'], ['6', '(15)']], true],
  ['sibling inner loops', `(lambda (n) (let ((sum 0))
    (dotimes (i n) (dotimes (j i) (incf sum j)) (dotimes (k i) (incf sum k))) sum))`,
    [['0', '(0)'], ['6', '(40)']], true],
  ['continue outer loop', `(lambda (n) (let ((i 0) (j 0) (count 0))
    (tagbody outer (when (>= i n) (go done)) (incf i) (setq j 0)
      inner (when (>= j 3) (go outer)) (incf j) (incf count)
      (when (= j 2) (go outer)) (go inner) done) count))`,
    [['0', '(0)'], ['5', '(10)']], true],
  ['second entry', `(lambda (flag n) (let ((i 0))
    (tagbody (if flag (go body) (go header))
      header (when (>= i n) (go done)) body (incf i) (go header) done) i))`,
    [['nil 0', '(0)'], ['t 0', '(1)'], ['nil 5', '(5)'], ['t 5', '(5)']], false],
  ['irreducible parent', `(lambda (flag n) (let ((i 0) (count 0))
    (tagbody (if flag (go body) (go header))
      header (when (>= i n) (go done))
      body (dotimes (j 3) (incf count)) (incf i) (go header) done) (values i count)))`,
    [['nil 0', '(0 0)'], ['t 0', '(1 3)'], ['nil 2', '(2 6)'], ['t 2', '(2 6)']], false],
  ['captured exit', `(lambda (n) (block done
    (dotimes (i n) (when (= i 3) (funcall (lambda () (return-from done (values i :captured))))))
    (values n :end)))`, [['0', '(0 :END)'], ['6', '(3 :CAPTURED)']], false],
  ['debug bindings', `(lambda (n) (declare (optimize (debug 2))) (let ((sum 0)) (dotimes (i n sum) (incf sum i))))`,
    [['0', '(0)'], ['6', '(15)']], false],
  ['cleanup', `(lambda (n) (let ((events nil)) (block done
    (dotimes (i n) (unwind-protect (when (= i 2) (return-from done (values i events))) (push i events)))
    (values n events))))`, [['0', '(0 NIL)'], ['5', '(2 (1 0))']], false],
];
try {
  vm.evaluate('(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler) (values)');
  for (const [name, source, calls, structured] of cases) {
    const sizes = [];
    {
      vm.evaluate(`(let* ((unit (compile-to-hir '${source})) (function (first (hir-unit-functions unit))))
        (unless (eq (not (null (let ((*wasm-optimization-barriers* (hir-optimization-barriers unit)))
                    (unless (native-block-targets function)
                      (native-loop-plan function (native-reachable-blocks function nil)))))) ${structured ? 't' : 'nil'})
          (error "Unexpected loop classification: ${name}"))
        (progn
            (multiple-value-bind (bytes literals) (emit-native-module unit)
              (defparameter *loop-test-function* (compiler-primitive 17 bytes literals nil))
              (with-open-file (out "/loop-test.wasm" :direction :output :if-exists :supersede
                  :element-type '(unsigned-byte 8)) (write-sequence bytes out))))) (values)`);
      const bytes = await vm.readFile('/loop-test.wasm');
      assert.ok(WebAssembly.validate(bytes), name);
      sizes.push(bytes.length);
      if (structured) assert.ok(WebAssembly.Module.imports(new WebAssembly.Module(bytes))
        .some(entry => entry.name === 'native-poll'), `${name}: backedges retain scheduling polls`);
      for (const [args, expected] of calls)
        assert.equal(vm.evaluate(`(multiple-value-list (funcall *loop-test-function* ${args}))`).output,
          `${expected}\n`, `${name}, structured standard, ${args}`);
    }
    if (name === 'typed sum') assert.ok(sizes[0] <= 500, `Structured loop size budget: ${sizes}`);
    console.log(`Loop ${name}: ${sizes[0]} bytes`);
  }
  console.log('Structured loops: branches/exits, effects, dynamic scopes, bignums, value counts, scheduling polls and conservative fallbacks pass.');
} finally {vm.close();}
