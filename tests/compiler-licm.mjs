import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl-precompiled.wasm'));
const firstBody = bytes => {
  let position = 8;
  const word = () => {let value = 0, scale = 1, byte; do {byte = bytes[position++]; value += (byte & 127) * scale; scale *= 128;} while (byte & 128); return value;};
  while (position < bytes.length) {
    const id = bytes[position++], size = word(), end = position + size;
    if (id === 10) {assert.ok(word() > 0); const length = word(); return bytes.subarray(position, position + length);}
    position = end;
  }
  throw new Error('Missing code section');
};
const loop = condition => `(let ((sum 0)) (declare (type (unsigned-byte 32) sum))
  (dotimes (i n sum) (declare (type (unsigned-byte 32) i)) (when ${condition} (incf sum))))`;
const policy = '(declare (type (unsigned-byte 32) n) (optimize (safety 0)) (inline + - * 1+ 1- logand logior logxor < consp null eq minusp zerop oddp >= <=))';
const cases = [
  ['stable tag', `(lambda (x n) ${policy} ${loop('(consp x)')})`,
    [['nil 0', '(0)'], ['nil 5', '(0)'], ['(list 1) 5', '(5)']], true],
  ['literal identity', `(lambda (x n) ${policy} ${loop('(eq x :marker)')})`,
    [[':marker 0', '(0)'], [':marker 5', '(5)'], [':other 5', '(0)']], true],
  ['null condition', `(lambda (x n) ${policy} ${loop('(null x)')})`,
    [['nil 0', '(0)'], ['nil 5', '(5)'], [':x 5', '(0)']], true],
  ['entry type proof', `(lambda (x n) ${policy} (let ((a (the fixnum x))) ${loop('(minusp a)')}))`,
    [['-1 0', '(0)'], ['-1 5', '(5)'], ['1 5', '(0)'], ['-9223372036854775808 5', '(5)']], true],
  ['wide negative literal', `(lambda (x n) ${policy} (let ((a (the fixnum x))) ${loop('(>= a -9223372036854775807)')}))`,
    [['-9223372036854775808 5', '(0)'], ['-9223372036854775807 5', '(5)']], true],
  ['wide positive literal', `(lambda (x n) ${policy} (let ((a (the fixnum x))) ${loop('(<= a 9223372036854775806)')}))`,
    [['9223372036854775807 5', '(0)'], ['9223372036854775806 5', '(5)']], true],
  ['two cached branch tests', `(lambda (x y n) ${policy}
    (let ((a (the fixnum x)) (b (the fixnum y)) (sum 0))
      (dotimes (i n sum) (when (minusp a) (incf sum)) (when (zerop b) (incf sum)))))`,
    [['-1 0 5', '(10)'], ['1 0 5', '(5)'], ['-1 2 5', '(5)'], ['1 2 5', '(0)']], true],
  ['observed Lisp boolean', `(lambda (x n) ${policy}
    (let ((a (the fixnum x)) (out :none))
      (dotimes (i n out) (setq out (minusp a)))))`,
    [['-1 0', '(:NONE)'], ['-1 5', '(T)'], ['1 5', '(NIL)']], true],
  ['body type assertion', `(lambda (x n) ${policy} ${loop('(locally (declare (type fixnum x)) (minusp x))')})`,
    [[':not-an-integer 0', '(0)'], ['-1 5', '(5)'], ['1 5', '(0)']], false],
  ['body literal assertion', `(lambda (n) ${policy} ${loop('(zerop (the fixnum :not-an-integer))')})`,
    [['0', '(0)']], false],
  ['operand effects', `(lambda (x n) ${policy} (setq *licm-effects* 0)
    (values ${loop('(consp (progn (incf *licm-effects*) x))')} *licm-effects*))`,
    [['nil 0', '(0 0)'], ['(list 1) 5', '(5 5)']], true],
  ['changing binding', `(lambda (x n) ${policy}
    (let ((sum 0)) (dotimes (i n) (when (consp x) (incf sum)) (setq x nil)) sum))`,
    [['(list 1) 0', '(0)'], ['(list 1) 5', '(1)']], false],
  ['mutable heap read', `(lambda (x n) ${policy} (declare (inline car not) (list x))
    (let ((sum 0)) (dotimes (i n) (when (car x) (incf sum)) (setf (car x) (not (car x))))
      (values sum (car x))))`, [['(list t) 0', '(0 T)'], ['(list t) 5', '(3 NIL)']], false],
  ['ordinary calls', `(lambda (x n) ${policy} (setq *licm-effects* 0)
    (values ${loop('(licm-probe x)')} *licm-effects*))`,
    [['t 0', '(0 0)'], ['t 5', '(5 5)']], false],
  ['full value groups', `(lambda (x n) ${policy}
    (let ((out nil)) (dotimes (i n out) (setq out (multiple-value-call #'list (consp x))))))`,
    [['nil 0', '(NIL)'], ['(list 1) 3', '((T))']], false],
  ['nested loops', `(lambda (x n) ${policy} (let ((sum 0))
    (dotimes (i n sum) (dotimes (j i) (when (consp x) (incf sum))))))`,
    [['nil 0', '(0)'], ['nil 6', '(0)'], ['(list 1) 6', '(15)']], true],
  ['outer iteration snapshot', `(lambda (n) ${policy} (let ((sum 0))
    (dotimes (i n sum) (declare (type (unsigned-byte 32) i))
      (let ((a i)) (dotimes (j n) (when (oddp a) (incf sum)))))))`,
    [['0', '(0)'], ['1', '(0)'], ['6', '(18)']], true],
  ['outer scoped type proof', `(lambda (x n) ${policy} (let ((sum 0))
    (dotimes (i n sum) (let ((a (the fixnum x)))
      (dotimes (j n) (when (minusp a) (incf sum)))))))`,
    [[':not-an-integer 0', '(0)'], ['-1 5', '(25)'], ['1 5', '(0)']], true],
  ['conditional inner entry proof', `(lambda (x n) ${policy} (let ((sum 0))
    (dotimes (i n sum) (when x (let ((a (the fixnum x)))
      (dotimes (j n) (when (minusp a) (incf sum))))))))`,
    [['nil 5', '(0)'], [':not-an-integer 0', '(0)'], ['-1 5', '(25)']], true],
  ...[['addition', '(+ a 1)', '11'], ['subtraction', '(- a 1)', '9'],
      ['multiplication', '(* a 3)', '30'], ['increment', '(1+ a)', '11'],
      ['decrement', '(1- a)', '9'], ['bitwise and', '(logand a 7)', '2'],
      ['bitwise or', '(logior a 7)', '15'], ['bitwise xor', '(logxor a 7)', '13']]
    .map(([name, operation, result]) => [`immediate ${name}`,
      `(lambda (x n) ${policy} (let ((a (the (integer -100 100) x)) (out nil))
        (dotimes (i n out) (setq out ${operation}))))`,
      [['10 0', '(NIL)'], ['10 5', `(${result})`]], true]),
  ['arithmetic body assertion', `(lambda (x n) ${policy} (let ((out nil))
    (dotimes (i n out) (setq out (+ (the (integer -100 100) x) 1)))))`,
    [[':not-an-integer 0', '(NIL)'], ['10 5', '(11)']], false],
  ['wide integer narrow mask', `(lambda (x n) ${policy}
    (let ((a (the fixnum x)) (out nil)) (dotimes (i n out) (setq out (logand a 7)))))`,
    [['-9223372036854775808 0', '(NIL)'], ['-9223372036854775808 2', '(0)'],
      ['9223372036854775807 2', '(7)']], true],
  ['allocating integer result', `(lambda (x n) ${policy}
    (let ((a (the (unsigned-byte 30) x)) (out nil))
      (dotimes (i n out) (setq out (+ a 1)))))`,
    [['1073741823 0', '(NIL)'], ['1073741823 2', '(1073741824)']], false],
  ['result assertion supplies no allocation proof', `(lambda (x y n) ${policy}
    (declare (type (signed-byte 32) x y))
    (let ((out nil)) (dotimes (i n out) (setq out (the (signed-byte 31) (* x y))))))`,
    [['2147483647 2147483647 0', '(NIL)'], ['10 -10 2', '(-100)']], false],
  ['division may signal', `(lambda (x n) ${policy} (declare (inline /))
    (let ((out nil)) (dotimes (i n out) (setq out (/ 1 x)))))`,
    [['0 0', '(NIL)'], ['2 2', '(1/2)']], false],
  ['allocation identity', `(lambda (x n) ${policy} (declare (inline cons))
    (let ((out nil)) (dotimes (i n) (push (cons x nil) out))
      (values (length out) (and (cdr out) (eq (car out) (cadr out))))))`,
    [[':x 0', '(0 NIL)'], [':x 2', '(2 NIL)']], false],
];
try {
  vm.evaluate(`(in-package :miricl-compiler) (setq *compile-evaluations* nil) (ensure-compiler)
    (defparameter *licm-effects* 0)
    (defun licm-probe (x) (incf *licm-effects*) x) (values)`);
  for (const [name, source, calls] of cases) {
    const variants = [];
    {
      vm.evaluate(`(progn
        (multiple-value-bind (bytes literals) (emit-native-module (compile-to-hir '${source}))
          (defparameter *licm-function* (compiler-primitive 17 bytes literals nil))
          (with-open-file (out "/licm.wasm" :direction :output :if-exists :supersede
              :element-type '(unsigned-byte 8)) (write-sequence bytes out)))) (values)`);
      const bytes = Buffer.from(await vm.readFile('/licm.wasm')); assert.ok(WebAssembly.validate(bytes), name); variants.push(bytes);
      if (name === 'entry type proof' || name === 'two cached branch tests')
        assert.ok(!WebAssembly.Module.imports(new WebAssembly.Module(bytes))
          .some(entry => entry.name === 'native-boolean'), `${name}: branch-only results stay unboxed`);
      if (name === 'stable tag') {
        const imports = WebAssembly.Module.imports(new WebAssembly.Module(bytes)).filter(entry => entry.kind === 'function');
        const index = imports.findIndex(entry => entry.name === 'native-consp-inline'); assert.ok(index >= 0);
        vm.mountFile('/licm-body.bin', firstBody(bytes));
        assert.equal(vm.evaluate(`(with-open-file (in "/licm-body.bin" :element-type '(unsigned-byte 8))
          (let ((body (make-array (file-length in) :element-type '(unsigned-byte 8))))
            (read-sequence body in)
            (let* ((ops (native-read-ops body))
                   (call (position-if (lambda (op) (and (= (native-op-code op) #x10)
                             (eql (native-op-argument op) ${index}))) ops))
                   (loop (position #x03 ops :key (lambda (op) (native-op-code op)))))
              (and call loop (< call loop)))))`).output, 'T\n', 'Predicate placement relative to LOOP');
      }
      for (const [args, expected] of calls)
        assert.equal(vm.evaluate(`(multiple-value-list (funcall *licm-function* ${args}))`).output,
          `${expected}\n`, `${name}, hoisting standard`);
    }
    if (name === 'entry type proof') assert.ok(variants[0].length <= 595, name);
    if (name.startsWith('immediate ')) assert.ok(variants[0].length <= 530, name);
    if (name === 'wide integer narrow mask') assert.ok(variants[0].length <= 565, name);
    if (name === 'observed Lisp boolean') assert.ok(variants[0].length <= 570, name);
    console.log(`LICM ${name}: ${variants[0].length} bytes`);
  }
  console.log('LICM: predicates, raw branch caches, observed booleans, nonallocating arithmetic, dominance, scoped proofs, zero iterations, effects, mutable state, full values and allocation identity pass.');
} finally {vm.close();}
