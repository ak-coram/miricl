import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {gzipSync} from 'node:zlib';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  const sizes = [];
  for (const [name, source, limit, calls] of [
    ['branch', '(lambda (flag) (if flag 1 2))', 250, [['t', '1\n'], ['nil', '2\n']]],
    ['nested', '(lambda (x y) (if x (if y 1 2) (if y 3 4)))', 315,
      [['t t', '1\n'], ['t nil', '2\n'], ['nil t', '3\n'], ['nil nil', '4\n']]],
    ['exits', '(lambda (x y) (block done (if x (return-from done (values 1 2))) (if y (return-from done (values))) 3))', 380,
      [['t nil', '1\n2\n'], ['nil t', ''], ['nil nil', '3\n']]],
    ['loop', '(lambda (n) (let ((a 1) (b 2)) (dotimes (i n) (if (evenp i) (incf a b) (incf b a))) (values a b)))', 650,
      [['0', '1\n2\n'], ['6', '21\n34\n']]],
    ['nested-loop', '(lambda (n) (let ((sum 0)) (dotimes (i n) (dotimes (j i) (incf sum j))) sum))', 630,
      [['0', '0\n'], ['6', '20\n']]],
  ]) {
    vm.evaluate(`(multiple-value-bind (bytes literals)
      (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))
      (declare (ignore literals))
      (with-open-file (out "/code.wasm" :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8)) (write-sequence bytes out)))`);
    const bytes = await vm.readFile('/code.wasm');
    assert.ok(WebAssembly.validate(bytes));
    assert.ok(bytes.length <= limit, `${name}: ${bytes.length} bytes exceeds ${limit}.`);
    vm.evaluate(`(defparameter *size-function* (miricl-compiler:compile-native '${source}))`);
    for (const [args, output] of calls)
      assert.equal(vm.evaluate(`(funcall *size-function* ${args})`).output, output, `${name}: ${args}`);
    sizes.push(`${name} ${bytes.length} raw/${gzipSync(bytes, {level: 9}).length} gzip`);
  }
  // Execution checks the observable state that stackification
  // and CFG coloring must preserve, rather than particular instruction shapes.
  for (const [source, calls] of [
    [`(lambda (flag)
       (let ((events nil))
         (flet ((mark (x) (push x events) x))
           (values (list (mark 1) (if flag (mark 2) (mark 3)) (mark 4)) events))))`,
      [['t', '(1 2 4)\n(4 2 1)\n'], ['nil', '(1 3 4)\n(4 3 1)\n']]],
    [`(lambda (n)
       (let ((a 1) (b 2))
         (dotimes (i n) (if (evenp i) (incf a b) (incf b a)))
         (values a b)))`, [['0', '1\n2\n'], ['6', '21\n34\n']]],
    ['(lambda (&optional (x :default supplied)) (values x supplied))',
      [['', ':DEFAULT\nNIL\n'], ['nil', 'NIL\nT\n'], ['17', '17\nT\n']]],
    [`(lambda (flag)
       (values (block done
                 (if flag (return-from done 11) (return-from done 22))
                 :unreachable)
               :tail))`, [['t', '11\n:TAIL\n'], ['nil', '22\n:TAIL\n']]],
    [`(lambda (n) (declare (type (signed-byte 32) n) (optimize (safety 0)))
       (values (+ n -2147483648) (- n 2147483647)))`,
      [['-2147483648', '-4294967296\n-4294967295\n'], ['0', '-2147483648\n-2147483647\n'],
       ['2147483647', '-1\n0\n']]],
    [`(lambda (n)
       (declare (type (signed-byte 31) n) (optimize (safety 0)) (inline +))
       (list (+ n 1) -1073741824 1073741823 -1073741825 1073741824))`,
      [['0', '(1 -1073741824 1073741823 -1073741825 1073741824)\n']]],
    [`(lambda (n flag)
       (let ((answer 0))
         (tagbody
          start (when (zerop n) (go done))
                (incf answer) (decf n) (go choose)
          other (when (zerop n) (go done))
                (incf answer 10) (decf n)
          choose (if flag (go start) (go other))
          done)
         answer))`, [['0 t', '0\n'], ['5 t', '5\n'], ['5 nil', '41\n']]],
    [`(lambda (n)
       (let ((events nil) (*size-dynamic* :outer))
         (declare (special *size-dynamic*))
         (dotimes (i n)
           (let ((*size-dynamic* i))
             (declare (special *size-dynamic*))
             (when (evenp i) (push *size-dynamic* events))))
         (values (reverse events) *size-dynamic*)))`,
      [['0', 'NIL\n:OUTER\n'], ['6', '(0 2 4)\n:OUTER\n']]],
    [`(lambda (n)
       (block done
         (let ((escape (lambda (x) (return-from done (values x 99)))))
           (dotimes (i n) (when (= i 3) (funcall escape i))))
         (values n -1)))`, [['0', '0\n-1\n'], ['6', '3\n99\n']]],
    [`(lambda (flag)
       (let ((x 7))
         (block done
           (let ((escape (lambda () (return-from done (values x 19)))))
             (setq x (if flag 41 43)) (funcall escape)))))`,
      [['t', '41\n19\n'], ['nil', '43\n19\n']]],
    [`(lambda (flag)
       (let ((x (list 1)))
         (values (progn (if flag (setf (car x) 2) (setf (car x) 3)) x)
                 (car x) (values))))`, [['t', '(2)\n2\nNIL\n'], ['nil', '(3)\n3\nNIL\n']]],
  ]) {
    {
      vm.evaluate(`(defparameter *size-function*
        (progn
          (miricl-compiler:compile-native '${source})))`);
      for (const [args, output] of calls)
        assert.equal(vm.evaluate(`(funcall *size-function* ${args})`).output, output,
          `${source}: ${args}, optimize standard`);
    }
  }
  // Public HIR need not place its entry block first. Rotating a cyclic CFG
  // keeps IDs dense while requiring a nonzero initial dispatcher index.
  {
    vm.evaluate(`(let* (
                       (unit (miricl-compiler:compile-to-hir
                         '(lambda (n) (let ((sum 0)) (dotimes (i n) (incf sum i)) sum))))
                       (function (first (miricl-compiler::hir-unit-functions unit)))
                       (blocks (miricl-compiler::hir-function-blocks function)))
      (setf (miricl-compiler::hir-function-blocks function) (append (cdr blocks) (list (car blocks))))
      (loop for block in (miricl-compiler::hir-function-blocks function) for id from 0
            do (setf (miricl-compiler::hir-block-id block) id))
      (multiple-value-bind (bytes literals) (miricl-compiler:emit-native-module unit)
        (defparameter *size-reordered* (miricl-compiler::compiler-primitive 17 bytes literals nil))))`);
    assert.equal(vm.evaluate('(funcall *size-reordered* 0)').output, '0\n');
    assert.equal(vm.evaluate('(funcall *size-reordered* 6)').output, '15\n');
  }
  const bindings = Array.from({length: 140}, (_, i) => `(v${i} (+ n ${i}))`).join(' ');
  // Removing a constant arm also removes its now-unused imports. The surviving
  // closure's REF.FUNC must follow the new function-index base.
  const constantArm = `(lambda () (if nil
    (progn (error "Unselected arm")
      (vector ${Array.from({length: 130}, (_, i) => `(lambda (x) (+ x ${i}))`).join(' ')}))
    (lambda (x) (+ x 1))))`;
  const importCounts = [];
  const functionCounts = [];
  const definedFunctions = bytes => {
    let position = 8;
    const u32 = () => {
      let value = 0, shift = 0, byte;
      do {byte = bytes[position++]; value += (byte & 127) * 2 ** shift; shift += 7;} while (byte & 128);
      return value;
    };
    while (position < bytes.length) {
      const id = bytes[position++], length = u32();
      if (id === 3) return u32();
      position += length;
    }
    return 0;
  };
  {
    // Inspect the lowering boundary to exercise Wasm cleanup and relocation.
    vm.evaluate(`(progn
      (multiple-value-bind (bytes literals)
          (miricl-compiler:emit-native-module (miricl-compiler::lower-source-to-hir '${constantArm}))
        (declare (ignore literals))
        (with-open-file (out "/constant-arm.wasm" :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8)) (write-sequence bytes out)))
      (defparameter *constant-arm* (miricl-compiler:compile-native '${constantArm})))`);
    const code = await vm.readFile('/constant-arm.wasm');
    const module = await WebAssembly.compile(code);
    importCounts.push(WebAssembly.Module.imports(module).length);
    functionCounts.push(definedFunctions(code));
    assert.equal(vm.evaluate('(funcall (funcall *constant-arm*) 41)').output, '42\n');
  }
  assert.ok(importCounts[0] <= 20, `Pruned import budget: ${importCounts}`);
  assert.equal(functionCounts[0], 2, `Only the factory and returned closure remain: ${functionCounts}`);
  assert.ok(Number(vm.evaluate(`(length (miricl-compiler:hir-unit-functions
    (miricl-compiler::lower-source-to-hir '${constantArm})))`).output.trim()) > 128 && functionCounts[0] < 128,
    'Function relocation must cross the one-byte LEB boundary.');
  const variables = Array.from({length: 140}, (_, i) => `v${i}`);
  const wide = `(lambda (n) (let (${bindings})
    (if (minusp n) (list ${variables.toReversed().join(' ')}) (list ${variables.join(' ')}))))`;
  {
    vm.evaluate(`(defparameter *size-function*
      (progn
        (miricl-compiler:compile-native '${wide})))`);
    for (const n of [-5, 5]) {
      const expected = Array.from({length: 140}, (_, i) => n + i);
      if (n < 0) expected.reverse();
      assert.equal(vm.evaluate(`(equal (funcall *size-function* ${n}) '(${expected.join(' ')}))`).output,
        'T\n', `wide live ranges: ${n}, optimize standard`);
    }
  }
  // Keep a wide set of bindings live at a call that can escape to a handler.
  // Its exceptional reads must survive even when normal-path locals are dead.
  const wideHandler = `(lambda (n flag) (block done (let (${bindings})
    (funcall (lambda () (when flag (return-from done (values v0 v139)))))
    (list ${variables.join(' ')}))))`;
  {
    vm.evaluate(`(defparameter *size-function*
      (progn
        (miricl-compiler:compile-native '${wideHandler})))`);
    assert.equal(vm.evaluate('(funcall *size-function* 5 t)').output, '5\n144\n');
    const expected = Array.from({length: 140}, (_, i) => i + 5);
    assert.equal(vm.evaluate(`(equal (funcall *size-function* 5 nil) '(${expected.join(' ')}))`).output,
      'T\n', `wide exceptional live ranges, optimize standard`);
  }
  console.log(`Native size budgets: ${sizes.join('; ')} bytes; branch and multiple-value paths pass.`);
} finally {vm.close();}
