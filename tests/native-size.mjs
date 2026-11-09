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
  // Differential execution checks the observable state that stackification
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
    for (const optimize of ['nil', 't']) {
      vm.evaluate(`(defparameter *size-function*
        (let ((miricl-compiler::*wasm-optimize* ${optimize}))
          (miricl-compiler:compile-native '${source})))`);
      for (const [args, output] of calls)
        assert.equal(vm.evaluate(`(funcall *size-function* ${args})`).output, output,
          `${source}: ${args}, optimize ${optimize}`);
    }
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
  for (const optimize of ['nil', 't']) {
    vm.evaluate(`(let ((miricl-compiler::*wasm-optimize* ${optimize}))
      (multiple-value-bind (bytes literals)
          (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${constantArm}))
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
  assert.ok(importCounts[1] < importCounts[0], `Unused imports must disappear: ${importCounts}`);
  assert.ok(functionCounts[1] < functionCounts[0], `Unreachable functions must disappear: ${functionCounts}`);
  assert.ok(functionCounts[0] > 128 && functionCounts[1] < 128,
    'Function relocation must cross the one-byte LEB boundary.');
  const variables = Array.from({length: 140}, (_, i) => `v${i}`);
  const wide = `(lambda (n) (let (${bindings})
    (if (minusp n) (list ${variables.toReversed().join(' ')}) (list ${variables.join(' ')}))))`;
  for (const optimize of ['nil', 't']) {
    vm.evaluate(`(defparameter *size-function*
      (let ((miricl-compiler::*wasm-optimize* ${optimize}))
        (miricl-compiler:compile-native '${wide})))`);
    for (const n of [-5, 5]) {
      const expected = Array.from({length: 140}, (_, i) => n + i);
      if (n < 0) expected.reverse();
      assert.equal(vm.evaluate(`(equal (funcall *size-function* ${n}) '(${expected.join(' ')}))`).output,
        'T\n', `wide live ranges: ${n}, optimize ${optimize}`);
    }
  }
  // Keep a wide set of bindings live at a call that can escape to a handler.
  // Its exceptional reads must survive even when normal-path locals are dead.
  const wideHandler = `(lambda (n flag) (block done (let (${bindings})
    (funcall (lambda () (when flag (return-from done (values v0 v139)))))
    (list ${variables.join(' ')}))))`;
  for (const optimize of ['nil', 't']) {
    vm.evaluate(`(defparameter *size-function*
      (let ((miricl-compiler::*wasm-optimize* ${optimize}))
        (miricl-compiler:compile-native '${wideHandler})))`);
    assert.equal(vm.evaluate('(funcall *size-function* 5 t)').output, '5\n144\n');
    const expected = Array.from({length: 140}, (_, i) => i + 5);
    assert.equal(vm.evaluate(`(equal (funcall *size-function* 5 nil) '(${expected.join(' ')}))`).output,
      'T\n', `wide exceptional live ranges, optimize ${optimize}`);
  }
  console.log(`Native size budgets: ${sizes.join('; ')} bytes; branch and multiple-value paths pass.`);
} finally {vm.close();}
