import {nativeModuleImports} from './helpers/compiler-imports.mjs';
// Literal/function lookup fusion must keep live lookup before argument effects.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const vm = await createCompilerTestVM(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
try {
  const names = Array.from({length: 140}, (_, i) => `literal-function-${i}`);
  vm.evaluate(names.map((name, i) => `(defun ${name} (x) (+ x ${i}))`).join('\n'));
  vm.evaluate(`(defun (setf literal-function-place) (x) (list :setter x))
    (defun literal-function-multiple (x) (values x :second)) (values)`);
  let exercised = false;
  for (const count of [1, 32, names.length]) {
    vm.evaluate(`(defun literal-function-live (x) (list :old x)) (values)`);
    const source = `(lambda (x)
      (list ${names.slice(0, count).map(name => `(${name} x)`).join(' ')}
        (literal-function-live
          (progn (setf (symbol-function 'literal-function-live)
                   (lambda (value) (list :new value))) x))
        (literal-function-live x)
        (funcall #'(setf literal-function-place) x)
        (multiple-value-list (literal-function-multiple x))
        (handler-case (funcall #'literal-function-undefined x)
          (undefined-function (condition)
            (eq (cell-error-name condition) 'literal-function-undefined)))))`;
    vm.evaluate(`(multiple-value-bind (bytes literals)
        (miricl-compiler:emit-native-module (miricl-compiler:compile-to-hir '${source}))
      (declare (ignore literals))
      (with-open-file (out "/literal-functions.wasm" :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values)`);
    const bytes = await vm.readFile('/literal-functions.wasm');
    assert.ok(WebAssembly.validate(bytes));
    const imports = nativeModuleImports(await WebAssembly.compile(bytes));
    exercised ||= imports.some(entry => entry.name === 'native-literal-function');
    vm.evaluate(`(defparameter *literal-function-probe* (miricl-compiler:compile-native '${source})) (values)`);
    const expected = `(${Array.from({length: count}, (_, i) => i + 7).join(' ')}
      (:old 7) (:new 7) (:setter 7) (7 :second) t)`;
    assert.equal(vm.evaluate(`(equal (funcall *literal-function-probe* 7) '${expected})`).output, 'T\n');
    assert.equal(vm.evaluate('(literal-function-live 8)').output, '(:NEW 8)\n',
      'A rejected undefined reference leaves the runtime usable');
  }
  assert.ok(exercised, 'Exercise function resolution from the literal pool through the shared helper');
  // Keep enough distinct closures alive to exercise function-index relocation
  // across LEB width boundaries while each closure reads its own literal pool.
  const factory = `(lambda () (vector ${names.map(name => `(lambda (x) (${name} x))`).join(' ')}))`;
  vm.evaluate(`(defparameter *literal-function-factory* (miricl-compiler:compile-native '${factory})) (values)`);
  const positions = [0, Math.floor(names.length / 2), names.length - 1];
  assert.equal(vm.evaluate(`(let ((functions (funcall *literal-function-factory*)))
    (equal (list ${positions.map(index => `(funcall (aref functions ${index}) 7)`).join(' ')})
      '(${positions.map(index => index + 7).join(' ')})))`).output, 'T\n');
  // Price whole modules when a shared entry has many exported aliases. An
  // additional import can move that entry across a function-index LEB tier.
  const unsigned = number => {
    const bytes = [];
    do {const byte = number & 127; number >>>= 7; bytes.push(number ? byte | 128 : byte);} while (number);
    return bytes;
  };
  const nameBytes = name => [...unsigned(name.length), ...Buffer.from(name)];
  const section = (id, bytes) => [id, ...unsigned(bytes.length), ...bytes];
  const pairs = 30, aliasCount = pairs * 4;
  for (const ordinaryImports of [126, 127]) {
    const helper = ordinaryImports;
    const importNames = ['native-literal', 'native-function-reference',
      ...Array.from({length: ordinaryImports - 2}, (_, i) => `native-cost-probe-${i}`), 'native-literal-function'];
    const body = [0];
    for (let i = 0; i < pairs; i++) body.push(0x20, 0, 0x41, 0, 0x10, 0, 0x10, 1, 0x1a);
    // Both original imports survive, so the fused helper adds one import.
    body.push(0x20, 0, 0x41, 0, 0x10, 0, 0x1a, 0xd0, 0x6f, 0x10, 1, 0x1a);
    for (let i = 2; i < ordinaryImports; i++) body.push(0x10, ...unsigned(i));
    body.push(0xd0, 0x6f, 0x0b);
    const moduleBytes = (order, body) => Buffer.from([0, 97, 115, 109, 1, 0, 0, 0,
      ...section(1, [4, 0x60, 2, 0x6f, 0x7f, 1, 0x6f, 0x60, 1, 0x6f, 1, 0x6f,
        0x60, 0, 0, 0x60, 2, 0x6f, 0x6f, 1, 0x6f]),
      ...section(2, [...unsigned(order.length), ...order.flatMap(i => [
        ...nameBytes('m'), ...nameBytes(importNames[i].slice(7)), 0,
        ...unsigned(i === 0 || i === helper ? 0 : i === 1 ? 1 : 2)])]),
      ...section(3, [1, 3]),
      ...section(7, [...unsigned(aliasCount), ...Array.from({length: aliasCount}, (_, i) => [
        ...nameBytes(`entry${i}`), 0, ...unsigned(order.length)]).flat()]),
      ...section(10, [1, ...unsigned(body.length), ...body])]);
    const imports = importNames.map((name, i) => `("${name}" ${i === 0 || i === helper
      ? '(:object :i32) :object' : i === 1 ? '(:object) :object' : 'nil nil'})`).join(' ');
    assert.equal(vm.evaluate(`(progn (let* ((miricl-compiler::*native-imports* '(${imports}))
      (miricl-compiler::*wasm-import-counts* (make-hash-table :test 'equal))
      (miricl-compiler::*native-function-indices* (make-hash-table :test 'eq))
      (miricl-compiler::*wasm-direct-workers* (make-hash-table :test 'eq))
      (miricl-compiler::*wasm-referenced-functions* nil) (miricl-compiler::*native-functions* '(:probe))
      (body (make-array ${body.length} :element-type '(unsigned-byte 8) :initial-contents '(${body.join(' ')})))
      (decoded (vector (miricl-compiler::native-read-ops body t)))
      (used (make-array ${importNames.length} :initial-element t)))
      (setf (aref used ${helper}) nil (gethash :probe miricl-compiler::*native-function-indices*) 0)
      (loop for import in miricl-compiler::*native-imports* for i from 0 do
        (setf (gethash (car import) miricl-compiler::*wasm-import-counts*)
          (cond ((< i 2) ${pairs + 1}) ((= i ${helper}) 0) (t 1))))
      (miricl-compiler::native-fuse-literal-functions (list body) decoded #(t) #(0) used
        (make-list ${aliasCount} :initial-element :probe))
      (let* ((order (stable-sort (loop for i below ${importNames.length} when (aref used i) collect i) #'>
                    :key (lambda (i) (gethash (car (nth i miricl-compiler::*native-imports*))
                                             miricl-compiler::*wasm-import-counts*))))
             (mapping (make-array ${importNames.length})))
        (loop for old in order for new from 0 do (setf (aref mapping old) new))
        (with-open-file (out "/cost-order.txt" :direction :output :if-exists :supersede)
          (format out "~{~D~^ ~}" order))
        (with-open-file (out "/cost-body.bin" :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
          (write-sequence (miricl-compiler::with-wasm-bytes
            (let ((cursor 0))
              (loop for op across (aref decoded 0) do
                (miricl-compiler::wasm-append body cursor (miricl-compiler::native-op-start op))
                (when (miricl-compiler::native-op-code op)
                  (miricl-compiler::wasm-byte (miricl-compiler::native-op-code op))
                  (miricl-compiler::wasm-u32 (aref mapping (miricl-compiler::native-op-argument op))))
                (setq cursor (miricl-compiler::native-op-end op)))
              (miricl-compiler::wasm-append body cursor))) out)))) (values))`).output, '');
    const order = Buffer.from(await vm.readFile('/cost-order.txt')).toString('utf8').split(' ').map(Number);
    const before = moduleBytes(Array.from({length: ordinaryImports}, (_, i) => i), body);
    const after = moduleBytes(order, await vm.readFile('/cost-body.bin'));
    assert.ok(WebAssembly.validate(before)); assert.ok(WebAssembly.validate(after));
    assert.ok(after.length <= before.length, 'Shared entry aliases must not turn lookup fusion into module growth');
    if (ordinaryImports === 126) assert.ok(after.length < before.length, 'Keep profitable nonboundary fusion');
  }
  console.log('Literal function lookup fusion preserves effects, live bindings, SETF names and values.');
} finally {vm.close();}
