// Reuse a retained CELL helper only for decoded CONS(value,NIL) instructions.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';
const vm = await createCompilerTestVM(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const word = value => {const bytes = []; do {const byte = value & 127; value >>>= 7; bytes.push(byte | (value ? 128 : 0));} while (value); return bytes;};
const text = value => [...word(value.length), ...Buffer.from(value)];
const section = (id, bytes) => [id, ...word(bytes.length), ...bytes];
const moduleBytes = (names, body, references = []) => Uint8Array.from([0,97,115,109,1,0,0,0,
  ...section(1, [3, 0x60,1,0x6f,1,0x6f, 0x60,2,0x6f,0x6f,1,0x6f, 0x60,0,0]),
  ...section(2, [...word(names.length), ...names.flatMap(name => [
    ...text('m'), ...text(name.startsWith('native-') ? name.slice(7) : name), 0, ...word(name === 'native-cell' ? 0 : name === 'native-cons' ? 1 : 2)])]),
  ...section(3, [1,1]), ...section(7, [1,...text('entry'),0,...word(names.length)]),
  ...(references.length ? section(9, [1,3,0,...word(references.length),...references.flatMap(word)]) : []),
  ...section(10, [1,...word(body.length),...body])]);
const helpers = names => Object.fromEntries(names.map(name => [name.startsWith('native-') ? name.slice(7) : name, name === 'native-cell'
  ? value => ({car:value,cdr:null}) : name === 'native-cons' ? (car,cdr) => ({car,cdr}) : () => {}]));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  for (const [count, unused, reference] of [[2,0], [127,0], [128,0], [140,0], [140,138],
                                          [2,0,'return-call'], [2,0,'ref-func']]) {
    // Keep all imports live and keep a non-NIL CONS, so the profitable change
    // has no declaration/pruning benefit that could hide body growth.
    const noops = Array.from({length:count - 2}, (_,i) => `cell-noop-${i}`);
    const names = unused ? [...noops, 'native-cell', 'native-cons'] : ['native-cell', 'native-cons', ...noops];
    const cell = names.indexOf('native-cell'), cons = names.indexOf('native-cons');
    const body = [0, 0x20,0,0x10,...word(cell),0x1a, 0x20,0,0xd0,0x6f,0x10,...word(cons),0x1a];
    if (!unused) for (let index=2;index<count;index++) body.push(0x10,...word(index));
    // Nonordinary references must keep CONS retained after another call folds.
    if (reference === 'ref-func') body.push(0xd2,...word(cons),0x1a);
    body.push(0x20,0,0x20,1,reference === 'return-call' ? 0x12 : 0x10,...word(cons),0x0b);
    vm.evaluate(`(in-package :miricl-compiler)
      (defparameter *cell-test-body* (make-array ${body.length} :element-type '(unsigned-byte 8) :initial-contents '(${body.join(' ')})))
      (defparameter *cell-test-imports* '(${names.map(name => `("${name}" ${name === 'native-cell' ? '(:object) :object' : name === 'native-cons' ? '(:object :object) :object' : 'nil nil'})`).join(' ')}))
      (let* ((*native-imports* *cell-test-imports*) (*native-functions* '(:cell-probe))
             (*native-function-indices* (make-hash-table :test 'eq))
             (*wasm-direct-workers* (make-hash-table :test 'eq)) (*wasm-referenced-functions* nil)
             (*wasm-import-counts* (make-hash-table :test 'equal)))
        (setf (gethash :cell-probe *native-function-indices*) 0)
        (loop for op across (native-read-ops *cell-test-body* t) do
          (incf (gethash (first (nth (native-op-argument op) *native-imports*)) *wasm-import-counts* 0)))
        (let ((bytes (first (native-prune-module (list *cell-test-body*) nil))))
          (with-open-file (out "/cell-body.bin" :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
            (write-sequence bytes out)))
        (defparameter *cell-test-order* (mapcar #'first *native-imports*))) (values)`);
    const result = await vm.readFile('/cell-body.bin');
    const order = vm.evaluate('(progn (format t "~{~A~%~}" *cell-test-order*) (values))').output.trim().split('\n');
    if (reference) assert.ok(order.includes('native-cons'), `${reference} retains CONS`);
    const before = moduleBytes(names, body, reference === 'ref-func' ? [cons] : []);
    const after = moduleBytes(order, result, reference === 'ref-func' ? [order.indexOf('native-cons')] : []);
    assert.ok(WebAssembly.validate(before) && WebAssembly.validate(after));
    if (count - unused < 128) assert.ok(after.length < before.length, 'Retained one-byte import indices yield a smaller complete module');
    else assert.equal(result.length, body.length, 'Conservative fallback keeps a wide-import body unchanged');
    for (const bytes of [before, after]) {
      const {instance} = await WebAssembly.instantiate(bytes, {m:helpers(names)});
      const car = {}, cdr = {};
      assert.deepEqual(instance.exports.entry(car,cdr), {car,cdr}, 'Non-NIL tail and input identities survive relocation');
    }
  }
  // The ABI NULL heap type must be extern. A different heap type and bytes in
  // a constant immediate must not be interpreted as a wrapping operation.
  for (const bytes of [[0,0x20,0,0xd0,0x70,0x10,1,0x0b],
                       [0,0x20,0,0x42,0xd0,0x6f,0x10,1,0x0b]]) {
    check(`(let* ((*native-imports* '(("native-cell" (:object) :object) ("native-cons" (:object :object) :object)))
                  (*wasm-import-counts* (make-hash-table :test 'equal))
                  (body #(${bytes.join(' ')})) (decoded (vector (native-read-ops body t))))
             (not (native-fold-cells (list body) decoded #(t) #(t t))))`);
  }
  // No added helper import, and no work on dead bodies or a module that has
  // not retained CELL already. Also allow CONS to disappear when truly unused.
  check(`(let* ((*native-imports* '(("native-cell" (:object) :object) ("native-cons" (:object :object) :object)))
                (*wasm-import-counts* (make-hash-table :test 'equal))
                (body #(0 32 0 208 111 16 1 11)))
           (and (not (native-fold-cells (list body) (vector (native-read-ops body t)) #(t) #(nil t)))
                (not (native-fold-cells (list body) (vector (native-read-ops body t)) #(nil) #(t t)))
                (let ((used (vector t t)))
                  (and (native-fold-cells (list body) (vector (native-read-ops body t)) #(t) used)
                       (aref used 0) (not (aref used 1))))))`);
  vm.evaluate('(in-package :cl-user) (values)');
  // Escaping closures require real mutable cells. Wrapping their return values
  // must preserve sharing, fresh list allocation, order and multiple values.
  vm.evaluate(`(defparameter *cell-factory* (miricl-compiler:compile-native
    '(lambda (x) (let ((state x))
      (vector (lambda () (values state :extra))
              (lambda (value) (setq state value))
              (lambda () (list state))))))) (values)`);
  check(`(let* ((functions (funcall *cell-factory* :initial))
                 (read (aref functions 0)) (write (aref functions 1)) (wrap (aref functions 2))
                 (first (funcall wrap)))
            (and (equal (multiple-value-list (funcall read)) '(:initial :extra))
                 (eq (funcall write :changed) :changed)
                 (equal (multiple-value-list (funcall read)) '(:changed :extra))
                 (equal first '(:initial)) (equal (funcall wrap) '(:changed))
                 (not (eq (funcall wrap) (funcall wrap)))))`);
  for (const [source, expression] of [
    ["(lambda (flag) (block done (if flag (return-from done (values 1 2))) (values :tail)))", "(and (equal (multiple-value-list (funcall *cell-function* t)) '(1 2)) (eq (funcall *cell-function* nil) :tail))"],
    ["(lambda (n) (let ((events nil)) (dotimes (i n) (push i events)) (values (nreverse events) n)))", "(equal (multiple-value-list (funcall *cell-function* 4)) '((0 1 2 3) 4))"],
    ["(lambda () (catch 'done (throw 'done (values :caught 7))))", "(equal (multiple-value-list (funcall *cell-function*)) '(:caught 7))"],
  ]) {
    vm.evaluate(`(defparameter *cell-function* (miricl-compiler:compile-native '${source})) (values)`);
    check(expression);
  }
  console.log('Cell folding: complete module size/validation, import-width fallback, instruction boundaries, mutable captures and multiple values pass.');
} finally {vm.close();}
