import {nativeModuleImports} from './helpers/compiler-imports.mjs';
// Target nominal proofs are shared by inference and native lowering.
import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {createCompilerTestVM} from './compiler-test-vm.mjs';

const vm = await createCompilerTestVM(await fs.readFile(process.argv[2] ?? 'miricl.wasm'));
const check = source => assert.equal(vm.evaluate(source).output, 'T\n', source);
try {
  vm.evaluate('(in-package :miricl-compiler) (ensure-compiler) (values)');
  const relations = [
    ['simple-vector', 'vector'], ['(simple-vector 5)', 'sequence'],
    ['simple-vector', 'simple-array'], ['simple-string', 'string'],
    ['(simple-string 5)', 'vector'], ['simple-base-string', 'simple-string'],
    ['simple-bit-vector', 'array'], ['bit-vector', 'sequence'],
    ['(simple-array t (5))', 'simple-vector'], ['(simple-array t 1)', 'simple-vector'],
    ['(simple-array * (*))', 'vector'], ['(array * 1)', 'vector'],
    ['(simple-array character (5))', 'simple-string'], ['(array base-char (*))', 'string'],
    ['integer', 'number'], ['(integer 0 255)', 'rational'], ['(mod 17)', 'real'],
    ['ratio', 'rational'], ['(rational -1 1)', 'real'], ['single-float', 'float'],
    ['(double-float -1d0 1d0)', 'number'], ['complex', 'number'], ['null', 'sequence'],
  ];
  for (const [type, required] of relations)
    check(`(hir-type-proves-p '${type} '${required})`);
  for (const [type, required] of [
    ['(simple-array bit (5))', 'simple-vector'], ['(simple-array character (5))', 'simple-vector'],
    ['(simple-array * (5))', 'simple-vector'], ['(simple-array t (5 5))', 'vector'],
    ['(simple-array t *)', 'simple-vector'], ['(vector t 5)', 'simple-vector'],
    ['array', 'vector'], ['vector', 'simple-array'], ['complex', 'real'], ['real', 'rational'],
    ['float', 'integer'], ['(or null simple-vector)', 'vector'],
    ['(or simple-string nominal-opaque)', 'vector'], ['(satisfies identity)', 'vector'],
    ['(simple-array (satisfies identity) (5))', 'simple-vector'],
    ['(simple-array (unsigned-byte 8) (5))', 'simple-vector'],
    ['simple-vector', '(simple-vector 5)'], ['simple-vector', '(vector bit)'],
  ]) check(`(not (hir-type-proves-p '${type} '${required}))`);
  check(`(and (not (hir-type-proves-p 'simple-vector 'vector 0))
    (not (hir-type-proves-p 'simple-vector 'vector 32 (list 0))))`);
  check(`(progn
    (defparameter *nominal-expansion-effects* 0)
    (deftype nominal-opaque () (incf *nominal-expansion-effects*) 'simple-vector)
    (defun nominal-satisfies (value) (declare (ignore value))
      (incf *nominal-expansion-effects*) t)
    (and (not (hir-type-proves-p 'nominal-opaque 'vector))
      (not (hir-type-proves-p '(satisfies nominal-satisfies) 'vector))
      (hir-type-proves-p '(and nominal-opaque simple-vector) 'vector)
      (hir-type-proves-p '(simple-array nominal-opaque (5)) 'vector)
      (not (hir-type-proves-p '(simple-array nominal-opaque (5)) 'simple-vector))
      (zerop *nominal-expansion-effects*)))`);
  // Check every claimed family relation against target TYPEP, with values
  // spanning representations. This must never depend on the host's hierarchy.
  check(`(let ((relations '(${relations.map(([a, b]) => `(${a} ${b})`).join(' ')}))
               (samples (list nil t :keyword '(1 2) #(1 2 3 4 5) "abcde"
                 (make-array '(5) :element-type 'bit :initial-element 0)
                 (make-array '(2 2) :initial-element 0)
                 (make-array '(5) :fill-pointer 2 :initial-element 0)
                 0 17 -17 (ash 1 100) 3/2 0.5f0 0.5d0 #c(1 2))))
    (every (lambda (pair)
      (every (lambda (value) (or (not (typep value (first pair)))
                               (typep value (second pair)))) samples)) relations))`);
  const compile = async source => {
    vm.evaluate(`(multiple-value-bind (bytes literals)
        (emit-native-module (compile-to-hir '${source}))
      (defparameter *nominal-function* (compiler-primitive 17 bytes literals nil))
      (with-open-file (out "/nominal.wasm" :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values)`);
    const bytes = await vm.readFile('/nominal.wasm');
    const module = await WebAssembly.compile(bytes);
    return {bytes: bytes.length, imports: nativeModuleImports(module).map(entry => entry.name)};
  };
  for (const [type, predicate, argument] of [
    ['simple-vector', 'vectorp', '#(1 2)'], ['simple-string', 'stringp', '"abc"'],
    ['(simple-array t (5))', 'simple-vector-p', '#(1 2 3 4 5)'],
  ]) {
    const source = declared => `(lambda (x)
      (declare ${declared ? `(type ${type} x)` : ''}
        (inline ${predicate}) (optimize (safety 0) (debug 0)))
      (if (${predicate} x) (values :yes :second) (values)))`;
    const ordinary = await compile(source(false));
    const proved = await compile(source(true));
    assert.ok(proved.bytes < ordinary.bytes, `${type}: a proven predicate removes its branch`);
    check(`(equal (multiple-value-list (funcall *nominal-function* ${argument})) '(:yes :second))`);
  }
  const vectorRead = await compile(`(lambda (x)
    (declare (type (simple-array t (5)) x) (inline svref) (optimize (safety 0) (debug 0)))
    (svref x 2))`);
  assert.ok(vectorRead.imports.includes('native-vector-ref-inline'), 'Rank-one general arrays support simple-vector lowering');
  check(`(eq (funcall *nominal-function* #(a b c d e)) 'c)`);
  const length = await compile(`(lambda (x)
    (declare (type simple-string x) (inline length +) (optimize (safety 0) (debug 0)))
    (list (+ (length x) 1)))`);
  assert.ok(length.imports.includes('native-unbox-int64'), 'Bounded string length supports scalar arithmetic');
  check(`(equal (funcall *nominal-function* "abc") '(4))`);
  vm.evaluate('(defparameter *nominal-effects* 0) (values)');
  await compile(`(lambda (x)
    (declare (type simple-vector x) (special *nominal-effects*)
      (inline vectorp 1+) (optimize (safety 0) (debug 0)))
    (if (vectorp (progn (setq *nominal-effects* (1+ *nominal-effects*)) (values x :ignored)))
        (values :yes :second) (values)))`);
  check(`(and (equal (multiple-value-list (funcall *nominal-function* #(1))) '(:yes :second))
              (= *nominal-effects* 1))`);
  // A local callee can use callers' type facts only while its entry stays
  // closed. Exporting that callee must preserve the predicate for outsiders.
  vm.evaluate(`(defparameter *nominal-definitions*
    '((defun nominal-helper (x) (declare (inline vectorp) (optimize (safety 0) (debug 0)))
        (if (vectorp x) :vector :other))
      (defun nominal-entry (x)
        (declare (type simple-vector x) (optimize (safety 0) (debug 0))) (nominal-helper x)))) (values)`);
  for (const exported of [false, true]) {
    vm.evaluate(`(multiple-value-bind (entries bytes)
        (compile-native-module *nominal-definitions*
          :entry-points '(nominal-entry ${exported ? 'nominal-helper' : ''}) :inline nil)
      (defparameter *nominal-entries* entries)
      (with-open-file (out "/nominal-module.wasm" :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8)) (write-sequence bytes out))) (values)`);
    const imports = nativeModuleImports(await WebAssembly.compile(await vm.readFile('/nominal-module.wasm')));
    assert.equal(imports.some(entry => entry.name === 'native-vector-p-inline'), exported,
      'Only closed callees inherit declared argument facts from their callers');
    check(`(eq (funcall (cdr (assoc 'nominal-entry *nominal-entries*)) #(1)) :vector)`);
    if (exported) check(`(eq (funcall (cdr (assoc 'nominal-helper *nominal-entries*)) 7) :other)`);
  }
  console.log('Nominal target proofs preserve array specialization, opaque types, effects and module entry contracts.');
} finally {vm.close();}
