import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
import {compilerSmokeCases} from './compiler-cases.mjs';

const bytes = await fs.readFile(process.argv[2] ?? 'miricl-cold.wasm');
const precompiled = process.argv.includes('--precompiled');
const module = await WebAssembly.compile(bytes);
assert.deepEqual(WebAssembly.Module.imports(module), []);
if (precompiled) {
  assert.equal(WebAssembly.Module.customSections(module, 'name').length, 0);
  assert.equal(WebAssembly.Module.customSections(module, 'miricl.repl-bridge').length, 1);
}
// Both adapter entry points provide the compiler immediately, without any
// filesystem mount, compiler option, companion artifact or asynchronous fetch.
for (const mode of ['async', 'sync']) {
  const vm = mode === 'async' ? await Miricl.instantiate(bytes)
    : new Miricl(new WebAssembly.Instance(module));
  try {
    assert.equal(vm.evaluate("(and (fboundp 'miricl-compiler:compile-native) (fboundp 'miricl-compiler:optimize-system))").output, 'T\n');
    assert.equal(vm.evaluate('miricl-compiler::*compiler-state*').output, precompiled ? ':READY\n' : ':COLD\n');

    assert.equal(vm.evaluate('miricl-compiler:*compile-evaluations*').output, precompiled ? 'T\n' : 'NIL\n');
    if (precompiled) {
      assert.equal(vm.evaluate('miricl-compiler::*compiler-origin*').output, ':IMAGE\n');
      assert.equal(vm.evaluate(`(every (lambda (name)
        (and (compiled-function-p (symbol-function name))
             (null (function-lambda-expression (symbol-function name)))))
        miricl-compiler::*native-compiler-seeds*)`).output, 'T\n');
      // Make an accidental first-use bootstrap fail instead of hiding it.
      vm.evaluate(`(setf (symbol-function 'miricl-compiler::compile-compiler)
        (lambda () (error "Precompiled image tried to bootstrap again."))) (values)`);
      // These cases explicitly compile selected bodies; keep their driver
      // interpreted in both images. Evaluation policy has its own regressions.
      vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil)');
    }
    assert.equal(vm.evaluate('(+ 20 22)').output, '42\n');
    assert.equal(vm.evaluate('miricl-compiler::*compiler-state*').output, precompiled ? ':READY\n' : ':COLD\n');
    assert.equal(vm.evaluate(`(progn
      (defun disassembly-startup-probe (x) (+ x 2))
      (let ((original #'disassembly-startup-probe))
        (and (search "Packed Lisp code:" (with-output-to-string (*standard-output*)
                       (disassemble 'disassembly-startup-probe)))
             (eq original #'disassembly-startup-probe)
             (not (compiled-function-p original)))))`).output, 'T\n');
    assert.equal(vm.evaluate('miricl-compiler::*compiler-state*').output, precompiled ? ':READY\n' : ':COLD\n',
      'Disassembly must not bootstrap the optimizing compiler.');
    assert.equal(vm.evaluate(`(funcall (miricl-compiler:compile-native '(lambda () 42)))`).output, '42\n');
    assert.equal(vm.evaluate('miricl-compiler::*compiler-origin*').output, precompiled ? ':IMAGE\n' : ':FIRST-USE\n');
    if (!precompiled) assert.equal(vm.evaluate(`(every (lambda (name)
      (and (compiled-function-p (symbol-function name))
           (null (function-lambda-expression (symbol-function name)))))
      miricl-compiler::*native-compiler-seeds*)`).output, 'T\n', 'First compilation must self-compile the compiler.');
    assert.equal(vm.evaluate(`(let ((miricl-compiler::*wasm-bytes*
        (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (original (symbol-function 'miricl-compiler::wasm-byte)))
      (unwind-protect
        (progn
          (setf (symbol-function 'miricl-compiler::wasm-byte)
            (lambda (&rest arguments) (declare (ignore arguments))
              (error "Compiler block called a redefined global helper.")))
          (miricl-compiler::wasm-u32 300)
          (equalp miricl-compiler::*wasm-bytes* #(172 2)))
        (setf (symbol-function 'miricl-compiler::wasm-byte) original)))`).output, 'T\n',
      'Compiler blocks must retain their internal function bindings.');
    vm.evaluate("(defparameter *saved-compiler* #'miricl-compiler:compile-to-hir)");
    for (const [name, source] of compilerSmokeCases)
      assert.equal(vm.evaluate(source, {eof: true}).output, 'T\n', `${mode}: ${name}`);
    assert.equal(vm.evaluate("(eq *saved-compiler* #'miricl-compiler:compile-to-hir)").output, 'T\n');
    // ASDF remains an explicitly loaded library.
    assert.equal(vm.evaluate(`(notany #'find-package '("ASDF" "UIOP"))`).output, 'T\n');
  } finally {vm.close();}
}
const concurrent = await Miricl.instantiate(bytes);
try {
  assert.equal((await concurrent.evaluateAsync(`(let
      ((first (miricl-threads:make-thread (lambda ()
         (funcall (miricl-compiler:compile-native '(lambda () 42))))))
       (second (miricl-threads:make-thread (lambda ()
         (funcall (miricl-compiler:compile-native '(lambda () 43)))))))
    (and (= (miricl-threads:join-thread first) 42)
         (= (miricl-threads:join-thread second) 43)
         (eq miricl-compiler::*compiler-state* :ready)))`)).output, 'T\n');
} finally {concurrent.close();}
console.log(precompiled
  ? 'Precompiled compiler: ready at startup, native kernels, no bootstrap, sync/async adapters, concurrent compilation and native test bodies pass.'
  : 'Built-in compiler: cold startup, first-use self-compilation, native reuse and no ASDF dependency pass.');
