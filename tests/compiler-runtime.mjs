import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {Miricl} from '../scripts/miricl.mjs';
import {compilerCases} from './compiler-cases.mjs';

const bytes = await fs.readFile(process.argv[2] ?? 'miricl.wasm');
const module = await WebAssembly.compile(bytes);
assert.deepEqual(WebAssembly.Module.imports(module), []);
// Both adapter entry points provide the compiler immediately, without any
// filesystem mount, compiler option, companion artifact or asynchronous fetch.
for (const mode of ['async', 'sync']) {
  const vm = mode === 'async' ? await Miricl.instantiate(bytes)
    : new Miricl(new WebAssembly.Instance(module));
  try {
    assert.equal(vm.evaluate("(and (fboundp 'miricl-compiler:compile-native) (fboundp 'miricl-compiler:optimize-system))").output, 'T\n');
    assert.equal(vm.evaluate('miricl-compiler::*compiler-state*').output, ':COLD\n');
    assert.equal(vm.evaluate('(+ 20 22)').output, '42\n');
    assert.equal(vm.evaluate('miricl-compiler::*compiler-state*').output, ':COLD\n');
    assert.equal(vm.evaluate(`(funcall (miricl-compiler:compile-native '(lambda () 42)))`).output, '42\n');
    assert.equal(vm.evaluate('miricl-compiler::*compiler-origin*').output, ':FIRST-USE\n');
    assert.equal(vm.evaluate(`(every (lambda (name)
      (and (compiled-function-p (symbol-function name))
           (null (function-lambda-expression (symbol-function name)))))
      miricl-compiler::*native-compiler-seeds*)`).output, 'T\n', 'First compilation must self-compile the compiler.');
    vm.evaluate("(defparameter *saved-compiler* #'miricl-compiler:compile-to-hir)");
    for (const [name, source] of compilerCases)
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
console.log('Built-in compiler: cold startup, first-use self-compilation, native reuse and no ASDF dependency pass.');
