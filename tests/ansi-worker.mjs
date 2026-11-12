// Disposable WASM process for the full-suite runner. The parent enforces
// deadlines even when guest Lisp or its failure printer loops forever.
import fs from 'node:fs';
import {parentPort, workerData} from 'node:worker_threads';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';
import {installCompiler, compiledTest, compilerCounters} from './ansi-compiler.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(workerData.binary));
// The harness compiles test bodies explicitly, independently of image policy.
vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
if (workerData.seed) vm.vm['seed-random'](...workerData.seed);
const evaluate = source => {
  const result = vm.evaluate(source, {eof: true});
  if (result.quit) throw new Error('Guest requested exit.');
  return result.output;
};
for (const [file, source] of workerData.sources) vm.mountFile(file, source);
evaluate('(load "rt-package.lsp") (load "rt.lsp")');
const compilerBootstrap = workerData.execution === 'compiler'
  ? installCompiler(vm) : undefined;
evaluate(`
  (defun compile-and-load (file &key force)
    (declare (ignore force)) (load file))
  (defun compile-and-load* (file &key force)
    (declare (ignore force))
    (load (concatenate 'string "auxiliary/" file)))`);
parentPort.postMessage({ready: true, compilerBootstrap});
const context = createContext({evaluate, source: ''});
const script = new Script('evaluate(source)');
parentPort.on('message', ({source, timeout = 2000, test = false}) => {
  const compiler = test && workerData.execution === 'compiler';
  context.source = compiler ? compiledTest(source) : source;
  let result;
  try {
    result = {output: script.runInContext(context, {timeout})};
  } catch (error) {
    if (error.code === 'ERR_SCRIPT_EXECUTION_TIMEOUT') {
      vm.vm.unwind();
      result = {error: `Exceeded ${timeout} ms.`, interrupted: true};
    } else result = {error: error.message, code: error.code, output: error.output ?? ''};
  }
  if (compiler) result.compilation = compilerCounters(vm);
  parentPort.postMessage(result);
});
