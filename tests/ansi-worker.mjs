// Disposable WASM process for the full-suite runner. The parent enforces
// deadlines even when guest Lisp or its failure printer loops forever.
import fs from 'node:fs';
import {parentPort, workerData} from 'node:worker_threads';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(workerData.binary));
if (workerData.seed) vm.vm['seed-random'](...workerData.seed);
const evaluate = source => {
  const result = vm.evaluate(source, {eof: true});
  if (result.quit) throw new Error('Guest requested exit.');
  return result.output;
};
for (const [file, source] of workerData.sources) vm.mountFile(file, source);
evaluate('(load "rt-package.lsp") (load "rt.lsp")');
evaluate(`
  (defun compile-and-load (file &key force)
    (declare (ignore force)) (load file))
  (defun compile-and-load* (file &key force)
    (declare (ignore force))
    (load (concatenate 'string "auxiliary/" file)))`);
parentPort.postMessage({ready: true});
const context = createContext({evaluate, source: ''});
const script = new Script('evaluate(source)');
parentPort.on('message', ({source, timeout = 2000}) => {
  context.source = source;
  try {
    parentPort.postMessage({output: script.runInContext(context, {timeout})});
  } catch (error) {
    if (error.code === 'ERR_SCRIPT_EXECUTION_TIMEOUT') {
      vm.vm.unwind();
      parentPort.postMessage({error: `Exceeded ${timeout} ms.`, interrupted: true});
      return;
    }
    parentPort.postMessage({error: error.message, code: error.code, output: error.output ?? ''});
  }
});
