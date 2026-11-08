// Load the normal upstream driver once; keep its RT table and interpreter alive.
import fs from 'node:fs';
import {parentPort, workerData} from 'node:worker_threads';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';
import {installCompiler, compiledTest, compilerCounters, compilerPhase} from './ansi-compiler.mjs';

const vm = await Miricl.instantiate(fs.readFileSync(workerData.binary));
vm.vm['seed-random'](...workerData.seed);
const context = createContext({source: '', evaluate: source => {
  const result = vm.evaluate(source, {eof: true});
  if (result.quit) throw new Error('Guest requested exit.');
  return result.output;
}});
const script = new Script('evaluate(source)');
const evaluate = (source, timeout) => {
  context.source = source;
  return script.runInContext(context, {timeout});
};
try {
  for (const [file, source] of workerData.sources) vm.mountFile(`/ansi/${file}`, source);
  vm.mountFile('/ansi-runner.lisp', fs.readFileSync(new URL('./ansi-upstream.lisp', import.meta.url), 'utf8'));
  const started = performance.now();
  const output = evaluate('(progn (load "/ansi-runner.lisp") (values))', workerData.loadTimeout);
  const compilerBootstrap = workerData.execution === 'compiler'
    ? installCompiler(vm) : undefined;
  const plan = JSON.parse(evaluate('(miricl-ansi-runner::print-plan)', workerData.loadTimeout));
  parentPort.postMessage({type: 'ready', plan, loadElapsedMs: performance.now() - started, output, compilerBootstrap});
  parentPort.once('message', ({start}) => {
    evaluate(`(setq miricl-ansi-runner::*cursor* ${start}) (values)`, workerData.timeout);
    for (let index = start; index < plan.length; index++) {
      const timeout = Math.max(workerData.timeout, plan[index].minimumTimeout ?? 0);
      const testStarted = performance.now();
      let result;
      try {
        const source = '(miricl-ansi-runner::run-next-test)';
        const output = evaluate(workerData.execution === 'compiler' ? compiledTest(source) : source, timeout);
        result = {status: output.endsWith(':PASS\n') ? 'pass' : 'fail', output};
      } catch (error) {
        const interrupted = error.code === 'ERR_SCRIPT_EXECUTION_TIMEOUT';
        if (interrupted) vm.vm.unwind();
        result = {status: interrupted ? 'timeout' : 'fail', error: error.message,
          code: error.code, output: error.output ?? '', ...(interrupted ? {interrupted: true} : {})};
      }
      if (workerData.execution === 'compiler') {
        result.compilation = compilerCounters(vm);
        result.phase = compilerPhase(result.compilation);
        if (result.status === 'pass' && !result.compilation.compiled)
          result = {...result, status: 'fail', error: 'Test passed without native compilation.'};
      }
      parentPort.postMessage({type: 'test', index, ...result, timeout,
        elapsedMs: Math.round((performance.now() - testStarted) * 1000) / 1000});
    }
    parentPort.postMessage({type: 'done'});
  });
} catch (error) {
  if (error.code === 'ERR_SCRIPT_EXECUTION_TIMEOUT') vm.vm.unwind();
  let file = 'init.lsp';
  try { file = evaluate(`(progn (format t "~{~A~^ -> ~}" (reverse miricl-ansi-runner::*loading-files*)) (values))`, 1000) || file; }
  catch { /* Initialization can fail before the reporting package exists. */ }
  parentPort.postMessage({type: 'load-error', error: error.message,
    file, code: error.code, output: error.output ?? ''});
}
