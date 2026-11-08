import fs from 'node:fs';

export function installCompiler(vm) {
  const started = performance.now();
  const output = vm.evaluate(`(cl:progn (miricl-compiler::ensure-compiler)
    (cl:multiple-value-bind (installed unsupported)
      (miricl-compiler::compile-compiler)
    (cl:when unsupported (cl:error "Compiler bootstrap failed: ~S" unsupported))
    (cl:unless (cl:every (cl:lambda (name) (cl:member name installed))
      '(miricl-compiler::hir-form miricl-compiler:compile-to-hir
        miricl-compiler::emit-native-function miricl-compiler::emit-native-module
        miricl-compiler:compile-native miricl-compiler::compile-compiler))
      (cl:error "Compiler bootstrap did not install the frontend and backend."))
    (cl:unless (cl:every (cl:lambda (name)
        (cl:and (cl:compiled-function-p (cl:symbol-function name))
                (cl:null (cl:function-lambda-expression (cl:symbol-function name))))) installed)
      (cl:error "Compiler bootstrap left an interpreted compiler function."))
    (cl:format cl:t "~D~%" (cl:length installed)) (cl:values)))`, {eof: true}).output;
  const match = /^(\d+)\n$/.exec(output);
  if (!match || Number(match[1]) === 0) throw new Error(`Invalid compiler bootstrap result: ${output}`);
  const origin = vm.evaluate('miricl-compiler::*compiler-origin*').output === ':IMAGE\n' ? 'image' : 'first-use';
  vm.mountFile('/ansi-compiler.lisp', fs.readFileSync(new URL('./ansi-compiler.lisp', import.meta.url), 'utf8'));
  vm.evaluate('(load "/ansi-compiler.lisp")', {eof: true});
  return {mode: 'self-compiled', origin, functions: Number(match[1]), elapsedMs: Math.round(performance.now() - started)};
}

export const compiledTest = source => `(cl:progn (miricl-ansi-compiler::reset-counters)
  (cl:let ((rt::*compile-tests* cl:t)) ${source}))`;

export function compilerCounters(vm) {
  const output = vm.evaluate(`(cl:list miricl-ansi-compiler::*attempted*
    miricl-ansi-compiler::*compiled* (cl:if miricl-ansi-compiler::*failed* 1 0))`, {eof: true}).output;
  const match = /^\((\d+) (\d+) ([01])\)\n$/.exec(output);
  if (!match) throw new Error(`Invalid compiler accounting: ${output}`);
  return {attempted: Number(match[1]), compiled: Number(match[2]), failed: Number(match[3])};
}

export function validateExecution(execution) {
  if (!['interpreter', 'compiler'].includes(execution)) throw new Error('Execution must be interpreter or compiler.');
}

export const compilerPhase = counters => counters.failed || counters.attempted > counters.compiled ? 'compile' : 'execute';

export function summarizeCompilation(report) {
  if (report.execution !== 'compiler') return;
  report.compilation = report.tests.reduce((total, test) => {
    for (const key of ['attempted', 'compiled', 'failed']) total[key] += test.compilation?.[key] ?? 0;
    return total;
  }, {attempted: 0, compiled: 0, failed: 0});
}
