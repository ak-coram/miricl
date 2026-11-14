// Compile the canonical FriCAS interpreter, with a bounded worker and fresh load.
import fs from 'node:fs';
import path from 'node:path';
import {Worker, isMainThread, parentPort, workerData} from 'node:worker_threads';
import {Miricl} from './miricl.mjs';
import {fricasFiles, fricasRevision} from './fricas-files.mjs';
import {createFriCAS, lispString} from './fricas.mjs';
if (!isMainThread) {
  let vm;
  const report = {revision: fricasRevision(), stages: [], tests: [], passed: false};
  try {
    const {files, order} = fricasFiles(workerData.native);
    const image = fs.readFileSync(workerData.image);
    const imageModule = await WebAssembly.compile(image);
    const parts = [];
    const fresh = async () => {
      vm?.close(); vm = await Miricl.instantiate(imageModule);
      for (const [file, bytes] of files)
        if (!file.startsWith('/fricas/algebra/') || file.endsWith('exposed.lisp')) vm.mountFile(file, bytes);
      let setup = '(load "/fricas/lisp/fricas-package.lisp")\n';
      for (const [previous, bytes] of parts.entries()) {
        vm.mountFile(`/fricas-unit-${previous}.wasm`, bytes);
        setup += `(load "/fricas-unit-${previous}.wasm")\n`;
      }
      await vm.evaluateAsync(setup + '(values)', {eof: true});
    };
    await fresh();
    for (const [index, name] of order.slice(1).entries()) {
      parentPort.postMessage({stage: `compile ${name}`});
      const start = performance.now(), target = `/fricas-unit-${index}.wasm`;
      const result = await vm.evaluateAsync(`(progn
        (compile-file ${lispString(name)} :output-file ${lispString(target)})
        (some (lambda (unit)
          (some (lambda (entry) (eq (second (second entry)) :constant))
            (third (miricl-compiler::ir-unit-nodes unit))))
          (miricl-compiler::read-units ${lispString(target)})))`, {eof: true});
      parts.push(await vm.readFile(target));
      // DEFCONSTANT may evaluate at compilation and load time. Upstream has
      // freshly allocated vector constants, so load such units in a fresh VM
      // instead of redefining them with non-EQL values (CLHS DEFCONSTANT).
      const constants = result.output.trim() === 'T';
      if (constants) await fresh();
      else await vm.evaluateAsync(`(load ${lispString(target)}) (values)`, {eof: true});
      report.stages.push({name, milliseconds: Math.round(performance.now() - start), freshLoad: constants});
    }
    vm.close(); vm = null;
    const bundle = new Uint8Array(8 + parts.reduce((size, bytes) => size + bytes.length - 8, 0));
    bundle.set([0, 97, 115, 109, 1, 0, 0, 0]);
    let position = 8;
    for (const part of parts) {bundle.set(part.subarray(8), position); position += part.length - 8;}
    const module = await WebAssembly.compile(bundle);
    if (WebAssembly.Module.imports(module).length || WebAssembly.Module.customSections(module, 'miricl.fasl').length !== order.length - 1)
      throw new Error('Invalid compiled FriCAS bundle.');
    fs.mkdirSync(path.dirname(workerData.core), {recursive: true}); fs.writeFileSync(workerData.core, bundle);
    if (workerData.buildOnly) {
      report.bundleBytes = bundle.length; report.passed = true; report.validation = 'not-run';
    } else {
      parentPort.postMessage({stage: 'fresh compiled startup'});
      const session = await createFriCAS(image, bundle, files);
      vm = session.vm;
      for (const [source, expected] of [
        ['2+3*4', /\(1\)\s+14\s+Type: PositiveInteger/],
        ['factor(x^4-1)', /Factored/], ['integrate(1\/(x^2+1),x)', /atan/],
        ['D(sin(x),x)', /cos\(x\)/], ['series(exp(x),x=0)', /UnivariatePuiseuxSeries/],
        ['determinant(matrix([[1,2],[3,4]]))', /\)\s+- 2\s+Type: Integer/],
      ]) {
        parentPort.postMessage({stage: source});
        const result = await session.evaluate(source);
        report.tests.push({source, passed: expected.test(result.output), ...result});
        if (!expected.test(result.output)) throw new Error(`${source}: ${result.output}`);
        console.log(result.output);
      }
      report.bundleBytes = bundle.length;
      report.passed = true;
    }
  } catch (error) {report.failure = {message: error.message, output: error.output, backtrace: error.backtrace};}
  finally {vm?.close(); parentPort.postMessage({report});}
} else if (process.argv[1] && path.resolve(process.argv[1]) === import.meta.filename) {
  const [image = 'miricl.wasm', native = 'build/fricas/native', output = 'build/fricas/compile.json'] = process.argv.slice(2);
  const core = path.join(path.dirname(path.resolve(output)), 'fricas.wasm');
  const worker = new Worker(new URL(import.meta.url), {workerData: {image: path.resolve(image), native: path.resolve(native), core, buildOnly: process.argv.includes('--build-only')}});
  const timeout = Number(process.env.MIRICL_FRICAS_TIMEOUT_MS ?? 900000);
  if (!Number.isFinite(timeout) || timeout <= 0) throw new Error('Expected a positive FriCAS timeout.');
  let stage;
  const started = Date.now();
  const heartbeat = setInterval(() => console.log(`FriCAS: still working on ${stage} (${Math.round((Date.now() - started) / 1000)} s elapsed)`), 30000);
  const timer = setTimeout(() => {void worker.terminate();}, timeout);
  const report = await new Promise(resolve => {
    worker.on('message', message => {if (message.report) resolve(message.report); else {stage = message.stage; console.log(`FriCAS: ${stage}`);}});
    worker.on('error', error => resolve({passed: false, failure: {stage, message: error.message}}));
    worker.on('exit', code => resolve({passed: false, failure: {stage, message: `Worker exited (${code}); timeout ${timeout} ms.`}}));
  }).finally(() => {clearTimeout(timer); clearInterval(heartbeat);});
  fs.mkdirSync(path.dirname(output), {recursive: true}); fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
  console.log(`${report.passed ? 'PASS' : 'FAIL'} FriCAS compiled bootstrap. Report: ${output}`);
  if (report.failure) console.error(report.failure);
  process.exitCode = report.passed ? 0 : 1;
}
