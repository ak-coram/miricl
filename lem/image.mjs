// Clean checked-source application build: guest ASDF compilation, then a pre-DOM
// checkpoint retaining the ready compiler, ASDF/UIOP and source/resource files.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from '../scripts/miricl.mjs';
import {prepareLemSources, root} from './files.mjs';
import {unicodeData, digest, lispString, atomicWrite} from './unicode.mjs';
import {collectCompilationStatistics, writeCompilationStatistics} from '../scripts/compilation-statistics.mjs';

export async function prepareBuild(runtime, asdf, output, options = {}) {
  // Always authenticate source checkouts, including on final-image cache hits.
  const {files, checkouts} = await prepareLemSources(options);
  const unicode = await unicodeData(path.join(path.dirname(output), 'unicode-data'), options);
  for (const [name, bytes] of unicode.files) files.set(name, bytes);
  const runtimeBytes = await fs.readFile(runtime), asdfBytes = await fs.readFile(asdf);
  const inputs = {runtime: digest(runtimeBytes), asdf: digest(asdfBytes), unicode: unicode.inputKey,
    checkouts, unicodeOutputs: unicode.outputs, hostGenerator: unicode.host, node: process.version};
  const hash = createHash('sha256').update(JSON.stringify(inputs));
  for (const [name, bytes] of [...files].sort(([a], [b]) => a < b ? -1 : a > b ? 1 : 0))
    hash.update(name + '\0' + bytes.length + '\0').update(bytes);
  for (const name of ['lem/image.mjs', 'lem/files.mjs', 'lem/lisp-mode-sources.mjs', 'lem/unicode.mjs',
    'lem/page.mjs', 'lem/app.mjs', 'lem/index.html', 'scripts/miricl.mjs', 'miricl.js',
    'scripts/compilation-statistics.mjs', 'scripts/version-assets.mjs'])
    hash.update(name).update(await fs.readFile(path.join(root, name)));
  const key = hash.digest('hex');
  await atomicWrite(output + '.inputs.json', JSON.stringify({key, inputs, sourceFiles: files.size}, null, 2) + '\n');
  return {key, inputs, files, runtimeBytes, asdfBytes};
}
export const applicationCheck = `(assert (and (find-package "ASDF") (find-package "UIOP")
  (eq miricl-compiler::*compiler-state* :ready)
  (asdf:find-system "lem-browser-lisp-mode")
  (asdf:component-version (asdf:find-system "lem-browser"))
  (fboundp 'lem-browser:start-editor) (fboundp 'lem-lisp-mode/connection:ensure-local-connection)
  (null lem-browser:*browser*) (null lem-lisp-mode/connection::*connection*))) (values)`;
export async function checkImage(bytes, {onOutput = text => process.stdout.write(text)} = {}) {
  const vm = await Miricl.instantiate(bytes, {onOutput});
  try {
    await vm.evaluateAsync(applicationCheck, {eof: true});
    assert.ok(vm.vm['core-checkpoint-safe'](), 'The restored application has no live worker thread.');
  }
  finally {vm.close();}
}
export async function buildImage(runtime, asdf, output, options = {}) {
  const prepared = await prepareBuild(runtime, asdf, output, options);
  const {key, inputs, files, runtimeBytes, asdfBytes} = prepared;
  const onOutput = options.onOutput ?? (text => process.stdout.write(text));
  try {
    const manifest = JSON.parse(await fs.readFile(output + '.json', 'utf8'));
    if (manifest.key === key) {
      const bytes = await fs.readFile(output);
      if (digest(bytes) !== manifest.sha256) throw new Error('LEM image cache digest mismatch.');
      await checkImage(bytes, {onOutput});
      await writeCompilationStatistics({workload: 'lem', phase: 'guest ASDF image cache', status: 'cached',
        counters: [], reason: 'Guest compilation reused; no current-run compiler counters.'});
      return {...manifest, cached: true, output};
    }
  } catch (error) {if (error.code !== 'ENOENT') throw error;}
  const started = performance.now();
  const stage = name => onOutput(`[LEM ${name}] ${JSON.stringify(process.memoryUsage())}\n`);
  const marker = await writeCompilationStatistics({workload: 'lem', phase: 'complete guest image',
    recordType: 'measurement', status: 'partial', counters: [], failure: 'Image build and fresh restore have not completed.'});
  let vm, bytes, manifest;
  try {
    stage('instantiate');
    vm = await Miricl.instantiate(runtimeBytes, {onOutput});
    try {
      if (!vm.threads) throw new Error('LEM requires the asynchronous Wasm thread scheduler.');
      assert.equal((await vm.evaluateAsync('(eq miricl-compiler::*compiler-state* :ready)')).output, 'T\n');
      for (const [name, bytes] of files) vm.mountFile(name, bytes);
      vm.mountFile('/miricl-asdf.wasm', asdfBytes);
      const evaluate = async source => {const result = await vm.evaluateAsync(source, {eof: true}); if (result.output) onOutput(result.output);};
      await evaluate('(load "/miricl-asdf.wasm") (setq miricl-compiler:*compile-evaluations* nil) (values)');
      await evaluate(`(asdf:initialize-source-registry '(:source-registry (:tree "/deps/") (:tree "/lem/")
        (:tree "/lem-browser-frontend/") (:tree "/lem-browser-lisp-mode/") :ignore-inherited-configuration))
        (asdf:initialize-output-translations '(:output-translations (t "/cache/") :ignore-inherited-configuration))
        (values)`);
      const definitions = ['anaphora/anaphora', 'bt-semaphore/bt-semaphore', 'inquisitor/inquisitor',
        'micros/micros', 'queues/queues', 'queues/queues.simple-queue', 'queues/queues.simple-cqueue',
        'split-sequence/split-sequence', 'trivial-types/trivial-types', 'yason/yason',
        'cl-ppcre/cl-ppcre-unicode', 'closer-mop/closer-mop', 'miricl-environment/miricl-environment',
        'trivial-cltl2/trivial-cltl2', 'miricl-gray/miricl-gray', 'miricl-ide-mop/miricl-ide-mop',
        'miricl-ide-services/miricl-ide-services'].map(name => `/deps/${name}.asd`);
      definitions.push('/lem/lem-browser.asd', '/lem-browser-frontend/lem-browser-frontend.asd',
        '/lem-browser-lisp-mode/lem-browser-lisp-mode.asd');
      await evaluate(`(dolist (file '(${definitions.map(lispString).join(' ')})) (asdf:load-asd file)) (values)`);
      stage('guest-ASDF-start');
      await collectCompilationStatistics(vm, {workload: 'lem', phase: 'guest ASDF portable compilation',
        recordType: 'measurement', reason: 'Portable COMPILE-FILE and LOAD; this is not a native application bundle.'},
        () => evaluate('(asdf:load-system "lem-browser-lisp-mode") (values)'));
      await writeCompilationStatistics({workload: 'lem', phase: 'native application compilation', status: 'skipped',
        counters: [], reason: 'This build emits portable guest FASLs; no native application bundle stage runs.'});
      stage('application-check');
      await evaluate(applicationCheck);
      stage('save-start');
      // Loading creates definitions only: no editor, connection, DOM or worker.
      bytes = vm.saveImage();
      stage('save-complete');
      manifest = {key, inputs, bytes: bytes.length, sha256: digest(bytes),
        elapsedMs: performance.now() - started, statisticsEnabled: Boolean(process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY),
        mode: 'guest-asdf-portable', sourceFiles: files.size};
    } finally {vm?.close(); vm = null;}
    // Release pending scheduler closures before the independent restore allocates
    // a second graph. CLOSE stops scheduling; it does not release this reference.
    await new Promise(resolve => setImmediate(resolve));
    stage('restore-start');
    await checkImage(bytes, {onOutput});
    stage('restore-complete');
    manifest.elapsedMs = performance.now() - started;
    await atomicWrite(output, bytes);
    await atomicWrite(output + '.json', JSON.stringify(manifest, null, 2) + '\n');
    await writeCompilationStatistics({...marker, workload: 'lem', phase: 'complete guest image', recordType: 'measurement',
      generatedAt: new Date().toISOString(), failure: undefined, status: 'complete', counters: [], elapsedMs: manifest.elapsedMs, imageBytes: bytes.length,
      runtimeBytes: runtimeBytes.length, inputBytes: [...files.values()].reduce((sum, bytes) => sum + bytes.length, 0),
      outputBytes: bytes.length, inputs});
    return {...manifest, cached: false, output};
  } catch (error) {
    await writeCompilationStatistics({...marker, workload: 'lem', phase: 'complete guest image', recordType: 'measurement',
      status: 'failed', counters: [], failure: error.message, elapsedMs: performance.now() - started});
    throw error;
  }
}
if (process.argv[1] && path.resolve(process.argv[1]) === import.meta.filename) {
  const args = process.argv.slice(2), prepareOnly = args.at(-1) === '--prepare-only';
  if (prepareOnly) args.pop();
  if (args.length > 3 || args.some(arg => arg.startsWith('--'))) throw new Error('Usage: node lem/image.mjs [RUNTIME.wasm] [ASDF.wasm] [OUTPUT.wasm] [--prepare-only]');
  const [runtime = 'miricl.wasm', asdf = 'miricl-asdf.wasm', output = 'build/lem/miricl-lem.wasm'] = args;
  try {
    const result = prepareOnly ? await prepareBuild(runtime, asdf, output) : await buildImage(runtime, asdf, output);
    console.log(`${prepareOnly ? 'Prepared checked LEM inputs' : result.cached ? 'Reused and checked LEM image' : 'Built LEM image'}: ${result.key}`);
    if (process.env.GITHUB_OUTPUT) await fs.appendFile(process.env.GITHUB_OUTPUT, `key=${result.key}\n`);
  } catch (error) {console.error(error.message, error.output ?? '', error.backtrace ?? ''); process.exitCode = 1;}
}
