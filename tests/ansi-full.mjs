#!/usr/bin/env node
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
import {Worker} from 'node:worker_threads';
import {progressReporter} from './ansi-progress.mjs';
import {measureWasmImages} from '../scripts/wasm-size.mjs';
import {runUpstream} from './ansi-upstream.mjs';
import {validateExecution, compilerPhase, summarizeCompilation} from './ansi-compiler.mjs';

const string = value => `"${value.replace(/["\\]/g, '\\$&')}"`;
const directory = fileURLToPath(new URL('./ansi-test/', import.meta.url));
// This upstream test explicitly waits for five seconds to elapse. Its minimum
// budget covers that required wall time; stress tests retain the normal limit.
const requiredTimeouts = new Map([['CL-TEST::GET-UNIVERSAL-TIME.3', 7000]]);

export function inventory(root = directory) {
  const temporary = fs.mkdtempSync(path.join(os.tmpdir(), 'miricl-ansi-'));
  const output = path.join(temporary, 'inventory.json');
  try {
    const result = spawnSync(process.env.SBCL ?? 'sbcl', [
      '--noinform', '--script', fileURLToPath(new URL('./ansi-inventory.lisp', import.meta.url)),
      root, output,
    ], {encoding: 'utf8', maxBuffer: 16 * 1024 * 1024, timeout: 120_000});
    if (result.error || result.status !== 0) {
      throw new Error(`ANSI inventory failed: ${result.error?.message ?? result.stderr}`);
    }
    return JSON.parse(fs.readFileSync(output, 'utf8'));
  } finally {
    fs.rmSync(temporary, {recursive: true, force: true});
  }
}

export async function runFull({binary, records, sources, timeout = 10000, heapLimitMb = 512, testDirectory = null,
  execution = 'interpreter',
  seed = Array.from(crypto.getRandomValues(new Uint32Array(4))),
  onFile = () => {}, onProgress = () => {}}) {
  if (!Number.isFinite(timeout) || timeout <= 0) throw new Error('Timeout must be positive.');
  validateExecution(execution);
  if (!Number.isSafeInteger(heapLimitMb) || heapLimitMb < 16) throw new Error('Heap limit must be an integer of at least 16 MiB.');
  if (!Array.isArray(seed) || seed.length !== 4 || seed.some(word => !Number.isInteger(word) || word < 0 || word > 0xffffffff))
    throw new Error('Seed must contain four unsigned 32-bit integers.');
  seed = [...seed];
  const report = {binary, profile: 'full', runner: 'diagnostic', execution, timeout, performanceBudgetMs: 2000, heapLimitMb, seed,
    imageSizes: measureWasmImages(binary),
    files: [...new Set(records.map(r => r.file))],
    inventory: 'SBCL with Miricl features; test bodies run in WASM',
    tests: [], loadErrors: [], inventoryMismatches: [], inventoryAliases: [], recoveries: 0, error: null};
  const startedAt = Date.now();
  report.startedAt = new Date(startedAt).toISOString();
  report.progress = {phase: 'initialize', startedAt, elapsedMs: 0,
    loadedForms: 0, totalForms: records.length, testsProcessed: 0,
    totalTests: records.reduce((n, record) => n + record.tests.length, 0),
    passed: 0, failed: 0, blocked: 0, timedOut: 0, loadErrors: 0};
  const progress = changes => {
    report.progress = {...report.progress, ...changes,
      elapsedMs: Date.now() - startedAt, loadErrors: report.loadErrors.length};
    onProgress(report, report.progress.phase, report.progress.file);
  };
  let worker;
  const setup = [];
  const request = (source, limit = timeout, test = false) => new Promise(resolve => {
    const finish = result => {
      clearTimeout(timer);
      worker.off('message', message);
      worker.off('error', error);
      worker.off('exit', exited);
      resolve(result);
    };
    const message = result => finish(result);
    const error = cause => finish({error: cause.message, crashed: true});
    const exited = code => finish({error: `Worker exited (${code}).`, crashed: true});
    // Give the worker's VM deadline time to report an interruption without
    // destroying its loaded state. The outer deadline remains a crash guard.
    const timer = setTimeout(() => finish({error: `Exceeded ${limit} ms.`, timedOut: true}),
      source === null ? limit : limit + 1000);
    worker.once('message', message);
    worker.once('error', error);
    worker.once('exit', exited);
    if (source !== null) worker.postMessage({source, timeout: limit, test});
  });
  const start = async () => {
    worker = new Worker(new URL('./ansi-worker.mjs', import.meta.url), {
      workerData: {binary, sources, seed, execution}, resourceLimits: {maxOldGenerationSizeMb: heapLimitMb},
    });
    // Compiler mode bootstraps every native kernel before running any test.
    // Keep that budget separate from individual test deadlines.
    const ready = await request(null, execution === 'compiler' ? 180_000 : 30_000);
    if (!ready.ready) throw new Error(`ANSI worker initialization failed: ${ready.error}`);
    if (ready.compilerBootstrap) report.compilerBootstrap = ready.compilerBootstrap;
    let replayedOperations = 0;
    for (const source of setup) {
      const result = await request(source);
      if (result.timedOut || result.crashed || result.interrupted) throw new Error(`ANSI setup replay failed: ${result.error}`);
      progress({replayedOperations: ++replayedOperations});
    }
  };
  const recover = async result => {
    if (!result.timedOut && !result.crashed) return;
    report.recoveries++;
    const resume = {...report.progress};
    progress({phase: 'recover', replayedOperations: 0, totalReplayOperations: setup.length});
    await worker.terminate();
    await start();
    progress(resume);
  };
  const context = record => `(cl:in-package ${string(record.package)})`;
  const load = record => `${context(record)}
    (cl:let ((cl:*load-pathname* (cl:pathname ${string(record.file)}))
             (cl:*load-truename* (cl:pathname ${string(record.file)})))
      ${record.source}
      (cl:values))`;
  const trim = result => ({...result, ...(result.output ? {output: result.output.slice(0, 4000)} : {})});
  try {
    progress({phase: 'initialize'});
    await start();
    // Keep RT's lookup table small while retaining the actual guest entry
    // objects. All source definitions load before any test body is run.
    const plan = [];
    const archive = record => `(cl:let
        ((cl:*package* (cl:or (cl:find-package ${string(record.package)}) (cl:find-package "CL-USER")))
         (known nil) (aliases nil) (key-index nil)
         (key-pairs '(${Object.entries(record.testKeys ?? {}).map(([name, key]) => `(${string(name)} . ${string(key)})`).join(' ')})))
      (cl:dolist (name-string '(${(record.definitions ?? record.tests).map(string).join(' ')}))
        (cl:let ((name (cl:ignore-errors (cl:read-from-string name-string))))
          (cl:when (cl:and name (cl:gethash name rt::*entries-table*)) (cl:push name known))))
      (cl:dolist (pair key-pairs)
        (cl:let ((entry (cl-user::%ansi-entry-for (cl:car pair) nil)))
          (cl:unless entry
            (cl:unless key-index (cl:setq key-index (cl-user::%ansi-entry-index known)))
            (cl:setq entry (cl:gethash (cl:cdr pair) key-index))
            (cl:when (cl:and entry (cl:not (cl:eq entry t))
                       (cl:= 1 (cl:count (cl:cdr pair) key-pairs :key (cl:function cl:cdr) :test (cl:function cl:equal))))
              (cl:push (rt::name entry) known)
              (cl:push (cl:cons (cl:car pair) entry) aliases)))))
      (cl:dolist (name-string '(${record.tests.map(string).join(' ')}))
        (cl:let* ((alias (cl:assoc name-string aliases :test (cl:function cl:equal)))
                  (entry (cl:or (cl:cdr alias) (cl-user::%ansi-entry-for name-string nil))))
          (cl:when alias (cl:format t "ANSI-ALIAS ~D ~S~%" (cl:length cl-user::*ansi-entries*) (rt::name entry)))
          (cl:push entry cl-user::*ansi-entries*)))
      (cl:dolist (entry (cl:cdr rt::*entries*))
        (cl:unless (cl:member (rt::name entry) known :test (cl:function cl:equal))
          (cl:push entry cl-user::*ansi-entries*)
          (cl:format t "ANSI-EXTRA ~S~%" (rt::name entry))))
      (rt:rem-all-tests))`;
    const prepare = `${fs.readFileSync(new URL('./ansi-test-key.lisp', import.meta.url), 'utf8')}
      (cl:defvar cl-user::*ansi-entries* nil)`;
    const prepared = await request(prepare);
    if (prepared.error) throw new Error(prepared.error);
    setup.push(prepare);
    let currentFile;
    for (const record of records) {
      if (record.file !== currentFile) {
        currentFile = record.file;
        onFile(currentFile, report, 'load');
      }
      progress({phase: 'load', file: record.file});
      const source = load(record);
      const loaded = await request(source);
      if (loaded.error) report.loadErrors.push({file: record.file,
        source: record.source.slice(0, 500), ...trim(loaded)});
      await recover(loaded);
      // Reproduce partial registrations and setup side effects as well as
      // successful forms. Exclude forms known to hang or crash the worker.
      if (!loaded.timedOut && !loaded.crashed && !loaded.interrupted) setup.push(source);
      const save = `(cl:in-package "CL-USER") ${archive(record)}`;
      const saved = await request(save);
      if (saved.error) throw new Error(`Cannot archive RT entries: ${saved.error}`);
      const extras = saved.output.split('\n').filter(line => line.startsWith('ANSI-EXTRA '))
        .map(line => line.slice('ANSI-EXTRA '.length));
      const aliases = new Map(saved.output.split('\n').filter(line => line.startsWith('ANSI-ALIAS '))
        .map(line => { const match = /^ANSI-ALIAS (\d+) (.*)$/.exec(line); return [Number(match[1]), match[2]]; }));
      if (extras.length) report.inventoryMismatches.push({file: record.file,
        extra: extras.length, tests: extras, source: record.source.slice(0, 500)});
      for (const name of [...record.tests, ...extras]) {
        const guestName = aliases.get(plan.length);
        if (guestName) report.inventoryAliases.push({file: record.file, name, guestName});
        plan.push({name, file: record.file, package: record.package,
          extra: extras.includes(name), ...(guestName ? {guestName} : {})});
      }
      if (extras.length) progress({totalTests: report.progress.totalTests + extras.length});
      setup.push(save);
      progress({loadedForms: report.progress.loadedForms + 1});
    }
    const reverse = `(cl:progn
      (cl:setq cl-user::*ansi-entries* (cl:nreverse cl-user::*ansi-entries*)) (cl:values))`;
    const reversed = await request(reverse);
    if (reversed.error) throw new Error(reversed.error);
    setup.push(reverse);
    currentFile = undefined;
    for (const record of plan) {
      if (record.file !== currentFile) {
        currentFile = record.file;
        onFile(currentFile, report, 'test');
      }
      {
        const {name} = record;
        const testTimeout = Math.max(timeout, requiredTimeouts.get(name) ?? 0);
        progress({phase: 'test', file: record.file, test: name});
        const testStartedAt = performance.now();
        const executed = await request(`
          (rt:rem-all-tests)
          (cl:let ((entry (cl:pop cl-user::*ansi-entries*))
                    ${testDirectory ? `(cl:*default-pathname-defaults* (cl:pathname ${string(testDirectory)}))` : ''})
            (cl:if entry
              (cl:progn ${context(record)} (rt::add-entry entry)
                (cl:if (rt:do-test (rt::name entry)) :pass :fail))
              :blocked))`, testTimeout, true);
        if (execution === 'compiler' && executed.output?.endsWith(':PASS\n') && !executed.compilation?.compiled)
          executed.error = 'Test passed without native compilation.';
        const status = executed.timedOut || executed.interrupted ? 'timeout' : executed.error ? 'fail'
          : executed.output.endsWith(':PASS\n') ? 'pass'
          : executed.output.endsWith(':BLOCKED\n') ? 'blocked' : 'fail';
        const elapsedMs = Math.round((performance.now() - testStartedAt) * 1000) / 1000;
        report.tests.push({name, file: record.file, ...(record.extra ? {extra: true} : {}),
          ...(testTimeout !== timeout ? {timeout: testTimeout} : {}),
          ...(record.guestName ? {guestName: record.guestName} : {}),
          passed: status === 'pass', status,
          ...(executed.compilation ? {phase: compilerPhase(executed.compilation)} : {}),
          elapsedMs, ...(elapsedMs > report.performanceBudgetMs ? {slow: true} : {}), ...trim(executed)});
        const counter = {pass: 'passed', fail: 'failed', blocked: 'blocked', timeout: 'timedOut'}[status];
        progress({testsProcessed: report.tests.length, [counter]: report.progress[counter] + 1});
        if (executed.timedOut || executed.crashed) {
          await recover(executed);
          const skipped = await request(`(cl:dotimes (i ${report.tests.length})
            (cl:pop cl-user::*ansi-entries*))`);
          if (skipped.error) throw new Error(`Cannot resume RT entries: ${skipped.error}`);
        }
      }
    }
  } catch (error) {
    report.error = error.stack ?? String(error);
  } finally {
    if (worker) await worker.terminate();
  }
  const count = status => report.tests.filter(test => test.status === status).length;
  report.summary = {passed: count('pass'), failed: count('fail'), blocked: count('blocked'),
    timedOut: count('timeout'), total: report.tests.length,
    expected: report.progress.totalTests,
    loadErrors: report.loadErrors.length};
  const extraTests = report.inventoryMismatches.reduce((n, mismatch) => n + mismatch.extra, 0);
  if (extraTests) Object.assign(report.summary, {extraTests,
    referenceTests: records.reduce((n, record) => n + record.tests.length, 0)});
  report.summary.notPassed = report.summary.total - report.summary.passed;
  const slow = report.tests.filter(test => test.slow).length;
  if (slow) report.summary.slow = slow;
  report.summary.complete = !report.error
    && report.summary.total === report.summary.expected && report.summary.total > 0;
  progress({phase: report.error ? 'aborted' : 'finished', test: undefined});
  report.elapsedMs = report.progress.elapsedMs;
  summarizeCompilation(report);
  return report;
}

export async function main(binary, args = []) {
  const wholeStartedAt = Date.now();
  const reportPath = process.env.MIRICL_ANSI_REPORT ?? 'ansi-results.json';
  fs.mkdirSync(path.dirname(reportPath), {recursive: true});
  const testInputCorrections = [];
  let report;
  try {
    const diagnostic = args.includes('--diagnostic');
    const compiler = args.includes('--compiler');
    const options = args.filter(arg => arg.startsWith('--timeout=') || arg.startsWith('--heap=') || arg.startsWith('--seed=') || arg.startsWith('--load-timeout='));
    if (args.length !== options.length + Number(diagnostic) + Number(compiler)) throw new Error('Full suite accepts --timeout=<milliseconds>, --load-timeout=<milliseconds>, --heap=<MiB>, --seed=<a,b,c,d>, --compiler, and --diagnostic.');
    const option = (name, fallback) => {
      const value = options.findLast(arg => arg.startsWith(`--${name}=`));
      return value ? Number(value.slice(value.indexOf('=') + 1)) : fallback;
    };
    let records, files;
    if (diagnostic) {
      console.log('Inventorying upstream ANSI sources for diagnostic form-by-form loading (SBCL)...');
      records = inventory();
      const fixtures = JSON.parse(fs.readFileSync(new URL('./ansi-fixtures.json', import.meta.url), 'utf8'));
      files = [...new Set(['rt-package.lsp', 'rt.lsp', ...records.map(r => r.file), ...fixtures])];
    } else {
      console.log('Loading the upstream ANSI driver in one persistent interpreter...');
      files = [];
      const collect = relative => {
        for (const entry of fs.readdirSync(path.join(directory, relative), {withFileTypes: true})) {
          if (entry.name.startsWith('.')) continue;
          const file = path.posix.join(relative, entry.name);
          if (entry.isDirectory()) collect(file);
          else if (/\.(lsp|txt|cl|sexp)$/.test(entry.name)) files.push(file);
        }
      };
      collect('');
    }
    const sources = files.map(file => {
      const source = fs.readFileSync(path.join(directory, file), 'utf8');
      if (diagnostic || file !== 'environment/encode-universal-time.lsp') return [file, source];
      // ENCODE-UNIVERSAL-TIME.1 generates one-based dates and months.
      // Its epoch-day guard must exclude zones that can yield a negative stamp.
      const corrected = source.replace('(and (= year 1900) (= date 0) (= month 0))',
        '(and (= year 1900) (= date 1) (= month 1))');
      if (corrected !== source) {
        const correction = 'ENCODE-UNIVERSAL-TIME.1 uses the January 1, 1900 guard for generated time zones.';
        testInputCorrections.push(correction);
        console.log(`ANSI test input correction: ${correction}`);
      }
      return [file, corrected];
    });
    let lastCheckpoint = 0;
    const display = progressReporter();
    try {
      report = await (diagnostic ? runFull : runUpstream)({binary, records, sources, testDirectory: '/sandbox/',
      execution: compiler ? 'compiler' : 'interpreter',
      // Checked against the pinned ebf9c806 upstream revision and the independent
      // diagnostic inventory. A partial load must never become a green report.
      ...(!diagnostic ? {expectedTests: 21945, loadTimeout: option('load-timeout', 120000)} : {}),
      ...(options.some(arg => arg.startsWith('--seed='))
        ? {seed: options.findLast(arg => arg.startsWith('--seed=')).slice(7).split(',').map(Number)} : {}),
      timeout: option('timeout', 10000), heapLimitMb: option('heap', 512),
      onProgress: (current, phase, file) => {
        display.update(current.progress);
        if (Date.now() - lastCheckpoint < 30_000) return;
        lastCheckpoint = Date.now();
        fs.writeFileSync(reportPath, `${JSON.stringify({...current,
          ...(testInputCorrections.length ? {testInputCorrections} : {}),
          summary: {complete: false, total: current.tests.length},
        }, null, 2)}\n`);
      }});
    } finally {
      display.stop();
    }
  } catch (error) {
    report = {binary, profile: 'full', error: error.stack ?? String(error),
      summary: {passed: 0, failed: 0, blocked: 0, timedOut: 0, total: 0, complete: false}};
  }
  if (testInputCorrections.length) report.testInputCorrections = testInputCorrections;
  report.totalElapsedMs = Date.now() - wholeStartedAt;
  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`);
  const s = report.summary;
  console.log(`ANSI full (${report.execution ?? 'interpreter'}): ${s.passed} passed, ${s.failed} failed, ${s.blocked} blocked, ${s.timedOut} timed out (${s.total}/${s.expected ?? '?'} tests accounted for; ${s.loadErrors ?? 0} load errors).`);
  if (report.error) console.error(report.error);
  process.exitCode = !s.complete || s.failed || s.blocked || s.timedOut || s.loadErrors ? 1 : 0;
  return report;
}
