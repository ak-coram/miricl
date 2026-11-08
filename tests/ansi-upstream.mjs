import {Worker} from 'node:worker_threads';
import {measureWasmImages} from '../scripts/wasm-size.mjs';
import {validateExecution, summarizeCompilation} from './ansi-compiler.mjs';

export async function runUpstream({binary, sources, timeout = 10000, loadTimeout = 120000,
  heapLimitMb = 512, expectedTests = null,
  execution = 'interpreter',
  seed = Array.from(crypto.getRandomValues(new Uint32Array(4))), onProgress = () => {}}) {
  for (const [name, value] of Object.entries({timeout, loadTimeout}))
    if (!Number.isSafeInteger(value) || value <= 0) throw new Error(`${name} must be a positive integer.`);
  validateExecution(execution);
  if (!Number.isSafeInteger(heapLimitMb) || heapLimitMb < 16) throw new Error('Heap limit must be an integer of at least 16 MiB.');
  if (!Array.isArray(seed) || seed.length !== 4 || seed.some(word => !Number.isInteger(word) || word < 0 || word > 0xffffffff))
    throw new Error('Seed must contain four unsigned 32-bit integers.');
  const imageSizes = measureWasmImages(binary);
  const startedAt = Date.now();
  const report = {binary, profile: 'full', runner: 'upstream', execution, timeout, loadTimeout, heapLimitMb,
    seed: [...seed], performanceBudgetMs: 2000, imageSizes,
    inventory: 'Unmodified upstream init.lsp and guest RT registration (no host Lisp)',
    startedAt: new Date(startedAt).toISOString(), tests: [], loadErrors: [], recoveries: 0, error: null};
  let plan;
  const counts = {passed: 0, failed: 0, timedOut: 0};
  const progress = changes => {
    report.progress = {...report.progress, ...changes, startedAt,
      elapsedMs: Date.now() - startedAt, testsProcessed: report.tests.length,
      ...counts,
      blocked: 0, loadErrors: report.loadErrors.length};
    onProgress(report, report.progress.phase, report.progress.file);
  };
  progress({phase: 'initialize', totalTests: expectedTests ?? 0});
  while (!report.error && (!plan || report.tests.length < plan.length)) {
    const resumeAt = report.tests.length;
    let worker;
    await new Promise(resolve => {
      let timer, settled = false, ready = false, testStartedAt;
      const finish = async () => {
        if (settled) return;
        settled = true;
        clearTimeout(timer);
        await worker?.terminate();
        resolve();
      };
      const failed = (error, timedOut = false) => {
        if (settled) return;
        if (!ready) {
          report.error = `Upstream initialization failed: ${error}`;
          report.loadErrors.push({file: 'init.lsp', error: String(error)});
        } else {
          const record = plan[report.tests.length];
          report.tests.push({...record, status: timedOut ? 'timeout' : 'fail', passed: false,
            elapsedMs: Date.now() - testStartedAt,
            error: String(error), ...(timedOut ? {timedOut: true} : {crashed: true})});
          counts[timedOut ? 'timedOut' : 'failed']++;
        }
        finish();
      };
      const arm = limit => {
        clearTimeout(timer);
        timer = setTimeout(() => failed(`Worker exceeded ${limit} ms.`, true), limit);
      };
      worker = new Worker(new URL('./ansi-upstream-worker.mjs', import.meta.url), {
        workerData: {binary, sources, seed: report.seed, timeout, loadTimeout, execution},
        resourceLimits: {maxOldGenerationSizeMb: heapLimitMb},
      });
      worker.on('error', error => failed(error.message));
      worker.on('exit', code => { if (!settled) failed(`Worker exited (${code}).`); });
      worker.on('message', message => {
        if (settled) return;
        if (message.type === 'load-error') {
          report.loadErrors.push({file: 'init.lsp', ...message});
          report.error = `Upstream initialization failed: ${message.error}`;
          finish();
        } else if (message.type === 'ready') {
          const loaded = message.plan.map(record => ({...record, file: record.file.replace(/^\/ansi\//, '')}));
          if (!loaded.length || (expectedTests !== null && loaded.length !== expectedTests)) {
            report.error = `Upstream registered ${loaded.length} tests; expected ${expectedTests ?? 'at least one'}.`;
            finish();
            return;
          }
          if (plan && JSON.stringify(plan) !== JSON.stringify(loaded)) {
            report.error = 'Upstream test inventory changed during recovery.';
            finish();
            return;
          }
          plan = loaded;
          ready = true;
          report.files = [...new Set(plan.map(record => record.file))];
          report.loadElapsedMs ??= message.loadElapsedMs;
          if (message.compilerBootstrap) report.compilerBootstrap = message.compilerBootstrap;
          if (message.output) report.initializationOutput = message.output.slice(0, 4000);
          progress({phase: 'test', totalTests: plan.length, file: plan[resumeAt].file, test: plan[resumeAt].name});
          worker.postMessage({start: resumeAt});
          testStartedAt = Date.now();
          arm(Math.max(timeout, plan[resumeAt].minimumTimeout ?? 0) + 1000);
        } else if (message.type === 'test') {
          if (message.index !== report.tests.length) {
            report.error = `Unexpected test index ${message.index}.`;
            finish();
            return;
          }
          const {type, index, timeout: testTimeout, ...result} = message;
          report.tests.push({...plan[index], ...result, passed: result.status === 'pass',
            ...(testTimeout !== timeout ? {timeout: testTimeout} : {}),
            ...(result.elapsedMs > report.performanceBudgetMs ? {slow: true} : {}),
            output: result.output.slice(0, 4000)});
          counts[{pass: 'passed', fail: 'failed', timeout: 'timedOut'}[result.status]]++;
          const next = plan[index + 1];
          testStartedAt = Date.now();
          progress({phase: 'test', file: next?.file ?? plan[index].file, test: next?.name});
          arm(Math.max(timeout, next?.minimumTimeout ?? 0) + 1000);
        } else if (message.type === 'done') finish();
      });
      arm(loadTimeout + 30000); // Includes instantiation and mounting the source tree.
    });
    if (plan && report.tests.length === resumeAt && !report.error) report.error = 'Worker made no progress.';
    if (!report.error && plan && report.tests.length < plan.length) {
      report.recoveries++;
      progress({phase: 'recover', replayedOperations: 0, totalReplayOperations: 1});
    }
  }
  const count = status => report.tests.filter(test => test.status === status).length;
  report.summary = {passed: count('pass'), failed: count('fail'), blocked: 0, timedOut: count('timeout'),
    total: report.tests.length, expected: expectedTests ?? plan?.length ?? 0,
    loadErrors: report.loadErrors.length};
  report.summary.notPassed = report.summary.total - report.summary.passed;
  const slow = report.tests.filter(test => test.slow).length;
  if (slow) report.summary.slow = slow;
  report.summary.complete = !report.error && report.summary.total > 0 && report.summary.total === report.summary.expected;
  progress({phase: report.error ? 'aborted' : 'finished', test: undefined});
  report.elapsedMs = report.progress.elapsedMs;
  summarizeCompilation(report);
  return report;
}
