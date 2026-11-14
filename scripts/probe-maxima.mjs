// node scripts/probe-maxima.mjs [miricl.wasm] [output.json] [source|upstream|compile|suite|share] [test names...]
// Run in a worker so a stuck Lisp computation can be stopped from the host.
// Suite files use separate workers; MIRICL_MAXIMA_TIMEOUT_MS bounds the run
// and MIRICL_MAXIMA_PROBLEM_TIMEOUT_MS bounds an individual problem.
import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';
import {execFileSync} from 'node:child_process';
import {Worker, isMainThread, workerData, parentPort} from 'node:worker_threads';
import {probeMaxima} from './maxima-probe.mjs';

const root = fileURLToPath(new URL('../', import.meta.url));

export function maximaProbeRevision() {
  return execFileSync('git', ['-C', path.join(root, 'vendor/maxima'), 'rev-parse', 'HEAD'],
    {encoding: 'utf8'}).trim();
}

// Keep configuration and generated files in the guest filesystem. Nothing is
// generated in vendor/maxima. Maxima itself runs entirely inside MIRICL.
export function maximaProbeFiles(libraries = false) {
  const upstream = path.join(root, 'vendor/maxima');
  const files = [];
  for (const dir of ['src', 'lisp-utils', ...(libraries ? ['tests', 'share', 'demo'] : [])]) {
    for (const name of fs.readdirSync(path.join(upstream, dir), {recursive: true})) {
      const file = path.join(upstream, dir, name);
      if (fs.statSync(file).isFile()) files.push([`/maxima/${dir}/${name}`, fs.readFileSync(file)]);
    }
  }
  for (const name of ['configure.lisp', 'configure.ac', 'maxima-local.in'])
    files.push([`/maxima/${name}`, fs.readFileSync(path.join(upstream, name))]);
  files.push(['/maxima-probe-port.lisp', fs.readFileSync(path.join(root, 'scripts/ports/maxima.lisp'))]);
  const docs = process.env.MIRICL_MAXIMA_DOCS ?? path.join(root, 'build/maxima/doc/info');
  if (libraries && fs.existsSync(docs)) {
    for (const name of fs.readdirSync(docs)) {
      if (/^maxima(?:\.info(?:-\d+)?|-index(?:-html)?\.lisp)$/.test(name) || name === 'build-html-index.lisp')
        files.push([`/maxima/doc/info/${name}`, fs.readFileSync(path.join(docs, name))]);
      if (name.endsWith('.html'))
        files.push([`/maxima/doc/html/${name}`, fs.readFileSync(path.join(docs, name))]);
    }
  }
  return files;
}

if (!isMainThread) {
  const tests = ['suite', 'share'].includes(workerData.mode);
  const files = maximaProbeFiles(tests);
  // The override selects an existing runtime core for suites. Compilation
  // always writes its portable input bundle beside the build report.
  const bundleFile = (tests ? process.env.MIRICL_MAXIMA_CORE : undefined)
    ?? path.join(path.dirname(workerData.output), 'maxima.wasm');
  const compiled = tests && fs.existsSync(bundleFile);
  if (compiled) files.push(['/maxima-core.wasm', fs.readFileSync(bundleFile)]);
  const report = await probeMaxima(fs.readFileSync(workerData.image), files,
    event => parentPort.postMessage({event}), workerData.mode, {compiled, tests: workerData.tests,
      listOnly: workerData.listOnly, buildOnly: workerData.buildOnly,
      saveBundle(bytes) {
        fs.mkdirSync(path.dirname(bundleFile), {recursive: true});
        fs.writeFileSync(bundleFile + '.tmp', bytes);
        fs.renameSync(bundleFile + '.tmp', bundleFile);
      }});
  parentPort.postMessage({report});
} else if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const image = path.resolve(process.argv[2] ?? path.join(root, 'miricl.wasm'));
  const output = path.resolve(process.argv[3] ?? path.join(root, 'build/maxima/probe.json'));
  const mode = process.argv[4] ?? 'source';
  if (!['source', 'upstream', 'compile', 'suite', 'share'].includes(mode)) throw new Error(`Unknown probe mode: ${mode}`);
  const revision = maximaProbeRevision();
  fs.mkdirSync(path.dirname(output), {recursive: true});
  const timeout = Number(process.env.MIRICL_MAXIMA_TIMEOUT_MS ??
    (['suite', 'share'].includes(mode) ? 7200000 : mode === 'compile' ? 900000 : 180000));
  // Some upstream problems run hundreds of special-function evaluations.
  // Keep them bounded without mistaking their coarse test markers for a hang.
  const problemTimeout = Number(process.env.MIRICL_MAXIMA_PROBLEM_TIMEOUT_MS ?? 1800000);
  if (![timeout, problemTimeout].every(value => Number.isFinite(value) && value > 0))
    throw new Error('Maxima timeouts must be finite positive milliseconds.');
  const started = Date.now();
  const suiteFiles = [];
  const persist = report => fs.writeFileSync(output + '.progress.json',
    JSON.stringify({...report, revision, suiteFiles}, null, 2) + '\n');
  const runWorker = (tests, listOnly = false) => {
    const worker = new Worker(new URL(import.meta.url), {workerData: {image, mode, output, tests, listOnly, buildOnly: process.argv.includes('--build-only')}});
    const partial = {mode, stages: [], loads: [], compilations: [], tests: [], suiteProgress: []};
    let lastEvent, failure, problemStarted;
    const remaining = Math.max(1, timeout - (Date.now() - started));
    const stop = message => {failure = {message}; void worker.terminate();};
    const timer = setTimeout(() => stop(`Probe exceeded ${timeout} ms overall.`), remaining);
    let problemTimer;
    const checkpoint = setInterval(() => {
      persist({...partial, lastEvent, problemMilliseconds: problemStarted ? Date.now() - problemStarted : undefined});
      if (problemStarted) console.log(`Maxima still computing (${Math.round((Date.now() - problemStarted) / 1000)}s): ` +
        `${tests.join(', ')} problem ${partial.currentProblem?.slice(0, 160)}`);
    }, 30000);
    return new Promise(resolve => {
      worker.on('message', message => {
        if (message.report) {resolve(message.report); return;}
        lastEvent = message.event;
        if (lastEvent.kind === 'stage-result') partial.stages.push(lastEvent.result);
        if (lastEvent.kind === 'load') partial.loads.push(lastEvent.file);
        if (lastEvent.kind === 'compile') partial.compilations.push(lastEvent.file);
        if (lastEvent.kind === 'test') partial.tests.push(lastEvent);
        if (lastEvent.kind === 'problem') {
          partial.currentProblem = lastEvent.file;
          problemStarted = Date.now();
          clearTimeout(problemTimer);
          problemTimer = setTimeout(() => stop(`Problem exceeded ${problemTimeout} ms: ${partial.currentProblem}`), problemTimeout);
          if (process.env.MIRICL_MAXIMA_PROGRESS) console.log(`Maxima problem ${lastEvent.file}`);
        }
        if (lastEvent.kind === 'suite-result') {
          clearTimeout(problemTimer);
          problemStarted = undefined;
        }
        if (lastEvent.kind === 'suite' || lastEvent.kind === 'suite-result') partial.suiteProgress.push(lastEvent);
        if (lastEvent.kind === 'suite') console.log(`Maxima suite: ${lastEvent.file}`);
        if (lastEvent.kind === 'suite-result') console.log(lastEvent.file);
        if (lastEvent.kind === 'stage') console.log(`Maxima: ${lastEvent.name}`);
        if (lastEvent.kind === 'test') console.log(`${lastEvent.passed ? 'PASS' : 'FAIL'} ${lastEvent.name}`);
      });
      worker.on('error', error => resolve({...partial, passed: false, lastEvent,
        failure: {message: error.message, stack: error.stack}}));
      worker.on('exit', code => resolve({...partial, passed: false, lastEvent,
        failure: failure ?? {message: `Worker exited (${code}).`}}));
    }).finally(() => {clearTimeout(timer); clearTimeout(problemTimer); clearInterval(checkpoint);});
  };
  const selected = process.argv.slice(5).filter(name => name !== '--build-only');
  let report;
  if (['suite', 'share'].includes(mode)) {
    // A host Wasm trap can leave the guest's dynamic state partially unwound.
    // A fresh worker per file also lets the host terminate a looping problem
    // without losing coverage of every remaining file in the upstream suite.
    const plan = await runWorker(selected, true);
    report = {...plan, passed: false, complete: false, suiteFiles};
    if (plan.passed) {
      for (const name of plan.plan) {
        if (Date.now() - started >= timeout) {
          report.failure = {message: `Probe exceeded ${timeout} ms overall; remaining files were not run.`};
          break;
        }
        console.log(`Maxima file ${suiteFiles.length + 1}/${plan.plan.length}: ${name}`);
        const result = await runWorker([name]);
        suiteFiles.push({name, ...result});
        if (result.failure) console.error(`FAIL ${name}: ${result.failure.message}`);
        persist(report);
      }
      report.passed = !report.failure && suiteFiles.length === plan.plan.length && suiteFiles.every(file => file.passed);
      report.suitePassed = report.passed;
      report.complete = suiteFiles.length === plan.plan.length;
    }
  } else report = await runWorker(selected);
  Object.assign(report, {revision, image, imageSha256: createHash('sha256').update(fs.readFileSync(image)).digest('hex')});
  fs.mkdirSync(path.dirname(output), {recursive: true});
  fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
  console.log(`${report.passed ? 'PASS' : 'FAIL'} Maxima ${mode}: ${report.loads?.length ?? 0} loads, ` +
    `${report.tests?.filter(test => test.passed).length ?? 0}/${report.tests?.length ?? 0} smoke checks. Report: ${output}`);
  if (report.plan) {
    console.log(`${suiteFiles.filter(file => file.passed).length}/${report.plan.length} test files passed; ` +
      `${report.plan.length - suiteFiles.length} not run.`);
    for (const file of suiteFiles.filter(file => !file.passed)) console.error(`Failed test file: ${file.name}`);
  }
  if (report.failure) console.error(report.failure.message, report.failure.backtrace ?? '');
  process.exitCode = report.passed ? 0 : 1;
}
