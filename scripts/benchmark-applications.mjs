// Full application builds, with identical inputs and independent validation.
// node scripts/benchmark-applications.mjs OUTPUT BEFORE.wasm AFTER.wasm [APP ...] [--resume] [--acl2-load-only]
// Applications: maxima, fricas, acl2, coalton. Node records CLI CPU time and RSS.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {createWriteStream} from 'node:fs';
import path from 'node:path';
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {measureFile} from './wasm-size.mjs';
import {coaltonFiles} from './coalton-files.mjs';

const resume = process.argv.includes('--resume'), acl2LoadOnly = process.argv.includes('--acl2-load-only');
const [output, before, after, ...requested] = process.argv.slice(2)
  .filter(arg => !['--resume', '--acl2-load-only'].includes(arg));
if (!output || !before || !after) throw new Error('Expected OUTPUT BEFORE.wasm AFTER.wasm [APP ...]');
const applications = requested.length ? requested : ['maxima', 'fricas', 'acl2', 'coalton'];
assert.ok(applications.every(app => ['maxima', 'fricas', 'acl2', 'coalton'].includes(app)));
await fs.mkdir(output, {recursive: true});
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const reportPath = path.join(output, 'results.json');
const report = resume ? JSON.parse(await fs.readFile(reportPath, 'utf8')) : {
  node: process.version, platform: process.platform, architecture: process.arch,
  rounds: 1, scope: 'Full builds and fresh-session correctness checks; exploratory paired run',
  started: new Date().toISOString(), variants: []};
if (resume) {
  assert.equal(report.node, process.version, 'Cannot resume with a different Node version');
  assert.equal(report.platform, process.platform, 'Cannot resume on a different platform');
  assert.equal(report.architecture, process.arch, 'Cannot resume on a different architecture');
}
for (const [name, file] of [['before', before], ['after', after]]) {
  const imageSha256 = hash(await fs.readFile(file));
  if (resume) {
    const previous = report.variants.find(v => v.name === name);
    assert.ok(previous, `Missing ${name} variant in resumed report`);
    assert.equal(previous.imageSha256, imageSha256, `Cannot resume with a different ${name} compiler`);
    assert.equal(path.resolve(previous.image), path.resolve(file), 'Compiler image path changed');
  } else report.variants.push({name, image: file, imageSha256,
    imageSizes: measureFile(file), applications: []});
}
if (resume) (report.resumptions ??= []).push(new Date().toISOString());
delete report.finished;
const save = () => fs.writeFile(reportPath, JSON.stringify(report, null, 2) + '\n');
await save();

async function run(directory, phase, args, environment = {}) {
  const logPath = path.join(directory, `${phase}.log`), timePath = path.join(directory, `${phase}.time.json`);
  const log = createWriteStream(logPath);
  console.log(`${directory}: ${phase}`);
  const started = performance.now();
  const child = spawn(process.execPath, ['scripts/measure-node.mjs', timePath, ...args],
    {env: {...process.env, ...environment}, stdio: ['ignore', 'pipe', 'pipe']});
  for (const stream of [child.stdout, child.stderr]) stream.on('data', bytes => {
    log.write(bytes); process.stdout.write(bytes);
  });
  let status;
  try {status = await new Promise((resolve, reject) => {child.on('error', reject); child.on('close', resolve);});}
  finally {await new Promise(resolve => log.end(resolve));}
  const measured = JSON.parse(await fs.readFile(timePath, 'utf8'));
  const measurement = {phase, ...measured, status,
    observedWallMs: performance.now() - started, log: logPath};
  return measurement;
}

for (const app of applications) {
  const input = app === 'coalton' ? null : `build/${app}/${app}.wasm`;
  const inputHash = input && hash(await fs.readFile(input));
  let sourceHash;
  if (app === 'coalton') {
    const sources = await coaltonFiles(), digest = createHash('sha256');
    for (const [name, bytes] of [...sources].sort(([a], [b]) => a.localeCompare(b))) digest.update(name).update('\0').update(bytes);
    sourceHash = digest.digest('hex');
  }
  for (const variant of report.variants) {
    const directory = path.join(output, app, variant.name);
    await fs.mkdir(directory, {recursive: true});
    let item = variant.applications.find(previous => previous.app === app);
    const validation = app === 'acl2' && acl2LoadOnly ? 'native-core-load' : 'full';
    const validationChanged = item && item.validation && item.validation !== validation;
    if (item) {
      assert.equal(item.inputSha256, inputHash, 'Cannot resume with changed application input');
      assert.equal(item.sourceSha256, sourceHash, 'Cannot resume with changed application sources');
      if (item.passed && !validationChanged) {console.log(`${directory}: already passed`); continue;}
      item.passed = false; delete item.failure;
    } else {
      item = {app, input, inputSha256: inputHash, sourceSha256: sourceHash, stages: [], passed: false};
      variant.applications.push(item);
    }
    item.validation = validation; await save();
    const core = path.join(directory, `${app}-native.wasm`), image = variant.image;
    const steps = app === 'coalton' ? [
      ['bootstrap-source', ['scripts/probe-coalton.mjs', image, path.join(directory, 'bootstrap/compile.json'), 'compiler'],
        {MIRICL_COALTON_CACHE_DIRECTORY: path.join(directory, 'bootstrap-cache')}],
      ['bootstrap-native', ['scripts/optimize-coalton.mjs', image, path.join(directory, 'bootstrap/compiler-portable.wasm'),
        path.join(directory, 'bootstrap/compiler.wasm')], {MIRICL_COALTON_SCOPE: 'compiler'}],
      ['library-source', ['scripts/probe-coalton.mjs', image, path.join(directory, 'compile.json')],
        {MIRICL_COALTON_BOOTSTRAP: path.join(directory, 'bootstrap/compiler.wasm'),
          MIRICL_COALTON_CACHE_DIRECTORY: path.join(directory, 'library-cache')}],
      ['library-native', ['scripts/optimize-coalton.mjs', image, path.join(directory, 'coalton-portable.wasm'), core],
        {MIRICL_COALTON_BOOTSTRAP: path.join(directory, 'bootstrap/compiler.wasm')}],
      ['validate', ['tests/coalton-session.mjs', image, core]],
    ] : [
      ['compile', [`scripts/optimize-${app}.mjs`, image, input, core]],
      ['validate', app === 'acl2'
        ? [acl2LoadOnly ? 'tests/acl2-load.mjs' : 'tests/acl2-native.mjs', image, core, path.join(directory, 'validation.json')]
        : app === 'maxima' ? ['tests/maxima.mjs', image, core]
          : ['scripts/benchmark-application-session.mjs', image, core, path.join(directory, 'validation.json')]],
    ];
    try {
      if (input) assert.equal(hash(await fs.readFile(input)), inputHash, 'Application input changed');
      for (const [phase, args, environment] of steps) {
        const previous = item.stages.findIndex(stage => stage.phase === phase);
        if (previous >= 0) {
          if (item.stages[previous].status === 0 && !(phase === 'validate' && validationChanged)) {
            console.log(`${directory}: ${phase} already passed`); continue;
          }
          (item.previousAttempts ??= []).push(item.stages.splice(previous, 1)[0]);
        }
        const measurement = await run(directory, phase, args, environment);
        item.stages.push(measurement); await save();
        assert.equal(measurement.status, 0, `${app} ${variant.name} ${phase} failed; see ${measurement.log}`);
      }
      item.coreSha256 = hash(await fs.readFile(core)); item.coreSizes = measureFile(core); item.passed = true;
    } catch (error) {item.failure = error.message; console.error(error.message);}
    await save();
  }
}
if (report.variants.every(variant => variant.applications.every(app => app.passed)))
  report.finished = new Date().toISOString();
await save();
if (report.variants.some(variant => variant.applications.some(app => applications.includes(app.app) && !app.passed)))
  process.exitCode = 1;
