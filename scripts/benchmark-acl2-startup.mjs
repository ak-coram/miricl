// Measure loading and logical-world initialization without running proof tests.
// node scripts/benchmark-acl2-startup.mjs IMAGE CORE REPORT [--profile] [--save-image] [--compile-limit=N]
import fs from 'node:fs/promises';
import path from 'node:path';
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {portAcl2Source} from '../acl2/host.mjs';
import {startWasmProfile} from './wasm-profile.mjs';

const profileEnabled = process.argv.includes('--profile');
const saveImage = process.argv.includes('--save-image');
const limitOption = process.argv.find(arg => arg.startsWith('--compile-limit='));
const compileLimit = limitOption ? Number(limitOption.split('=')[1]) : null;
if (compileLimit !== null && (!Number.isSafeInteger(compileLimit) || compileLimit <= 0))
  throw new Error('Expected a positive integer compilation limit');
if (compileLimit && saveImage) throw new Error('A bounded initialization cannot produce an initialized image');
const [imagePath, corePath, reportPath] = process.argv.slice(2)
  .filter(arg => !['--profile', '--save-image', limitOption].includes(arg));
if (!reportPath) throw new Error('Expected IMAGE CORE REPORT [--profile] [--save-image] [--compile-limit=N]');
const revision = execFileSync('git', ['-C', 'vendor/acl2', 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
const [image, core] = await Promise.all([fs.readFile(imagePath), fs.readFile(corePath)]);
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const report = {revision, imagePath, corePath, imageSha256: hash(image), coreSha256: hash(core),
  profiled: profileEnabled, compileLimit, stages: []};
let stage = 'runtime', vm;
const started = performance.now(), cpu = process.cpuUsage();
const heartbeat = setInterval(() => console.log(`ACL2 startup: ${stage} (${Math.round((performance.now() - started) / 1000)} s)`), 30000);
async function evaluate(name, source) {
  stage = name;
  console.log(`ACL2 startup: ${name}`);
  const finish = profileEnabled && name === 'initialize world'
    ? await startWasmProfile(vm, image, reportPath.replace(/\.json$/, '') + '-world') : null;
  const before = performance.now(), usage = process.cpuUsage();
  try {
    const {output} = await vm.evaluateAsync(source, {eof: true});
    const consumed = process.cpuUsage(usage);
    report.stages.push({name, milliseconds: performance.now() - before,
      cpuMilliseconds: (consumed.user + consumed.system) / 1000, outputBytes: Buffer.byteLength(output)});
  } finally {if (finish) await finish();}
}
try {
  vm = await Miricl.instantiate(image);
  for (const entry of await fs.readdir('vendor/acl2', {withFileTypes: true})) {
    if (!entry.isFile() || entry.name === '.git') continue;
    vm.mountFile(`/acl2/${entry.name}`, portAcl2Source(entry.name,
      await fs.readFile(path.join('vendor/acl2', entry.name)), {revision}));
  }
  vm.mountFile('/acl2/miricl-host.lisp', await fs.readFile('acl2/miricl-host.lisp'));
  vm.mountFile('/acl2.core', core);
  await evaluate('load core', `
    (setq *default-pathname-defaults* #P"/acl2/")
    (pushnew :cltl2 *features*) (pushnew :unix *features*)
    (load "init.lisp") (acl2::proclaim-optimize)
    (let ((*readtable* acl2::*acl2-readtable*)) (load "/acl2.core"))
    (set 'acl2::state acl2::*the-live-state*)
    (delete-file "/acl2.core") (values)`);
  if (compileLimit) {
    await evaluate('bound compilation prefix', `
      (defparameter cl-user::*acl2-benchmark-compiles* 0)
      (let ((original (symbol-function 'miricl-compiler:compile-native)))
        (setf (symbol-function 'miricl-compiler:compile-native)
          (lambda (form &rest arguments)
            (when (>= cl-user::*acl2-benchmark-compiles* ${compileLimit})
              (throw 'cl-user::acl2-benchmark-limit :prefix-complete))
            (multiple-value-prog1 (apply original form arguments)
              (incf cl-user::*acl2-benchmark-compiles*))))) (values)`);
  }
  await evaluate('initialize world', `
    (in-package :acl2)
    ${compileLimit ? "(catch 'cl-user::acl2-benchmark-limit" : '(progn'}
      (unless (acl2::initialize-acl2) (error "ACL2 world initialization did not complete."))
      (values))`);
  if (compileLimit) {
    report.compilations = Number((await vm.evaluateAsync('cl-user::*acl2-benchmark-compiles*', {eof: true})).output.trim());
    report.prefixCompleted = report.compilations === compileLimit;
  }
  if (saveImage) {
    stage = 'save initialized image';
    const checkpoint = vm.saveImage(), filename = reportPath.replace(/\.json$/, '') + '-image.wasm';
    await fs.mkdir(path.dirname(filename), {recursive: true});
    await fs.writeFile(filename, checkpoint);
    report.checkpoint = {filename, bytes: checkpoint.length, sha256: hash(checkpoint)};
  }
  report.completed = !compileLimit;
} catch (error) {
  report.failure = {stage, message: error.message, output: error.output, backtrace: error.backtrace};
  throw error;
} finally {
  clearInterval(heartbeat); vm?.close();
  const consumed = process.cpuUsage(cpu);
  report.milliseconds = performance.now() - started;
  report.cpuMilliseconds = (consumed.user + consumed.system) / 1000;
  report.maximumRssKiB = process.resourceUsage().maxRSS;
  await fs.mkdir(path.dirname(reportPath), {recursive: true});
  await fs.writeFile(reportPath, JSON.stringify(report, null, 2) + '\n');
}
console.log(JSON.stringify(report));
