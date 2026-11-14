// Assemble the Lisp/Coalton REPL with the shared application session UI.
import fs from 'node:fs/promises';
import path from 'node:path';
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {gzipSync} from 'node:zlib';
import {examples} from '../coalton/examples.mjs';
import {versionAssets} from './version-assets.mjs';
const root = path.resolve(import.meta.dirname, '..');
const [imagePath = 'miricl.wasm', corePath = 'build/coalton/coalton.wasm',
  output = 'build/coalton/site', ...options] = process.argv.slice(2);
const initialized = options.length === 3 && options[0] === '--initialized-image';
if (options.length && !initialized) throw new Error('Expected --initialized-image IMAGE TEST-REPORT.');
const [image, core, markdown, template, exampleSource] = await Promise.all([
  fs.readFile(imagePath), fs.readFile(corePath),
  fs.readFile(path.join(root, 'vendor/coalton/docs/manual/site/topics/whirlwind-tour.md'), 'utf8'),
  fs.readFile(path.join(root, 'coalton/index.html'), 'utf8'),
  fs.readFile(path.join(root, 'coalton/examples.mjs'), 'utf8')]);
new WebAssembly.Module(image); new WebAssembly.Module(core);
const revision = execFileSync('git', ['-C', path.join(root, 'vendor/coalton'), 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
const optimization = JSON.parse(await fs.readFile(path.join(path.dirname(corePath), 'optimization.json'), 'utf8'));
if (optimization.backend !== 'native-wasm' || optimization.scope !== 'full' || optimization.coreSha256 !== sha256(core)
    || optimization.imageSha256 !== sha256(image) || optimization.revision !== revision)
  throw new Error('Expected a native Coalton core built for this runtime and source revision.');
let checkpoint, snapshotReport;
if (initialized) {
  [checkpoint, snapshotReport] = await Promise.all([
    fs.readFile(options[1]), fs.readFile(options[2], 'utf8').then(JSON.parse)]);
  if (!snapshotReport.passed || snapshotReport.revision !== revision
      || snapshotReport.runtimeSha256 !== sha256(image) || snapshotReport.coreSha256 !== sha256(core)
      || snapshotReport.snapshotSha256 !== sha256(checkpoint))
    throw new Error('Expected a Coalton image that passed restored-session validation.');
}
const dataAssets = checkpoint ? ['coalton-image.wasm.gz'] : ['miricl.wasm.gz', 'coalton.wasm.gz'];
const replacements = {'@@REVISION@@': revision};
await fs.mkdir(output, {recursive: true});
await Promise.all([
  fs.writeFile(path.join(output, 'index.html'), template.replace(/@@[A-Z]+@@/gu, marker => {
    if (!Object.hasOwn(replacements, marker)) throw new Error(`Unknown REPL marker ${marker}`);
    return replacements[marker];
  })),
  fs.writeFile(path.join(output, 'tour.md'), markdown),
  fs.copyFile(path.join(root, 'coalton/worker.mjs'), path.join(output, 'worker.mjs')),
  fs.copyFile(path.join(root, 'coalton/client.mjs'), path.join(output, 'client.mjs')),
  fs.writeFile(path.join(output, 'examples.mjs'),
    exampleSource.replaceAll('@@REVISION@@', revision)),
  ...['app.mjs', 'session-store.mjs'].map(name => fs.copyFile(path.join(root, 'app-common', name), path.join(output, name))),
  ...['miricl.mjs', 'coalton.mjs', 'repl-debugger.mjs'].map(name => fs.copyFile(path.join(root, 'scripts', name), path.join(output, name))),
  fs.copyFile(path.join(root, 'vendor/coalton/LICENSE.txt'), path.join(output, 'LICENSE.txt')),
  fs.copyFile(path.join(root, 'LICENSE'), path.join(output, 'MIRICL-LICENSE')),
]);
for (const name of ['miricl.wasm.gz', 'coalton.wasm.gz', 'coalton-image.wasm.gz']) {
  const data = name === 'coalton-image.wasm.gz' ? (checkpoint && gzipSync(checkpoint, {level: 9}))
    : checkpoint ? null : gzipSync(name === 'miricl.wasm.gz' ? image : core, {level: 9});
  if (data) await fs.writeFile(path.join(output, name), data);
  else await fs.rm(path.join(output, name), {force: true});
}
await Promise.all(['miricl.wasm', 'coalton.wasm'].map(name => fs.rm(path.join(output, name), {force: true})));
const dependencies = JSON.parse(await fs.readFile(path.join(root, 'coalton/dependencies.json'), 'utf8'));
const notices = [];
for (const dependency of dependencies) {
  const directory = path.join(root, 'build/coalton/deps', dependency.name);
  const names = (await fs.readdir(directory)).filter(name => /^(?:licen[cs]e|copying|copyright)(?:\.|$)/iu.test(name));
  // Trivial-garbage carries its license in its README and source headers.
  if (!names.length) names.push(...(await fs.readdir(directory)).filter(name => /^readme/iu.test(name)));
  notices.push(`${dependency.name}\n${dependency.url}\nRevision ${dependency.revision}\n`);
  for (const name of names) notices.push(`${name}\n${await fs.readFile(path.join(directory, name), 'utf8')}`);
}
await fs.writeFile(path.join(output, 'DEPENDENCY-LICENSES.txt'), notices.join('\n\n'));
const assetVersion = await versionAssets(output, ['index.html', 'client.mjs', 'repl-debugger.mjs', 'app.mjs', 'worker.mjs', 'examples.mjs', 'session-store.mjs', 'coalton.mjs',
  'miricl.mjs', ...dataAssets]);
await fs.writeFile(path.join(output, 'build.json'), JSON.stringify({revision, assetVersion, dependencies,
  imageSha256: sha256(image), coreSha256: sha256(core), coreBytes: core.length,
  backend: 'native-wasm', initializedImage: Boolean(checkpoint),
  ...(checkpoint ? {snapshotSha256: sha256(checkpoint), snapshotBytes: checkpoint.length,
    snapshotDownloadBytes: snapshotReport.snapshotDownloadBytes} : {}),
  examples: examples.length, documentationSha256: sha256(markdown)}, null, 2) + '\n');
console.log(`Built Lisp/Coalton REPL: ${examples.length} documentation examples (${output}).`);
