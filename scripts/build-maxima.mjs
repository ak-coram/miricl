// Assemble a static browser distribution from the compiled core; validation belongs to CI.
import fs from 'node:fs/promises';
import path from 'node:path';
import {gzipSync} from 'node:zlib';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';
import {maximaProbeRevision} from './probe-maxima.mjs';
import {versionAssets} from './version-assets.mjs';

const root = fileURLToPath(new URL('../', import.meta.url));
const imagePath = path.resolve(process.argv[2] ?? 'miricl.wasm');
const corePath = path.resolve(process.argv[3] ?? 'build/maxima/maxima-optimized.wasm');
const output = path.resolve(process.argv[4] ?? 'build/maxima/site');
const [image, core] = await Promise.all([fs.readFile(imagePath), fs.readFile(corePath)]);
const report = {revision: maximaProbeRevision(),
  imageSha256: createHash('sha256').update(image).digest('hex'),
  coreSha256: createHash('sha256').update(core).digest('hex'), coreBytes: core.length};
await fs.mkdir(output, {recursive: true});
await Promise.all([
  fs.writeFile(path.join(output, 'miricl.wasm.gz'), gzipSync(image, {level: 9})),
  fs.writeFile(path.join(output, 'maxima.core.gz'), gzipSync(core, {level: 9})),
  ...['miricl.mjs', 'maxima.mjs'].map(name => fs.copyFile(path.join(root, 'scripts', name), path.join(output, name))),
  ...['index.html', 'worker.mjs', 'examples.mjs'].map(name =>
    fs.copyFile(path.join(root, 'maxima', name), path.join(output, name))),
  ...['app.mjs', 'session-store.mjs'].map(name =>
    fs.copyFile(path.join(root, 'app-common', name), path.join(output, name))),
  fs.copyFile(path.join(root, 'vendor/maxima/COPYING'), path.join(output, 'COPYING')),
  fs.copyFile(path.join(root, 'LICENSE'), path.join(output, 'MIRICL-LICENSE')),
]);
for (const name of ['files.json', 'files.json.gz', 'doc', 'filesystem.bin.gz', 'filesystem-archive.mjs', 'maxima.wasm',
  'preview-desktop.png', 'preview-mobile.png'])
  await fs.rm(path.join(output, name), {force: true, recursive: true});
await Promise.all(['miricl.wasm', 'maxima.core'].map(name => fs.rm(path.join(output, name), {force: true})));
report.assetVersion = await versionAssets(output, ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs',
  'worker.mjs', 'miricl.mjs', 'maxima.mjs', 'miricl.wasm.gz', 'maxima.core.gz']);
await fs.writeFile(path.join(output, 'build.json'), JSON.stringify(report, null, 2) + '\n');
console.log(`Built Maxima browser distribution in ${output}`);
