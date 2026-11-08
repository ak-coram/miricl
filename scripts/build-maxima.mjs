// Assemble a static browser distribution from the validated compiled core.
import fs from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';
import {gzipSync} from 'node:zlib';
import {maximaProbeFiles, maximaProbeRevision} from './probe-maxima.mjs';
import {probeMaxima} from './maxima-probe.mjs';

const root = fileURLToPath(new URL('../', import.meta.url));
const imagePath = path.resolve(process.argv[2] ?? 'miricl.wasm');
const corePath = path.resolve(process.argv[3] ?? 'build/maxima/maxima.wasm');
const output = path.resolve(process.argv[4] ?? 'build/maxima/site');
const docs = path.resolve(process.argv[5] ?? path.join(path.dirname(corePath), 'doc/info'));
process.env.MIRICL_MAXIMA_DOCS = docs;
const [image, core] = await Promise.all([fs.readFile(imagePath), fs.readFile(corePath)]);
const files = maximaProbeFiles(true);
const report = await probeMaxima(image, [...files, ['/maxima-core.wasm', core]], event => {
  if (event.kind === 'stage') console.log(`Maxima distribution: ${event.name}`);
}, 'source', {compiled: true});
if (!report.passed) throw new Error(JSON.stringify(report.failure ?? report.tests));
if (report.stages.some(stage => /unable to set up the help system/.test(stage.output ?? '')))
  throw new Error('Build both help indexes before assembling the browser distribution.');
await fs.mkdir(output, {recursive: true});
await Promise.all([
  fs.writeFile(path.join(output, 'miricl.wasm'), image),
  fs.writeFile(path.join(output, 'maxima.wasm'), core),
  fs.writeFile(path.join(output, 'files.json.gz'), gzipSync(JSON.stringify(files.map(([name, bytes]) =>
    [name, bytes.toString('base64')])), {level: 9})),
  ...['miricl.mjs', 'maxima.mjs'].map(name => fs.copyFile(path.join(root, 'scripts', name), path.join(output, name))),
  ...['index.html', 'worker.mjs', 'app.mjs', 'examples.mjs', 'session-store.mjs'].map(name =>
    fs.copyFile(path.join(root, 'maxima', name), path.join(output, name))),
  fs.copyFile(path.join(root, 'vendor/maxima/COPYING'), path.join(output, 'COPYING')),
  fs.copyFile(path.join(root, 'LICENSE'), path.join(output, 'MIRICL-LICENSE')),
]);
await fs.rm(path.join(output, 'files.json'), {force: true});
await fs.cp(docs, path.join(output, 'doc/html'), {recursive: true, filter: async file =>
  (await fs.stat(file)).isDirectory() || /\.(html|css|png|svg|gif|jpe?g|ico)$/.test(file)});
Object.assign(report, {revision: maximaProbeRevision(),
  imageSha256: createHash('sha256').update(image).digest('hex'),
  coreSha256: createHash('sha256').update(core).digest('hex')});
await fs.writeFile(path.join(output, 'build.json'), JSON.stringify(report, null, 2) + '\n');
console.log(`Built Maxima browser distribution in ${output}`);
