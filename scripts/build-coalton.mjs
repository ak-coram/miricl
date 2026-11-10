// Assemble the tour from the pinned upstream Markdown and real native bundle.
import fs from 'node:fs/promises';
import path from 'node:path';
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {parseTour, renderTour} from '../coalton/tour.mjs';
import {versionAssets} from './version-assets.mjs';
const root = path.resolve(import.meta.dirname, '..');
const [imagePath = 'miricl-precompiled.wasm', corePath = 'build/coalton/coalton.wasm',
  output = 'build/coalton/site'] = process.argv.slice(2);
const [image, core, markdown, template] = await Promise.all([
  fs.readFile(imagePath), fs.readFile(corePath),
  fs.readFile(path.join(root, 'vendor/coalton/docs/manual/site/topics/whirlwind-tour.md'), 'utf8'),
  fs.readFile(path.join(root, 'coalton/index.html'), 'utf8')]);
new WebAssembly.Module(image); new WebAssembly.Module(core);
const revision = execFileSync('git', ['-C', path.join(root, 'vendor/coalton'), 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
const optimization = JSON.parse(await fs.readFile(path.join(path.dirname(corePath), 'optimization.json'), 'utf8'));
if (optimization.backend !== 'native-wasm' || optimization.scope !== 'full' || optimization.coreSha256 !== sha256(core)
    || optimization.imageSha256 !== sha256(image) || optimization.revision !== revision)
  throw new Error('Expected a native Coalton core built for this runtime and source revision.');
const tour = parseTour(markdown);
if (!tour.cells.length) throw new Error('The upstream tour contains no editable examples.');
const replacements = {'@@TOUR@@': renderTour(tour), '@@COUNT@@': tour.cells.length, '@@REVISION@@': revision};
await fs.mkdir(output, {recursive: true});
await Promise.all([
  fs.writeFile(path.join(output, 'index.html'), template.replace(/@@[A-Z]+@@/gu, marker => {
    if (!Object.hasOwn(replacements, marker)) throw new Error(`Unknown tour marker ${marker}`);
    return replacements[marker];
  })),
  fs.writeFile(path.join(output, 'miricl.wasm'), image), fs.writeFile(path.join(output, 'coalton.wasm'), core),
  fs.writeFile(path.join(output, 'tour.md'), markdown),
  ...['app.mjs', 'worker.mjs'].map(name => fs.copyFile(path.join(root, 'coalton', name), path.join(output, name))),
  ...['miricl.mjs', 'coalton.mjs'].map(name => fs.copyFile(path.join(root, 'scripts', name), path.join(output, name))),
  fs.copyFile(path.join(root, 'vendor/coalton/LICENSE.txt'), path.join(output, 'LICENSE.txt')),
  fs.copyFile(path.join(root, 'LICENSE'), path.join(output, 'MIRICL-LICENSE')),
]);
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
const assetVersion = await versionAssets(output, ['index.html', 'app.mjs', 'worker.mjs', 'coalton.mjs',
  'miricl.mjs', 'miricl.wasm', 'coalton.wasm']);
await fs.writeFile(path.join(output, 'build.json'), JSON.stringify({revision, assetVersion, dependencies,
  imageSha256: sha256(image), coreSha256: sha256(core), coreBytes: core.length,
  backend: 'native-wasm', editableExamples: tour.cells.length, tourSha256: sha256(markdown)}, null, 2) + '\n');
console.log(`Built Coalton tour: ${tour.cells.length} editable examples (${output}).`);
