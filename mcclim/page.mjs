// Package the browser page and its reusable Lisp image for GitHub Pages.
import fs from 'node:fs/promises';
import path from 'node:path';
import {gzipSync} from 'node:zlib';
import {versionAssets} from '../scripts/version-assets.mjs';
const [image = 'mcclim/build/miricl-mcclim.wasm', output = 'build/mcclim/site'] = process.argv.slice(2);
await fs.mkdir(output, {recursive:true});
// Remove the former separate adapter when repackaging an existing site.
await Promise.all(['miricl.js', 'miricl.mjs'].map(name => fs.rm(path.join(output, name), {force: true})));
for (const file of ['index.html', 'app.mjs', 'startup.lisp'])
  await fs.copyFile(new URL(file, import.meta.url), path.join(output, file));
await fs.writeFile(path.join(output, 'miricl-mcclim.wasm.gz'), gzipSync(await fs.readFile(image), {level: 9}));
await fs.rm(path.join(output, 'miricl-mcclim.wasm'), {force: true});
await fs.copyFile(new URL('../scripts/miricl-loader.mjs', import.meta.url), path.join(output, 'miricl-loader.mjs'));
await versionAssets(output, ['index.html', 'app.mjs', 'miricl-loader.mjs', 'miricl-mcclim.wasm.gz', 'startup.lisp']);
console.log(`Wrote McCLIM page ${path.resolve(output)}`);
