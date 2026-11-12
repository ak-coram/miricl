// Package the browser page and its reusable Lisp image for GitHub Pages.
import fs from 'node:fs/promises';
import path from 'node:path';
import {versionAssets} from '../scripts/version-assets.mjs';
const [image = 'mcclim/build/miricl-mcclim.wasm', output = 'build/mcclim/site'] = process.argv.slice(2);
await fs.mkdir(output, {recursive:true});
for (const file of ['index.html', 'app.mjs', 'startup.lisp'])
  await fs.copyFile(new URL(file, import.meta.url), path.join(output, file));
await fs.copyFile(image, path.join(output, 'miricl-mcclim.wasm'));
await fs.copyFile(new URL('../miricl.js', import.meta.url), path.join(output, 'miricl.js'));
await versionAssets(output, ['index.html', 'app.mjs', 'miricl.js', 'miricl-mcclim.wasm', 'startup.lisp']);
console.log(`Wrote McCLIM page ${path.resolve(output)}`);
