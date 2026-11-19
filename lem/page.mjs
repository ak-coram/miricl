// Package a matching frontend-ready guest-compiled image as a separate application.
import fs from 'node:fs/promises';
import path from 'node:path';
import {gzipSync} from 'node:zlib';
import {versionAssets} from '../scripts/version-assets.mjs';
const [image = 'build/lem/miricl-lem.wasm', output = 'build/lem/site', ...extra] = process.argv.slice(2);
if (extra.length) throw new Error('Usage: node lem/page.mjs [IMAGE.wasm] [OUTPUT-DIRECTORY]');
await fs.mkdir(output, {recursive: true});
for (const name of ['index.html', 'app.mjs'])
  await fs.copyFile(new URL(name, import.meta.url), path.join(output, name));
await fs.copyFile(new URL('../miricl.js', import.meta.url), path.join(output, 'miricl.js'));
await fs.writeFile(path.join(output, 'miricl-lem.wasm.gz'), gzipSync(await fs.readFile(image), {level: 9}));
await fs.rm(path.join(output, 'miricl-lem.wasm'), {force: true});
await versionAssets(output, ['index.html', 'app.mjs', 'miricl.js', 'miricl-lem.wasm.gz']);
console.log(`Wrote LEM application page ${path.resolve(output)}`);
