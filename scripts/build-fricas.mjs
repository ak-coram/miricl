// Package the optimized interpreter and the complete on-demand algebra library.
import fs from 'node:fs/promises';
import path from 'node:path';
import {gzipSync} from 'node:zlib';
import {createHash} from 'node:crypto';
import {fricasFiles, fricasRevision} from './fricas-files.mjs';
import {encodeFilesystem} from './filesystem-archive.mjs';
import {prepareFriCASLibraries} from './prepare-fricas-libraries.mjs';
import {versionAssets} from './version-assets.mjs';
const root = path.resolve(import.meta.dirname, '..');
const [imagePath = 'miricl-precompiled.wasm', corePath = 'build/fricas/fricas-optimized.wasm',
  native = 'build/fricas/native', output = 'build/fricas/site'] = process.argv.slice(2);
const [image, core] = await Promise.all([fs.readFile(imagePath), fs.readFile(corePath)]);
const {files} = fricasFiles(path.resolve(native));
const libraries = await prepareFriCASLibraries(image, core, native, path.join(path.dirname(corePath), 'algebra'));
const shipped = [...files.filter(([name]) => (name.startsWith('/fricas/algebra/') && !name.endsWith('.lsp'))
  || name.startsWith('/fricas/share/') || name === '/fricas/lisp/fricas-package.lisp' || name === '/fricas-port.lisp'),
  ...libraries];
await fs.mkdir(output, {recursive: true});
await Promise.all([
  fs.writeFile(path.join(output, 'miricl.wasm'), image), fs.writeFile(path.join(output, 'fricas.wasm'), core),
  fs.writeFile(path.join(output, 'filesystem.bin.gz'), gzipSync(encodeFilesystem(shipped), {level: 9})),
  ...['miricl.mjs', 'fricas.mjs', 'filesystem-archive.mjs', 'mounted-filesystem.mjs'].map(name => fs.copyFile(path.join(root, 'scripts', name), path.join(output, name))),
  ...['index.html', 'worker.mjs', 'examples.mjs'].map(name => fs.copyFile(path.join(root, 'fricas', name), path.join(output, name))),
  ...['app.mjs', 'session-store.mjs'].map(name => fs.copyFile(path.join(root, 'maxima', name), path.join(output, name))),
  fs.copyFile(path.join(root, 'vendor/fricas/LICENSE.txt'), path.join(output, 'LICENSE.txt')),
  fs.copyFile(path.join(root, 'vendor/fricas/license/LICENSE.AXIOM'), path.join(output, 'LICENSE.AXIOM')),
  fs.copyFile(path.join(root, 'LICENSE'), path.join(output, 'MIRICL-LICENSE')),
]);
await fs.rm(path.join(output, 'files.json.gz'), {force: true});
const assetVersion = await versionAssets(output, ['index.html', 'app.mjs', 'examples.mjs', 'session-store.mjs',
  'worker.mjs', 'miricl.mjs', 'fricas.mjs', 'filesystem-archive.mjs', 'mounted-filesystem.mjs', 'miricl.wasm', 'fricas.wasm', 'filesystem.bin.gz']);
await fs.writeFile(path.join(output, 'build.json'), JSON.stringify({revision: fricasRevision(), assetVersion,
  imageSha256: createHash('sha256').update(image).digest('hex'), coreSha256: createHash('sha256').update(core).digest('hex'),
  algebraFiles: libraries.length, algebraFormat: 'native-wasm', algebraOptimizer: 'shake-file :strip nil + compile-native-bundle',
}, null, 2) + '\n');
console.log(`Built FriCAS browser distribution in ${output}`);
