import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {Miricl} from '../scripts/miricl.mjs';
import {portAcl2Source} from '../acl2/host.mjs';

export async function loadAcl2TestCore(imagePath, corePath) {
  const [image, core, metadata] = await Promise.all([
    fs.readFile(imagePath), fs.readFile(corePath),
    fs.readFile(path.join(path.dirname(corePath), 'optimization.json'), 'utf8').then(JSON.parse)]);
  const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
  assert.equal(metadata.backend, 'native-wasm');
  assert.equal(sha256(image), metadata.imageSha256);
  assert.equal(sha256(core), metadata.coreSha256);
  const vm = await Miricl.instantiate(image);
  try {
    for (const entry of await fs.readdir('vendor/acl2', {withFileTypes: true})) {
      if (entry.isFile() && entry.name !== '.git')
        vm.mountFile(`/acl2/${entry.name}`, portAcl2Source(entry.name,
          await fs.readFile(path.join('vendor/acl2', entry.name)), {revision: metadata.revision}));
    }
    vm.mountFile('/acl2/miricl-host.lisp', await fs.readFile('acl2/miricl-host.lisp'));
    vm.mountFile('/acl2.core', core);
    await vm.evaluateAsync(`
      (setq *default-pathname-defaults* #P"/acl2/")
      (pushnew :cltl2 *features*) (pushnew :unix *features*)
      (load "init.lisp") (acl2::proclaim-optimize)
      (let ((*readtable* acl2::*acl2-readtable*)) (load "/acl2.core")) (values)`, {eof: true});
    return {vm, metadata};
  } catch (error) {vm.close(); throw error;}
}
