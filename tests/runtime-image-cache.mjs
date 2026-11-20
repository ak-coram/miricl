// Input-key regression only: no compiler build, VM or Git mutation.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {runtimeInputKey, runtimeArtifacts, runtimeOutputRows, verifyRuntimeDistribution} from '../scripts/runtime-image-cache.mjs';
test('runtime cache follows compiler and host dependencies independently of applications', async () => {
  const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'miricl-runtime-inputs-'));
  try {
    await fs.cp(new URL('../scripts/', import.meta.url), path.join(workspace, 'scripts'), {recursive: true});
    const tracked = ['miricl.asd', 'compiler.lisp', 'lem/provider.lisp', 'patches/asdf/series', 'scripts/ports/runtime.lisp'];
    const files = [...tracked, '.github/actions/setup-miricl/action.yml',
      'compatibility/package-local-nicknames.lisp', 'libwasmtime.so', 'vendor/asdf/asdf.lisp'];
    for (const file of files) {
      const destination = path.join(workspace, file);
      await fs.mkdir(path.dirname(destination), {recursive: true}); await fs.writeFile(destination, file);
    }
    const dependency = path.join(workspace, 'host-dependency.lisp');
    await fs.writeFile(dependency, 'host library');
    await fs.mkdir(path.join(workspace, 'build'), {recursive: true});
    await fs.writeFile(path.join(workspace, 'build/runtime-host-inputs.json'), JSON.stringify({
      implementation: 'fixture host', asdf: 'fixture ASDF', systems: [{name: 'library', version: '1', files: [dependency]}]}));
    let revision = 'a'.repeat(40);
    const execute = async (_command, args, options) => {
      if (args[0] === 'ls-files') return {stdout: args.includes('--stage') ? `160000 ${revision} 0\tvendor/asdf\n` : tracked.join('\0') + '\0'};
      assert.equal(options?.cwd, undefined);
      assert.equal(args[0], '-C');
      if (args[2] === 'rev-parse') return {stdout: revision};
      if (args[2] === 'diff') return {stdout: ''};
      assert.equal(args[2], 'ls-files'); return {stdout: 'asdf.lisp\0'};
    };
    const key = () => runtimeInputKey({workspace, execute});
    const initial = await key();
    await fs.writeFile(path.join(workspace, 'lem/provider.lisp'), 'application edit');
    assert.equal(await key(), initial, 'Application-only source changes reuse the compiler');
    await fs.writeFile(path.join(workspace, 'scripts/repl.html'), 'page template edit');
    assert.equal(await key(), initial, 'Pages regenerate independently of compiler images');
    await fs.writeFile(path.join(workspace, 'compiler.lisp'), 'compiler edit');
    const compilerChanged = await key(); assert.notEqual(compilerChanged, initial);
    await fs.writeFile(dependency, 'changed host library');
    let previous = await key(); assert.notEqual(previous, compilerChanged, 'Host dependency bytes invalidate the distribution');
    for (const file of ['scripts/miricl.mjs', 'scripts/compiler-image-bootstrap.lisp',
      'vendor/asdf/asdf.lisp', 'libwasmtime.so']) {
      await fs.appendFile(path.join(workspace, file), '\nchanged input');
      const changed = await key(); assert.notEqual(changed, previous, file); previous = changed;
    }
    revision = 'b'.repeat(40);
    assert.notEqual(await key(), previous, 'The matching ASDF pin is part of the key');
    const wasm = Buffer.from([0,97,115,109,1,0,0,0]);
    for (const file of runtimeArtifacts) {
      const target = path.join(workspace, file); await fs.mkdir(path.dirname(target), {recursive: true});
      await fs.writeFile(target, file.endsWith('.wasm') ? wasm : 'support file');
    }
    const rows = await runtimeOutputRows(workspace);
    await fs.writeFile(path.join(workspace, 'build/runtime-images.json'), JSON.stringify({key: 'expected', rows}));
    await verifyRuntimeDistribution(workspace, 'expected');
    await assert.rejects(verifyRuntimeDistribution(workspace, 'other'), /inputs or output digests/);
    await fs.appendFile(path.join(workspace, 'miricl.wasm'), Buffer.from([0]));
    await assert.rejects(verifyRuntimeDistribution(workspace, 'expected'), /inputs or output digests/);
    await fs.writeFile(path.join(workspace, 'miricl.wasm'), wasm);
    await verifyRuntimeDistribution(workspace, 'expected');
    await fs.unlink(path.join(workspace, 'miricl-asdf.wasm'));
    await assert.rejects(verifyRuntimeDistribution(workspace, 'expected'), /ENOENT/);
    await fs.writeFile(path.join(workspace, 'miricl-asdf.wasm'), wasm);
    await verifyRuntimeDistribution(workspace, 'expected');
    const image = path.join(workspace, 'miricl-asdf.wasm');
    await fs.unlink(image); await fs.symlink(path.join(workspace, 'miricl.wasm'), image);
    await assert.rejects(verifyRuntimeDistribution(workspace, 'expected'), /size\/type/);
    await fs.unlink(image); await fs.writeFile(image, wasm);
    await verifyRuntimeDistribution(workspace, 'expected');
    const manifest = path.join(workspace, 'build/runtime-images.json'), valid = await fs.readFile(manifest);
    await fs.writeFile(manifest, Buffer.alloc(1024 * 1024 + 1));
    await assert.rejects(verifyRuntimeDistribution(workspace, 'expected'), /manifest size\/type/);
    await fs.writeFile(manifest, valid);
    await verifyRuntimeDistribution(workspace, 'expected');
    const runtime = path.join(workspace, 'runtime'), owned = path.join(workspace, 'runtime-owned');
    await fs.rename(runtime, owned); await fs.symlink(owned, runtime, 'dir');
    await assert.rejects(verifyRuntimeDistribution(workspace, 'expected'), /size\/type/);
    await fs.unlink(runtime); await fs.rename(owned, runtime);
    await verifyRuntimeDistribution(workspace, 'expected');
  } finally {await fs.rm(workspace, {recursive: true, force: true});}
});
