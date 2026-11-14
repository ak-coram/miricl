// Compile portable algebra sources during packaging, rather than on first use.
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {fricasFiles} from './fricas-files.mjs';
import {createFriCAS, lispString} from './fricas.mjs';

export async function prepareFriCASLibraries(image, core, native, output) {
  const {files} = fricasFiles(path.resolve(native));
  const sources = files.filter(([name]) => name.startsWith('/fricas/algebra/') && name.endsWith('.lsp'));
  const hash = createHash('sha256').update('miricl-fricas-native-fasl-v1\0').update(image).update(core);
  for (const [name, bytes] of sources) hash.update(name).update(bytes);
  const fingerprint = hash.digest('hex');
  const cache = path.join(output, fingerprint);
  await fs.mkdir(cache, {recursive: true});
  const manifestPath = path.join(output, 'manifest.json');
  const compiled = [];
  let session;
  try {
    for (const [index, [source]] of sources.entries()) {
      const name = path.basename(source, '.lsp') + '.wasm';
      const target = '/fricas/algebra/' + name;
      let bytes = await fs.readFile(path.join(cache, name)).catch(() => null);
      if (!bytes) {
        if (!session) {
          session = await createFriCAS(image, core, files);
          await session.vm.evaluateAsync('(miricl-compiler::ensure-compiler) (values)', {eof: true});
        }
        if (index % 25 === 0) console.log(`FriCAS algebra: ${index + 1}/${sources.length} (${name})`);
        try {
          await session.vm.evaluateAsync(`
            (compile-file ${lispString(source)} :output-file ${lispString(target)})
            (miricl-compiler:shake-file ${lispString(target)} ${lispString(target)} :strip nil)
            (load ${lispString(target)})
            (miricl-compiler:compile-native-bundle ${lispString(target)} ${lispString(target)})
            (values)`, {eof: true});
        } catch (error) {error.message = `${source}: ${error.message}`; throw error;}

        bytes = await session.vm.readFile(target);
        await fs.writeFile(path.join(cache, name + '.tmp'), bytes);
        await fs.rename(path.join(cache, name + '.tmp'), path.join(cache, name));
      }
      compiled.push([target, bytes]);
    }
    await fs.writeFile(manifestPath, JSON.stringify({fingerprint, files: compiled.length}, null, 2) + '\n');
    return compiled;
  } finally {session?.close();}
}

if (process.argv[1] && path.resolve(process.argv[1]) === import.meta.filename) {
  const [image = 'miricl.wasm', core = 'build/fricas/fricas-optimized.wasm',
    native = 'build/fricas/native', output = 'build/fricas/algebra'] = process.argv.slice(2);
  const files = await prepareFriCASLibraries(await fs.readFile(image), await fs.readFile(core), native, output);
  console.log(`Prepared ${files.length} FriCAS algebra libraries in ${output}.`);
}
