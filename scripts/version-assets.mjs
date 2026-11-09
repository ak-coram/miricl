// Keep a published application's runtime, native bundle and adapter in sync.
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';

export async function versionAssets(directory, filenames) {
  const files = await Promise.all([...filenames].sort().map(async name => [name, await fs.readFile(path.join(directory, name))]));
  const hash = createHash('sha256');
  for (const [name, bytes] of files) hash.update(name).update('\0').update(bytes);
  const version = hash.digest('hex').slice(0, 16);
  const names = filenames.filter(name => /\.(?:m?js|wasm|core|gz)$/.test(name)).map(name => name.replaceAll('.', '\\.'));
  const references = new RegExp(`(["'])((?:\\./)?(?:${names.join('|')}))\\1`, 'g');
  await Promise.all(files.map(async ([name, bytes]) => {
    // The adapter also serves as a Node CLI; its default disk paths stay literal.
    if (!/\.(?:html|mjs)$/.test(name) || /^(?:miricl\.mjs)$/.test(name)) return;
    const source = bytes.toString('utf8');
    const next = source.replace(references, (_, quote, filename) => `${quote}${filename}?v=${version}${quote}`);
    if (next !== source) await fs.writeFile(path.join(directory, name), next);
  }));
  return version;
}

if (process.argv[1] && await fs.realpath(process.argv[1]) === import.meta.filename) {
  const [directory, ...filenames] = process.argv.slice(2);
  if (!directory || !filenames.length) throw new Error('Usage: version-assets.mjs DIRECTORY FILE...');
  console.log(`Versioned application assets: ${await versionAssets(directory, filenames)}`);
}
