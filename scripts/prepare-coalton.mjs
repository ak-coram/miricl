// Each dependency is a clean, pinned checkout inside this project's build tree.
// Existing checkouts are inspected only: never fetch, reset or switch them.
import fs from 'node:fs/promises';
import path from 'node:path';
import {execFileSync} from 'node:child_process';
const root = path.resolve(import.meta.dirname, '..');
const dependencies = JSON.parse(await fs.readFile(path.join(root, 'coalton/dependencies.json'), 'utf8'));
for (const {name, url, revision} of dependencies) {
  if (!/^[a-z][a-z-]*$/u.test(name) || !/^[a-f0-9]{40}$/u.test(revision)) throw new Error('Invalid dependency manifest.');
  const directory = path.join(root, 'build/coalton/deps', name);
  try {await fs.access(directory);} catch (error) {
    if (error.code !== 'ENOENT') throw error;
    await fs.mkdir(path.dirname(directory), {recursive: true});
    execFileSync('git', ['clone', '--revision', revision, url, directory], {stdio: 'inherit'});
  }
  const actual = execFileSync('git', ['-C', directory, 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
  if (actual !== revision) throw new Error(`${directory} is at ${actual}; expected ${revision}. Use a separate clean checkout.`);
  console.log(`${name}: ${actual}`);
}
