// Setup follows the discovered dependency graph; build must not require extras.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {execFileSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';

const root = fileURLToPath(new URL('../', import.meta.url));
const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "miricl application ' "));
try {
  const app = path.join(temporary, 'app');
  const output = path.join(temporary, 'distribution');
  const dependency = path.join(temporary, 'fset sources');
  await Promise.all([fs.mkdir(app), fs.mkdir(path.join(output, 'runtime'), {recursive: true}), fs.mkdir(dependency)]);
  for (const name of ['miricl-cold.wasm', 'miricl.wasm', 'miricl-repl.wasm', 'miricl-asdf.wasm'])
    await fs.writeFile(path.join(output, name), new Uint8Array());
  await fs.copyFile(path.join(root, 'game-of-life/justfile'), path.join(app, 'justfile'));
  execFileSync(process.execPath, [path.join(root, 'scripts/setup-application.mjs'),
    app, 'game-of-life', root, output, 'fset', dependency]);
  assert.deepEqual(await fs.readdir(path.join(app, 'quicklisp')), ['fset']);
  const registry = await fs.readFile(path.join(app, 'systems.lisp'), 'utf8');
  assert.match(registry, /\/quicklisp\/fset\//);
  assert.doesNotMatch(registry, /alexandria/);
  execFileSync('just', ['--justfile', path.join(app, 'justfile'), '--dry-run', 'build'], {stdio: 'pipe'});

  const executable = path.join(temporary, 'fake wasmtime');
  const record = path.join(temporary, 'arguments.json');
  await fs.writeFile(executable, `#!/usr/bin/env node\nrequire('node:fs').writeFileSync(process.env.MIRICL_TEST_ARGUMENTS, JSON.stringify(process.argv.slice(2)));\n`, {mode: 0o755});
  execFileSync('just', ['--justfile', path.join(app, 'justfile'), `wasmtime=${executable}`,
    'dev', '18080', '14060', '--repl-bridge', '--directory=app sources', '--literal=$(exit 1)'],
    {env: {...process.env, MIRICL_TEST_ARGUMENTS: record}, stdio: 'pipe'});
  const args = JSON.parse(await fs.readFile(record, 'utf8'));
  const preopens = args.flatMap((arg, i) => arg === '--dir' ? [args[i + 1]] : []);
  assert.deepEqual(preopens, ['.::.', `${await fs.realpath(path.join(output, 'runtime'))}::runtime`,
    `${await fs.realpath(output)}::images`, `${await fs.realpath(dependency)}::quicklisp/fset`]);
  assert.ok(args.includes('--http-port=18080') && args.includes('--websocket-port=14060'));
  assert.deepEqual(args.slice(-3), ['--repl-bridge', '--directory=app sources', '--literal=$(exit 1)']);
  console.log('Application setup: discovered dependencies, production build without optional libraries, and WASI preopens with quoted paths pass.');
} finally {
  await fs.rm(temporary, {recursive: true, force: true});
}
