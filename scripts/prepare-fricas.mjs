// Use FriCAS's supported native bootstrap to generate portable Lisp and databases.
// Only the generated Lisp is subsequently compiled/evaluated by MIRICL.
import fs from 'node:fs';
import path from 'node:path';
import {spawnSync} from 'node:child_process';
import {fricasRevision} from './fricas-files.mjs';
const root = path.resolve(import.meta.dirname, '..');
const output = path.resolve(process.argv[2] ?? 'build/fricas/native');
const stamp = path.join(output, 'miricl-bootstrap.json');
const revision = fricasRevision();
if (fs.existsSync(stamp) && JSON.parse(fs.readFileSync(stamp)).revision === revision) {
  console.log(`Using FriCAS bootstrap ${revision} in ${output}`);
} else {
  fs.mkdirSync(output, {recursive: true});
  const run = (command, args) => {
    const result = spawnSync(command, args, {cwd: output, stdio: 'inherit'});
    if (result.error) throw result.error;
    if (result.status !== 0) throw new Error(`${command} exited ${result.status}`);
  };
  run(path.join(root, 'vendor/fricas/configure'), ['--with-lisp=' + (process.env.SBCL ?? 'sbcl') + ' --dynamic-space-size 2048',
    '--without-x', '--disable-gmp', '--disable-aldor', '--prefix=' + path.join(output, 'install')]);
  run('make', ['-j' + (process.env.MIRICL_BUILD_JOBS ?? '2')]);
  // The browser has no native HyperDoc/X front end. Its four algebra databases
  // are required; the X-only databases do not exist in a --without-x build.
  run('make', ['dist-lisp', 'dbs=browse.daase category.daase interp.daase operation.daase']);
  fs.writeFileSync(stamp, JSON.stringify({revision, generatedBy: 'upstream configure, make, dist-lisp'}, null, 2) + '\n');
}
