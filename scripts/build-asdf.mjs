// Prepare the optional guest library without modifying the upstream submodule.
// node scripts/build-asdf.mjs [output-directory]
import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {execFileSync} from 'node:child_process';

const root = fileURLToPath(new URL('../', import.meta.url));
const upstream = path.join(root, 'vendor/asdf');
const patches = path.join(root, 'patches/asdf');

export function buildAsdf(output = path.join(root, 'build/asdf')) {
  if (!fs.existsSync(path.join(upstream, '.git')))
    throw new Error('ASDF submodule is not initialized; run just asdf-init.');
  output = path.resolve(output);
  // Never allow a destination inside the submodule, even through a symlink.
  const checkOutput = directory => {
    if (directory === upstream || directory.startsWith(upstream + path.sep))
      throw new Error('ASDF output must be outside vendor/asdf.');
  };
  checkOutput(output);
  let parent = output;
  while (!fs.existsSync(parent)) parent = path.dirname(parent);
  checkOutput(fs.realpathSync(parent));
  fs.mkdirSync(output, {recursive: true});
  output = fs.realpathSync(output);
  checkOutput(output);
  const git = (...args) => execFileSync('git', ['-C', upstream, ...args], {encoding: 'utf8'});
  const revision = git('rev-parse', 'HEAD').trim();
  // Refuse local edits rather than silently ignoring them when copying sources.
  git('diff', '--quiet', 'HEAD', '--');
  const series = fs.readFileSync(path.join(patches, 'series'), 'utf8')
    .split(/\r?\n/).map(line => line.trim()).filter(line => line && !line.startsWith('#'));
  for (const patch of series) {
    if (path.basename(patch) !== patch || !patch.endsWith('.patch'))
      throw new Error(`Invalid ASDF patch name: ${patch}`);
  }
  const work = fs.mkdtempSync(path.join(output, '.source-'));
  try {
    for (const name of git('ls-files', '-z').split('\0').filter(Boolean)) {
      const source = path.join(upstream, name);
      const stat = fs.lstatSync(source, {throwIfNoEntry: false});
      // ASDF's own optional development submodules are not needed to assemble it.
      if (!stat || stat.isDirectory()) continue;
      const destination = path.join(work, name);
      fs.mkdirSync(path.dirname(destination), {recursive: true});
      if (stat.isSymbolicLink()) fs.symlinkSync(fs.readlinkSync(source), destination);
      else { fs.copyFileSync(source, destination); fs.chmodSync(destination, stat.mode); }
    }
    for (const patch of series) {
      execFileSync('git', ['apply', '--whitespace=error-all', path.join(patches, patch)],
        {cwd: work, stdio: 'inherit'});
    }
    execFileSync('sh', ['make-asdf.sh', 'build_asdf'], {cwd: work, stdio: 'inherit'});
    fs.writeFileSync(path.join(work, 'miricl-build.json'),
      JSON.stringify({revision, patches: series}, null, 2) + '\n');
    // Publish only a complete, successfully patched source tree and library.
    const source = path.join(output, 'source');
    fs.rmSync(source, {recursive: true, force: true});
    fs.renameSync(work, source);
    const temporary = path.join(output, 'asdf.lisp.tmp');
    fs.copyFileSync(path.join(source, 'build/asdf.lisp'), temporary);
    fs.renameSync(temporary, path.join(output, 'asdf.lisp'));
    return path.join(output, 'asdf.lisp');
  } finally {
    fs.rmSync(work, {recursive: true, force: true});
  }
}

export async function compileAsdf(source, image, output) {
  const {Miricl} = await import('./miricl.mjs');
  const lisp = await Miricl.instantiate(fs.readFileSync(image));
  try {
    lisp.mountFile('/asdf.lisp', fs.readFileSync(source, 'utf8'));
    await lisp.evaluateAsync('(compile-file "/asdf.lisp" :output-file "/miricl-asdf.wasm")', {eof: true});
    const bytes = await lisp.readFile('/miricl-asdf.wasm');
    const filename = path.resolve(output);
    fs.mkdirSync(path.dirname(filename), {recursive: true});
    fs.writeFileSync(filename + '.tmp', bytes);
    fs.renameSync(filename + '.tmp', filename);
    console.log(`Wrote ${filename} (${bytes.length} bytes)`);
    return filename;
  } finally { lisp.close(); }
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  try {
    const source = buildAsdf(process.argv[2]);
    console.log(`Wrote ${source}`);
    if (process.argv[3]) await compileAsdf(source, process.argv[3], process.argv[4] ?? 'miricl-asdf.wasm');
  } catch (error) {
    if (error.output) console.error(error.output);
    console.error(`ASDF build failed: ${error.message}`);
    if (error.backtrace) console.error(error.backtrace);
    process.exitCode = 1;
  }
}
