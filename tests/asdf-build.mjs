// Exercise the build pipeline with ordered patches and a conflicting update.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import assert from 'node:assert/strict';
import {execFileSync, spawnSync} from 'node:child_process';
import {buildAsdf} from '../scripts/build-asdf.mjs';

const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'miricl-asdf-build-'));
try {
  const upstream = path.join(fixture, 'vendor/asdf');
  const patches = path.join(fixture, 'patches/asdf');
  const script = path.join(fixture, 'scripts/build-asdf.mjs');
  for (const directory of [upstream, patches, path.dirname(script)])
    fs.mkdirSync(directory, {recursive: true});
  fs.copyFileSync(new URL('../scripts/build-asdf.mjs', import.meta.url), script);
  const git = (...args) => execFileSync('git', ['-C', upstream, ...args], {encoding: 'utf8'});
  const original = '(defparameter *port* :upstream)\n';
  fs.writeFileSync(path.join(upstream, 'port.lisp'), original);
  fs.writeFileSync(path.join(upstream, 'make-asdf.sh'),
    'mkdir -p build\ncat port.lisp > build/asdf.lisp\n');
  git('init', '--quiet');
  git('add', '.');
  git('-c', 'user.name=Build Test', '-c', 'user.email=build@example.invalid',
    '-c', 'commit.gpgsign=false', 'commit', '--quiet', '-m', 'Upstream fixture');
  const patch = (before, after) => `--- a/port.lisp\n+++ b/port.lisp\n@@ -1 +1 @@\n-(defparameter *port* :${before})\n+(defparameter *port* :${after})\n`;
  fs.writeFileSync(path.join(patches, 'series'), '# Apply in this order.\nfirst.patch\nsecond.patch\n');
  fs.writeFileSync(path.join(patches, 'first.patch'), patch('upstream', 'miricl'));
  fs.writeFileSync(path.join(patches, 'second.patch'), patch('miricl', 'patched'));
  const output = path.join(fixture, 'output');
  const run = (destination = output) => spawnSync(process.execPath, [script, destination], {encoding: 'utf8'});
  const successful = () => {
    const result = run();
    assert.equal(result.status, 0, result.stderr);
    return fs.readFileSync(path.join(output, 'asdf.lisp'), 'utf8');
  };
  const artifact = successful();
  assert.equal(artifact, '(defparameter *port* :patched)\n');
  assert.deepEqual(JSON.parse(fs.readFileSync(path.join(output, 'source/miricl-build.json'))),
    {revision: git('rev-parse', 'HEAD').trim(), patches: ['first.patch', 'second.patch']});
  fs.writeFileSync(path.join(output, 'source/stale'), 'previous build');
  assert.equal(successful(), artifact);
  assert.equal(fs.existsSync(path.join(output, 'source/stale')), false);

  // A failed patch preserves the previous artifact and removes its working copy.
  fs.writeFileSync(path.join(patches, 'second.patch'), patch('conflict', 'bad'));
  const failed = run();
  assert.notEqual(failed.status, 0);
  assert.match(failed.stderr, /patch failed|does not apply/);
  assert.equal(fs.readFileSync(path.join(output, 'asdf.lisp'), 'utf8'), artifact);
  assert.deepEqual(fs.readdirSync(output).sort(), ['asdf.lisp', 'source']);
  assert.equal(fs.readFileSync(path.join(upstream, 'port.lisp'), 'utf8'), original);
  assert.equal(git('status', '--porcelain'), '');

  const invalid = path.join(upstream, 'output');
  assert.notEqual(run(invalid).status, 0);
  assert.equal(fs.existsSync(invalid), false);
  fs.writeFileSync(path.join(upstream, 'port.lisp'), 'local edit\n');
  assert.notEqual(run().status, 0, 'Local upstream edits must stop the build.');
  assert.equal(fs.readFileSync(path.join(output, 'asdf.lisp'), 'utf8'), artifact);
  // Apply the shipped series to the actual pinned upstream, too: synthetic
  // patches alone cannot catch conflicts introduced by a submodule update.
  const prepared = buildAsdf(path.join(fixture, 'pinned'));
  const source = fs.readFileSync(prepared, 'utf8');
  const version = JSON.parse(fs.readFileSync(new URL('../vendor/asdf/version.lisp-expr', import.meta.url), 'utf8'));
  assert.ok(source.includes(`;;; This is ASDF ${version}:`));
  console.log('ASDF preparation: ordered patches, repeatable output, clean sources, and failure recovery pass.');
} finally {
  fs.rmSync(fixture, {recursive: true, force: true});
}
