// Compare already optimized application cores in fresh Chromium profiles.
// Build cores separately into ROOT/{before,after}/{maxima,fricas}/ first.
// Usage: node scripts/benchmark-cas-browser-startup.mjs BEFORE.wasm AFTER.wasm [ROOT]
import fs from 'node:fs';
import path from 'node:path';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {spawnSync} from 'node:child_process';
import {versionAssets} from './version-assets.mjs';

const [before, after, root = 'build/integer-tiers/browser-startup'] = process.argv.slice(2);
if (!before || !after) throw new Error('Expected BEFORE.wasm AFTER.wasm [ROOT]');
const hashFile = file => createHash('sha256').update(fs.readFileSync(file)).digest('hex');
const copy = (source, target) => fs.copyFileSync(source, target);
const report = {rounds: 3, node: process.version,
  scope: 'Optimized native cores; ready for input; fresh Chromium profile then cached reload; loopback HTTP; no first calculation',
  // Startup opens the databases and mounts this archive lazily. The algebra
  // binaries inside it are not executed; this is a startup-only fixture.
  filesystem: {file: 'build/fricas/site/filesystem.bin.gz',
    sha256: hashFile('build/fricas/site/filesystem.bin.gz')},
  variants: []};
for (const [name, image] of [['before', before], ['after', after]]) {
  const variant = {name, image, imageSha256: hashFile(image), applications: []};
  for (const app of ['maxima', 'fricas']) {
    const directory = path.join(root, name, app);
    const coreName = app === 'maxima' ? 'maxima.core' : 'fricas.wasm';
    const core = path.join(directory, coreName);
    assert.ok(fs.statSync(core).size > 0, `Missing optimized ${core}`);
    copy(image, path.join(directory, 'miricl.wasm'));
    const assets = ['miricl.wasm', coreName];
    for (const asset of ['index.html', 'worker.mjs', 'examples.mjs']) {
      copy(path.join(app, asset), path.join(directory, asset)); assets.push(asset);
    }
    for (const asset of ['app.mjs', 'session-store.mjs']) {
      copy(path.join('maxima', asset), path.join(directory, asset)); assets.push(asset);
    }
    for (const asset of ['miricl.mjs', `${app}.mjs`, ...(app === 'fricas'
      ? ['filesystem-archive.mjs', 'mounted-filesystem.mjs'] : [])]) {
      copy(path.join('scripts', asset), path.join(directory, asset)); assets.push(asset);
    }
    if (app === 'fricas') {
      copy(report.filesystem.file, path.join(directory, 'filesystem.bin.gz'));
      assets.push('filesystem.bin.gz');
    }
    await versionAssets(directory, assets);
    variant.applications.push({app, directory, coreSha256: hashFile(core), coreBytes: fs.statSync(core).size, runs: []});
  }
  report.variants.push(variant);
}
for (let round = 0; round < report.rounds; round++) {
  for (const variant of round % 2 ? report.variants.toReversed() : report.variants) {
    for (const application of variant.applications) {
      console.log(`Round ${round + 1}: ${variant.name} ${application.app}`);
      const result = spawnSync(process.execPath,
        ['tests/browser-startup-benchmark.mjs', application.directory, '1'],
        {encoding: 'utf8', timeout: 420000, maxBuffer: 16 * 1024 * 1024});
      assert.ifError(result.error);
      assert.equal(result.status, 0, result.stderr + result.stdout);
      const measured = JSON.parse(result.stdout);
      for (const cache of ['cold', 'warm']) assert.ok(measured.runs[0][cache].nativeModules > 0);
      application.runs.push({round, ...measured});
      console.log(`Cold ${measured.summary.cold.pageMs.toFixed(1)} ms; cached ${measured.summary.warm.pageMs.toFixed(1)} ms`);
    }
  }
}
const median = values => values.toSorted((a, b) => a - b)[1];
for (const variant of report.variants) for (const application of variant.applications)
  application.medians = Object.fromEntries(['cold', 'warm'].map(cache => [cache,
    Object.fromEntries(Object.keys(application.runs[0].summary[cache]).map(field =>
      [field, median(application.runs.map(run => run.summary[cache][field]))]))]));
const output = path.join(root, 'startup.json');
fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
console.log(`Wrote ${output}`);
