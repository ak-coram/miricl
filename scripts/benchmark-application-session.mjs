// Fresh FriCAS startup and documented calculations, including lazy algebra loads.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createFriCAS} from './fricas.mjs';
import {fricasFiles} from './fricas-files.mjs';
import {examples} from '../fricas/examples.mjs';

const [image, core, output] = process.argv.slice(2);
if (!image || !core || !output) throw new Error('Expected IMAGE CORE OUTPUT.json');
const {files} = fricasFiles(path.resolve('build/fricas/native'));
const report = {passed: false, executionIncludesLazyAlgebraCompilation: true, calculations: []};
const started = performance.now(), cpu = process.cpuUsage();
let session;
try {
  session = await createFriCAS(await fs.readFile(image), await fs.readFile(core), files,
    {progress: message => console.log(`FriCAS: ${message}`)});
  const startupCpu = process.cpuUsage(cpu);
  report.startup = {wallMs: performance.now() - started, cpuMs: (startupCpu.user + startupCpu.system) / 1000};
  for (const example of examples.slice(0, 3)) {
    const before = performance.now(), usage = process.cpuUsage();
    const result = await session.evaluate(example.code);
    const elapsed = process.cpuUsage(usage);
    assert.ok(result.output.includes(example.expected), `${example.id}: ${result.output}`);
    for (const text of example.checks ?? []) assert.ok(result.output.includes(text));
    assert.doesNotMatch(result.output, /System error|Internal Error|not implemented|cannot find|not available/iu);
    report.calculations.push({id: example.id, wallMs: performance.now() - before,
      cpuMs: (elapsed.user + elapsed.system) / 1000, output: result.output});
    console.log(`PASS FriCAS: ${example.title}`);
  }
  report.passed = true;
} finally {
  session?.close(); await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
}
