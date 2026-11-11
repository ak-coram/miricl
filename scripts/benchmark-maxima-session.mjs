// Compare startup and warm calculations in fresh native Maxima sessions.
// node scripts/benchmark-maxima-session.mjs OUTPUT BEFORE_IMAGE BEFORE_CORE AFTER_IMAGE AFTER_CORE
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {createMaxima} from './maxima.mjs';

const [output, beforeImage, beforeCore, afterImage, afterCore] = process.argv.slice(2);
if (!output || !beforeImage || !beforeCore || !afterImage || !afterCore)
  throw new Error('Expected OUTPUT BEFORE_IMAGE BEFORE_CORE AFTER_IMAGE AFTER_CORE');
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const variants = [];
for (const [name, image, core] of [['before', beforeImage, beforeCore], ['after', afterImage, afterCore]]) {
  const imageBytes = await fs.readFile(image), coreBytes = await fs.readFile(core);
  variants.push({name, image, core, imageSha256: hash(imageBytes), coreSha256: hash(coreBytes),
    imageBytes, coreBytes, samples: []});
}
const cases = [
  ['factor', 'factor(x^12-1)', 25],
  ['ratsimp', 'ratsimp((x^20-1)/(x^2-1))', 100],
  ['integrate', 'integrate(x^7+3*x,x)', 25],
];
const expected = new Map();
const elapsed = (start, usage) => {
  const cpu = process.cpuUsage(usage);
  return {wallMs: performance.now() - start, cpuMs: (cpu.user + cpu.system) / 1000};
};
for (let round = 0; round < 3; round++) {
  for (const variant of round % 2 ? variants.toReversed() : variants) {
    const start = performance.now(), usage = process.cpuUsage();
    const session = await createMaxima(variant.imageBytes, variant.coreBytes);
    const sample = {round, startup: elapsed(start, usage), calculations: []};
    try {
      for (const [name, expression, repeats] of cases) {
        const warm = await session.evaluate(expression + ';');
        const value = JSON.stringify(warm.results.map(result => result.text));
        assert.ok(warm.results.length, `${name}: no calculation result`);
        if (expected.has(name)) assert.equal(value, expected.get(name), `${name}: result changed`);
        else expected.set(name, value);
        const began = performance.now(), cpu = process.cpuUsage();
        await session.evaluate(`for i:1 thru ${repeats} do ${expression}$`);
        sample.calculations.push({name, repeats, ...elapsed(began, cpu), resultSha256: hash(value)});
      }
    } finally {session.close();}
    variant.samples.push(sample);
    console.log(`${variant.name} round ${round + 1}: startup ${sample.startup.wallMs.toFixed(0)} ms`);
  }
}
const median = numbers => numbers.toSorted((a, b) => a - b)[1];
for (const variant of variants) {
  variant.medians = {startup: Object.fromEntries(['wallMs', 'cpuMs'].map(key =>
    [key, median(variant.samples.map(sample => sample.startup[key]))])),
  calculations: cases.map(([name]) => ({name, ...Object.fromEntries(['wallMs', 'cpuMs'].map(key =>
    [key, median(variant.samples.map(sample => sample.calculations.find(item => item.name === name)[key]))]))}))};
  delete variant.imageBytes; delete variant.coreBytes;
}
await fs.mkdir(path.dirname(output), {recursive: true});
await fs.writeFile(output, JSON.stringify({node: process.version, rounds: 3, variants}, null, 2) + '\n');
console.log(`Wrote ${output}`);
