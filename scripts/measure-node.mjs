// Measure a Node CLI in-process, including its worker-thread CPU time.
import fs from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const [output, script, ...args] = process.argv.slice(2);
if (!output || !script) throw new Error('Expected OUTPUT.json SCRIPT [ARG ...]');
const started = performance.now(), cpu = process.cpuUsage();
setInterval(() => {
  const elapsed = process.cpuUsage(cpu);
  console.log(`Benchmark ${path.basename(script)}: ${Math.round((performance.now() - started) / 1000)} s elapsed, `
    + `${Math.round((elapsed.user + elapsed.system) / 1e6)} s CPU, `
    + `${Math.round(process.resourceUsage().maxRSS / 1024)} MiB peak RSS`);
}, 30000).unref();
process.on('exit', status => {
  const elapsed = process.cpuUsage(cpu);
  fs.writeFileSync(output, JSON.stringify({status, wallMs: performance.now() - started,
    cpuMs: (elapsed.user + elapsed.system) / 1000,
    maxRssKiB: process.resourceUsage().maxRSS}) + '\n');
});
process.once('SIGINT', () => process.exit(130));
process.once('SIGTERM', () => process.exit(143));
process.argv = [process.execPath, path.resolve(script), ...args];
await import(pathToFileURL(path.resolve(script)).href);
