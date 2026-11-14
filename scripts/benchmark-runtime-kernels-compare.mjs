// node scripts/benchmark-runtime-kernels-compare.mjs wasm.json sbcl.json
import fs from 'node:fs';
const [wasmPath, sbclPath] = process.argv.slice(2);
if (!wasmPath || !sbclPath) throw new Error('Expected Wasm and SBCL result JSON paths');
const wasm = JSON.parse(fs.readFileSync(wasmPath, 'utf8'));
const sbcl = JSON.parse(fs.readFileSync(sbclPath, 'utf8'));
const reference = new Map(sbcl.results.map(row => [row.name, row]));
const rows = wasm.results.map(row => {
  const host = reference.get(row.name);
  if (!host) throw new Error(`Missing SBCL benchmark for ${row.name}`);
  const wasmNs = row.milliseconds * 1e6 / row.count;
  const sbclNs = host.milliseconds * 1e6 / host.count;
  const ratio = sbclNs > 0 ? wasmNs / sbclNs : null;
  return {name: row.name, entry: row.entry, wasmNs, sbclNs, ratio,
    exceedsFiveTimes: ratio === null ? null : ratio > 5,
    clockResolutionWarning: host.milliseconds < 50 * (sbcl.clockResolutionMicroseconds ?? 1000) / 1000};
}).sort((a, b) => (b.ratio ?? -1) - (a.ratio ?? -1));
console.log(JSON.stringify({schemaVersion: 1, wasmPath, sbclPath, results: rows}, null, 2));
