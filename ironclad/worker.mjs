import {loadMiricl} from './miricl-loader.mjs';

let vm;
let cases;
let busy = false;
const dataGroups = new Set(['ciphers', 'digests', 'macs', 'modes']);

async function fetchAsset(name) {
  const response = await fetch(new URL(name, import.meta.url));
  if (!response.ok) throw new Error(`Cannot load ${name} (HTTP ${response.status}).`);
  return response;
}

async function evaluate(source) {
  const result = await vm.evaluateAsync(source, {eof: true});
  return result.output;
}

function entropy(size) {
  if (!Number.isInteger(size) || size < 0 || size > 65536)
    throw new RangeError('Invalid benchmark entropy request.');
  return Array.from(crypto.getRandomValues(new Uint8Array(size)), byte => byte.toString(16).padStart(2, '0')).join('');
}

async function initialize() {
  const [runtime, library, runner, upstream, build] = await Promise.all([
    fetchAsset('miricl.wasm.gz'), fetchAsset('ironclad.wasm.gz'),
    fetchAsset('runner.lisp'), fetchAsset('upstream-benchmarks.lisp'),
    fetchAsset('build.json').then(response => response.json()),
  ]);
  if (build.backend !== 'native-wasm') throw new Error('Expected a native Wasm Ironclad build.');
  vm = await loadMiricl(runtime, {imports: {benchmark: {entropy}}});
  const body = library.body.pipeThrough(new DecompressionStream('gzip'));
  vm.mountFile('/ironclad.wasm', new Uint8Array(await new Response(body).arrayBuffer()));
  vm.mountFile('/runner.lisp', new Uint8Array(await runner.arrayBuffer()));
  vm.mountFile('/upstream-benchmarks.lisp', new Uint8Array(await upstream.arrayBuffer()));
  await evaluate('(load "/ironclad.wasm") (values)');
  await evaluate('(setq miricl-compiler:*compile-evaluations* t) (load "/runner.lisp") (ironclad-benchmark-check)');
  const listing = await evaluate('(ironclad-benchmark-load)');
  cases = Array.from(listing.matchAll(/^IB_CASE BENCHMARK-([A-Z-]+) ([A-Z0-9/-]+)$/gmu),
    ([, group, name]) => ({group: group.toLowerCase(), name: name.toLowerCase()}));
  if (!cases.length) throw new Error('Ironclad provided no benchmark cases.');
  postMessage({type: 'ready', cases, revision: build.revision, backend: build.backend,
    imageSha256: build.imageSha256, coreSha256: build.coreSha256});
}

async function run({selection, blocks, iterations, samples}) {
  if (!Array.isArray(selection) || !selection.length || !Number.isInteger(blocks) || blocks < 1 || blocks > 32 ||
      !Number.isInteger(iterations) || iterations < 1 || iterations > 100 ||
      !Number.isInteger(samples) || samples < 1 || samples > 5) throw new Error('Invalid benchmark configuration.');
  for (const {group, name} of selection) {
    if (!cases.some(item => item.group === group && item.name === name)) throw new Error('Unknown benchmark case.');
    const dataSize = blocks * 32768;
    postMessage({type: 'case-start', group, name});
    try {
      const timings = [];
      for (let i = 0; i <= samples; i++) {
        const output = await evaluate(`(ironclad-benchmark-run 'benchmark-${group} :${name} ${dataSize} ${iterations})`);
        const match = /^IB_RESULT (\d+) (\d+)$/mu.exec(output);
        const ticks = Number(match?.[1]);
        const units = Number(match?.[2]);
        if (![ticks, units].every(value => Number.isSafeInteger(value) && value > 0))
          throw new Error(output.trim() || 'Invalid benchmark result.');
        if (i > 0) timings.push({ticks, units});
      }
      const byteWorkload = dataGroups.has(group);
      const work = byteWorkload ? dataSize : iterations;
      const speeds = timings.map(({ticks, units}) => work * units / ticks);
      postMessage({type: 'result', group, name, timings, speeds, unit: byteWorkload ? 'bytes/s' : 'ops/s', work});
    } catch (error) {
      postMessage({type: 'case-error', group, name, message: error.message || String(error)});
    }
  }
  postMessage({type: 'done'});
}

self.onmessage = async ({data}) => {
  if (busy) return;
  busy = true;
  try {
    if (data.type === 'load' && !vm) await initialize();
    else if (data.type === 'run' && cases) await run(data);
  } catch (error) {
    postMessage({type: 'error', message: error.message || String(error)});
  } finally {
    busy = false;
  }
};
