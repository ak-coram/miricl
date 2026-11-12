// Prune metadata and compile the complete dynamic Maxima environment ahead of time.
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {Miricl} from './miricl.mjs';
import {measureWasm} from './wasm-size.mjs';

const [image = 'miricl-precompiled.wasm', input = 'build/maxima/maxima.wasm',
  output = 'build/maxima/maxima-optimized.wasm'] = process.argv.slice(2);
const original = await fs.readFile(input);
const compilerImage = await fs.readFile(image);
const stages = [];
let initializers = 0;
const vm = await Miricl.instantiate(compilerImage, {imports: {build: {
  progress(count) {initializers = count; if (count % 1000 === 0) console.log(`Maxima: compiled ${count} initializers`);},
}}});
async function phase(name, source) {
  const cpu = process.cpuUsage();
  const start = performance.now();
  await vm.evaluateAsync(`(progn ${source} (values))`, {eof: true});
  const milliseconds = Math.round(performance.now() - start);
  const elapsed = process.cpuUsage(cpu);
  stages.push({name, milliseconds, cpuMilliseconds: (elapsed.user + elapsed.system) / 1000});
  console.log(`Maxima: ${name} completed in ${(milliseconds / 1000).toFixed(1)} s`);
}
try {
  vm.mountFile('/maxima-input.wasm', original);
  // A REPL can call any Maxima function and load packages using its macros.
  // Preserve that interface while pooling constants, compacting the graph,
  // and removing the optional compiler/source index.
  await phase('compact portable input', `
    (miricl-compiler:shake-file "/maxima-input.wasm" "/maxima-output.wasm"
      :strip nil :report "/maxima-retention.txt")`);
  await phase('load build environment', `
    (setq miricl:*trap-floating-point-underflow* nil)
    (load "/maxima-output.wasm")
    (js:define-import native-build-progress ("build" "progress") (:i32) :void)`);
  await phase('compile native code', `
    (miricl-compiler:compile-native-bundle "/maxima-output.wasm" "/maxima-output.wasm"
      :progress (lambda (file count) (declare (ignore file)) (native-build-progress count)))`);
  const bytes = await vm.readFile('/maxima-output.wasm');
  if (!WebAssembly.validate(bytes))
    throw new Error('Maxima optimization did not produce a valid native bundle.');
  await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(output, bytes);
  await fs.writeFile(path.join(path.dirname(output), 'retention.txt'), await vm.readFile('/maxima-retention.txt'));
  await fs.writeFile(path.join(path.dirname(output), 'optimization.json'), JSON.stringify({
    inputBytes: original.length, outputBytes: bytes.length, stages, initializers,
    inputSha256: createHash('sha256').update(original).digest('hex'),
    imageSha256: createHash('sha256').update(compilerImage).digest('hex'),
    outputSha256: createHash('sha256').update(bytes).digest('hex'),
    image: measureWasm(image), output: measureWasm(output), node: process.version,
    retainedInterface: 'complete dynamic Maxima environment',
    optimizer: 'miricl-compiler:shake-file + compile-native-bundle', backend: 'native-wasm', strip: false,
  }, null, 2) + '\n');
  console.log(`Optimized Maxima: ${original.length} → ${bytes.length} bytes (${output}).`);
} finally {vm.close();}
