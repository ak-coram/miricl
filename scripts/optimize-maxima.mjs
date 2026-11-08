// Compact the complete dynamic Maxima environment with MIRICL's optimizer.
import fs from 'node:fs/promises';
import path from 'node:path';
import {Miricl} from './miricl.mjs';

const [image = 'miricl.wasm', input = 'build/maxima/maxima.wasm',
  output = 'build/maxima/maxima-optimized.wasm'] = process.argv.slice(2);
const original = await fs.readFile(input);
const vm = await Miricl.instantiate(await fs.readFile(image));
try {
  vm.mountFile('/maxima-input.wasm', original);
  // A REPL can call any Maxima function and load packages using its macros.
  // Preserve that interface while pooling constants, compacting the graph,
  // and removing the optional compiler/source index.
  await vm.evaluateAsync(`(progn
    (miricl-compiler:shake-file "/maxima-input.wasm" "/maxima-output.wasm"
      :strip nil :report "/maxima-retention.txt") (values))`, {eof: true});
  const bytes = await vm.readFile('/maxima-output.wasm');
  if (!WebAssembly.validate(bytes) || bytes.length > original.length)
    throw new Error('Maxima optimization did not produce a valid, smaller bundle.');
  await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(output, bytes);
  await fs.writeFile(path.join(path.dirname(output), 'retention.txt'), await vm.readFile('/maxima-retention.txt'));
  await fs.writeFile(path.join(path.dirname(output), 'optimization.json'), JSON.stringify({
    inputBytes: original.length, outputBytes: bytes.length,
    retainedInterface: 'complete dynamic Maxima environment',
    optimizer: 'miricl-compiler:shake-file', strip: false,
  }, null, 2) + '\n');
  console.log(`Optimized Maxima: ${original.length} → ${bytes.length} bytes (${output}).`);
} finally {vm.close();}
