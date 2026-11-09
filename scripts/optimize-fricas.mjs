// Compact the complete dynamic FriCAS environment with MIRICL's optimizer.
import fs from 'node:fs/promises';
import path from 'node:path';
import {Miricl} from './miricl.mjs';
import {fricasFiles} from './fricas-files.mjs';

const [image = 'miricl-precompiled.wasm', input = 'build/fricas/fricas.wasm',
  output = 'build/fricas/fricas-optimized.wasm'] = process.argv.slice(2);
const original = await fs.readFile(input);
const vm = await Miricl.instantiate(await fs.readFile(image), {imports: {build: {
  progress(count) {if (count % 1000 === 0) console.log(`FriCAS: compiled ${count} initializers`);},
}}});
try {
  vm.mountFile('/fricas-input.wasm', original);
  const {files} = fricasFiles(path.resolve(process.argv[5] ?? 'build/fricas/native'));
  for (const [name, bytes] of files) vm.mountFile(name, bytes);
  // A REPL can call any FriCAS function and load packages using its macros.
  // Preserve that interface while pooling constants, compacting the graph,
  // and removing the optional compiler/source index.
  await vm.evaluateAsync(`(progn
    (miricl-compiler:shake-file "/fricas-input.wasm" "/fricas-output.wasm"
      :strip nil :report "/fricas-retention.txt")
    (load "/fricas/lisp/fricas-package.lisp")
    (load "/fricas-output.wasm")
    (js:define-import native-build-progress ("build" "progress") (:i32) :void)
    (miricl-compiler:compile-native-bundle "/fricas-output.wasm" "/fricas-output.wasm"
      :progress (lambda (file count) (declare (ignore file)) (native-build-progress count)))
    (values))`, {eof: true});
  const bytes = await vm.readFile('/fricas-output.wasm');
  if (!WebAssembly.validate(bytes))
    throw new Error('FriCAS optimization did not produce a valid native bundle.');
  await fs.mkdir(path.dirname(output), {recursive: true});
  await fs.writeFile(output, bytes);
  await fs.writeFile(path.join(path.dirname(output), 'retention.txt'), await vm.readFile('/fricas-retention.txt'));
  await fs.writeFile(path.join(path.dirname(output), 'optimization.json'), JSON.stringify({
    inputBytes: original.length, outputBytes: bytes.length,
    retainedInterface: 'complete dynamic FriCAS environment',
    optimizer: 'miricl-compiler:shake-file + compile-native-bundle', backend: 'native-wasm', strip: false,
  }, null, 2) + '\n');
  console.log(`Optimized FriCAS: ${original.length} → ${bytes.length} bytes (${output}).`);
} finally {vm.close();}
