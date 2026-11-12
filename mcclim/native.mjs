// Compile the portable McCLIM library with miricl's ready compiler.
// node mcclim/native.mjs [portable.wasm] [native.wasm] [runtime.wasm]
import fs from 'node:fs/promises';
import path from 'node:path';
import {Miricl} from '../scripts/miricl.mjs';
import {mcclimFiles} from './files.mjs';
const [input = 'mcclim/build/mcclim-demo-library.wasm',
  output = 'mcclim/build/mcclim-demo-native.wasm', runtime = 'miricl-precompiled.wasm'] = process.argv.slice(2);
let count = 0, file = '', started = performance.now();
const vm = await Miricl.instantiate(await fs.readFile(runtime), {
  onOutput: text => process.stdout.write(text),
  imports: {mcclimBuild: {progress(name, compiled) {file = name; count = compiled;}}},
});
const heartbeat = setInterval(() =>
  console.log(`McCLIM native: ${count} initializers, ${file} (${Math.round((performance.now()-started)/1000)} s)`), 30000);
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  for (const [name, bytes] of await mcclimFiles())
    if (name.startsWith('/mcclim/Auxiliary/data/') || name.startsWith('/mcclim/Examples/'))
      vm.mountFile(name, bytes);
  vm.mountFile('/mcclim-input.wasm', await fs.readFile(input));
  await vm.evaluateAsync(`
    (js:define-import mcclim-native-progress ("mcclimBuild" "progress") (:string :i32) :void)
    (miricl-compiler:compile-native-bundle "/mcclim-input.wasm" "/mcclim-native.wasm"
      :load t :progress (lambda (file count)
        (mcclim-native-progress (if file (princ-to-string file) "<library>") count)))
    (values)`, {eof:true});
  const bytes = await vm.readFile('/mcclim-native.wasm');
  new WebAssembly.Module(bytes);
  await fs.mkdir(path.dirname(output), {recursive:true});
  await fs.writeFile(output, bytes);
  console.log(`Wrote native McCLIM library ${output} (${bytes.length} bytes).`);
} catch (error) {
  console.error(`McCLIM native failed at initializer ${count}: ${file}`);
  console.error(error.message, error.output ?? '', error.backtrace ?? '');
  process.exitCode = 1;
} finally {clearInterval(heartbeat); vm.close();}
