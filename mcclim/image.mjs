// Build the reusable library image in a fresh runtime, without build-time ASDF.
// node mcclim/image.mjs [library.wasm] [image.wasm] [runtime.wasm]
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {Miricl} from '../scripts/miricl.mjs';
import {mcclimFiles} from './files.mjs';
const [library = 'mcclim/build/mcclim-demo-native.wasm',
  output = 'mcclim/build/miricl-mcclim.wasm', runtime = 'miricl-precompiled.wasm'] = process.argv.slice(2);
const vm = await Miricl.instantiate(await fs.readFile(runtime),
  {onOutput: text => process.stdout.write(text)});
try {
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* nil) (values)');
  // Icons, example images and source-viewer fixtures are Lisp filesystem
  // resources. Keep them with the image, without retaining build-time ASDF.
  for (const [name, bytes] of await mcclimFiles())
    if (name.startsWith('/mcclim/Auxiliary/data/') ||
        name.startsWith('/mcclim/Examples/images/') ||
        (name.startsWith('/mcclim/Examples/') && name.endsWith('.lisp')))
      vm.mountFile(name, bytes);
  vm.mountFile('/mcclim-library.wasm', await fs.readFile(library));
  await vm.evaluateAsync('(load "/mcclim-library.wasm") (delete-file "/mcclim-library.wasm") (values)', {eof: true});
  assert.equal(vm.evaluate(`(and (find-package "CLIM")
    (not (find-package "ASDF")) (not (find-package "UIOP"))
    (eq miricl-compiler::*compiler-state* :ready))`).output, 'T\n');
  // Applications retain the ready interactive compiler inherited from the base image.
  vm.evaluate('(setq miricl-compiler:*compile-evaluations* t) (values)');
  await fs.mkdir(path.dirname(output), {recursive: true});
  const image = vm.saveImage();
  await fs.writeFile(output, image);
  console.log(`Wrote McCLIM image ${output} (${image.length} bytes; ready compiler, no ASDF).`);
} finally {vm.close();}
