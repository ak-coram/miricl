// Run the shared CL bootstrap in the optional Node Wasm host. Common Lisp
// owns the bootstrap algorithm and links the returned native module manifest.
import fs from 'node:fs/promises';
import {Miricl} from './miricl.mjs';
import {collectCompilationStatistics} from './compilation-statistics.mjs';

const [bootstrap, output] = process.argv.slice(2);
if (!bootstrap || !output) throw new Error('Usage: node prepare-compiler-image.mjs BOOTSTRAP OUTPUT-PREFIX');
const vm = await Miricl.instantiate(await fs.readFile(bootstrap));
try {
  const source = await fs.readFile(new URL('./compiler-image-bootstrap.lisp', import.meta.url), 'utf8');
  vm.evaluate(source, {eof: true});
  const bootstrapStarted = performance.now();
  await collectCompilationStatistics(vm, {workload: 'compiler-bootstrap', phase: 'bootstrap native functions'},
    () => vm.evaluate('(miricl-compiler::bootstrap-compiler-native-functions) (values)', {eof: true}));
  const interpretedBootstrapMs = performance.now() - bootstrapStarted;
  const moduleStarted = performance.now();
  await collectCompilationStatistics(vm, {workload: 'compiler-bootstrap', phase: 'sealed compiler module'},
    () => vm.evaluate('(miricl-compiler::bootstrap-compiler-image) (values)', {eof: true}));
  const nativeModuleAndManifestMs = performance.now() - moduleStarted;
  console.log(`Compiler phase times: ${JSON.stringify({interpretedBootstrapMs, nativeModuleAndManifestMs,
    statisticsEnabled: Boolean(process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY)})}`);
  const image = await vm.readFile('/compiler-image.lisp');
  await fs.writeFile(`${output}.lisp`, image);
  console.log(`Self-compiled compiler manifest: ${image.length} bytes; host Common Lisp performs linking.`);
} finally {vm.close();}
