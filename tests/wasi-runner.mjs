// Exercise the WASI command through real stdin/stdout without a Lisp host.
import {spawnSync} from 'node:child_process';
const result = spawnSync(process.env.WASMTIME ?? 'wasmtime',
  ['run', '-W', 'stack-switching=y', '-S', 'p3=y', process.argv[2]], {stdio: 'inherit'});
if (result.error) throw result.error;
process.exitCode = result.status ?? 1;
