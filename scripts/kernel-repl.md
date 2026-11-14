Start each host REPL once, then submit ordinary Lisp files to it. Definitions,
compiler caches and fixtures remain live. The workspace queue also works when
the host REPL and its clients have separate sandbox `/tmp` directories.

```sh
node scripts/kernel-repl.mjs start runtime-host host
node scripts/kernel-repl.mjs start runtime-source wasm build/kernel-migration/miricl-cold.wasm
node scripts/kernel-repl.mjs submit runtime-host build/kernel-migration/experiment.lisp
node scripts/kernel-repl.mjs result runtime-host REQUEST-ID
```

`start` stays running. Run `submit` and `result` as separate commands; no
interactive tool input is needed. Each host request uses LOAD with printing
enabled and catches errors at the request boundary. Use explicit output files
for long disassemblies. Queue files/results/logs are in `build/kernel-repl/NAME`.
Use a unique session name. SBCL loads MIRICL through ASDF once; `SBCL` and
`QUICKLISP_SETUP` override its executable/setup path. Its compilation cache
needs the same filesystem access as an ordinary project build.

The Wasm session starts a cold image, disables automatic compilation, selects
the interpreted source compiler, and reloads `wasm-compiler.lisp`. It evaluates
request text directly, so compiler-source changes can be reloaded by submitting
the source file itself. Reload other affected compiler files explicitly; use a
matching fresh cold image after native runtime/import/representation changes.
Explicit COMPILE-NATIVE still emits real Wasm. Host requests can call
MIRICL-CROSS:COMPILE-LAMBDA, COMPILE-MODULE or RUNTIME-KERNEL-WAT directly and
reload edited backend definitions without bootstrapping the guest compiler.
