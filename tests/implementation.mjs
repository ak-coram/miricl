// MIRICL host and runtime contracts beyond ANSI Common Lisp.
// node tests/implementation.mjs [miricl.wasm] [miricl-repl.wasm]
// The matching miricl-interpreter.wasm must be beside the selected image.
// The optional second image also exercises Wasmtime's stack-switching adapter.
// Hostless source evaluation uses miricl-interpreter.wasm beside IMAGE.wasm.
const [image = 'miricl.wasm', wasi, ...extra] = process.argv.slice(2);
if (extra.length) throw new Error('Expected [IMAGE.wasm] [WASI-REPL.wasm]');
for (const name of ['transport', 'filesystem-archive', 'mounted-filesystem', 'dev-filesystem', 'hir-debugger', 'threads']) {
  console.log(`Implementation: ${name} (${image}${name === 'threads' && wasi ? `; ${wasi}` : ''})`);
  await import(`./implementation/${name}.mjs`);
}
console.log('Implementation checks passed.');
