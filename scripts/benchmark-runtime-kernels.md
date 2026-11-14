Load the benchmark helpers once in the existing host SBCL REPL after MIRICL.
Reload this file after changing fixtures; the connection and compiler stay live.

```lisp
(load "scripts/benchmark-runtime-kernels.lisp")
(miricl-port-bench:write-benchmark-image
 "build/kernel-batch/ports-bench.wasm"
 :catalog "build/kernel-batch/ports-catalog.json"
 :wat-path "build/kernel-batch/ports-workers.wat")
(miricl-port-bench:disassemble-kernels
 "build/kernel-batch/ports-sbcl-disassembly.txt")
(miricl-port-bench:disassemble-emitted-kernels
 "build/kernel-batch/ports-emitted-disassembly.txt"
 :names '("text-equal-p" "ascii-white-p" "integer-gcd"))
(miricl-port-bench:benchmark-sbcl "build/kernel-batch/ports-sbcl.json")
```

Run the matching Wasm microbenchmarks after the SBCL measurement finishes:

```sh
node scripts/benchmark-runtime-kernels.mjs \
  build/kernel-batch/ports-bench.wasm build/kernel-batch/ports-catalog.json \
  > build/kernel-batch/ports-wasm.json
node scripts/benchmark-runtime-kernels-compare.mjs \
  build/kernel-batch/ports-wasm.json build/kernel-batch/ports-sbcl.json \
  > build/kernel-batch/ports-comparison.json
```

`write-benchmark-image` temporarily appends one raw diagnostic export to each
selected runtime worker generator, restores all definitions afterwards, and
clears the compiler-free image cache for this build. The shipped image gains
no diagnostic exports. It discovers worker names, argument counts and truth
versus object result ABI from the actual source wiring and fixtures. Both ABI
forms are supported. A changed WAT worker requires rebuilding this diagnostic
image; a changed fixture only requires writing a new catalog. No baseline image
or guest compiler bootstrap is needed.

The Node process keeps one Wasm instance throughout all fixtures, warmups and
samples. Setups use the ordinary interpreter to create one list of arguments in
`cl-user::*runtime-port-bench-arguments*` before each sample. Each separate
native driver extracts references once and repeatedly calls its actual linked
worker. Fixture allocation/reset, interpreted evaluation, symbol lookup, and
argument extraction stay outside the loop; lookup/extraction happen once per
sample within the measured driver call. Each result contributes its truth value
to a checksum. The equivalent compiled SBCL driver binds arguments once and
uses a direct, explicitly NOTINLINE call to the same CL implementation and the
same checksum. Object results use Lisp truth semantics, so integer zero is true.
Full returned values, errors, effects and identities belong to the semantic
regression suite; this checksum only detects gross fixture/driver failures.

Every fixture/branch warms four times before any samples are collected. Each
worker has its own driver to avoid a shared dispatcher changing JIT inlining
budgets. Five separate samples produce a median. Fixtures are recreated before
every warmup and timed sample. Mutating fill/copy workers repeatedly operate on
the same valid region, while vector reversal alternates equivalent states.
Allocation inside a kernel, including garbage collection, is timed. Host and
target representations/collectors differ; compare the recorded operations and
disassembly before assigning a mechanism to a timing difference.

Use `:names '("integer-gcd" "isqrt/bignum")` on Lisp entry points to select
worker names or case names. `:scale 0.02d0 :samples 3` gives a short smoke run of
the SBCL benchmark. Node accepts `--filter 'gcd|isqrt' --scale 0.02 --samples 3`;
the filter matches case or worker names. Matching names/scales make comparisons
easier to reproduce, though the comparison reports per-call times and accepts
different counts. After warming all cases, Node probes each fixture and caps its
iteration count to approximately `--sample-ms 10` milliseconds per sample;
`--sample-ms 0` retains the catalog's fixed counts. This keeps slow generic-call
workers from monopolizing a broad run. Calibration never exceeds the scaled
catalog count. SBCL uses `SB-EXT:GET-TIME-OF-DAY` because its cached
`GET-INTERNAL-REAL-TIME` clock has millisecond granularity. Increase counts for
host medians under 50 clock ticks (50 microseconds on SBCL); the comparison
marks these as clock-resolution warnings and reports a null ratio for zero
medians. Do not interpret a zero-duration sample as infinite target slowdown.

`ports-workers.wat` contains `runtime-kernel-wat` output for the actual workers,
including closed helper inlining and runtime linkage. SBCL listings use the
ordinary `DISASSEMBLE` function at the persistent host REPL. Compare these with
the same optimized build policy; inspection does not alter DEBUG or other
compiler policies. `disassemble-emitted-kernels` temporarily intercepts linkage
and calls `MIRICL-COMPILER::DISASSEMBLE-NATIVE` with the exact emitted byte vector
and function index, then restores the linker. It uses a private cache binding
so prior cached workers cannot suppress this inspection. Image raw/compressed
size in the Node JSON includes the
diagnostic exports and must not replace measurements of the production image.

The fixture catalog covers broad arithmetic, text, calendar, macro-expansion,
sequence, checking, argument, path, array, type, printer, sorting and tree
workers. The earlier nine access/cursor microbenchmarks remain in
`benchmark-access-kernels.*`; they include per-iteration resets for argument
popping that this steady-state fixture framework deliberately does not model.
Neither catalog claims exhaustive branch or exception coverage.
