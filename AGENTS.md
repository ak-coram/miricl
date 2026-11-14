# Goals

This project has two main goals: provide an interactive development
environment for developing Common Lisp programs targetting Wasm and a
whole program optimizing compiler also targetting Wasm. The host
Common Lisp system is intented to bootstrap the interpreter running
inside the Wasm environment.

While providing an interactive environment for development, the
intention is to ship small and efficient binaries using the
compiler. The interpreter / compiler can also be used to extend
programs dynamically at runtime.

The Common Lisp type system shall be used for optimization and for
generating smaller binaries.

The long-term goal is a compiler with advanced type analysis and
inference. Build type-based optimizations as foundations that become
more useful as declared and inferred type information improves.

Full ANSI Common Lisp compliance is important.

# Type proofs for optimization

Build a small, compiler-integrated proof engine around Common Lisp type
system semantics. Its scope is deriving facts that justify optimizations,
not general theorem proving. Share the target's type rules between inference,
proof queries and lowering instead of maintaining separate approximations.

Use declared and inferred types, singleton values, integer intervals,
predicates and control/data flow to establish facts. Propagate them within
functions, across closed module calls and returns, and through whole-program
analysis where ownership and visibility permit. Exported or escaping functions
must account for outside callers. Mutable state, aliases, capture and changes
to the HIR must invalidate facts that depend on them.

Proofs must be sound but may be incomplete. Bound reasoning depth and total
work; an unknown relationship or exhausted budget supplies no proof and keeps
the ordinary implementation. Respect target Common Lisp semantics rather
than assuming the host's type relationships or numeric representation.
Opaque type specifiers must not trigger arbitrary DEFTYPE expansion or
SATISFIES execution inside the proof engine.

Apply proven facts under the relevant declaration, safety and redefinition
contracts. Preserve operand effects and order, multiple values, numeric
contagion, bignum promotion, and observable errors and traps. Structural input
validation remains mandatory before unchecked compiler internals; an
optimization proof does not replace boundary checks.

Keep the engine small enough to ship with the compiler. Treat it as a
long-term investment: retain useful generic rules with modest image cost even
when mostly untyped examples show little immediate gain. Iterate primarily on
the compiled compiler compiling a fixed original compiler-source corpus;
validate and instantiate its output without installing further compiler
generations. Measure speed and generated size, use typed regression cases,
and periodically compare full Maxima, FriCAS, ACL2 and Coalton compilations.

# Common Lisp semantics

Consult the [Common Lisp HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
when implementing Common Lisp functions. Check the relevant entries
for argument and return types, multiple values, copying and sharing,
side effects, and exceptional situations before implementing them.
Use those requirements to guide the implementation and regression tests.

# Input validation

Treat user input as untrusted, including source forms, macroexpansion
results, public compiler IR, and serialized data. Validate structure,
types, bounds, and cycles at the boundaries before passing data to
unchecked internals, especially code compiled with safety level 0.
Type declarations and safety 0 are not substitutes for input validation.

Reject malformed input with useful Lisp conditions identifying the
offending input and the reason. It must not cause host/Wasm traps,
hangs from malformed structures, or corrupt subsequent compiler use.
Add regression coverage for malformed input and recovery after rejection.

# Performance

Keep the default precompiled browser image below 1 MiB (1,048,576 raw bytes). Track raw
and compressed sizes separately; download compression does not replace this
size budget. Preserve the compiler-free image for lightweight scripting.

The long-term runtime direction is ordinary Common Lisp cross-compiled on the
bootstrap host by MIRICL, above a small native substrate for representation and platform
operations. Migrate measured runtime slices incrementally, preserving target
semantics and comparing emitted code with SBCL. Host macro expansion, type
relationships and object representations are not target contracts. Measure
raw and compressed image size alongside performance; host tooling must not
become a dependency of the shipped interpreter or application images.
Compile migrated runtime code as sealed modules with block compilation and
private helper inlining. Keep checked boundaries before SAFETY 0 internals.
The host compiler path is bootstrap tooling; development and application
compilation belong in the compiler running inside Wasm. Share compiler sources
and passes rather than building an independent host application compiler.
Keep a persistent SBCL REPL for the host cross-compiler too. After loading
MIRICL, use MIRICL-CROSS:COMPILE-LAMBDA or COMPILE-MODULE to emit target code
directly, reload edited backend definitions, and compare the actual Wasm worker
with SBCL DISASSEMBLE on the equivalent CL. This loop needs no guest compiler
bootstrap or image rebuild. Supply target macro/load-time adapters explicitly;
an accepted host expansion is not evidence that its dependencies exist in Wasm.

For compiler and Lisp runtime experiments, prefer a persistent Wasm REPL with
the source compiler over rebuilding images for each edit. Start the ordinary
cold compiler image with `node scripts/repl.mjs miricl-cold.wasm --directory .`, set
`miricl-compiler:*compile-evaluations*` to NIL and
`miricl-compiler::*compiler-state*` to `:interpreted`, then LOAD the affected
compiler source (for example, `/wasm-compiler.lisp`). Explicit COMPILE-NATIVE
still emits and instantiates Wasm without bootstrapping the compiler. Reload
edited definitions, inspect their generated code and run warmed small benchmarks
in the same session. For optimized code whose DEBUG policy omits a listing,
use `miricl-compiler::disassemble-native` on the emitted bytes and actual
function index rather than changing the optimization policy to inspect it.
Precompiled compiler modules can retain sealed internal function bindings, so
redefining one global function there may not update its callers. Use the source
compiler for this loop. Runtime WAT/import changes require rebuilding a matching
cold image, and representation/bootstrap changes may require a fresh session.
When rebuilding runtime code in a persistent host REPL, clear the matching
image cache explicitly: `*interpreter-wasm*` / `*interpreter-wasm-compact*`,
`*interpreter-only-wasm*`, or `*interpreter-native-compiler-wasm*` in MIRICL.
The native image cache keys its compiler payload, so reloading a runtime WAT
generator alone does not invalidate it. Keep the compiled payload to avoid
repeating compiler bootstrap for runtime-only changes.
When a source file also defines record layouts or registration tables, reload
the affected definitions while preserving the target's existing private symbol
identities. Successful small emissions do not establish that linker and
bootstrap registrations were reloaded consistently; check those paths in a
fresh compiled image before changing their tests.
Confirm promising changes with the precompiled compiler and application checks
before relying on them; interpreter compilation timings are not native compiler
performance measurements.
Do full compiler builds at meaningful checkpoints and full application rebuilds
less frequently. Reuse an existing native application bundle for runtime-only
experiments, and rebuild the application after measured changes justify it.

Keep compiler optimizations generic: derive them from Common Lisp
semantics, types, values, and control/data flow. Libraries and
applications, including Game of Life, Maxima and FriCAS are benchmarks
rather than compiler special cases.

Retain useful, correct type-based optimizations when image-size growth is
modest, even if existing tests or mostly untyped workloads show no immediate
speedup. Evaluate their benefits on sufficiently typed code and larger
applications. Keep measuring compilation time, application performance and
image size, and report the tradeoffs.

Prefer direct Wasm implementations for performance-critical functions,
especially frequently used primitives and allocation or traversal
loops. Use the interpreter for functions where its performance impact
is negligible. Follow the project's Common Lisp WAT integration style
for direct Wasm implementations.

# Style

Test intended behavior and success criteria, not incidental details of the
current implementation or environment. Avoid fixed counts, exact inventories,
ordering, sizes or cosmetic values unless they are part of the contract being
tested. Check that required results are present and correct, allow unrelated
additions, and derive expectations from the actual inputs or configuration
where possible. For example, a test suite should verify that all discovered
tests succeed rather than require a hardcoded number of tests. Keep relevant
checks consistent with changes so stale assumptions do not waste CI runs.

Wasm supports a Lisp-style syntax, the intention is to integrate Wasm
modules into the Common Lisp source code using reader macros, regular
macros and hand over compilation and some optimizations to the
wasmtime library.

The Wasmtime runtime also serves for running tests, the REPL by
default. The main target is the browser. Use only stable WASM and WASI
features; do not rely on experimental proposals or features requiring
experimental runtime flags. Advanced, stable WASM features such as
WasmGC should be used where applicable.

Explicit exception: native WASM stack switching is permitted for green
threads. Its experimental runtime flag and the current lack of ARM
support in the WASI adapter are accepted for now. This exception does
not permit other unstable WASM or WASI features.

# Other

Do not use Python or introduce Python dependencies. Use the project's
Common Lisp and JavaScript/Node.js tooling for implementation, tests, and
benchmarking.

Cloning repositories into clean checkouts is generally allowed without
asking, including clones needed for a requested submodule addition.
Use the requested remote and keep checkouts inside this workspace;
do not reuse repositories from other projects unless the user
explicitly asks.

Do not search, browse, or inspect folders unrelated to the project
you are working on without explicit permission. This applies to all
files and folders, not just Git repositories, including the user's
home directory and other projects. Limit file discovery to the active
project workspace and temporary directories created for the current
task, unless the user authorizes another specific location.

Other remote Git operations, including push, pull, fetch, and submodule
updates that access a remote, require the user's explicit approval.

Except for creating a clean clone, local Git operations that change
repository state require the user's explicit approval for the specific
action. This includes staging,
commits, amends, rebases, merges, cherry-picks, resets, restores,
branch changes, and worktree changes. Read-only local inspection is
allowed. Requests to implement, integrate, publish, or deploy work do
not imply Git approval.

When you're told to keep working, don't stop at arbitrary points: keep
working indefinitely as long there are any parts of the task
remaining.
