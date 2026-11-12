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
