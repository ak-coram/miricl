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

# Artifact compatibility

Compatibility between compiler versions is not required for any generated
artifacts, images, snapshots, FASLs, native modules, or serialized compiler
data. Keep only the formats produced and consumed by the current compiler
and runtime; remove obsolete readers, adapters, migration paths, and
compatibility tests. Rebuild artifacts with matching compiler and runtime
versions when formats change. Preserve input validation and current-version
loading, save/restore, round-trip, and rejection/recovery tests.

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

Reuse the standard-function contract metadata and target type rules. Extend
conditional proof queries and their lowering consumers before adding another
fact inventory. Missing facts are unknown. Prove signaling, callbacks,
allocation, mutable effects and result counts separately. Proving an arithmetic
or list operation non-signaling does not make its polling path non-signaling
or unobservable. Primary-value demand does not prove that a callee returns
exactly one value.

Executable proof seeds must describe checks actually emitted at the relevant
observation, not stronger declarations. Track every SSA operand on which a
refinement depends. Discard speculative checks, facts and dependent lowering
plans together when admission fails. Complete bounded reachability before
excluding control-flow edges; an incomplete traversal supplies no proof.
Logical integer range does not establish physical i31 representation, and an
unchecked extractor is not an input validator.

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

ANSI Common Lisp is the authority for language semantics, not the current
implementation. Preserve defined ANSI behavior; do not make accidental current
behavior or behavior whose consequences ANSI leaves undefined into a compiler
contract. In particular, optimizations may rely on the standard definitions of
COMMON-LISP functions: redefining or binding CL:EQL and other standard functions
where ANSI specifies undefined consequences need not be supported or guarded.
Use standardized operation identity rather than package membership alone.
Fixed standard operators do not make user callbacks, permitted method extensions
or documented runtime hooks immutable. This does not relax
the redefinition and identity requirements ANSI actually imposes on ordinary
user functions, or the
contracts of explicit MIRICL extensions such as scheduling and debugging.
Tests must follow this distinction rather than constrain optimizations to
undefined behavior of the current implementation.

Optimizations may use every compile-time and runtime consistency assumption
ANSI permits, including the applicable INLINE, same-file, recursive-call and
declaration rules. Do not impose stronger redefinition guarantees, add
redefinition guards ANSI does not require, or restrict proof propagation
categorically to local, sealed or inlined functions. Outside those ANSI
permissions, preserve the prevailing runtime definition. Honor NOTINLINE exactly
as ANSI specifies, including its required out-of-line calls. Reachability alone
does not establish an ANSI permission. Tests must not make undefined or
unspecified redefinition behavior an optimization constraint. Preserve
explicitly documented MIRICL extension contracts separately.

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

Compiler proof metadata is a claim to validate, not authority to skip checks.
WasmGC types are canonical across modules; a matching layout or immutable flag
does not authenticate a proof. Bind runtime permissions to the actual validated
executable entry using runtime-owned identity or equivalent authority. Recheck
final code after rewriting and outlining, audit wrappers and arity copies, and
strip transient permissions during serialization unless loading reestablishes
their authority. Ordinary structural validation remains independent.

# Performance

On shared development machines, preserve ample memory and CPU headroom for
interactive use. Check available memory and current resource use before
starting heavy work, accounting for aggregate usage and peak memory across
all agents and child Node, Wasm and browser processes. Size concurrent work
to the measured resource budget and cap resources where supported; do not
start additional workloads under resource pressure. Avoid duplicate full
builds and compiler bootstraps. Keep persistent REPLs and reuse compiled
payloads for fast iteration. Idle REPLs are not active workloads; do not
close them or reload reusable state merely to reduce process counts.

Do not impose explicit process memory or heap limits in CI workflows or build tools
invoked by CI. Use the CI runner and runtime defaults. Keep local shared-machine
resource checks and optional caps separate from CI configuration.

Keep the default precompiled browser image below 1 MiB (1,048,576 raw bytes). Track raw
and compressed sizes separately; download compression does not replace this
size budget. Preserve the compiler-free image for lightweight scripting.

Cold-runtime sizes and standalone helper modules do not certify the default
precompiled compiler image budget. Measure matching complete artifacts including
the compiler payload; historical images and estimates remain explicitly labeled.
Compare compression with identical settings and report JavaScript adapter growth
separately from Wasm image growth.

Design optimization policy around compact browser deployment by default:
`(optimize (space 3) (speed 1))`. Give users local control over measured hot
paths, for example `(optimize (speed 3) (space 1))`. When SPACE exceeds SPEED,
favor shared helpers, conservative inlining and little code duplication. When
SPEED exceeds SPACE, allow larger inlining budgets, specialization and code
duplication where they plausibly reduce execution time. At equal priorities,
prefer changes improving both and resolve uncertain tradeoffs toward smaller
code. Keep sound optimizations that improve both size and speed available at
every policy level, including proof-based removal of checks, dead code and
redundant boxing.

Inlining growth follows the caller's policy, while preserving the callee's
semantic contracts. INLINE strongly encourages expansion within bounded work
and growth limits; NOTINLINE vetoes it. Estimate growth after simplification
and account for complete emitted modules, including imports, literals and
retained shared helpers, rather than relying only on source-form counts.
COMPILATION-SPEED independently controls analysis effort. SPEED and SPACE must
not implicitly lower SAFETY or DEBUG, change redefinition/visibility contracts,
or imply STRIP. The 1 MiB budget applies to the default shipped image; it does
not forbid users from deliberately trading space for speed in their programs.
Keep the shipped interpreter/compiler compact and annotate measured hot paths
selectively. This is the intended policy; distinguish it from heuristics that
do not yet implement these controls.

During the runtime migration, prioritize completing ports. Compare generated
code and performance with equivalent SBCL code; optimize further during a port
only when generated code is substantially slower. Describe remaining speed and
size mechanisms in compiler.org with affected functions and design tradeoffs, so
later work can prioritize mechanisms benefiting many functions. Keep measured
evidence in temporary experiment records and report results to the user.

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
Retain the full Wasm bootstrap path: build the interpreter, run the compiler
interpreted inside it, then compile that compiler to native Wasm. This remains
a required benchmark of interpreter performance and correctness; do not replace
it with host compilation of the full compiler to bypass the interpreted stage.
Host cross-compilation remains useful for runtime kernels and fast iteration.
Measure interpreted bootstrap separately from native compiler throughput.
Keep a persistent SBCL REPL for the host cross-compiler too. After loading
MIRICL, use MIRICL-CROSS:COMPILE-LAMBDA or COMPILE-MODULE to emit target code
directly, reload edited backend definitions, and compare the actual Wasm worker
with SBCL DISASSEMBLE on the equivalent CL. This loop needs no guest compiler
bootstrap or image rebuild. Supply target macro/load-time adapters explicitly;
an accepted host expansion is not evidence that its dependencies exist in Wasm.

In multi-agent work, the coordinator must launch and own persistent SBCL and
Wasm REPL tool processes. Workers must attach through shared clients and hand
off connection endpoints and loaded session state. Worker interruption or
restart must not terminate these REPLs; a shared mailbox or socket does not
detach a process's lifetime from its owning agent. Verify that a restarted
worker can reuse the existing sessions without rebuilding or reloading them.

In multi-agent optimization work, keep the coordinator on coordination and
workflow, with investigation/implementation, measurement and semantic-review
roles. For each iteration, initial workers gather evidence and write handoffs;
rotate every role to fresh context for independent verification before
implementation, review and validation. Use one source writer per area and an
explicit serial token for heavy workloads. Waiting roles can investigate future
questions without launching competing workloads.

Check worker status when handing off work. A message to a completed worker does
not restart it; use a dispatch operation that starts a turn, such as
followup_task. Across conversation handoffs, verify advancing REPL heartbeats
and loaded-state markers before requests; a stale status file can still say
ready. Root-local process handles and ownership may not transfer. A client
timeout does not cancel server work, and readiness can lag a completed response:
wait by request identity rather than blindly resubmitting. The workload token
remains authoritative when status does not track a host-side operation.

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
The source compiler itself uses live Lisp definitions. Tests that temporarily
replace primitives used by the compiler must run against the sealed native
image, or isolate their planner queries: replacing those dependencies can also
break the source compiler during the test. Preserve the redefinition regression;
do not weaken it to accommodate the REPL workflow.
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

Send host REPL requests as files LOADed inside HANDLER-CASE, so reader errors
(including unknown package nicknames) are caught rather than terminating SBCL.
Preserve baseline callable objects and matching emitted bytes before reloading
definitions. Existing native workers do not change with a backend reload, and
recompiling a baseline may require runtime imports absent from its old image.

Verify the actual installed entry, module and reachable hot helpers when
claiming native performance. Re-emitting an equivalent expression or checking
COMPILED-FUNCTION-P alone is insufficient. An interpreted compiler frontend can
emit native workers; native drivers and methods do not establish that generated
effective methods or fallback helpers are native.

Confirm promising changes with the precompiled compiler and application checks
before relying on them; interpreter compilation timings are not native compiler
performance measurements.
For JIT measurements, warm every candidate and representative branch before
collecting samples. A shared benchmark dispatcher can make candidates compete
for the same inlining budget; use equivalent separate drivers when that would
distort the comparison. Alternating sample order alone does not remove warmup
or feedback bias. Inspect generated code or JIT traces when tiny-case results
contradict the emitted operations.

Compare equivalent sources and policies, retain complete driver timings, and
report loop floors separately. Matched unrolled drivers and overhead subtraction
are diagnostic; neither replaces the original success criterion. Calibrate
sample duration against clock resolution and retain raw ticks, iterations,
results and sample variation. Keep builds, compression and profiling out of
retained timing windows. Measure cache misses, alternating working sets and
ordinary fallback paths as well as favorable hits; a small hit gain does not
justify a large common-path regression. Structural simplification is not evidence
of a speedup without measurement.

Caches over mutable state must distinguish a location from its current value.
Account for replacement of backing tables or roots, including initialization
before image restoration. Shallow copying a literal container does not freeze
its elements or establish ownership for every constructor path. Preserve quoted
data, sharing and serialization semantics when considering cached references.

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

# Testing

Keep ANSI tests as the language conformance suite in both interpreter and
compiler modes. Preserve application-specific tests. Keep the additional
local core suite small and focused on contracts ANSI does not already cover;
check existing ANSI coverage before adding or recovering a test.

Core coverage has three categories:

- Basic soundness: validation of untrusted compiler inputs, public IR and
  serialized data; useful conditions and recovery after rejection; sound
  optimization proofs, mutation/redefinition contracts, and current-format
  serialization round trips.
- Implementation-specific behavior: green threads, scheduling, synchronization,
  interruption and cleanup; Wasm and host adapter integration; virtual,
  mounted and development filesystems; platform boundaries and error recovery.
- Compiler optimization capabilities: general observable assumptions such as
  deep tail calls without stack exhaustion, preservation of values and effects,
  and safe inlining and retained recursive frames. Prefer behavioral checks
  over MIRICL-specific IR layouts, opcode/import/local inventories, exact
  instruction counts or timing thresholds.

WASI cannot load generated code at runtime and is exempt from all compiler
optimization tests, including TCO requirements. Keep applicable WASI runtime
and implementation tests separate from compiler optimization checks.

Use matching compiler, runtime and adapter artifacts. Local core checks run
against existing builds; rebuild explicitly when needed rather than making
every test command repeat compiler bootstrap. Use the persistent source
compiler loop for focused diagnosis and confirm changes in the matching
precompiled image. Keep running relevant tests and fix failures without
weakening their behavioral requirements; distinguish stale artifacts and
test setup mistakes from implementation defects.

In REPL save/restore tests, keep host callbacks and other unsaveable host values
out of retained REPL history. Warm the intended state immediately before the
observation or save, without intervening helpers that invalidate it. Diagnose
harness and history-retention failures before changing runtime contracts.

Do not add benchmarks or benchmark infrastructure to the repository. The
applications and their existing benchmarks are sufficient for now. Keep
one-off measurement scripts and results in temporary directories; continue
reporting runtime/application performance and raw/compressed image sizes
without restoring the removed compiler/interpreter microbenchmark suite.

# Documentation

Keep compiler.org focused on how the current compiler works: architecture,
interfaces, representations, algorithms, semantic contracts and implementation
limits. Consolidate related mechanisms and write in the present tense.
Do not add development chronology, batch records, benchmark results, size or memory
measurements, test pass/fail or coverage reports, application validation status,
or per-run artifact/session handoffs. Report validation and measurements to the
user and keep one-off evidence in temporary directories. Document input-validation
mechanisms and compiler contracts as part of the architecture.

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
