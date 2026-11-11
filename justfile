# Recipes use Bash quoting and argument forwarding, including on Windows.
# Git for Windows provides Bash; run just from Git Bash or put it on PATH.
set shell := ['bash', '-euo', 'pipefail', '-c']

# Override these with environment variables or `just name=value <recipe>`.
sbcl := env('SBCL', 'sbcl')
node := env('NODE', 'node')
wasmtime := env('WASMTIME', 'wasmtime')
quicklisp := env('QUICKLISP_SETUP', home_directory() / 'quicklisp/setup.lisp')
build_dir := env('MIRICL_BUILD_DIR', '.')
interpreter := build_dir / 'miricl.wasm'
wasi_repl := build_dir / 'miricl-repl.wasm'
precompiled := build_dir / 'miricl-precompiled.wasm'
interpreter_only := build_dir / 'miricl-interpreter.wasm'

# List available commands.
default:
    @just --list

# Fetch and check out the latest upstream revision of every submodule, recursively.
update-submodules:
    git submodule update --init --recursive --remote

# Build the runtime, compiler-free interpreter, precompiled compiler and WASI REPL.
build:
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --script scripts/build-wasm.lisp {{ quote(build_dir) }}

# Report raw, gzip, Brotli, and per-section sizes for all runtime images.
size: build
    {{ quote(node) }} scripts/wasm-size.mjs {{ quote(interpreter) }} {{ quote(interpreter_only) }} {{ quote(precompiled) }} {{ quote(wasi_repl) }}

# Benchmark the native compiler recompiling its own fixed source corpus.
benchmark-self-compiler output='build/compiler-benchmark': build
    {{ quote(node) }} scripts/benchmark-self-compiler.mjs {{ quote(output / 'corpus') }} {{ quote(interpreter) }} --extract
    {{ quote(node) }} scripts/benchmark-self-compiler.mjs {{ quote(output / 'native') }} {{ quote(precompiled) }} {{ quote(output / 'corpus/sources.lisp') }}

# Build a self-contained browser REPL (miricl.html), including the interpreter.
build-html:
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --script scripts/build-html.lisp {{ quote(build_dir) }}

# Build the standalone CLOS TODO application (todo.html).
build-todo:
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --script scripts/build-todo.lisp {{ quote(build_dir) }}

# Prepare an independent application directory with runtime and library symlinks.
setup-game-of-life: build asdf-init build-asdf
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --script scripts/setup-application.lisp {{ quote(absolute_path("game-of-life")) }} game-of-life {{ quote(absolute_path(build_dir)) }} {{ quote(node) }}

# Build Game of Life with separate HTML, adapter, runtime and application files.
build-game-of-life: setup-game-of-life
    {{quote(just_executable())}} --justfile game-of-life/justfile wasmtime={{quote(wasmtime)}} build

# Benchmark real application and runtime sizes, including a fresh optimized load.
benchmark-game-of-life: build-game-of-life
    {{ quote(node) }} scripts/benchmark-game-of-life.mjs game-of-life

# Check source loading and interaction through the browser's precompiled compiler.
test-game-of-life-dev: setup-game-of-life
    {{ quote(node) }} tests/browser-game-of-life.mjs game-of-life/dev.html --dev

# Clone pinned Coalton dependencies into this workspace when absent.
prepare-coalton:
    {{ quote(node) }} scripts/prepare-coalton.mjs

# Build the complete interactive compiler and standard library as native Wasm.
build-coalton: build build-asdf prepare-coalton
    {{ quote(node) }} scripts/probe-coalton.mjs {{ quote(precompiled) }} build/coalton/compile.json
    {{ quote(node) }} scripts/optimize-coalton.mjs {{ quote(precompiled) }}
    {{ quote(node) }} scripts/snapshot-coalton.mjs {{ quote(precompiled) }} build/coalton/coalton.wasm build/coalton/coalton-image.wasm
    {{ quote(node) }} scripts/build-coalton.mjs {{ quote(precompiled) }} build/coalton/coalton.wasm build/coalton/site --initialized-image build/coalton/coalton-image.wasm build/coalton/coalton-image.wasm.json

test-coalton:
    {{ quote(node) }} tests/coalton-examples.mjs
    {{ quote(node) }} tests/coalton-host.mjs {{ quote(precompiled) }}
    {{ quote(node) }} tests/coalton-session.mjs build/coalton/coalton-image.wasm --snapshot
    {{ quote(node) }} tests/coalton-repl-session.mjs build/coalton/coalton-image.wasm --snapshot

# Build raw, gzip and Brotli size reports for the Wasm images and HTML pages.
build-sizes: build build-asdf build-html build-todo build-game-of-life build-maxima build-fricas
    {{ quote(node) }} scripts/build-size-html.mjs {{ quote(build_dir) }} {{ quote(build_dir / 'image-sizes.html') }} miricl.html {{ quote(absolute_path('game-of-life')) }}

# Check size accounting and the standalone report in a browser.
test-size-html:
    {{ quote(node) }} tests/size-html.mjs

# Start a local terminal REPL without network listeners.
repl: build
    {{ quote(node) }} scripts/repl.mjs {{ quote(interpreter) }}

# Start the local WASI terminal REPL without network access.
repl-wasi: build
    {{ quote(wasmtime) }} run -W stack-switching=y -S p3=y {{ quote(wasi_repl) }}

# Connect the terminal to an interpreter in a webapp, without HTTP or TCP REPL.
browser-repl: build
    {{ quote(node) }} scripts/repl.mjs {{ quote(interpreter) }} --browser-repl

browser-repl-wasi: build
    {{ quote(wasmtime) }} run -W stack-switching=y -S p3=y -S inherit-network=y {{ quote(wasi_repl) }} --browser-repl

# Relay a TCP REPL to an interpreter inside a webapp (Node's built-in sockets).
repl-bridge: build
    {{ quote(node) }} scripts/repl.mjs {{ quote(interpreter) }} --repl-bridge

# The same relay using WASI 0.3 sockets, with no Node dependency.
repl-bridge-wasi: build
    {{ quote(wasmtime) }} run -W stack-switching=y -S p3=y -S inherit-network=y {{ quote(wasi_repl) }} --repl-bridge

# Connect the terminal and serve a local webapp at http://127.0.0.1:8000/.
dev-server directory='.' : build
    {{ quote(node) }} scripts/repl.mjs {{ quote(interpreter) }} --dev-server --directory={{ quote(directory) }}

# Serve a directory with WASI 0.3 filesystem and socket capabilities.
dev-server-wasi directory='.' : build
    {{ quote(wasmtime) }} run -W stack-switching=y -S p3=y -S inherit-network=y --dir {{ quote(directory + '::.') }} {{ quote(wasi_repl) }} --dev-server

# Serve local files and enable the TCP REPL listener together.
dev-server-repl-bridge directory='.' : build
    {{ quote(node) }} scripts/repl.mjs {{ quote(interpreter) }} --dev-server --repl-bridge --directory={{ quote(directory) }}

dev-server-repl-bridge-wasi directory='.' : build
    {{ quote(wasmtime) }} run -W stack-switching=y -S p3=y -S inherit-network=y --dir {{ quote(directory + '::.') }} {{ quote(wasi_repl) }} --dev-server --repl-bridge

# Exercise all four browser-connected modes, including custom ports.
test-repl-bridge: build
    {{ quote(node) }} tests/repl-bridge.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }}
    {{ quote(node) }} tests/repl-websocket.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }}
    {{ quote(node) }} tests/dev-filesystem-server.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }}

test-browser-dev: build build-asdf
    {{ quote(node) }} tests/browser-dev-repl.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }} {{ quote(build_dir / 'build/asdf/asdf.lisp') }}

# Start the Common Lisp host REPL backed by Wasmtime.
repl-host:
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --eval '(require :asdf)' --eval '(asdf:load-asd (truename "miricl.asd"))' --eval '(ql:quickload :miricl)' --eval '(miricl:repl)' --quit

# Run all Common Lisp project tests (FiveAM through ASDF).
test:
    {{ quote(sbcl) }} --noinform --non-interactive --load {{ quote(quicklisp) }} --eval '(require :asdf)' --eval '(asdf:load-asd (truename "miricl.asd"))' --eval '(ql:quickload :miricl/test)' --eval '(asdf:test-system :miricl)'

# Use Roswell's SBCL and Quicklisp, including in Windows CI. Disable the
# debugger before Roswell loads Quicklisp, so startup errors fail the job.
[script('ros', '-L', 'sbcl-bin', 'run', '--', '--non-interactive', '--load')]
test-sbcl-roswell:
    (require :asdf)
    (asdf:load-asd (truename "miricl.asd"))
    (ql:quickload :miricl/test)
    (asdf:test-system :miricl)

# Run integration groups shared with CI; ANSI owns broad language conformance.
test-js group='all': build build-asdf build-html build-todo
    {{ quote(node) }} scripts/test-integration.mjs {{ quote(group) }} {{ quote(build_dir) }}

# Check the shared green-thread scheduler in Node.
test-threads: build
    {{ quote(node) }} tests/threads.mjs {{ quote(interpreter) }}

# Also check the native stack-switching adapter in Wasmtime.
test-threads-wasi: build
    {{ quote(node) }} tests/threads.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }}

# Check the standalone module's object transport.
test-standalone: build
    {{ quote(node) }} tests/standalone.mjs {{ quote(interpreter) }}

# Check text evaluation and both Node and WASI REPLs.
test-text: build
    {{ quote(node) }} --experimental-wasm-wasmfx tests/text-repl.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }}

# Check the standalone HTML offline in headless Chromium (CHROMIUM overrides the executable).
test-browser: build-html
    {{ quote(node) }} tests/browser-repl.mjs {{ quote(build_dir / 'miricl.html') }}

# Check TODO interactions, persistence, offline startup, and mobile layout.
test-todo: build-todo
    {{ quote(node) }} tests/browser-todo.mjs {{ quote(build_dir / 'todo.html') }}

# Compatibility name; test-text already exercises both terminal hosts.
alias test-wasmtime := test-text

# Check macros, source loading, and guest development facilities.
test-development: build
    {{ quote(node) }} tests/development.mjs {{ quote(interpreter) }}

test-compilation: build
    {{ quote(node) }} scripts/test-integration.mjs compiler {{ quote(build_dir) }}

test-hash: build
    {{ quote(node) }} tests/hash-structural.mjs {{ quote(interpreter) }}

# Exercise both standalone production variants in Chromium, offline.
test-game-of-life: build-game-of-life
    {{ quote(node) }} tests/browser-game-of-life.mjs game-of-life/build/index.html

test-environment: build
    {{ quote(node) }} tests/environment.mjs {{ quote(interpreter) }}

test-character-case: build
    {{ quote(node) }} tests/character-case.mjs {{ quote(interpreter) }}

# Check guest reader labels, sharing, cycles, and nested read scopes.
test-reader-labels: build
    {{ quote(node) }} tests/reader-labels.mjs {{ quote(interpreter) }}

test-reader-buffer: build
    {{ quote(node) }} tests/reader-buffer.mjs {{ quote(interpreter) }}

test-vector-reader: build
    {{ quote(node) }} tests/vector-reader.mjs {{ quote(interpreter) }}

test-array-reader: build
    {{ quote(node) }} tests/array-reader.mjs {{ quote(interpreter) }}

test-print-circle: build
    {{ quote(node) }} tests/print-circle.mjs {{ quote(interpreter) }}

test-output-buffer: build
    {{ quote(node) }} tests/output-buffer.mjs {{ quote(interpreter) }}

test-input-streams: build
    {{ quote(node) }} tests/input-streams.mjs {{ quote(interpreter) }}

# Check portable file streams and physical pathname semantics.
test-filesystem: build
    {{ quote(node) }} tests/filesystem.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/dev-filesystem.mjs {{ quote(interpreter) }}

test-composite-streams: build
    {{ quote(node) }} tests/composite-streams.mjs {{ quote(interpreter) }}

test-pathnames: build
    {{ quote(node) }} tests/pathnames.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/logical-pathnames.mjs {{ quote(interpreter) }}

test-declarations: build
    {{ quote(node) }} tests/declarations.mjs {{ quote(interpreter) }}

test-time: build
    {{ quote(node) }} tests/time.mjs {{ quote(interpreter) }}

test-float-properties: build
    {{ quote(node) }} tests/float-properties.mjs {{ quote(interpreter) }}

test-derived-types: build
    {{ quote(node) }} tests/derived-types.mjs {{ quote(interpreter) }}

test-sequence-transformations: build
    {{ quote(node) }} tests/sequence-transformations.mjs {{ quote(interpreter) }}

test-random: build
    {{ quote(node) }} tests/random.mjs {{ quote(interpreter) }}

# Check optional JavaScript interop, callbacks, and typed bindings.
test-javascript: build
    {{ quote(node) }} tests/javascript.mjs {{ quote(interpreter) }}

# Check bounded Lisp error traces and stack restoration across nonlocal exits.
test-backtrace: build
    {{ quote(node) }} tests/backtrace.mjs {{ quote(interpreter) }}

# Check ANSI accounting and recovery from load errors and hanging tests.
test-ansi-runner: build
    {{ quote(node) }} tests/ansi-runner.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/ansi-upstream-runner.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/ansi-compiler-runner.mjs {{ quote(interpreter) }}

# Initialize the pinned upstream ANSI test submodule.
ansi-init:
    git submodule update --init tests/ansi-test

# Initialize the pinned upstream ASDF/UIOP sources.
asdf-init:
    git submodule update --init vendor/asdf

# Probe ACL2 bootstrap in the guest; port mode uses the MIRICL host adaptations.
# Requires vendor/acl2 to be present locally.
probe-acl2 mode='upstream': build
    {{ quote(node) }} scripts/probe-acl2.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/acl2' / (mode + '.json')) }} {{ quote(mode) }}

# Emit and measure portable ACL2 FASLs; logical-world initialization is separate.
compile-acl2: build
    {{ quote(node) }} scripts/probe-acl2.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/acl2/fasl.json') }} fasl {{ quote(build_dir / 'build/acl2/acl2.wasm') }}

test-acl2-host: build
    {{ quote(node) }} tests/acl2-host.mjs {{ quote(precompiled) }}
    {{ quote(node) }} tests/acl2-invariant-risk.mjs {{ quote(precompiled) }}
    {{ quote(node) }} tests/acl2-fnnames.mjs {{ quote(precompiled) }}
    {{ quote(node) }} tests/acl2-session.mjs {{ quote(precompiled) }}

# Compile the complete dynamic prover to native Wasm on the default stack.
compile-acl2-native: compile-acl2
    {{ quote(node) }} scripts/optimize-acl2.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/acl2/acl2.wasm') }} {{ quote(build_dir / 'build/acl2/acl2-native.wasm') }}

# Initialize and verify the native prover before packaging it for the browser.
build-acl2: compile-acl2-native build-acl2-books
    {{ quote(node) }} tests/acl2-native.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/acl2/acl2-native.wasm') }} {{ quote(build_dir / 'build/acl2/native-tests.json') }} {{ quote(build_dir / 'build/acl2/acl2-image.wasm') }}
    {{ quote(node) }} scripts/build-acl2.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/acl2/acl2-native.wasm') }} {{ quote(build_dir / 'build/acl2/site') }} --initialized-image {{ quote(build_dir / 'build/acl2/acl2-image.wasm') }} {{ quote(build_dir / 'build/acl2/native-tests.json') }}

# Build the UI without a prover core; evaluation stays disabled in this preview.
preview-acl2: build build-acl2-books
    {{ quote(node) }} scripts/build-acl2.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/acl2/acl2.wasm') }} {{ quote(build_dir / 'build/acl2/preview') }} --frontend-only

test-acl2-frontend: preview-acl2
    {{ quote(node) }} tests/acl2-session.mjs {{ quote(precompiled) }}
    {{ quote(node) }} tests/browser-acl2-app.mjs {{ quote(build_dir / 'build/acl2/preview') }}

# Package selected math sources separately; users certify/compile in the browser.
build-acl2-books:
    {{ quote(node) }} scripts/build-acl2-books.mjs {{ quote(build_dir / 'build/acl2/books') }}

test-acl2-books: build build-acl2-books
    {{ quote(node) }} tests/acl2-books.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/acl2/books') }}

# Initialize the pinned canonical Maxima sources.
maxima-init:
    git submodule update --init vendor/maxima

# Load Maxima from source and check representative algebra; upstream/compile probe build paths.
probe-maxima mode='source': build maxima-init
    {{ quote(node) }} scripts/probe-maxima.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/maxima' / (mode + '.json')) }} {{ quote(mode) }}

# Exercise the same Maxima core offline inside a Chromium worker.
test-maxima-browser: build-maxima
    {{ quote(node) }} tests/browser-maxima-app.mjs {{ quote(build_dir / 'build/maxima/site') }}

# Check documented mathematics and representative upstream regressions.
test-maxima-examples: build-maxima
    MIRICL_MAXIMA_DOCS={{ quote(build_dir / 'build/maxima/doc/info') }} {{ quote(node) }} tests/maxima.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/maxima/maxima-optimized.wasm') }}
    MIRICL_MAXIMA_CORE={{ quote(build_dir / 'build/maxima/maxima-optimized.wasm') }} {{ quote(node) }} scripts/probe-maxima.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/maxima/core-tests.json') }} suite rtest13s

# Compile Maxima and assemble a static browser REPL without docs or tests.
build-maxima: (probe-maxima 'compile')
    {{ quote(node) }} scripts/optimize-maxima.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/maxima/maxima.wasm') }} {{ quote(build_dir / 'build/maxima/maxima-optimized.wasm') }}
    {{ quote(node) }} scripts/build-maxima.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/maxima/maxima-optimized.wasm') }} {{ quote(build_dir / 'build/maxima/site') }}

# Includes upstream's known-failure accounting; failures produce a JSON report.
test-maxima: build-maxima
    MIRICL_MAXIMA_DOCS={{ quote(build_dir / 'build/maxima/doc/info') }} MIRICL_MAXIMA_CORE={{ quote(build_dir / 'build/maxima/maxima-optimized.wasm') }} MIRICL_MAXIMA_TIMEOUT_MS=7200000 {{ quote(node) }} scripts/probe-maxima.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/maxima/suite.json') }} suite

# Initialize and prepare canonical FriCAS Lisp and algebra databases with SBCL.
fricas-init:
    git submodule update --init vendor/fricas

prepare-fricas: fricas-init
    {{ quote(node) }} scripts/prepare-fricas.mjs {{ quote(build_dir / 'build/fricas/native') }}

# Compile FriCAS in MIRICL and validate a fresh load.
probe-fricas: build prepare-fricas
    {{ quote(node) }} scripts/probe-fricas.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/fricas/native') }} {{ quote(build_dir / 'build/fricas/compile.json') }}

# Optimize the dynamic interpreter and assemble the browser workbench.
build-fricas: probe-fricas
    {{ quote(node) }} scripts/optimize-fricas.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/fricas/fricas.wasm') }} {{ quote(build_dir / 'build/fricas/fricas-optimized.wasm') }} {{ quote(build_dir / 'build/fricas/native') }}
    {{ quote(node) }} scripts/build-fricas.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/fricas/fricas-optimized.wasm') }} {{ quote(build_dir / 'build/fricas/native') }} {{ quote(build_dir / 'build/fricas/site') }}

# Documented examples, complete selected upstream suites, persistence and offline UI.
test-fricas: build-fricas
    {{ quote(node) }} tests/fricas.mjs {{ quote(precompiled) }} {{ quote(build_dir / 'build/fricas/fricas-optimized.wasm') }} {{ quote(build_dir / 'build/fricas/native') }} {{ quote(build_dir / 'build/fricas/tests.json') }}
    {{ quote(node) }} tests/browser-fricas-app.mjs {{ quote(build_dir / 'build/fricas/site') }}

# Assemble the optional guest ASDF library from a patched source copy.
build-asdf: build
    {{ quote(node) }} scripts/build-asdf.mjs {{ quote(build_dir / 'build/asdf') }} {{ quote(interpreter) }} {{ quote(build_dir / 'miricl-asdf.wasm') }}

# Check patch preparation, system loading, and production FASL bundles.
test-asdf: build build-asdf
    {{ quote(node) }} tests/asdf-build.mjs
    {{ quote(node) }} tests/asdf.mjs {{ quote(interpreter) }} {{ quote(build_dir / 'build/asdf/asdf.lisp') }}
    {{ quote(node) }} tests/asdf-bundle.mjs {{ quote(interpreter) }} {{ quote(build_dir / 'build/asdf/asdf.lisp') }}

# Run the default ANSI profile, or pass upstream paths and runner options.
[positional-arguments]
ansi *files: build
    {{ quote(node) }} tests/ansi.mjs {{ quote(interpreter) }} "$@"

# Run ANSI bodies through the native compiler; unsupported forms fail.
[positional-arguments]
ansi-compiler *files: build
    MIRICL_ANSI_REPORT={{ quote(env('MIRICL_ANSI_REPORT', build_dir / 'ansi-compiler-results.json')) }} {{ quote(node) }} tests/ansi.mjs {{ quote(interpreter) }} --compiler "$@"

# Run the full upstream ANSI suite through the native compiler.
[positional-arguments]
full-ansi-compiler *options: build
    SBCL={{ quote(sbcl) }} MIRICL_ANSI_REPORT={{ quote(env('MIRICL_ANSI_REPORT', build_dir / 'ansi-compiler-results.json')) }} {{ quote(node) }} tests/ansi.mjs {{ quote(interpreter) }} --full --compiler "$@"

# Load the upstream ANSI driver and run every test in one WASM interpreter.
[positional-arguments]
ansi-full *options: build
    SBCL={{ quote(sbcl) }} {{ quote(node) }} tests/ansi.mjs {{ quote(interpreter) }} --full "$@"

alias full-ansi := ansi-full

# Build a standalone HTML page from an existing full ANSI JSON report.
ansi-html report=(env('MIRICL_ANSI_REPORT', 'ansi-results.json')):
    {{ quote(node) }} scripts/build-ansi-html.mjs {{ quote(report) }} {{ quote(build_dir / 'ansi-results.html') }}

# Check report accounting, escaping, and offline browser interactions.
test-ansi-html:
    {{ quote(node) }} tests/ansi-html.mjs

# Run selected ANSI files with the incremental upstream cons helpers.
[positional-arguments]
ansi-cons +files: build
    {{ quote(node) }} tests/ansi.mjs {{ quote(interpreter) }} --helpers=cons "$@"

# Run project tests, Node integration tests, and the default ANSI profile.
check: test test-js ansi

# Build ASDF FASL bundles through both terminal hosts, without a browser.
test-bundle: build build-asdf
    {{ quote(node) }} tests/bundle-command.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }} {{ quote(build_dir / 'miricl-asdf.wasm') }}

# Compile and execute Lisp in Chromium using the built-in compiler.
test-browser-compiler: build
    {{ quote(node) }} tests/browser-compiler.mjs {{ quote(interpreter) }} {{ quote(build_dir / "miricl.js") }}
    {{ quote(node) }} tests/browser-compiler.mjs {{ quote(precompiled) }} {{ quote(build_dir / "miricl.js") }} --precompiled
