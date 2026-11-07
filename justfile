# Override these with environment variables or `just name=value <recipe>`.
sbcl := env('SBCL', 'sbcl')
node := env('NODE', 'node')
wasmtime := env('WASMTIME', 'wasmtime')
quicklisp := env('QUICKLISP_SETUP', env('HOME') / 'quicklisp/setup.lisp')
build_dir := env('MIRICL_BUILD_DIR', '.')
interpreter := build_dir / 'miricl-interpreter.wasm'
wasi_repl := build_dir / 'miricl-repl.wasm'

# List available commands.
default:
    @just --list

# Build the import-free interpreter and WASI REPL.
build:
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --script scripts/build-wasm.lisp {{ quote(build_dir) }}

# Report raw, gzip, Brotli, and per-section sizes for both images.
size: build
    {{ quote(node) }} scripts/wasm-size.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }}

# Build a self-contained browser REPL (miricl.html), including the interpreter.
build-html:
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --script scripts/build-html.lisp {{ quote(build_dir) }}

# Build the standalone CLOS TODO application (todo.html).
build-todo:
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --script scripts/build-todo.lisp {{ quote(build_dir) }}

# Prepare an independent application directory with runtime and library symlinks.
setup-game-of-life: build asdf-init build-asdf
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --script scripts/setup-application.lisp {{ quote(absolute_path("game-of-life")) }} game-of-life {{ quote(absolute_path(build_dir)) }} {{ quote(node) }}

# Build the FSet 2 example application FASL for its production HTML page.
build-game-of-life: setup-game-of-life
    just --justfile game-of-life/justfile build

# Build raw, gzip and Brotli size reports for the Wasm images and HTML pages.
build-sizes: build build-html build-todo
    {{ quote(node) }} scripts/build-size-html.mjs {{ quote(build_dir) }}

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

# Run all Node integration tests, including the WASI REPL.
test-js: test-threads test-standalone test-text test-repl-bridge test-development test-compilation test-asdf test-environment test-character-case test-reader-labels test-reader-buffer test-vector-reader test-array-reader test-print-circle test-output-buffer test-input-streams test-filesystem test-composite-streams test-pathnames test-declarations test-time test-float-properties test-derived-types test-sequence-transformations test-random test-javascript test-backtrace test-ansi-runner

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

# Also check the WASI REPL with the Wasmtime CLI.
test-wasmtime: build
    {{ quote(node) }} --experimental-wasm-wasmfx tests/text-repl.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }}

# Check macros, source loading, and guest development facilities.
test-development: build
    {{ quote(node) }} tests/development.mjs {{ quote(interpreter) }}

test-compilation: build
    {{ quote(node) }} tests/compilation.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/file-compilation.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/nested-backquote.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/structure-printers.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/readtable-iteration.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/nested-backquote.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/structure-printers.mjs {{ quote(interpreter) }}

# Exercise the FSet 2 application and its separate production assets in Chromium.
test-game-of-life: build-game-of-life
    {{ quote(node) }} tests/browser-game-of-life.mjs game-of-life/index.html

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

# Initialize the pinned upstream ANSI test submodule.
ansi-init:
    git submodule update --init tests/ansi-test

# Initialize the pinned upstream ASDF/UIOP sources.
asdf-init:
    git submodule update --init vendor/asdf

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

# Load the upstream ANSI driver and run every test in one WASM interpreter.
[positional-arguments]
ansi-full *options: build
    SBCL={{ quote(sbcl) }} {{ quote(node) }} tests/ansi.mjs {{ quote(interpreter) }} --full "$@"

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
