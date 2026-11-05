# Override these with environment variables or `just name=value <recipe>`.
sbcl := env('SBCL', 'sbcl')
node := env('NODE', 'node')
wasmtime := env('WASMTIME', 'wasmtime')
quicklisp := env('QUICKLISP_SETUP', env('HOME') / 'quicklisp/setup.lisp')
build_dir := env('MIRICL_BUILD_DIR', '.')
interpreter := build_dir / 'interpreter.wasm'
wasi_repl := build_dir / 'miricl-repl.wasm'

# List available commands.
default:
    @just --list

# Build the import-free interpreter and standalone WASI REPL.
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

# Start the standalone interpreter's Node REPL.
repl: build
    {{ quote(node) }} scripts/repl.mjs {{ quote(interpreter) }}

# Start the standalone WASI REPL with Wasmtime.
repl-wasi: build
    {{ quote(wasmtime) }} run -W gc=y -W exceptions=y {{ quote(wasi_repl) }}

# Start the Common Lisp host REPL backed by Wasmtime.
repl-host:
    {{ quote(sbcl) }} --noinform --load {{ quote(quicklisp) }} --eval '(require :asdf)' --eval '(asdf:load-asd (truename "miricl.asd"))' --eval '(ql:quickload :miricl)' --eval '(miricl:repl)' --quit

# Run all Common Lisp project tests (FiveAM through ASDF).
test:
    {{ quote(sbcl) }} --noinform --non-interactive --load {{ quote(quicklisp) }} --eval '(require :asdf)' --eval '(asdf:load-asd (truename "miricl.asd"))' --eval '(ql:quickload :miricl/test)' --eval '(asdf:test-system :miricl)'

# Run all Node integration tests, including the WASI REPL.
test-js: test-standalone test-text test-development test-compilation test-environment test-character-case test-reader-labels test-reader-buffer test-vector-reader test-array-reader test-print-circle test-output-buffer test-input-streams test-filesystem test-composite-streams test-pathnames test-declarations test-time test-float-properties test-derived-types test-sequence-transformations test-random test-javascript test-ansi-runner

# Check the standalone module's object transport.
test-standalone: build
    {{ quote(node) }} tests/standalone.mjs {{ quote(interpreter) }}

# Check text evaluation and both Node and WASI REPLs.
test-text: build
    {{ quote(node) }} tests/text-repl.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }}

# Check the standalone HTML offline in headless Chromium (CHROMIUM overrides the executable).
test-browser: build-html
    {{ quote(node) }} tests/browser-repl.mjs {{ quote(build_dir / 'miricl.html') }}

# Check TODO interactions, persistence, offline startup, and mobile layout.
test-todo: build-todo
    {{ quote(node) }} tests/browser-todo.mjs {{ quote(build_dir / 'todo.html') }}

# Also check the WASI REPL with the Wasmtime CLI.
test-wasmtime: build
    MIRICL_TEST_WASMTIME=1 {{ quote(node) }} tests/text-repl.mjs {{ quote(interpreter) }} {{ quote(wasi_repl) }}

# Check macros, source loading, and guest development facilities.
test-development: build
    {{ quote(node) }} tests/development.mjs {{ quote(interpreter) }}

test-compilation: build
    {{ quote(node) }} tests/compilation.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/file-compilation.mjs {{ quote(interpreter) }}

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

# Check ANSI accounting and recovery from load errors and hanging tests.
test-ansi-runner: build
    {{ quote(node) }} tests/ansi-runner.mjs {{ quote(interpreter) }}
    {{ quote(node) }} tests/ansi-upstream-runner.mjs {{ quote(interpreter) }}

# Initialize the pinned upstream ANSI test submodule.
ansi-init:
    git submodule update --init tests/ansi-test

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
