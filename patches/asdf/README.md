These patches port the pinned upstream ASDF/UIOP sources to miricl. Apply them
in `series` order to a copy of `vendor/asdf`, before running `make-asdf.sh`.
`just build-asdf` performs these steps and writes `build/asdf/asdf.lisp`.
The submodule stays unchanged, and a failed patch stops the build.

The initial port supports loading systems and compiling Lisp components in
miricl's mounted filesystem, including host files read through the dev server.
Compiler output stays in the browser overlay. UIOP uses POSIX pathname syntax,
Unicode strings and virtual working directory. The guest has no process
environment or command-line arguments; launching processes and dumping native
images remain unsupported. Compiled files are miricl FASLs, not standalone
application executables.

`0002-fasl-bundles.patch` implements UIOP's `combine-fasls`, enabling standard
`compile-bundle-op`, `monolithic-compile-bundle-op` and their load operations.
A bundle has one Wasm
header and an ordered `miricl.fasl` custom section per compilation unit;
combining bundles flattens their sections. The writer belongs to optional
UIOP. The base interpreter's `load` reads individual FASLs and bundles without
ASDF, keeping each unit's object graph, package and readtable bindings separate.

Keep implementation-specific UIOP changes here. Fix Common Lisp conformance
issues in the interpreter itself. Remove patches as equivalent support is
accepted upstream, updating the submodule revision and tests together.

`just test-asdf` checks preparation failures and runs upstream ASDF in the
interpreter, including recursive system discovery, dependencies, compilation,
cached loads, forced rebuilds, and loading generated FASLs and application
bundles without ASDF.
