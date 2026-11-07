# Goals

This project has two main goals: provide an interactive development
environment for developing Common Lisp programs targetting WASM and a
whole program optimizing compiler also targetting WASM. The host
Common Lisp system is intented to host the compiler and bootstrap the
interpreter running inside the WASM environment.

While providing an interactive environment for development, the
intention is to ship small and efficient binaries using the
compiler. The interpreter can also be used to extend programs
dynamically at runtime.

The Common Lisp type system shall be used for optimization and for
generating smaller binaries.

Eventual goal is full ANSI Common Lisp compliance.

# Common Lisp semantics

Consult the [Common Lisp HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
when implementing new Common Lisp functions. Check the relevant entries
for argument and return types, multiple values, copying and sharing,
side effects, and exceptional situations before implementing them.
Use those requirements to guide the implementation and regression tests.

# Performance

Prefer direct WASM implementations for performance-critical functions,
especially frequently used primitives and allocation or traversal loops.
Use the interpreter for functions where its performance impact is
negligible. Follow the project's Common Lisp WAT integration style for
direct WASM implementations.

# Style

WASM supports a Lisp-style syntax, the intention is to integrate WASM
modules into the Common Lisp source code using reader macros, regular
macros and hand over compilation and some optimizations to the
wasmtime library.

The wasmtime runtime also serves for running tests, the REPL by
default. The main target is the browser. Use only stable WASM and WASI
features; do not rely on experimental proposals or features requiring
experimental runtime flags. Advanced, stable WASM features such as
WasmGC should be used where applicable.

Explicit exception: native WASM stack switching is permitted for green
threads. Its experimental runtime flag and the current lack of ARM
support in the WASI adapter are accepted for now. This exception does
not permit other unstable WASM or WASI features.

# Other

When you're told to keep working, don't stop at arbitrary points: keep
working indefinitely as long there are any parts of the task
remaining.
