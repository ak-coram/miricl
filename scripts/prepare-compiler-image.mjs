// Build-time only: compile the compiler with itself and translate the small
// instruction vocabulary emitted by our backend into the runtime's WAT.
import fs from 'node:fs/promises';
import {Miricl} from './miricl.mjs';
import {linkNativeModules} from './native-module-linker.mjs';
import {collectCompilationStatistics} from './compilation-statistics.mjs';

const [bootstrap, output] = process.argv.slice(2);
if (!bootstrap || !output) throw new Error('Usage: node prepare-compiler-image.mjs BOOTSTRAP OUTPUT-PREFIX');
const vm = await Miricl.instantiate(await fs.readFile(bootstrap));
try {
  const bootstrapStarted = performance.now();
  await collectCompilationStatistics(vm, {workload: 'compiler-bootstrap', phase: 'bootstrap native functions'}, () => vm.evaluate(`(in-package :miricl-compiler)
    (setq *compiler-bootstrapping* t)
    (defparameter *image-sources* (mapcar (lambda (name)
      (list name (compiler-source-policy (compiler-function-source name)))) *native-compiler-seeds*))
    ;; Capture the same original runtime corpus before compiler preparation.
    ;; Runtime functions retain their declarations, including SAFETY and DEBUG.
    (defparameter *image-runtime-sources*
      (mapcar (lambda (name)
        (multiple-value-bind (source captured) (function-lambda-expression (fdefinition name))
          (unless (and source (not captured))
            (error "Native runtime image function has no closed source: ~S" name))
          (list name source))) *native-runtime-image-functions*))
    (multiple-value-bind (installed unsupported) (compile-compiler)
      (unless (and (= (length installed) (length *image-sources*)) (null unsupported))
        (error "Compiler bootstrap failed: ~S" unsupported)))
    (values)`, {eof: true}));
  const interpretedBootstrapMs = performance.now() - bootstrapStarted;
  const moduleStarted = performance.now();
  await collectCompilationStatistics(vm, {workload: 'compiler-bootstrap', phase: 'sealed compiler module'}, () => vm.evaluate(`
    (progv (mapcar (lambda (name) (compiler-primitive 10 1 name))
        '("%FASL-RECORDS" "%FASL-SEEN" "%FILE-LOAD-VALUES"))
      (list (make-array 32 :adjustable t :fill-pointer 0) (make-hash-table :test 'eq) nil)
      (let ((names nil) (codes nil) (roots nil)
            (dump (symbol-function (compiler-primitive 10 1 "%FASL-DUMP"))))
        ;; This second compilation uses the already self-compiled frontend and emitter.
        (dolist (entry *image-sources*)
          (push (list (and (symbol-package (first entry)) (package-name (symbol-package (first entry))))
                      (symbol-name (first entry))) names))
        ;; The compiler owns its sealed internal bindings. Runtime entries stay
        ;; in separate singleton modules; private runtime calls retain their live
        ;; bindings while standard CL calls use their ANSI definitions. Factories preserve
        ;; the actual private symbols and general (including SETF) function names.
        (dolist (sources (cons *image-sources* (mapcar #'list *image-runtime-sources*)))
          (let* ((*wasm-bootstrap-direct-workers* t)
                 (unit (compile-module-to-hir
                        (mapcar (lambda (entry) (list* 'defun (first entry) (cdr (second entry)))) sources)
                        :inline nil)))
            (multiple-value-bind (code literals) (emit-native-module-checked unit nil)
            ;; One factory shares the module's literals and closes over its
            ;; internal function bindings. Preserve only its literal graph;
            ;; runtime startup creates and installs the native entry points.
            (let ((entries (funcall (compiler-primitive 17 code literals nil))))
              ;; Validate and instantiate the output without installing another
              ;; compiler generation. Keep compiling the fixed original corpus.
              (unless (every (lambda (entry) (compiled-function-p (cdr entry))) entries)
                (error "Module compiler output contains an uncompiled entry")))
            (unless (= 42 (funcall (compile-native '(lambda (x) (+ x 2))) 40))
              (error "Module-compiled compiler failed its compilation check"))
            (push (coerce code 'list) codes)
              (push (funcall dump (list :module literals)) roots))))
        (with-open-file (out "/compiler-image.lisp" :direction :output :if-exists :supersede)
          (let ((*print-readably* t) (*print-pretty* nil) (*print-circle* t) (*print-base* 10) (*print-radix* nil))
            (write (list (nreverse names) (nreverse codes) (nreverse roots)
              (coerce (symbol-value (compiler-primitive 10 1 "%FASL-RECORDS")) 'list)) :stream out)))))
    (values)`, {eof: true}));
  const nativeModuleAndManifestMs = performance.now() - moduleStarted;
  console.log(`Compiler phase times: ${JSON.stringify({interpretedBootstrapMs, nativeModuleAndManifestMs,
    statisticsEnabled: Boolean(process.env.MIRICL_COMPILER_STATISTICS_DIRECTORY)})}`);
  const image = await vm.readFile('/compiler-image.lisp');
  // A JSON copy of the code vectors avoids implementing a Lisp reader in JS.
  const codeOutput = vm.evaluate(`(progn
    (with-open-file (in "/compiler-image.lisp")
      (let ((codes (second (read in))))
        (write-string "[")
        (loop for code in codes for first = t then nil do
          (unless first (write-string ",")) (write-string "[")
          (loop for byte in code for first = t then nil do
            (unless first (write-string ",")) (princ byte)) (write-string "]"))
        (write-string "]"))) (values))`, {eof: true}).output;
  const codes = JSON.parse(codeOutput);
  const {wat, declarations, callCounts} = linkNativeModules(codes);
  await fs.writeFile(`${output}.wat`, wat);
  await fs.writeFile(`${output}.lisp`, image);
  console.log(`Self-compiled compiler: ${codes.length} module, ${declarations.length} bodies, ${codes.reduce((n, code) => n + code.length, 0)} module bytes before linking.`);
  console.log(`Compiler typed call sites: ${JSON.stringify(Object.fromEntries([...callCounts]
    .filter(([name]) => /^(?:native-vector-(?:ref|set)|native-car-list|native-cdr-list|native-(?:add|sub|mul)-int64)/u.test(name))))}`);
} finally {vm.close();}
