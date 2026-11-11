// The browser REPL and Node tests use the same Coalton compiler and library.
import {Miricl} from './miricl.mjs';

export function coaltonInput(source, mode = 'lisp', {persistentPackage = false} = {}) {
  if (typeof source !== 'string') throw new TypeError('Expected source text.');
  if (!['lisp', 'expression', 'definitions'].includes(mode)) throw new Error('Unknown input mode.');
  const body = mode === 'expression' ? `(coalton:coalton\n${source}\n)`
    : mode === 'definitions' ? `(coalton:coalton-toplevel\n${source}\n)` : source;
  // Coalton's metadata-free REPL reader clones the remaining input stream.
  // Give it the complete source explicitly, and read each form only after the
  // preceding form has run (and possibly defined a package, reader or macro).
  // Reader labels share uninterned temporaries without capturing user globals.
  const text = `"${body.replace(/["\\]/gu, '\\$&')}"`;
  const persistent = persistentPackage && mode === 'lisp';
  return `(cl:let* ((cl:*package* ${persistent ? 'miricl::%coalton-repl-package' : '(cl:find-package :coalton-user)'})
      (cl:*readtable* ${persistent ? 'miricl::%coalton-repl-readtable' : "(named-readtables:ensure-readtable 'coalton:coalton)"})
      (coalton-impl/reader::*source* (coalton-impl/source:make-source-string ${text} :name "repl")))
    ${persistent ? '(cl:unwind-protect' : ''}
    (cl:with-open-stream (#1=#:coalton-input
        (coalton-impl/source:source-stream coalton-impl/reader::*source*))
      (cl:loop :with #2=#:eof = (cl:list cl:nil)
        :for #3=#:form = (cl:read #1# cl:nil #2#)
        :until (cl:eq #3# #2#)
        :do (cl:dolist (#4=#:value (cl:multiple-value-list (cl:eval #3#)))
          (cl:prin1 #4#) (cl:terpri))))
    ${persistent ? `(cl:setq miricl::%coalton-repl-package cl:*package*
                            miricl::%coalton-repl-readtable cl:*readtable*))` : ''}
    (cl:values))`;
}

export async function createCoalton(image, core, {progress = () => {}, replPackage, ...host} = {}) {
  if (core == null) progress('Restoring Coalton compiler and standard library');
  const vm = await Miricl.instantiate(image, host);
  try {
    let startup = {output: ''};
    if (core != null) {
      progress('Loading Coalton compiler and standard library');
      vm.mountFile('/coalton.wasm', core);
      startup = await vm.evaluateAsync(`(declaim (declaration values)) (load "/coalton.wasm")
        (delete-file "/coalton.wasm") (values)`, {eof: true});
    } else {
      await vm.evaluateAsync(`(unless (and (find-package :coalton) (find-package :coalton-user))
        (error "Expected an initialized Coalton image."))
        ;; Also support snapshots made before the core restart rebuilt this
        ;; process's public main-thread object against the restored heap.
        (miricl-threads::%thread 2 (miricl-threads::%thread 0)
          (miricl-threads::%new-thread (miricl-threads::%thread 0) "Main thread"))
        (values)`, {eof: true});
    }
    await vm.evaluateAsync(coaltonInput('(cl:values)'), {eof: true});
    if (replPackage !== undefined) {
      if (typeof replPackage !== 'string') throw new TypeError('Expected an initial Lisp package name.');
      const name = `"${replPackage.replace(/["\\]/gu, '\\$&')}"`;
      await vm.evaluateAsync(`(cl:defparameter miricl::%coalton-repl-package
          (cl:or (cl:find-package ${name}) (cl:error "Unknown REPL package")))
        (cl:defparameter miricl::%coalton-repl-readtable
          (named-readtables:ensure-readtable 'coalton:coalton)) (cl:values)`, {eof: true});
    }
    progress('Ready');
    return {startup: startup.output,
      evaluate: (source, {mode = 'lisp'} = {}) => vm.evaluateAsync(coaltonInput(source, mode,
        {persistentPackage: replPackage !== undefined}), {eof: true}),
      debugCommand: (stop, command) => vm.debugCommand(stop, command),
      saveImage: () => vm.saveImage(), close: () => vm.close()};
  } catch (error) {vm.close(); throw error;}
}
