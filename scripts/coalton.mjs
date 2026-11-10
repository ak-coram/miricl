// The tour and Node tests use the same Coalton compiler and standard library.
import {Miricl} from './miricl.mjs';

export function coaltonInput(source, mode = 'lisp') {
  if (typeof source !== 'string') throw new TypeError('Expected source text.');
  if (!['lisp', 'expression', 'definitions'].includes(mode)) throw new Error('Unknown input mode.');
  const body = mode === 'expression' ? `(coalton:coalton\n${source}\n)`
    : mode === 'definitions' ? `(coalton:coalton-toplevel\n${source}\n)` : source;
  // Coalton's metadata-free REPL reader clones the remaining input stream.
  // Give it the complete source explicitly, and read each form only after the
  // preceding form has run (and possibly defined a package, reader or macro).
  // Reader labels share uninterned temporaries without capturing user globals.
  const text = `"${body.replace(/["\\]/gu, '\\$&')}"`;
  return `(cl:let* ((cl:*package* (cl:find-package :coalton-user))
      (cl:*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
      (coalton-impl/reader::*source* (coalton-impl/source:make-source-string ${text} :name "repl")))
    (cl:with-open-stream (#1=#:coalton-input
        (coalton-impl/source:source-stream coalton-impl/reader::*source*))
      (cl:loop :with #2=#:eof = (cl:list cl:nil)
        :for #3=#:form = (cl:read #1# cl:nil #2#)
        :until (cl:eq #3# #2#)
        :do (cl:dolist (#4=#:value (cl:multiple-value-list (cl:eval #3#)))
          (cl:prin1 #4#) (cl:terpri))))
    (cl:values))`;
}

export async function createCoalton(image, core, {progress = () => {}, ...host} = {}) {
  const vm = await Miricl.instantiate(image, host);
  try {
    progress('Loading Coalton compiler and standard library');
    vm.mountFile('/coalton.wasm', core);
    const startup = await vm.evaluateAsync('(declaim (declaration values)) (load "/coalton.wasm") (values)', {eof: true});
    await vm.evaluateAsync(coaltonInput('(cl:values)'), {eof: true});
    progress('Ready');
    return {startup: startup.output,
      evaluate: (source, {mode = 'lisp'} = {}) => vm.evaluateAsync(coaltonInput(source, mode), {eof: true}),
      close: () => vm.close()};
  } catch (error) {vm.close(); throw error;}
}
