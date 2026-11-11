// ACL2's logical top level in a browser/Node MIRICL session.
import {Miricl} from './miricl.mjs';
import {Acl2Books} from './acl2-books.mjs';
const lispString = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';

export async function createAcl2(image, core, files, {progress = () => {}, booksURL, ...host} = {}) {
  let accepted = false, busy = false, closed = false;
  let books;
  if (!core) progress('Restoring ACL2 logical world');
  const vm = await Miricl.instantiate(image, {...host, imports: {...host.imports,
    acl2Browser: {result(value) {accepted = value;}}}});
  const evaluate = source => vm.evaluateAsync(source, {eof: true});
  try {
    for (const [name, bytes] of files) vm.mountFile(name, bytes);
    if (booksURL) {
      vm.mountFile('/acl2/books/.miricl-books', new Uint8Array());
      books = new Acl2Books(vm, booksURL);
    }
    if (core) {
      vm.mountFile('/acl2.core', core);
      progress('Loading ACL2');
      await evaluate(`
        (setq *default-pathname-defaults* #P"/acl2/")
        (pushnew :cltl2 *features*) (pushnew :unix *features*)
        (load "init.lisp")
        (acl2::proclaim-optimize)
        (let ((*readtable* acl2::*acl2-readtable*)) (load "/acl2.core"))
        (set 'acl2::state acl2::*the-live-state*)
        (delete-file "/acl2.core")
        (values)`);
      progress('Initializing ACL2 logical world');
    }
    const startup = await evaluate(`
      (in-package :acl2)
      ${booksURL ? `(setf acl2::*miricl-environment*
        (acons "ACL2_SYSTEM_BOOKS" "/acl2/books/"
          (remove "ACL2_SYSTEM_BOOKS" acl2::*miricl-environment* :key #'car :test #'string=)))` : ''}
      ${core ? '(unless (acl2::initialize-acl2) (error "ACL2 world initialization did not complete."))' : ''}
      (with-open-file (stream acl2::*acl2-status-file*)
        (unless (eq (read stream) :initialized) (error "ACL2 did not mark its world initialized.")))
      (js:define-import cl-user::acl2-browser-result ("acl2Browser" "result") (:boolean) :void)
      ${core ? `(acl2::set-initial-cbd)
      (multiple-value-bind (erp val state)
          (acl2::ld '((acl2::logic)) :ld-skip-proofsp nil :ld-error-action :return
            :ld-prompt nil :ld-verbose nil :ld-pre-eval-print nil :ld-post-eval-print nil)
        (declare (ignore state))
        (when (or erp (eq val :error) (and (consp val) (eq (car val) :stop-ld)))
          (error "ACL2 could not enter logic mode.")))` : ''}
      (values)`);
    progress('Ready');
    return {vm, startup: startup.output,
      async evaluate(source) {
        if (typeof source !== 'string') throw new TypeError('ACL2 input must be text.');
        if (closed) throw new Error('The ACL2 session is closed.');
        if (busy) throw new Error('ACL2 is already evaluating an input.');
        busy = true; accepted = false;
        const start = performance.now();
        try {
          // LD reads the submitted text itself, including colon commands.
          // Text remains stream data and is never inserted as raw Lisp code.
          // Only LD prints results; discard the adapter's intermediate values.
          const {output} = await evaluate(`
            (progn
              (with-open-file (stream "/acl2/browser-input.lsp" :direction :output :if-exists :supersede)
                (write-string ${lispString(source + '\n')} stream))
              (multiple-value-bind (erp val state)
                  (acl2::ld "/acl2/browser-input.lsp" :ld-skip-proofsp nil
                    :ld-error-action :return :ld-prompt nil :ld-pre-eval-print nil
                    :ld-post-eval-print t :ld-verbose nil)
                (declare (ignore state))
                (cl-user::acl2-browser-result
                  (not (or erp (eq val :error)
                           (and (consp val) (eq (car val) :stop-ld))))))
              (values))`);
          return {results: [], output, failed: !accepted, milliseconds: Math.round(performance.now() - start)};
        } catch (error) {
          // A host stack/engine trap can bypass Lisp cleanup. Require a fresh
          // worker instead of continuing with potentially damaged state.
          if (error.code === 0) {closed = true; books?.close(); vm.close(); error.fatal = true;}
          throw error;
        } finally {busy = false;}
      },
      close() {closed = true; books?.close(); vm.close();},
    };
  } catch (error) {books?.close(); vm.close(); throw error;}
}
