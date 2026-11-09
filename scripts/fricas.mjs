// FriCAS sessions in MIRICL; no native algebra or remote evaluation.
import {Miricl} from './miricl.mjs';
export const lispString = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
export const fricasInitialize = `(progn
  (setq *package* (find-package "BOOT") *default-pathname-defaults* #P"/fricas/")
  (boot::|initroot|)
  (boot::|interpsysInitialization| nil)
  (setq boot::|$has_category_hash| (make-hash-table :test #'equal)
        boot::|$operation_hash| (make-hash-table))
  (boot::|open_category_db| t) (boot::|open_operation_db| t) (boot::|open_browse_db| t)
  (values))`;
export async function createFriCAS(image, core, files, {progress = () => {}, ...host} = {}) {
  const vm = await Miricl.instantiate(image, host);
  try {
    for (const [name, bytes] of files) vm.mountFile(name, bytes);
    vm.mountFile('/fricas-core.wasm', core);
    progress('Loading FriCAS');
    await vm.evaluateAsync('(load "/fricas/lisp/fricas-package.lisp") (load "/fricas-core.wasm") (load "/fricas-port.lisp") (fricas-lisp::|miricl-host-services|) (values)', {eof: true});
    progress('Initializing FriCAS');
    const startup = await vm.evaluateAsync(fricasInitialize, {eof: true});
    progress('Ready');
    return {vm, startup: startup.output,
      async evaluate(source) {
        const start = performance.now();
        // parseAndInterpret returns internal domain graphs. Print only FriCAS's
        // own algebra output; exposing those graphs would also traverse cycles.
        const {output} = await vm.evaluateAsync(`(progn
          (catch 'fricas-lisp::miricl-fricas-quit
            (catch 'boot::SPAD_READER (catch 'boot::|top_level|
              (boot::|interpret_block| ${lispString(source + '\n')})))) (values))`, {eof: true});
        return {results: [], output, milliseconds: Math.round(performance.now() - start)};
      },
      close() {vm.close();},
    };
  } catch (error) {vm.close(); throw error;}
}
