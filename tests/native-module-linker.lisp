;;;; Exercise the production native-module linker without a compiler bootstrap.
(let* ((root (asdf:system-source-directory :miricl))
       (directory (merge-pathnames
                   (format nil "miricl-native-linker-~D-~D/" (get-universal-time)
                           (random most-positive-fixnum))
                   (uiop:temporary-directory)))
       (wat (merge-pathnames "linked.wat" directory))
       (wasm (merge-pathnames "linked.wasm" directory))
       (script (merge-pathnames "tests/native-module-linker.mjs" root)))
  (ensure-directories-exist wat)
  (uiop:run-program (list "node" "--max-old-space-size=256" (namestring script)
                          "prepare" (namestring wat))
                    :output *standard-output* :error-output *error-output*)
  (with-open-file (output wasm :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (write-sequence (wasmtime:wat-to-wasm (uiop:read-file-string wat)) output))
  (uiop:run-program (list "node" "--max-old-space-size=256" (namestring script)
                          "verify" (namestring wasm))
                    :output *standard-output* :error-output *error-output*))
