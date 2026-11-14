;;;; Shared builder for standalone, offline browser pages.
(require :asdf)

(defun html-repl-base64 (bytes)
  "Encode image payload octets without adding a build dependency."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (out)
      (loop for start from 0 below (length bytes) by 3
            for remaining = (- (length bytes) start)
            for bits = (logior (ash (aref bytes start) 16)
                               (if (> remaining 1) (ash (aref bytes (1+ start)) 8) 0)
                               (if (> remaining 2) (aref bytes (+ start 2)) 0))
            do (write-char (char alphabet (ldb (byte 6 18) bits)) out)
               (write-char (char alphabet (ldb (byte 6 12) bits)) out)
               (write-char (if (> remaining 1) (char alphabet (ldb (byte 6 6) bits)) #\=) out)
               (write-char (if (> remaining 2) (char alphabet (ldb (byte 6 0) bits)) #\=) out)))))

(defparameter *browser-page-root*
  (uiop:pathname-parent-directory-pathname (uiop:pathname-directory-pathname *load-truename*)))

(defun browser-compressed-image (bytes)
  "Embed a level-9 gzip image; MIRICL.INSTANTIATE decompresses it in the browser."
  (uiop:with-temporary-file (:stream out :pathname image :type "wasm"
                                    :element-type '(unsigned-byte 8))
    (write-sequence bytes out)
    (finish-output out)
    (let ((compressed (pathname (concatenate 'string (namestring image) ".gz"))))
      (unwind-protect
          (progn
            (uiop:run-program
              (list (or (uiop:getenv "NODE") "node")
                    (namestring (merge-pathnames "scripts/compress-images.mjs" *browser-page-root*))
                    (namestring image)))
            (with-open-file (in compressed :element-type '(unsigned-byte 8))
              (let ((payload (make-array (file-length in) :element-type '(unsigned-byte 8))))
                (read-sequence payload in)
                (html-repl-base64 payload))))
        (uiop:delete-file-if-exists compressed)))))

(defun browser-inline-module (pathname)
  "Embed these project modules together, resolving their static imports locally."
  (let ((source
          (with-output-to-string (out)
            (dolist (line (uiop:read-file-lines pathname :external-format :utf-8))
              (unless (uiop:string-prefix-p "import " line)
                (write-line (if (uiop:string-prefix-p "export " line)
                                (subseq line (length "export ")) line)
                            out))))))
    (when (search "</script" source :test #'char-equal)
      (error "Embedded module ~A contains an HTML script terminator." pathname))
    source))

(defun build-browser-page (template-name output-name output-directory &key (compiler t))
  (let* ((root *browser-page-root*)
         (output (uiop:ensure-directory-pathname output-directory))
         (path (merge-pathnames output-name output))
         (template (uiop:read-file-string (merge-pathnames template-name root)
                                          :external-format :utf-8))
         ;; Use the adapter as a classic script, both in workers and on the
         ;; main thread, so the resulting page also runs directly from file://.
         (adapter (with-output-to-string (out)
                    (dolist (line (uiop:read-file-lines (merge-pathnames "scripts/miricl.mjs" root)
                                                       :external-format :utf-8))
                      (write-line (if (uiop:string-prefix-p "export class " line)
                                      (subseq line (length "export ")) line)
                                  out)))))
    (asdf:load-asd (merge-pathnames "miricl.asd" root))
    (if (find-package :ql) (uiop:symbol-call :ql :quickload :miricl) (asdf:load-system :miricl))
    ;; The adapter is embedded in a script element.
    (when (search "</script" adapter :test #'char-equal)
      (error "The JavaScript adapter contains an HTML script terminator."))
    (let ((replacements
            (list (cons "@@MIRICL_ADAPTER@@" adapter)
                  (cons "@@MIRICL_WASM@@"
                        (browser-compressed-image (uiop:symbol-call :miricl :interpreter-wasm
                                                                  :compiler compiler :native-compiler compiler))))))
      (dolist (replacement replacements)
        (let ((start (search (car replacement) template)))
          (unless (and start (not (search (car replacement) template :start2 (1+ start))))
            (error "Expected exactly one ~A marker in the HTML template." (car replacement)))))
      (dolist (entry '(("@@MIRICL_SESSION_STORE@@" . "app-common/session-store.mjs")
                       ("@@MIRICL_REPL_EXAMPLES@@" . "scripts/repl-examples.mjs")
                       ("@@MIRICL_REPL_DEBUGGER@@" . "scripts/repl-debugger.mjs")
                       ("@@MIRICL_REPL_CLIENT@@" . "scripts/repl-client.mjs")
                       ("@@MIRICL_SESSION_APP@@" . "app-common/app.mjs")))
        (when (search (car entry) template)
          (push (cons (car entry) (browser-inline-module (merge-pathnames (cdr entry) root)))
                replacements)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                               :external-format :utf-8)
        (loop with position = 0
              for start = (search "@@MIRICL_" template :start2 position)
              do (write-string template out :start position :end start)
              while start
              do (let ((replacement (find-if (lambda (entry)
                                               (let ((end (+ start (length (car entry)))))
                                                 (and (<= end (length template))
                                                      (string= (car entry) template
                                                               :start2 start :end2 end))))
                                             replacements)))
                   (unless replacement (error "Unknown HTML template marker at ~D." start))
                   (write-string (cdr replacement) out)
                   (setf position (+ start (length (car replacement))))))))
    (format t "Wrote ~A (~:D bytes). Open it directly in a browser.~%"
            path (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in)))))
