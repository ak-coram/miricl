;;;; sbcl --load ~/quicklisp/setup.lisp --script scripts/build-html.lisp [output-directory]
(require :asdf)

(defun html-repl-base64 (bytes)
  "Encode the WASM octets without adding a build dependency."
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

(let* ((root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-directory-pathname *load-truename*)))
       (output (uiop:ensure-directory-pathname
                (or (first (uiop:command-line-arguments)) (uiop:getcwd))))
       (path (merge-pathnames "miricl.html" output))
       (template (uiop:read-file-string (merge-pathnames "scripts/repl.html" root)
                                        :external-format :utf-8))
       ;; A classic Blob worker works from file:// as well as HTTP. Drop the
       ;; adapter's ESM class exports; the worker uses the classes directly.
       (adapter (with-output-to-string (out)
                  (dolist (line (uiop:read-file-lines (merge-pathnames "scripts/miricl.mjs" root)
                                                     :external-format :utf-8))
                    (write-line (if (uiop:string-prefix-p "export class " line)
                                    (subseq line (length "export ")) line)
                                out)))))
  (asdf:load-asd (merge-pathnames "miricl.asd" root))
  (if (find-package :ql) (uiop:symbol-call :ql :quickload :miricl) (asdf:load-system :miricl))
  ;; The adapter is embedded in the worker's inert script element.
  (when (search "</script" adapter :test #'char-equal)
    (error "The JavaScript adapter contains an HTML script terminator."))
  (let ((replacements
          (list (cons "@@MIRICL_ADAPTER@@" adapter)
                (cons "@@MIRICL_WASM@@"
                      (html-repl-base64 (uiop:symbol-call :miricl :interpreter-wasm))))))
    (dolist (replacement replacements)
      (let ((start (search (car replacement) template)))
        (unless (and start (not (search (car replacement) template :start2 (1+ start))))
          (error "Expected exactly one ~A marker in the HTML template." (car replacement)))))
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
          path (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))))
