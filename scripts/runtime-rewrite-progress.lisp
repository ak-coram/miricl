;;;; Source inventory, not a completion percentage or a test-count assertion.
;;;; sbcl --load quicklisp/setup.lisp --script scripts/runtime-rewrite-progress.lisp [output.json]
(require :asdf)
(defparameter *rewrite-root*
  (uiop:pathname-parent-directory-pathname (uiop:pathname-directory-pathname *load-truename*)))
(unless (and (find-package :miricl) (fboundp (find-symbol "INTERPRETER-WASM" :miricl)))
  (asdf:load-asd (merge-pathnames "miricl.asd" *rewrite-root*))
  (if (find-package :ql) (uiop:symbol-call :ql :quickload :miricl) (asdf:load-system :miricl)))

(defun rewrite-json-string (value out)
  (write-char #\" out)
  (loop for char across value do
    (case char
      (#\" (write-string "\\\"" out)) (#\\ (write-string "\\\\" out))
      (#\Newline (write-string "\\n" out)) (#\Return (write-string "\\r" out))
      (#\Tab (write-string "\\t" out))
      (otherwise (if (< (char-code char) 32) (format out "\\u~4,'0X" (char-code char))
                     (write-char char out)))))
  (write-char #\" out))

(defun rewrite-kernel-call (form)
  (let ((pending (list form)) (seen (make-hash-table :test 'eq)))
    (loop while pending for item = (pop pending) do
      (when (and (consp item) (not (gethash item seen)))
        (setf (gethash item seen) t)
        (when (eq (car item) 'miricl-cross:runtime-kernel-wat) (return item))
        (push (car item) pending) (push (cdr item) pending)))))

(defun write-runtime-rewrite-progress (output)
  (let ((definitions nil) (handwritten 0) (compiled 0) (generators 0))
    (dolist (file (sort (directory (merge-pathnames "runtime-*.lisp" *rewrite-root*)) #'string< :key #'namestring))
      (let* ((*package* (find-package :miricl)) (*read-eval* nil)
             (*readtable* (named-readtables:find-readtable :miricl-wat))
             (source (uiop:read-file-string file :external-format :utf-8)))
        (with-input-from-string (in source)
          (loop for start = (file-position in) for form = (read in nil in) until (eq form in) do
            (when (and (consp form) (symbolp (second form))
                       (or (eq (car form) 'miricl::define-runtime-function)
                           (and (eq (car form) 'defun)
                                (uiop:string-prefix-p "$" (symbol-name (second form))))))
              (let* ((kernel (rewrite-kernel-call form))
                     (kind (cond (kernel (incf compiled) "compiled-cl")
                                 ((eq (car form) 'miricl::define-runtime-function)
                                  (incf handwritten) "handwritten-wat")
                                 (t (incf generators) "other-wat-generator"))))
                (push (list (file-namestring file) (string-downcase (symbol-name (second form))) kind
                            (1+ (count #\Newline source :start start :end (file-position in)))
                            (and kernel (prin1-to-string (third kernel)))) definitions)))))))
    (ensure-directories-exist output)
    (with-open-file (out output :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "{~%  ~S: 1,~%  ~S: ~S,~%  ~S: ~D,~%  ~S: ~D,~%  ~S: ~D,~%  ~S: [~%"
        "schemaVersion" "scope" "Runtime source definitions; excludes the precompiled compiler and interpreted CL libraries. Counts are not percentages of work remaining; native substrate and generated wrappers are included separately."
        "handwrittenDefinitions" handwritten "compiledClKernels" compiled "otherWatGenerators" generators "definitions")
      (loop for (file name kind lines entry) in (nreverse definitions) for first = t then nil do
        (unless first (write-string ",
" out))
        (write-string "    {\"file\":" out) (rewrite-json-string file out)
        (write-string ",\"name\":" out) (rewrite-json-string name out)
        (write-string ",\"kind\":" out) (rewrite-json-string kind out)
        (format out ",~S:~D" "sourceLinesIncludingComments" lines)
        (when entry (write-string ",\"entry\":" out) (rewrite-json-string entry out))
        (write-char #\} out))
      (format out "~%  ]~%}~%"))
    (format t "Runtime rewrite: ~D compiled CL kernels, ~D handwritten WAT definitions, ~D other WAT generators.~%Wrote ~A~%"
      compiled handwritten generators output)))

(when (uiop:command-line-arguments)
  (write-runtime-rewrite-progress (first (uiop:command-line-arguments))))
