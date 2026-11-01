;;;; Inventory the upstream suite without running its test bodies.
;;;; SBCL supplies source boundaries and generated test names; Miricl reads
;;;; the original source again and runs the tests itself.
(require :asdf)
(defpackage :miricl-ansi-inventory (:use :cl))
(in-package :miricl-ansi-inventory)

(defvar *root* (truename (first (uiop:command-line-arguments))))
(defvar *records* nil)
(defvar *names* nil)
(defvar *entries* nil)
(defvar *original-load* #'load)

(defun json-string (value stream)
  (write-char #\" stream)
  (loop for char across value
        do (case char
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (otherwise
              (if (< (char-code char) 32)
                  (format stream "\\u~4,'0X" (char-code char))
                  (write-char char stream)))))
  (write-char #\" stream))

(defun source-load (file &rest options)
  (declare (ignore options))
  (let* ((pathname (truename (merge-pathnames file)))
         (*load-pathname* pathname)
         (*load-truename* pathname)
         (*package* *package*)
         (*readtable* *readtable*)
         (source (uiop:read-file-string pathname)))
    (with-input-from-string (input source)
      (loop for start = (file-position input)
            for package = (package-name *package*)
            for form = (read input nil input)
            for end = (file-position input)
            until (eq form input)
            do (let ((*names* nil) (*entries* nil)
                     (previous *records*))
                 (eval form)
                 ;; Nested LOADs have already recorded their leaf forms.
                 ;; Do not execute the enclosing loader a second time in WASM.
                 (when (eq previous *records*)
                   (let ((names (nreverse *names*)))
                     (push (list (enough-namestring pathname *root*) package
                                 (subseq source start end) names names
                                 (when (cdr names)
                                   (loop for (name . entry) in *entries*
                                         for key = (cl-user::%ansi-test-key
                                                     (list (uiop:symbol-call :rt :form entry)
                                                           (uiop:symbol-call :rt :vals entry)))
                                         when key collect (cons name key))))
                           *records*)))))))
  t)

(funcall *original-load* (merge-pathnames "rt-package.lsp" *root*))
(funcall *original-load* (merge-pathnames "rt.lsp" *root*))
(funcall *original-load* (merge-pathnames "ansi-test-key.lisp" *load-truename*))

(let ((add-entry (symbol-function 'rt::add-entry)))
  (setf (symbol-function 'rt::add-entry)
        (lambda (entry)
          (let ((*package* (find-package :cl-user))
                (*print-case* :upcase)
                (*print-readably* t))
            (let ((name (prin1-to-string (rt::name entry))))
              (push name *names*) (push (cons name entry) *entries*)))
          (funcall add-entry entry))))

;; The upstream loaders call these through CL-USER. No FASLs are needed
;; for an inventory, and no files in the upstream submodule are modified.
(setf (symbol-function 'cl-user::compile-and-load)
      (lambda (file &key force)
        (declare (ignore force))
        (source-load (merge-pathnames file *load-pathname*))))
(setf (symbol-function 'cl-user::compile-and-load*)
      (lambda (file &key force)
        (declare (ignore force))
        (source-load (merge-pathnames file (merge-pathnames "auxiliary/" *root*)))))

;; Intercept LOAD only in this disposable SBCL process. This preserves
;; upstream load order, dynamic pathname bindings, and generated tests.
(sb-ext:unlock-package :cl)
(setf (symbol-function 'cl:load) #'source-load)
(let ((*default-pathname-defaults* *root*)
      (*features* '(:miricl :common-lisp :ansi-cl :ieee-floating-point))
      (*package* (find-package :cl-user))
      (*compile-verbose* nil)
      (*load-verbose* nil)
      (*standard-output* (make-broadcast-stream)))
  (source-load "cl-test-package.lsp")
  (setf *package* (find-package :cl-test))
  (dolist (file '("auxiliary/ansi-aux-macros.lsp" "universe.lsp"
                  "auxiliary/random-aux.lsp" "auxiliary/ansi-aux.lsp"
                  "cl-symbol-names.lsp" "notes.lsp" "gclload2.lsp"))
    (source-load file)))

;; A failed or incomplete host inventory must never become a misleading
;; compliance denominator. Also catch lost registrations in nested loaders.
;; RT replaces duplicate names. Keep the final definition, as RT would.
(let ((seen (make-hash-table :test 'equal)))
  (dolist (record *records*)
    (setf (fourth record)
          (remove-if (lambda (name)
                       (let ((key (read-from-string name)))
                         (prog1 (gethash key seen) (setf (gethash key seen) t))))
                     (fourth record)))))
(assert (= (length (cdr rt::*entries*))
           (loop for record in *records* sum (length (fourth record)))))
(assert (plusp (length (cdr rt::*entries*))))

(with-open-file (output (second (uiop:command-line-arguments))
                        :direction :output :if-exists :supersede
                        :external-format :utf-8)
  (write-char #\[ output)
  (loop for (file package source names definitions keys) in (nreverse *records*)
        for first = t then nil
        do (unless first (write-char #\, output))
           (write-string "{\"file\":" output) (json-string file output)
           (write-string ",\"package\":" output) (json-string package output)
           (write-string ",\"source\":" output) (json-string source output)
           (write-string ",\"tests\":[" output)
           (loop for name in names for first-name = t then nil
                 do (unless first-name (write-char #\, output))
                    (json-string name output))
           (write-string "],\"definitions\":[" output)
           (loop for name in definitions for first-name = t then nil
                 do (unless first-name (write-char #\, output))
                    (json-string name output))
           (write-string "],\"testKeys\":{" output)
           (loop for (name . key) in keys for first-key = t then nil
                 do (unless first-key (write-char #\, output))
                    (json-string name output) (write-char #\: output) (json-string key output))
           (write-string "}}" output))
  (write-char #\] output))
