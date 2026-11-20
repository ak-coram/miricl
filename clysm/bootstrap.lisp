;;;; Build Clysm's browser REPL inside MIRICL, from the mounted Lisp sources.
;;;; Each STEP is one source file or assembly phase, so the host can report
;;;; progress without putting the bootstrap algorithm in JavaScript.

(defpackage #:miricl-clysm
  (:use #:cl)
  (:shadow #:step)
  (:export #:start #:step #:status #:bootstrap))
(in-package #:miricl-clysm)

(defvar *root* #p"/clysm/")
(defvar *output* #p"/clysm.wasm")
(defvar *tasks* nil)
(defvar *started* nil)
(defvar *failure* nil)

(defun source-name-p (name)
  (and (stringp name) (plusp (length name)) (< (length name) 128)
       (not (char= (char name 0) #\/))
       (not (char= (char name (1- (length name))) #\/))
       (not (search "//" name))
       (every (lambda (c) (or (find c "abcdefghijklmnopqrstuvwxyz0123456789-/") nil)) name)))

(defun read-source-list (path)
  "Read only a bounded list of source-name strings, never general reader syntax."
  (with-open-file (in path :external-format :utf-8)
    (let ((size (file-length in)))
      (when (> size 65536)
        (error "Clysm source manifest ~A exceeds its size limit." path))
      (let* ((source (make-string size))
             (end (read-sequence source in))
             (position 0)
             (names nil)
             (count 0))
        (labels ((reject (reason)
                   (error "Invalid Clysm source manifest ~A at character ~D: ~A."
                          path position reason))
                 (skip-space ()
                   (loop while (< position end)
                         for char = (char source position)
                         do (cond
                              ((find char '(#\Space #\Tab #\Newline #\Return #\Page))
                               (incf position))
                              ((char= char #\;)
                               (loop while (and (< position end)
                                                (not (char= (char source position) #\Newline)))
                                     do (incf position)))
                              (t (return)))))
                 (take-char (expected)
                   (unless (and (< position end) (char= (char source position) expected))
                     (reject (format nil "expected ~S" expected)))
                   (incf position)))
          ;; The grammar excludes nesting, escapes, labels and reader macros
          ;; before they can consume reader stack or construct cyclic objects.
          (unless (eq (read-char in nil in) in)
            (reject "manifest grew beyond its size limit while being read"))
          (skip-space)
          (take-char #\()
          (loop
            (skip-space)
            (when (>= position end) (reject "missing closing parenthesis"))
            (when (char= (char source position) #\)) (incf position) (return))
            (when (>= count 1024) (reject "too many source names"))
            (take-char #\")
            (let ((start position))
              (loop while (and (< position end) (not (char= (char source position) #\")))
                    do (when (>= (- position start) 127) (reject "source name is too long"))
                       (incf position))
              (let ((name (subseq source start position)))
                (take-char #\")
                (unless (source-name-p name) (reject "invalid relative source name"))
                (when (member name names :test #'string=) (reject "duplicate source name"))
                (push name names)
                (incf count))))
          (when (zerop count) (reject "expected at least one source name"))
          (skip-space)
          (unless (= position end) (reject "unexpected trailing input"))
          (nreverse names))))))

(defun source-path (name)
  (merge-pathnames (concatenate 'string name ".lisp") *root*))

(defun compiler-symbol (name)
  (or (and (find-package "CLYSM") (find-symbol name "CLYSM"))
      (error "Clysm compiler entry ~A is unavailable." name)))

(defun compiler-call (name &rest arguments)
  (apply (symbol-function (compiler-symbol name)) arguments))

(defun compiler-value (name)
  (symbol-value (compiler-symbol name)))

(defun (setf compiler-value) (value name)
  (setf (symbol-value (compiler-symbol name)) value))

(defun start (&key (root #p"/clysm/") (output #p"/clysm.wasm"))
  "Schedule the source-only bootstrap. Use a fresh MIRICL instance to restart."
  (when *started*
    (error "Clysm bootstrap already started; restart with a fresh MIRICL instance."))
  (let* ((*root* (pathname root))
         (compiler (read-source-list (merge-pathnames "compiler-files.lisp" *root*)))
         (runtime (read-source-list (merge-pathnames "tools/runtime-files.lisp" *root*))))
    (unless (and (null (pathname-name *root*)) (null (pathname-type *root*)))
      (error "Clysm source root must name a directory: ~S." root))
    (setf *tasks*
          (append (mapcar (lambda (file) (list :load file)) compiler)
                  (list (list :reset))
                  (mapcar (lambda (file) (list :runtime (concatenate 'string "runtime/" file))) runtime)
                  (list (list :finish-runtime))
                  (mapcar (lambda (file) (list :compile file)) compiler)
                  (list (list :compile "lib/live-image")
                        (list :compile "lib/web-main")
                        (list :emit)))
          *failure* nil
          *started* t))
  (setf *root* (pathname root) *output* (pathname output))
  (status))

(defun status ()
  "Describe the next bootstrap step, or NIL when the image has been written."
  (when *failure* (error "Clysm bootstrap failed: ~A" *failure*))
  (when *tasks*
    (destructuring-bind (phase &optional file) (car *tasks*)
      (case phase
        (:load (format nil "Loading Clysm compiler: ~A.lisp" file))
        (:reset "Preparing Clysm runtime")
        (:runtime (format nil "Compiling Clysm runtime: ~A.lisp" file))
        (:finish-runtime "Finishing Clysm runtime")
        (:compile (format nil "Compiling Clysm REPL: ~A.lisp" file))
        (:emit "Assembling Clysm WebAssembly")))))

(defun call-in-system-package (function)
  (progv (list (compiler-symbol "*READ-PACKAGE*"))
         (list (compiler-call "FIND-TARGET-PACKAGE" "SYSTEM"))
    (funcall function)))

(defun finish-runtime ()
  (call-in-system-package
   (lambda ()
     (setf (compiler-value "*RECORD-DEFINITIONS*") nil)
     (compiler-call "PROCESS-TOPLEVEL"
                    (compiler-call "RECORDED-DEFINITIONS-FORM")
                    (compiler-call "MAKE-CENV"))
     (compiler-call "FINISH-RUNTIME")))
  ;; Clysm's compiler uses CLYSM-SYS for the target runtime's SYSTEM package.
  (setf (gethash "CLYSM-SYS" (compiler-value "*TARGET-PACKAGES*"))
        (find-package "CLYSM-SYS"))
  (push "CLYSM-SYS" (gethash "SYSTEM" (compiler-value "*TARGET-NICKNAMES*"))))

(defun emit-image ()
  ;; Live modules import the runtime helpers exported by the initial image.
  (setf (gethash (intern "*LIVE-HELPER-IMPORTS*" "CLYSM-T/CLYSM")
                 (compiler-value "*INITIAL-SYMBOL-VALUES*"))
        (list t))
  (let ((bytes
          (progv (list (compiler-symbol "*EXPORT-LIVE-HELPERS*")) (list t)
            (compiler-call "BUILD-MODULE"))))
    (with-open-file (out *output* :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
      (write-sequence bytes out))))

(defun step ()
  "Run one scheduled bootstrap step and return the next status, or NIL."
  (unless *started* (error "Start the Clysm bootstrap before calling STEP."))
  (let ((description (status)))
    (when *tasks*
      (handler-case
          (destructuring-bind (phase &optional file) (car *tasks*)
            (case phase
              (:load (load (source-path file) :verbose nil :print nil))
              (:reset
               (compiler-call "RESET-COMPILER")
               (setf (compiler-value "*RECORD-DEFINITIONS*") t))
              (:runtime
               (call-in-system-package
                (lambda () (compiler-call "COMPILE-SOURCE-FILE" (namestring (source-path file))))))
              (:finish-runtime (finish-runtime))
              (:compile
               (compiler-call "COMPILE-SOURCE-FILE" (namestring (source-path file))))
              (:emit (emit-image))))
        (error (condition)
          (setf *failure* (format nil "~A: ~A" description condition))
          (error "~A" *failure*)))
      (pop *tasks*))
    (status)))

(defun bootstrap (&rest arguments)
  "Build the entire REPL; STEP provides the same work with host progress."
  (apply #'start arguments)
  (loop for message = (status) while message
        do (format t "~&~A~%" message) (finish-output) (step))
  (namestring *output*))
