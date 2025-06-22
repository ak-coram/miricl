;;;; miricl-readtables.lisp

(in-package #:miricl)

(eval-when (:compile-toplevel :load-toplevel)
  (defun read-wat-form (stream char p)
    (declare (ignore char p))
    (let ((table (copy-readtable nil)))
      (setf (readtable-case table) :preserve)
      (let ((*readtable* table))
        (read stream t nil t))))

  (named-readtables:defreadtable :miricl-wat
    (:merge :standard)
    (:macro-char #\# :dispatch)
    (:dispatch-macro-char #\# #\@ #'read-wat-form)))
