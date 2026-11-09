;;;; Build the compiler with itself, then link its code into the runtime.
(in-package #:miricl)

(defun compiler-image-library-forms ()
  (if (null *native-compiler-image*) *clos-library*
      (destructuring-bind (wat names roots records) *native-compiler-image*
        (declare (ignore wat))
        (labels ((filter-form (form)
                   (cond
                     ((atom form) form)
                     ((and (eq (car form) 'defun) (symbolp (second form))
                           (eq (symbol-package (second form)) (find-package :miricl-compiler))
                           (member (symbol-name (second form)) names :test #'equal)) nil)
                     ((eq (car form) 'let)
                      (list* 'let (second form) (remove nil (mapcar #'filter-form (cddr form)))))
                     (t form))))
          (append (remove nil (mapcar #'filter-form *clos-library*))
            (list (lower-runtime-library
              `(let ((%fasl-records (coerce ',records 'vector))
                     (%fasl-values (make-array ,(length records)))
                     (%fasl-states (make-array ,(length records) :initial-element 0))
                     (%fasl-readtable (copy-readtable nil)) (%fasl-pending nil))
                 ,@(loop for root in roots for index from 0 collect
                   `(let ((%fasl-work (%fasl-object ,root)))
                      (setf (symbol-function (car %fasl-work))
                        (if (caddr %fasl-work)
                            (%%compile 30 (%%compile 20 ,index (cadr %fasl-work) (car %fasl-work)) (caddr %fasl-work))
                            (%%compile 20 ,index (cadr %fasl-work) (car %fasl-work)))
                        (gethash (car %fasl-work) miricl-compiler::*native-compiler-functions*) t)))
                 (setq miricl-compiler::*compiler-state* :ready
                       miricl-compiler::*compiler-origin* :image
                       miricl-compiler:*compile-evaluations* ,*compile-evaluations-default*)))))))))

(defun ensure-native-compiler-image ()
  (unless *native-compiler-image*
    (uiop:with-temporary-file (:stream out :pathname bootstrap :type "wasm"
                                     :element-type '(unsigned-byte 8))
      ;; The temporary seed contains the interpreted compiler. It is never
      ;; distributed: Node uses it to produce the self-compiled native image.
      (write-sequence (wasm-without-debug-names (wt:wat-to-wasm (interpreter-wat))) out)
      (finish-output out)
      (let ((wat (make-pathname :type "wat" :defaults bootstrap))
            (manifest (make-pathname :type "lisp" :defaults bootstrap)))
        (unwind-protect
            (progn
              (uiop:run-program
                (list (or (uiop:getenv "NODE") "node")
                      (namestring (asdf:system-relative-pathname :miricl "scripts/prepare-compiler-image.mjs"))
                      (namestring bootstrap)
                      (namestring (make-pathname :type nil :defaults bootstrap)))
                :output *standard-output* :error-output *error-output*)
              (with-open-file (in manifest)
                (with-standard-io-syntax
                  (let ((*read-eval* nil))
                    (destructuring-bind (names codes roots records) (read in)
                      (declare (ignore codes))
                      (setf *native-compiler-image*
                        (list (uiop:read-file-string wat)
                              (loop for (package name) in names
                                    when (equal package "MIRICL-COMPILER") collect name)
                              roots records)))))))
          (uiop:delete-file-if-exists wat)
          (uiop:delete-file-if-exists manifest)))))
  *native-compiler-image*)
