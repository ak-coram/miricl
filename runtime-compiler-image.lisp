;;;; Build the compiler with itself, then link its code into the runtime.
(in-package #:miricl)

(defun compiler-image-records-string (records)
  ;; Reuse the current FASL record codec. Only the descriptions are packed;
  ;; their IDs still reconstruct the same literal graph during startup.
  (uiop:with-temporary-file (:stream out :pathname file :type "fasl"
                                   :element-type '(unsigned-byte 8))
    (miricl-compiler::write-binary-chunk (coerce records 'vector) out 0)
    (finish-output out)
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (read-sequence bytes in)
        (map 'string #'code-char bytes)))))

(defun compiler-image-library-forms ()
  (if (null *native-compiler-image*)
      (append *clos-library*
        ;; Cold compiler images exercise the HIR interpreter. Lightweight
        ;; scripting keeps source evaluation; explicit :INTERPRETED remains
        ;; available without making every definition pay frontend costs.
        (list (lower-runtime-library
                `(setq miricl-compiler:*compile-evaluations* ,(when *include-compiler* :interpreted)))))
      (destructuring-bind (wat names roots records) *native-compiler-image*
        (declare (ignore wat))
        (let ((packed (compiler-image-records-string records)))
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
                `(let ((%fasl-records (%%compile 16 ,packed 0 ,(length packed)))
                       (%fasl-values (make-array ,(length records)))
                       (%fasl-states (make-array ,(length records) :initial-element 0))
                       (%fasl-readtable (copy-readtable nil)) (%fasl-pending nil))
                   ,@(loop for root in roots for index from 0 collect
                     `(let ((%fasl-work (%fasl-object ,root)))
                        (dolist (entry (funcall (%%compile 20 ,index (cadr %fasl-work) nil)))
                          (setf (symbol-function (car entry)) (cdr entry)
                                (gethash (car entry) miricl-compiler::*native-compiler-functions*) t))))
                   (%invalidate-all-dispatch)
                   (setq miricl-compiler::*compiler-state* :ready
                         miricl-compiler::*compiler-origin* :image
                         miricl-compiler:*compile-evaluations* ,*compile-evaluations-default*))))))))))

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
