;;;; Compatibility entry points for the compiler-free runtime. No compiler
;;;; frontend, optimizer, backend, or bootstrap seed is included in this image.
(defvar miricl-compiler:*compile-evaluations* nil)
(defvar %file-load-values nil)
(defvar %compilation-complete nil)

;; Object serialization uses this adapter to prepare MAKE-LOAD-FORM recipes.
;; The interpreter executes their source directly.
(defun %compile-form (form environment)
  (declare (ignore environment))
  form)

(defun compile-file (input-file &key (output-file nil outputp)
                      (verbose *compile-verbose*) (print *compile-print*) (external-format :default))
  (declare (ignore print external-format))
  (let* ((source (truename input-file))
         (target (if outputp (compile-file-pathname input-file :output-file output-file)
                     (compile-file-pathname input-file)))
         (existing (probe-file target)))
    (when verbose (format t "~&; Copying source ~A to ~A~%" source target))
    ;; Copy bytes without READ, macroexpansion, or compile-time side effects.
    ;; LOAD detects the source contents even when the output has a .wasm suffix.
    ;; Do not truncate the input when OUTPUT-FILE denotes the source itself.
    (unless (and existing (equal (namestring source) (namestring existing)))
      (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
        (with-open-file (in source :element-type '(unsigned-byte 8))
          (with-open-file (out target :direction :output :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
            (loop for count = (read-sequence buffer in) while (plusp count)
                  do (write-sequence buffer out :end count))))))
    (values (truename target) nil nil)))
