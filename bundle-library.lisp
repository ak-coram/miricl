;;;; ASDF builds run inside the same interpreter as application code. No browser
;;;; or JavaScript is involved in creating the portable FASL compilation units.
(in-package :miricl-bundle)

(defun asdf-call (name &rest arguments)
  (apply (fdefinition (or (find-symbol name "ASDF") (error "Unknown ASDF operation: ~A" name))) arguments))

(defun asdf-artifact ()
  ;; A symlink beside the application can be outside a WASI preopen even
  ;; though the same artifact is accessible through the images/ preopen.
  (find-if (lambda (path) (ignore-errors (probe-file path)))
    '("miricl-asdf.wasm" "runtime/miricl-asdf.wasm" "images/miricl-asdf.wasm")))

(defun initialize ()
  (unless (find-package "ASDF")
    (let ((source (asdf-artifact)))
      (unless source (error "MIRICL-ASDF.WASM not found; place the ASDF artifact beside the application or in runtime/."))
      (load source)))
  (when (probe-file "systems.lisp") (load "systems.lisp"))
  (values))

(defun dump-system (system &key output (save t))
  "Compile SYSTEM and its dependencies into one FASL. SAVE publishes OUTPUT to
 the terminal host; use NIL to retain it only in the interpreter filesystem.
 Load-time forms retain ordinary ASDF behavior; no entry point is called."
  (initialize)
  (let* ((name (string-downcase (string system)))
         (slash (position #\/ name))
         (definition (concatenate 'string (subseq name 0 slash) ".asd")))
    (when (probe-file definition) (asdf-call "LOAD-ASD" (truename definition)))
    (let* ((component (asdf-call "FIND-SYSTEM" name))
           (operation (asdf-call "MAKE-OPERATION"
                        (find-symbol "MONOLITHIC-COMPILE-BUNDLE-OP" "ASDF"))))
      (asdf-call "OPERATE" operation component)
      (let* ((bundle (first (asdf-call "OUTPUT-FILES" operation component)))
             (target (or output bundle)))
        (unless (equal (namestring (merge-pathnames target)) (namestring (merge-pathnames bundle)))
          (ensure-directories-exist target)
          (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
            (with-open-file (in bundle :element-type '(unsigned-byte 8))
              (with-open-file (out target :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
                (loop for count = (read-sequence buffer in) while (plusp count)
                      do (write-sequence buffer out :end count))))))
        (when save (miricl::%%filesystem 7 (miricl::%source-path target) nil))
        (pathname target)))))
