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
      (destructuring-bind (wat names roots records policies) *native-compiler-image*
        (declare (ignore wat policies))
        (let ((packed (compiler-image-records-string records))
              (compiler-package (find-package :miricl-compiler)))
          (labels ((defun-name (form)
                     (when (and (consp form) (eq (car form) 'defun)) (second form)))
                   (filter-form (form &optional top-level)
                     (let ((name (defun-name form)))
                       (cond
                         ((atom form) form)
                         ((and (member name names)
                               (or (eq (symbol-package name) compiler-package)
                                   ;; A plain private DEFUN installs only its
                                   ;; function and optional documentation.
                                   (and top-level (not (stringp (fourth form)))
                                        (= 1 (count name *clos-library* :key #'defun-name))))) nil)
                         ((eq (car form) 'let)
                          (list* 'let (second form) (remove nil (mapcar #'filter-form (cddr form)))))
                         (t form)))))
            (append (loop for form in *clos-library* for retained = (filter-form form t)
                          when retained collect retained)
              (list (lower-runtime-library
                `(let ((%fasl-records (%%compile 16 ,packed 0 ,(length packed)))
                       (%fasl-values (make-array ,(length records)))
                       (%fasl-states (make-array ,(length records) :initial-element 0))
                       (%fasl-readtable (copy-readtable nil)) (%fasl-pending nil))
                   ,@(loop for root in roots for index from 0 collect
                     `(let ((%fasl-work (%fasl-object ,root)))
                        (dolist (entry (funcall (%%compile 20 ,index (cadr %fasl-work) nil)))
                          (setf (fdefinition (car entry)) (cdr entry)
                                (gethash (car entry) miricl-compiler::*native-compiler-functions*) t))))
                   (%invalidate-all-dispatch)
                   (setq miricl-compiler::*compiler-state* :ready
                         miricl-compiler::*compiler-origin* :image
                         miricl-compiler:*compile-evaluations* ,*compile-evaluations-default*))))))))))

(defun compiler-image-private-functions (names)
  ;; These file preparation and FASL writing helpers are first called after
  ;; the native compiler factory is installed. The startup record reader,
  ;; including %FASL-OBJECT-SLOW for numeric records, still needs source.
  (loop for symbol in '(%compile-form %fasl-reference %fasl-dump
                        %file-lexical-name %file-lexical-specials %file-lexical-bind
                        %file-lexical-reference %file-code-cons %file-lexical-forms
                        %file-lexical-function %file-lexical-call %file-lexical-form)
        for name = (symbol-name symbol)
        when (and (member (list nil name 1) names :test #'equal)
                  (eq symbol (find name *clos-library-symbols* :test #'string= :key #'symbol-name))
                  (= 1 (count name *clos-library-symbols* :test #'string= :key #'symbol-name)))
          collect symbol))

(defun native-compiler-image-from-manifest (manifest &key bootstrap-ownership-p)
  ;; Treat the current build manifest as untrusted serialized compiler data.
  ;; Validate list spines before iteration and executable modules before the
  ;; private policy reader; reuse the ordinary FASL graph validator.
  (unless (= 4 (miricl-compiler::compiler-list-length manifest "native compiler manifest"))
    (native-link-fail manifest "expected names, modules, roots and records"))
  (destructuring-bind (names codes roots records) manifest
    (miricl-compiler::compiler-list-length names "native compiler names")
    (miricl-compiler::compiler-list-length codes "native compiler modules")
    (miricl-compiler::compiler-list-length roots "native compiler roots")
    (miricl-compiler::compiler-list-length records "native compiler records")
    (dolist (name names)
      (unless (and (= 3 (miricl-compiler::compiler-list-length name "native compiler name"))
                   (stringp (second name)) (typep (third name) '(integer 0 2))
                   (or (null (first name)) (and (stringp (first name)) (zerop (third name)))))
        (native-link-fail name "expected a package, symbol name and symbol namespace")))
    (unless (= (length codes) (length roots))
      (native-link-fail manifest "module and literal-root counts differ"))
    (let ((graph (coerce records 'vector)))
      (miricl-compiler::validate-fasl-graph roots graph)
      ;; Current compiler factories take one general literal vector. Verify
      ;; the (:MODULE vector) wrapper before startup follows its CADR and
      ;; passes that pool into unchecked compiler-generated entry points.
      (labels ((symbol-record-p (record name package)
                 (and (= 4 (first record)) (string= name (second record))
                      (equal package (third record)) (zerop (fourth record)))))
        (dolist (root roots)
          (let ((head (aref graph root)))
            (unless (= 5 (first head)) (native-link-fail root "expected a module literal wrapper"))
            (let ((marker (aref graph (second head))) (tail (aref graph (third head))))
              (unless (and (symbol-record-p marker "MODULE" "KEYWORD") (= 5 (first tail)))
                (native-link-fail root "expected (:MODULE literal-vector)"))
              (let ((pool (aref graph (second tail))) (end (aref graph (third tail))))
                (unless (and (= 0 (first end)) (= 6 (first pool)) (= 1 (length (second pool)))
                             (symbol-record-p (aref graph (third pool)) "T" "COMMON-LISP"))
                  (native-link-fail root "expected one simple general literal vector"))))))))
    (let* ((bytes (mapcar #'native-link-octets codes))
           (wat (link-native-modules bytes))
           (policies
             (loop for code in bytes for i from 0 append
               (handler-case (miricl-compiler::native-module-outline-policies code i)
                   (error (condition) (native-link-fail code "malformed compilation policies: ~A" condition)))))
           (compiler-package (find-package :miricl-compiler)))
      (list wat
            (append (loop for (package name) in names
                          for symbol = (when (equal package "MIRICL-COMPILER")
                                         (find-symbol name compiler-package))
                          when (and symbol (eq (symbol-package symbol) compiler-package)) collect symbol)
                    ;; Only the local producer has checked its instantiated
                    ;; factory entries. Graphs with active records may run
                    ;; code before the factory has installed those entries.
                    (when (and bootstrap-ownership-p roots
                               (every (lambda (record) (<= 0 (first record) 6)) records))
                      (compiler-image-private-functions names)))
            roots records policies))))

(defun ensure-native-compiler-image ()
  (unless *native-compiler-image*
    (uiop:with-temporary-file (:stream out :pathname bootstrap :type "wasm"
                                     :element-type '(unsigned-byte 8))
      ;; The temporary seed contains the interpreted compiler. It is never
      ;; distributed: Node uses it to produce the self-compiled native image.
      (write-sequence (wasm-without-debug-names (wt:wat-to-wasm (interpreter-wat))) out)
      (finish-output out)
      (let ((manifest (make-pathname :type "lisp" :defaults bootstrap)))
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
                    (let ((image (native-compiler-image-from-manifest (read in) :bootstrap-ownership-p t))
                          (eof (gensym)))
                      (unless (eq eof (read in nil eof))
                        (native-link-fail manifest "trailing compiler manifest data"))
                      ;; Bootstrap runs reuse these linker names. Each name has
                      ;; one policy supplied by the current validated manifest.
                      (dolist (entry (fifth image))
                        (setf (gethash (car entry) miricl-compiler::*wasm-outline-policies*) (cdr entry)))
                      (setf *native-compiler-image* image))))))
          (uiop:delete-file-if-exists manifest)))))
  *native-compiler-image*)
