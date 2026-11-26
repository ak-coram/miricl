;;;; Shared guest compiler bootstrap, run by the CL or Node Wasm host.
(in-package #:miricl-compiler)

(defun bootstrap-compiler-native-functions ()
  (setq *compiler-bootstrapping* t)
    (defparameter *image-sources* (mapcar (lambda (name)
      (list name (compiler-source-policy (compiler-function-source name)))) *native-compiler-seeds*))
    ;; Capture the same original runtime corpus before compiler preparation.
    ;; Runtime functions retain their declarations, including SAFETY and DEBUG.
    (defparameter *image-runtime-sources*
      (mapcar (lambda (name)
        (multiple-value-bind (source captured) (function-lambda-expression (fdefinition name))
          (unless (and source (not captured))
            (error "Native runtime image function has no closed source: ~S" name))
          (list name source))) *native-runtime-image-functions*))
    ;; The first native compiler is disposable. Favor its execution speed,
    ;; then restore the original policy before emitting the saved source corpus
    ;; for shipping. SAFETY, DEBUG and COMPILATION-SPEED keep their contracts.
    (let ((policy (compiler-primitive 52 nil))
          ;; Cold helpers stay interpreted in the shipped compiler, while the
          ;; disposable bootstrap can use them natively for speed-oriented work.
          (*native-compiler-seeds* (append *native-compiler-seeds* *native-compiler-cold-functions*)))
      (unwind-protect
          (progn
            (proclaim '(optimize (speed 3) (space 1)))
            (multiple-value-bind (installed unsupported) (compile-compiler)
              (unless (and (= (length installed) (length *native-compiler-seeds*)) (null unsupported))
                (error "Compiler bootstrap failed: ~S" unsupported))))
        (proclaim (list 'optimize (list 'speed (logand policy 3))
                                  (list 'space (logand 3 (ash policy -2))))))
      (unless (= policy (compiler-primitive 52 nil))
        (error "Compiler bootstrap changed the final optimization policy.")))
    (values)
)

(defun bootstrap-compiler-image (&optional (output "/compiler-image.lisp"))
  ;; Use the package's compact SPACE default while emitting shipped sources.
  ;; Source declarations and the guest's other policy qualities still apply.
  (progv (mapcar (lambda (name) (compiler-primitive 10 1 name))
        '("%FASL-RECORDS" "%FASL-SEEN" "%FILE-LOAD-VALUES" "%SIZE-DEFAULT-POLICY"))
      (list (make-array 32 :adjustable t :fill-pointer 0) (make-hash-table :test 'eq) nil
            (native-size-policy '((declare (optimize (space 3)))) (compiler-primitive 52 nil)))
      (let ((names nil) (codes nil) (roots nil)
            (dump (symbol-function (compiler-primitive 10 1 "%FASL-DUMP"))))
        ;; This second compilation uses the already self-compiled frontend and emitter.
        ;; The compiler owns its sealed internal bindings. Runtime entries stay
        ;; in separate singleton modules; private runtime calls retain their live
        ;; bindings while standard CL calls use their ANSI definitions. Factories preserve
        ;; the actual private symbols and general (including SETF) function names.
        (dolist (sources (cons *image-sources* (mapcar #'list *image-runtime-sources*)))
          (let* ((*wasm-bootstrap-direct-workers* t)
                 (definitions
                   (mapcar (lambda (entry) (list* 'defun (first entry) (cdr (second entry)))) sources))
                 (*hir-grouped-module-definitions* (and (eq sources *image-sources*) definitions))
                 (unit (compile-module-to-hir definitions :inline nil)))
            (multiple-value-bind (code literals) (emit-native-module-checked unit nil)
              ;; One factory shares the module's literals and closes over its
              ;; internal function bindings. Runtime startup recreates the
              ;; same entries from the retained literal graph.
              (let ((entries (funcall (compiler-primitive 17 code literals nil))))
                ;; Instantiate without installing another compiler generation.
                (unless (every (lambda (entry) (compiled-function-p (cdr entry))) entries)
                  (error "Module compiler output contains an uncompiled entry"))
                (when (eq sources *image-sources*)
                  (let ((entry-names (mapcar #'car entries)))
                    (unless (equal entry-names (mapcar #'first *image-sources*))
                      (error "Compiler factory entries differ from the ordered source names"))
                    ;; Record the actual installed identities using the same
                    ;; symbol domains as the factory's FASL literals.
                    (setf names
                      (mapcar (lambda (name)
                                (let ((package (symbol-package name)))
                                  (list (and package (package-name package)) (symbol-name name)
                                        (if package 0 (compiler-primitive 9 name)))))
                              entry-names)))))
              (unless (= 42 (funcall (compile-native '(lambda (x) (+ x 2))) 40))
                (error "Module-compiled compiler failed its compilation check"))
              (push (coerce code 'list) codes)
              (push (funcall dump (list :module literals)) roots))))
        (with-open-file (out output :direction :output :if-exists :supersede)
          (let ((*print-readably* t) (*print-pretty* nil) (*print-circle* t) (*print-base* 10) (*print-radix* nil))
            (write (list names (nreverse codes) (nreverse roots)
              (coerce (symbol-value (compiler-primitive 10 1 "%FASL-RECORDS")) 'list)) :stream out)))))
    (values)
)
