;;;; Closer MOP's portable helpers over MIRICL's native metaobjects.
(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((native (or (find-package "MIRICL-MOP")
                     (error 'package-error :package "MIRICL-MOP")))
         (common-lisp (find-package "COMMON-LISP"))
         (closer (find-package "CLOSER-MOP"))
         (shared-names '("REQUIRED-ARGS" "ENSURE-FINALIZED" "SUBCLASSP"
                         "CLASSP" "FIX-SLOT-INITARGS"))
         (exports
           (append
             (loop for symbol being the external-symbols of native collect symbol)
             '(cl:built-in-class cl:class cl:generic-function cl:method
               cl:method-combination cl:standard-class cl:standard-generic-function
               cl:standard-method cl:standard-object cl:defclass cl:defgeneric
               cl:define-method-combination cl:defmethod cl:ensure-generic-function
               cl:typep cl:subtypep))))
    (labels ((collision (package)
               (error 'package-error :package package))
             (check-package (name nickname uses symbols)
               (let ((package (find-package name)))
                 (when (or (find-package nickname) package)
                   (unless (and package
                                (string= (package-name package) name)
                                (eq (find-package nickname) package)
                                (member nickname (package-nicknames package) :test #'string=)
                                (= (length uses) (length (package-use-list package)))
                                (every (lambda (used) (member used (package-use-list package))) uses)
                                (every (lambda (symbol)
                                         (multiple-value-bind (found status)
                                             (find-symbol (symbol-name symbol) package)
                                           (and (eq found symbol) (eq status :external))))
                                       symbols))
                     (collision (or package (find-package nickname)))))
                 package)))
      (unless (and native (string= (package-name native) "MIRICL-MOP"))
        (collision "MIRICL-MOP"))
      ;; RENAME-PACKAGE replaces every nickname. Only release our native alias,
      ;; and preflight the entire adapter namespace before changing that list.
      ;; A failed upstream DEFPACKAGE may have replaced CLOSER-MOP/MOP by C2MOP.
      (dolist (name '("CLOSER-MOP" "C2MOP"))
        (when (eq (find-package name) native)
          (unless (member name (package-nicknames native) :test #'string=)
            (collision native))))
      (when (and (find-package "MOP") (not (eq (find-package "MOP") native)))
        (collision (find-package "MOP")))
      (when (eq closer native) (setq closer nil))
      (when (or closer (and (find-package "C2MOP") (not (eq (find-package "C2MOP") native))))
        (setq closer (check-package "CLOSER-MOP" "C2MOP" (list common-lisp) exports)))
      (when closer
        (dolist (name shared-names)
          (multiple-value-bind (symbol status) (find-symbol name closer)
            (unless (and (eq status :external) (eq (symbol-package symbol) closer))
              (collision closer)))))
      (let* ((shared (when closer (mapcar (lambda (name) (find-symbol name closer)) shared-names)))
             (common-exports
               (union (append exports shared)
                      (loop for symbol being the external-symbols of common-lisp collect symbol))))
        (unless closer
          (dolist (name '("CLOSER-COMMON-LISP" "C2CL" "CLOSER-COMMON-LISP-USER" "C2CL-USER"))
            (when (find-package name) (collision (find-package name)))))
        (let ((common (check-package "CLOSER-COMMON-LISP" "C2CL" nil common-exports)))
          (if common
              (check-package "CLOSER-COMMON-LISP-USER" "C2CL-USER" (list common) nil)
              (dolist (name '("CLOSER-COMMON-LISP-USER" "C2CL-USER"))
                (when (find-package name) (collision (find-package name))))))
        (rename-package native (package-name native)
                        (adjoin "MOP"
                                (remove-if (lambda (name) (member name '("CLOSER-MOP" "C2MOP") :test #'string=))
                                           (package-nicknames native))
                                :test #'string=))
        (unless closer
          (setq closer (make-package "CLOSER-MOP" :nicknames '("C2MOP") :use '("COMMON-LISP"))))
        (import exports closer)
        (export exports closer)
        ;; Each of these names has its actual body in closer-mop-shared.lisp.
        (dolist (name shared-names)
          (let ((symbol (intern name closer)))
            (export symbol closer)
            (pushnew symbol common-exports)))
        (let ((common (or (find-package "CLOSER-COMMON-LISP")
                          (make-package "CLOSER-COMMON-LISP" :nicknames '("C2CL") :use nil))))
          (import common-exports common)
          (export common-exports common)
          (unless (find-package "CLOSER-COMMON-LISP-USER")
            (make-package "CLOSER-COMMON-LISP-USER" :nicknames '("C2CL-USER") :use (list common))))))))
