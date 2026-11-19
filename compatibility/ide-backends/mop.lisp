;;;; Shared optional MOP adaptation, independent of any server or transport.
(defpackage #:miricl-ide-mop
  (:use #:cl)
  (:export #:import-native-mop #:slot-definition-documentation
           #:mop-adaptation-error))
(in-package #:miricl-ide-mop)

(define-condition mop-adaptation-error (package-error simple-condition) ())

(defgeneric slot-definition-documentation (slot)
  (:documentation "Return retained slot documentation. Effective slots do not merge direct documentation."))
(defmethod slot-definition-documentation ((slot miricl-mop:slot-definition))
  (documentation slot t))
(defmethod slot-definition-documentation ((slot t))
  (error 'type-error :datum slot :expected-type 'miricl-mop:slot-definition))

(defun %fail (package control &rest arguments)
  (error 'mop-adaptation-error :package package
         :format-control control :format-arguments arguments))

(defun %binding-query (name package)
  ;; This compiler integration resolves the actual private bootstrap identity,
  ;; as the optional declaration service does. Callers use only nonexpanding
  ;; binding reads, never expander invocation or registration operations.
  (let ((query (miricl-compiler::compiler-primitive 10 1 name)))
    (unless (and query (symbolp query) (fboundp query))
      (%fail package "Native nonexpanding binding query ~A is unavailable." name))
    query))

(defun %inert-placeholder-p (symbol package type-query setf-query)
  (and (eq (symbol-package symbol) package)
       (not (boundp symbol)) (not (constantp symbol))
       (not (miricl-compiler::compiler-primitive 18 symbol))
       (not (fboundp symbol)) (not (fboundp (list 'setf symbol)))
       (not (compiler-macro-function symbol))
       (not (compiler-macro-function (list 'setf symbol)))
       (not (find-class symbol nil)) (null (symbol-plist symbol))
       (not (funcall type-query 0 symbol nil nil))
       (not (funcall setf-query nil symbol nil nil))
       (not (let ((*macroexpand-hook* #'funcall))
              (nth-value 1 (macroexpand-1 symbol))))
       (not (member symbol (package-shadowing-symbols package)))))

(defun %canonical-symbol (name package)
  (multiple-value-bind (symbol status) (find-symbol name package)
    (and (eq status :external) (eq (symbol-package symbol) package) symbol)))

(defun %source-symbol (name target)
  (let ((symbol (or (%canonical-symbol name (find-package "COMMON-LISP"))
                    (%canonical-symbol name (find-package "MIRICL-MOP"))
                    (and (string= name "SLOT-DEFINITION-DOCUMENTATION")
                         'slot-definition-documentation))))
    (unless symbol (%fail target "No native MOP capability for ~A." name))
    (if (member name '("STANDARD-GENERIC-FUNCTION" "STANDARD-SLOT-DEFINITION"
                       "STANDARD-METHOD" "STANDARD-CLASS" "EQL-SPECIALIZER")
                :test #'string=)
        (unless (find-class symbol nil)
          (%fail target "Native MOP class ~A is unavailable." name))
        (unless (and (fboundp symbol) (not (macro-function symbol))
                     (typep (fdefinition symbol) 'generic-function))
          (%fail target "Native MOP generic function ~A is unavailable." name)))
    symbol))

(defun %check-export-users (target old canonical)
  (let ((name (symbol-name canonical)))
    (dolist (user (package-used-by-list target))
      (unless (find name (package-shadowing-symbols user)
                    :key #'symbol-name :test #'string=)
        (multiple-value-bind (found status) (find-symbol name user)
          (cond
            ((eq found canonical))
            ((and (eq status :inherited) (eq found old))
             ;; Another used package may still export the old placeholder.
             ;; Replacing only TARGET would then introduce an inherited conflict.
             (dolist (used (package-use-list user))
               (unless (eq used target)
                 (multiple-value-bind (other other-status) (find-symbol name used)
                   (when (and (eq other-status :external) (not (eq other canonical)))
                     (%fail target "Exporting ~A conflicts in using package ~A."
                            name (package-name user)))))))
            (t (%fail target "Exporting ~A conflicts in using package ~A."
                      name (package-name user)))))))))

(defun import-native-mop (package-designator)
  "Replace inert exports of a dedicated, USE-free MOP facade with native identities.
Preflight every export and using package before changing any symbol. Return the
package and the names of explicitly unsupported capabilities as a second value."
  (unless (or (packagep package-designator) (stringp package-designator)
              (symbolp package-designator) (characterp package-designator))
    (error 'type-error :datum package-designator
           :expected-type '(or package string symbol character)))
  (let ((target (find-package package-designator)) (plan nil) (unsupported nil)
        (count 0) (type-query nil) (setf-query nil))
    (unless (and target (package-name target))
      (%fail package-designator "Missing MOP facade package ~S." package-designator))
    ;; Upstream facades deliberately use no packages. This also prevents
    ;; UNINTERN from exposing inherited conflicts during placeholder removal.
    (when (package-use-list target)
      (%fail target "MOP facade ~A must have an empty USE list." (package-name target)))
    (setq type-query (%binding-query "%%DERIVED-TYPE" target)
          setf-query (%binding-query "%%SETF-EXPANDER" target))
    (do-external-symbols (old target)
      (when (> (incf count) 128)
        (%fail target "MOP facade ~A has excessive exports." (package-name target)))
      (let ((name (symbol-name old)))
        (if (string= name "GENERIC-FUNCTION-DECLARATIONS")
            (progn
              (unless (%inert-placeholder-p old target type-query setf-query)
                (%fail target "Unsupported MOP name ~A has existing state." name))
              (push name unsupported))
            (let ((canonical (%source-symbol name target)))
              (unless (or (eq old canonical) (%inert-placeholder-p old target type-query setf-query))
                (%fail target "MOP name ~A has an existing foreign identity or binding." name))
              (when (member old (package-shadowing-symbols target))
                (%fail target "MOP name ~A is a shadowing symbol." name))
              (%check-export-users target old canonical)
              (push (cons old canonical) plan)))))
    (when (zerop count) (%fail target "MOP facade ~A has no requested exports." (package-name target)))
    ;; No source or destination capability is changed until the whole plan is
    ;; accepted. Canonical symbols preserve both class and generic identities.
    (dolist (entry plan)
      (unless (eq (car entry) (cdr entry))
        (unintern (car entry) target)
        (import (cdr entry) target)
        (export (cdr entry) target)))
    (values target (nreverse unsupported))))
