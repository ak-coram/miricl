;;;; Optional public access to the runtime's single package-local name registry.
(defpackage #:miricl-packages
  (:use #:cl)
  (:export #:package-local-nicknames #:add-package-local-nickname
           #:remove-package-local-nickname))
(in-package #:miricl-packages)

;; This source precedes UIOP in the optional ASDF library. Install the real API
;; during compilation too, before UIOP reads its capability-dependent forms.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition package-local-nickname-error (package-error simple-condition) ())

  (defun %bridge (package selector names)
    (let ((primitive (miricl-compiler::compiler-primitive 10 1 "%%PACKAGE-NODE-REF")))
      (unless (and (symbolp primitive) (fboundp primitive))
        (error "Native package-local nickname access is unavailable."))
      (funcall primitive package selector names)))

  (defun %package (designator)
    (unless (or (packagep designator) (stringp designator)
                (symbolp designator) (characterp designator))
      (error 'type-error :datum designator
             :expected-type '(or package string symbol character)))
    (let ((package (find-package designator)))
      (unless (and package (package-name package))
        (error 'package-error :package designator))
      package))

  (defun package-local-nicknames (package)
    "Return a detached alist of (nickname-string . target-package) mappings."
    (%bridge (%package package) 13 nil))

  (defun add-package-local-nickname (nickname actual-package &optional (package *package*))
    "Add an owner-local name, returning the owner. Conflicting targets signal."
    (let* ((owner (%package package)) (target (%package actual-package))
           (name (string nickname)) (names (package-local-nicknames owner))
           (old (assoc name names :test #'string=)))
      (when (and old (not (eq (cdr old) target)))
        (error 'package-local-nickname-error :package owner
               :format-control "Conflicting package-local nickname ~S."
               :format-arguments (list name)))
      (%bridge owner 14 (cons (cons name target) names))))

  (defun remove-package-local-nickname (nickname &optional (package *package*))
    "Remove an owner-local name, returning whether a mapping was present."
    (let* ((owner (%package package)) (name (string nickname))
           (names (package-local-nicknames owner))
           (entry (assoc name names :test #'string=)))
      (when entry
        (%bridge owner 14 (remove entry names :test #'eq))
        t)))

  ;; Probe the new checked selector before claiming that this API is available.
  (package-local-nicknames *package*)
  (pushnew :package-local-nicknames *features*))

(in-package #:cl-user)
