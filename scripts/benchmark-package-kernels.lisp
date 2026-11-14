;;;; Load after benchmark-runtime-kernels.lisp in a persistent SBCL REPL.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (search "package-kernel/" (first case))) miricl-port-bench:*cases*)
       '(("package-kernel/bootstrap-names" package-bootstrap-name-list (list '("ONE" "TWO" "THREE")) :calls 50000)
         ("package-kernel/global-primary" find-global-package-name
          (list "COMMON-LISP" (list (find-package :keyword) (find-package :cl-user) (find-package :cl)) #'package-name #'package-nicknames) :calls 50000)
         ("package-kernel/global-nickname" find-global-package-name
          (list "CL" (list (find-package :keyword) (find-package :cl-user) (find-package :cl)) #'package-name #'package-nicknames) :calls 50000)
         ("package-kernel/global-miss" find-global-package-name
          (list "PACKAGE-MISSING" (list (find-package :keyword) (find-package :cl-user) (find-package :cl)) #'package-name #'package-nicknames) :calls 50000)
         ("package-kernel/local" find-local-package-name
          (list "LOCAL-CL" (list (cons "LOCAL-KEYWORD" (find-package :keyword)) (cons "LOCAL-CL" (find-package :cl)))) :calls 100000)
         ("package-kernel/local-miss" find-local-package-name
          (list "LOCAL-MISSING" (list (cons "LOCAL-KEYWORD" (find-package :keyword)) (cons "LOCAL-CL" (find-package :cl)))) :calls 100000)
         ("package-kernel/all-car" find-all-package-symbols
          (list "CAR" (list (find-package :cl) (find-package :cl-user) (find-package :keyword)) (vector #'package-name #'find-symbol)) :calls 20000)
         ("package-kernel/all-nil" find-all-package-symbols
          (list "NIL" (list (find-package :cl) (find-package :cl-user) (find-package :keyword)) (vector #'package-name #'find-symbol)) :calls 20000)
         ("package-kernel/all-miss" find-all-package-symbols
          (list "SYMBOL-MISSING" (list (find-package :cl) (find-package :cl-user) (find-package :keyword)) (vector #'package-name #'find-symbol)) :calls 20000)
         ("package-kernel/names-strings" package-name-list (list '("ONE" "TWO" "THREE")) :calls 50000)
         ("package-kernel/names-designators" package-name-list (list '(one :two #\3)) :calls 50000)
         ("package-kernel/names-empty" package-name-list (list nil))
         ("package-kernel/users" collect-package-users
          (list (find-package :cl) (list (find-package :cl-user) (find-package :keyword)) #'package-use-list) :calls 50000)
         ("package-kernel/live" collect-live-packages
          (list (list (find-package :cl-user) (find-package :keyword)) #'package-name) :calls 50000)
         ("package-kernel/fold-ascii" package-fold-name (list "alphabetical" #'char-upcase) :calls 20000)
         ("package-kernel/fold-unicode" package-fold-name (list "αλφαβητικός" #'char-upcase) :calls 20000)
         ("package-kernel/contains-match" package-name-contains-folded-p (list "PHAB" "alphabetical" #'char-upcase) :calls 100000)
         ("package-kernel/contains-miss" package-name-contains-folded-p (list "ZZ" "alphabetical" #'char-upcase) :calls 100000)
         ("package-kernel/contains-empty" package-name-contains-folded-p (list "" "alphabetical" #'char-upcase))
         ;; A singleton match and an unchanged all-miss spine remain valid
         ;; across repeated destructive-filter calls in the steady-state driver.
         ("package-kernel/filter-match" filter-apropos-symbols (list "ALPHA" (list 'alpha) #'char-upcase) :calls 50000)
         ("package-kernel/filter-miss" filter-apropos-symbols (list "ZZ" (list 'alpha 'beta 'gamma 'delta) #'char-upcase) :calls 50000)
         ("package-kernel/options" check-package-options
          (list '((:use :cl) (:shadow a b) (:export a b) (:intern c) (:size 16) (:documentation "Doc"))) :calls 20000)
         ("package-kernel/options-empty" check-package-options (list nil)))))
