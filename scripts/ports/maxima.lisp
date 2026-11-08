;;;; Maxima build/runtime adaptation, loaded inside the browser/Node guest.
;;;; MK:DEFSYSTEM's fallback for an unfamiliar Lisp reads LISP:REQUIRE.
;;;; Keep MIRICL's real feature set and leave the upstream sources untouched.
(unless (find-package "LISP")
  (defpackage "LISP" (:use "COMMON-LISP") (:export "REQUIRE")))
;; Maxima expects gradual underflow followed by IEEE zero, as configured in
;; its CLISP/ECL/ABCL ports. Retain MIRICL's trapping default for other users.
(setq miricl:*trap-floating-point-underflow* nil)
