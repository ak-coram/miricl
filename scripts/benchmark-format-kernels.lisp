;;;; LOAD after benchmark-runtime-kernels.lisp to add FORMAT migration fixtures.
(in-package #:cl-user)
(setf miricl-port-bench:*cases*
      (append (remove-if (lambda (case) (member (first case)
                    '("format/slash-position" "format/slash-missing" "format/constant" "format/constant-reject"
                      "format/leading-zero" "format/keep-zero" "format/rounded-even" "format/rounded-away"
                      "format/rounded-large" "format/decimal-parts") :test #'equal))
                 miricl-port-bench:*cases*)
              '(("format/slash-position" format-slash-position (list "CL-USER:PRINT/remaining" 0 23))
                ("format/slash-missing" format-slash-position (list "no-slash" 0 8))
                ("format/constant" format-constant-text-p (list "plain λ🙂 text"))
                ("format/constant-reject" format-constant-text-p (list "plain ~ text"))
                ("format/leading-zero" format-leading-zero-text (list "-0.125" 5 t) :calls 20000)
                ("format/keep-zero" format-leading-zero-text (list "-0.125" 6 t))
                ("format/rounded-even" format-rounded-integer (list 5/2 0 t) :calls 20000)
                ("format/rounded-away" format-rounded-integer (list -1/8 2 nil) :calls 20000)
                ("format/rounded-large" format-rounded-integer (list 123456789012345678901/2 4 t) :calls 20000)
                ("format/decimal-parts" format-decimal-parts (list "-1.23456789012345d-10") :calls 20000))))

(setf miricl-port-bench:*cases*
      (append (remove-if (lambda (case) (search "format-parser/" (first case)))
                         miricl-port-bench:*cases*)
              '(("format-parser/plain" format-control-values
                 (let ((text "A")) (list text nil (vector 0 (length text) nil 0 nil))) :calls 100000)
                ("format-parser/numeric" format-control-values
                 (let ((text "1234,-56:@d")) (list text nil (vector 0 (length text) nil 0 nil))) :calls 100000)
                ("format-parser/large" format-control-values
                 (let ((text "123456789012345678901d")) (list text nil (vector 0 (length text) nil 0 nil))) :calls 20000)
                ("format-parser/quoted" format-control-values
                 (let ((text "'🙂a")) (list text nil (vector 0 (length text) nil 0 nil))) :calls 100000)
                ("format-parser/arguments" format-control-values
                 (let ((text "v,#a")) (list text '(17 :object :extra) (vector 0 (length text) nil 0 nil))) :calls 100000)
                ("format-parser/growth" format-control-values
                 (let ((text "1,2,3,4,5,6,7,8,9,10a")) (list text nil (vector 0 (length text) nil 0 nil))) :calls 20000)
                ("format-parser/nested-close" format-control-values
                 (let ((text "plain ~A ~{~[zero~;~A~]~} ~:]tail"))
                   (list text nil (vector 0 (length text) t 93 nil))) :calls 20000)
                ("format-parser/slash-close" format-control-values
                 (let ((text "~/FOO~]BAR/~]tail")) (list text nil (vector 0 (length text) t 93 nil))) :calls 100000)
                ("format-parser/separator" format-control-values
                 (let ((text "plain ~A~:;tail~]")) (list text nil (vector 0 (length text) t 93 t))) :calls 100000)
                ("format-parser/malformed" format-control-values
                 (let ((text "::A")) (list text nil (vector 0 (length text) nil 0 nil))) :calls 100000))))
