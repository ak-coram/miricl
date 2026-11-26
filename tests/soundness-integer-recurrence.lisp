(in-package :miricl-compiler)

;; Private recurrence state must retain exact values after HIR reconstruction.
(let* ((source '(lambda ()
                  (declare (optimize (space 3) (speed 1) (safety 1) (debug 1)))
                  (let ((clock 0) (total 7) (product 1))
                    (tagbody again
                      (when (< clock 5)
                        (setq total (+ total (* clock 1000000000)))
                        (setq product (logand (* product 31) 17592186044415))
                        (setq clock (1+ clock))
                        (go again)))
                    (values total product clock))))
       (wire (emit-hir-code (lower-source-to-hir source))))
  (dolist (worker (list (compile-native source) (compile-native-hir wire)))
    (assert (equal (multiple-value-list (funcall worker)) '(10000000007 28629151 5)))))

;; A store's mask bounds the next accumulator value. The preceding product
;; still needs exact promotion when it exceeds the target's signed range.
(let ((worker (compile-native
                '(lambda ()
                   (declare (optimize (speed 3) (space 1) (safety 1) (debug 1)))
                   (let ((clock 0) (product 4611686018427387904) (exact 0))
                     (tagbody again
                       (when (< clock 2)
                         (setq exact (* product 3))
                         (setq product (logand exact 9223372036854775807))
                         (setq clock (1+ clock))
                         (go again)))
                     (values exact product clock))))))
  (assert (equal (multiple-value-list (funcall worker))
                 '(13835058055282163712 4611686018427387904 2))))
