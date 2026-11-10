// Raw Common Lisp counterpart of ACL2's ALL-FNNAMES1-INVARIANT-RISK.
// The logical definition remains upstream's recursive definition. This uses
// a heap work list for pending visits and preserves its right-to-left order.
export const iterativeInvariantRisk = `(defun all-fnnames1-invariant-risk (flg x i wrld acc)
  (prog ((pending nil))
   visit
    (cond
     (flg
      (when (null x) (go resume))
      (push (cons (car x) (car i)) pending)
      (setq x (cdr x) i (cdr i)))
     ((variablep x) (go resume))
     ((fquotep x)
      (unless (and (eq i :fn)
                   (well-formed-lambda-objectp (unquote x) wrld))
        (go resume))
      ;; The guard is captured before visiting the body, just as the outer
      ;; recursive call evaluated its arguments before the inner call.
      (push (cons (lambda-object-guard (unquote x)) nil) pending)
      (setq x (lambda-object-body (unquote x)) flg nil i nil))
     ((flambda-applicationp x)
      (push (cons (lambda-body (ffn-symb x)) nil) pending)
      (setq x (fargs x) flg t i nil))
     ((eq (ffn-symb x) 'return-last)
      (cond
       ((equal (fargn x 1) '(quote mbe1-raw))
        (setq x (fargn x 2) flg nil i nil))
       ((and (equal (fargn x 1) '(quote ec-call1-raw))
             (nvariablep (fargn x 3))
             (not (fquotep (fargn x 3)))
             (not (flambdap (ffn-symb (fargn x 3)))))
        (setq x (fargs (fargn x 3)) flg t i nil))
       (t
        (let* ((args (fargs x))
               (new-acc (add-to-set-eq (ffn-symb x) acc)))
          (setq x args flg t i nil acc new-acc)))))
     (t
      (let* ((args (fargs x))
             (ilks (and (special-loop$-scion-callp x wrld)
                        (access apply$-badge
                                (executable-badge (ffn-symb x) wrld) :ilks)))
             (new-acc (add-to-set-eq (ffn-symb x) acc)))
        (setq x args flg t i ilks acc new-acc))))
    (go visit)
   resume
    (when (null pending) (return acc))
    (let ((task (pop pending)))
      (setq x (car task) i (cdr task) flg nil))
    (go visit)))`;
