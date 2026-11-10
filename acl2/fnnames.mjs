// Raw host traversal; ACL2's logical definition stays unchanged.
export const iterativeFnnames = `(defun all-fnnames1 (flg x acc)
  (prog ((pending nil))
   visit
    (cond
     (flg
      (when (null x) (go resume))
      (push (car x) pending)
      (setq x (cdr x)))
     ((variablep x) (go resume))
     ((fquotep x) (go resume))
     ((flambda-applicationp x)
      (push (lambda-body (ffn-symb x)) pending)
      (setq x (fargs x) flg t))
     (t
      (let* ((args (fargs x)) (new-acc (add-to-set-eq (ffn-symb x) acc)))
        (setq x args flg t acc new-acc))))
    (go visit)
   resume
    (when (null pending) (return acc))
    (setq x (pop pending) flg nil)
    (go visit)))`;

// Execution-only names omit the logical arm of MBE and the guarded callee
// inside EC-CALL, while still traversing that callee's argument expressions.
export const iterativeExecFnnames = `(defun all-fnnames1-exec (flg x acc)
  (prog ((pending nil))
   visit
    (cond
     (flg
      (when (null x) (go resume))
      (push (car x) pending)
      (setq x (cdr x)))
     ((variablep x) (go resume))
     ((fquotep x) (go resume))
     ((flambda-applicationp x)
      (push (lambda-body (ffn-symb x)) pending)
      (setq x (fargs x) flg t))
     ((and (eq (ffn-symb x) 'return-last)
           (equal (fargn x 1) '(quote mbe1-raw)))
      (setq x (fargn x 2)))
     ((and (eq (ffn-symb x) 'return-last)
           (equal (fargn x 1) '(quote ec-call1-raw))
           (nvariablep (fargn x 3))
           (not (fquotep (fargn x 3)))
           (not (flambdap (ffn-symb (fargn x 3)))))
      (setq x (fargs (fargn x 3)) flg t))
     (t
      (let* ((args (fargs x)) (new-acc (add-to-set-eq (ffn-symb x) acc)))
        (setq x args flg t acc new-acc))))
    (go visit)
   resume
    (when (null pending) (return acc))
    (setq x (pop pending) flg nil)
    (go visit)))`;
