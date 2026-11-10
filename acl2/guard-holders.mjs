// Preserve the logical walk's order, changed flags, normalization, and shared
// suffixes. Continuations for terms and term lists live on a heap work list.
export const iterativeGuardHolders = `(progn
(defun miricl-remove-guard-holders-work (kind changedp0 item lamp)
  (let ((work (list (list kind changedp0 item))) (changed nil) (value nil))
    (loop while work do
      (let* ((task (pop work)) (kind (car task)))
        (case kind
          (:term
           (let ((force (cadr task)) (term (caddr task)))
             (cond
               ((or (variablep term) (fquotep term)
                    (and (eq (ffn-symb term) 'hide)
                         (remove-guard-holders-blocked-by-hide-p)))
                (setq changed force value term))
               ((member-eq (ffn-symb term) '(return-last mv-list the-check))
                (push (list :term t (car (last (fargs term)))) work))
               ((eq (ffn-symb term) 'cons-with-hint)
                (push (list :cons-second term) work)
                (push (list :term nil (fargn term 1)) work))
               ((eq (ffn-symb term) 'to-df)
                (let ((arg (fargn term 1)))
                  (if (and (quotep arg) (dfp (unquote arg)))
                      (setq changed t value arg)
                    (progn (push (list :to-df force) work)
                           (push (list :term nil arg) work)))))
               ((eq (ffn-symb term) 'from-df)
                (push (list :force) work)
                (push (list :term nil (fargn term 1)) work))
               ((eq (ffn-symb term) 'df0) (setq changed t value *0*))
               ((eq (ffn-symb term) 'df1) (setq changed t value *1*))
               ((flambdap (ffn-symb term))
                (case-match term
                  ((('lambda ('var) ('the-check & & 'var)) val)
                   (push (list :term t val) work))
                  ((('lambda formals ('return-last ''mbe1-raw & logic)) . args)
                   (push (list :mbe-logic formals logic) work)
                   (push (list :list nil args) work))
                  (&
                   (push (list :lambda-args term force) work)
                   (push (list :term nil (lambda-body (ffn-symb term))) work))))
               (t (push (list :call-finish term force) work)
                  (push (list :list nil (fargs term)) work)))))
          ((:list :lists)
           (let ((items (caddr task)))
             (if (if (eq kind :list) (endp items) (null items))
                 (setq changed nil value nil)
               (progn
                 (push (list :list-rest kind items) work)
                 (push (list (if (eq kind :list) :term :list) nil (car items)) work)))))
          (:list-rest
           (push (list :list-finish (caddr task) changed value) work)
           (push (list (cadr task) nil (cdr (caddr task))) work))
          (:list-finish
           (if (or (caddr task) changed)
               (setq changed t value (cons (cadddr task) value))
             (setq changed nil value (cadr task))))
          (:force (setq changed t))
          (:cons-second
           (push (list :cons-finish value) work)
           (push (list :term nil (fargn (cadr task) 2)) work))
          (:cons-finish
           (setq changed t value (mcons-term* 'cons (cadr task) value)))
          (:to-df
           (setq changed (or (cadr task) changed) value (fcons-term* 'to-df value)))
          (:mbe-logic
           (push (list :mbe-finish (cadr task) value) work)
           (push (list :term nil (caddr task)) work))
          (:mbe-finish
           (setq changed t value (subcor-var (cadr task) (caddr task) value)))
          (:lambda-args
           (push (list :lambda-finish (cadr task) (caddr task) changed value) work)
           (push (list :list nil (fargs (cadr task))) work))
          (:lambda-finish
           (let* ((term (cadr task)) (force (caddr task)) (body-changed (cadddr task))
                  (body (car (cddddr task))) (formals (lambda-formals (ffn-symb term))))
             (cond ((and lamp (consp formals) (null (cdr formals)) (eq (car formals) body))
                    (setq changed t value (car value)))
                   ((and lamp (trivial-lambda-p formals value body))
                    (setq changed t value body))
                   ((or force body-changed changed)
                    (setq changed t value (mcons-term
                      (if body-changed (make-lambda formals body) (ffn-symb term)) value)))
                   (t (setq changed force value term)))))
          (:call-finish
           (let ((term (cadr task)) (force (caddr task)))
             (cond ((and (eq (ffn-symb term) 'do$) (quotep (fargn term 6))
                         (unquote (fargn term 6)))
                    (setq changed t value (mcons-term 'do$ (append (take 5 value) (list *nil*)))))
                   ((null changed)
                    (if (quote-listp value)
                        (let ((new-term (mcons-term (ffn-symb term) value)))
                          (if (equal term new-term) (setq changed force value term)
                            (setq changed t value new-term)))
                      (setq changed force value term)))
                   (t (setq changed t value (mcons-term (ffn-symb term) value)))))))))
    (mv changed value)))
(defun remove-guard-holders1 (changedp0 term lamp)
  (miricl-remove-guard-holders-work :term changedp0 term lamp))
(defun remove-guard-holders1-lst (lst lamp)
  (miricl-remove-guard-holders-work :list nil lst lamp)))`;

export const iterativeGuardHolderLists = `(defun remove-guard-holders1-lst-lst (lst lamp)
  (miricl-remove-guard-holders-work :lists nil lst lamp))`;
