;;;; Opt-in diagnostics. Collection does not change optimization decisions.
(in-package #:miricl-compiler)

(defvar *compilation-statistics* nil)

(defmacro with-compilation-statistics ((statistics) &body body)
  "Collect counters in STATISTICS, a fresh EQUAL hash table, during BODY.
Nested collectors are independent; ordinary compilation collects nothing.
BODY retains its normal values. Read counters with COMPILATION-STATISTICS-REPORT."
  (list* 'let* (list (list statistics (list 'make-hash-table :test (list 'quote 'equal)))
                    (list '*compilation-statistics* statistics)) body))

(defmacro compiler-statistic (rule &optional detail (amount 1))
  ;; Keep disabled collection to one test, without evaluating labels or
  ;; calling the recorder on the compiler's normal path.
  (list 'if '*compilation-statistics*
    (list 'record-compilation-statistic rule detail amount) nil))

(defun record-compilation-statistic (rule detail amount)
  (when *compilation-statistics*
    (unless (and (keywordp rule) (or (symbolp detail) (integerp detail))
                 (typep amount '(integer 0 *)))
      (compiler-input-fail "compilation statistic" (list rule detail amount)
        "expected a keyword, atomic label and nonnegative increment"))
    (let ((key (list rule detail)))
      (setf (gethash key *compilation-statistics*)
        (+ (gethash key *compilation-statistics* 0) amount))))
  (values))

(defun compilation-statistics-report (&optional (statistics *compilation-statistics*))
  "Return fresh (RULE DETAIL COUNT) rows. Missing counters mean unobserved events.
Coverage is explicit in the instrumented passes, not an inventory of all rules."
  (let ((rows nil))
    (when statistics
      (unless (hash-table-p statistics)
        (compiler-input-fail "compilation statistics" statistics "expected a hash table or NIL"))
      (maphash (lambda (key count)
        (unless (and (consp key) (keywordp (car key)) (consp (cdr key)) (null (cddr key))
                     (or (symbolp (cadr key)) (integerp (cadr key)))
                     (typep count '(integer 0 *)))
          (compiler-input-fail "compilation statistics counter" (list key count)
            "expected (keyword atomic-label) and a nonnegative count"))
        (push (list (car key) (cadr key) count) rows)) statistics))
    rows))
