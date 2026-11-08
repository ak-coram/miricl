// The same Lisp cases run through JSPI, the browser worker and Wasmtime.
export const threadCases = [
  ['native package and feature isolation', `(and
    (null (package-nicknames (find-package :miricl-threads)))
    (member :miricl-threads *features*) miricl-threads:+supports-threads-p+
    (not (member :bordeaux-threads *features*))
    (notany #'find-package '("BORDEAUX-THREADS" "BORDEAUX-THREADS-2" "BT" "BT2")))`],
  ['main thread', '(and (miricl-threads:threadp (miricl-threads:current-thread)) (string= (miricl-threads:thread-name (miricl-threads:current-thread)) "Main thread"))'],
  ['multiple values', `(equal (multiple-value-list (miricl-threads:join-thread
    (miricl-threads:make-thread (lambda () (miricl-threads:thread-yield) (values 42 :ok))))) '(42 :ok))`],
  ['zero values', `(null (multiple-value-list (miricl-threads:join-thread (miricl-threads:make-thread (lambda () (values))))))`],
  ['shared heap and timeslicing', `(let ((stop nil) (count 0))
    (let ((worker (miricl-threads:make-thread (lambda () (loop until stop do (incf count))))))
      (sleep 0.01) (setq stop t) (miricl-threads:join-thread worker) (plusp count)))`],
  ['special bindings', `(progn (defparameter *thread-special* :global)
    (let ((*thread-special* :main))
      (let ((worker (miricl-threads:make-thread
          (lambda () (miricl-threads:thread-yield) (setq *thread-special* :changed) *thread-special*)
          :initial-bindings '((*thread-special* . :worker)))))
        (and (eq (miricl-threads:join-thread worker) :changed) (eq *thread-special* :main)))))`],
  ['global bindings', `(let ((worker (miricl-threads:make-thread (lambda () (setq *thread-special* :shared)))))
    (miricl-threads:join-thread worker) (eq *thread-special* :shared))`],
  ['locks', `(let ((lock (miricl-threads:make-lock :name "counter")) (count 0))
    (let ((workers
            (loop repeat 3 collect
              (miricl-threads:make-thread
                (lambda ()
                  (dotimes (i 25)
                    (miricl-threads:with-lock-held (lock)
                      (let ((old count))
                        (miricl-threads:thread-yield)
                        (setq count (1+ old))))))))))
      (mapc #'miricl-threads:join-thread workers)
      (= count 75)))`],
  ['recursive locks and cleanup', `(let ((lock (miricl-threads:make-recursive-lock)))
    (handler-case (miricl-threads:with-recursive-lock-held (lock)
      (miricl-threads:with-recursive-lock-held (lock) (error "cleanup"))) (error () nil))
    (and (miricl-threads:acquire-recursive-lock lock :wait nil) (progn (miricl-threads:release-recursive-lock lock) t)))`],
  ['keyword lock API', `(let ((lock (miricl-threads:make-lock :name "native lock"))
        (recursive (miricl-threads:make-recursive-lock :name "recursive")))
    (and (miricl-threads:lockp lock) (not (miricl-threads:lockp recursive))
      (miricl-threads:recursive-lock-p recursive)
      (string= (miricl-threads:lock-name lock) "native lock")
      (miricl-threads:acquire-lock lock :wait nil :timeout 0)
      (eq lock (miricl-threads:release-lock lock))
      (miricl-threads:acquire-recursive-lock recursive :wait nil :timeout 0)
      (eq recursive (miricl-threads:release-recursive-lock recursive))
      (handler-case (miricl-threads:acquire-lock lock :timeout -1) (type-error () t))))`],
  ['lock contention timeout', `(let ((lock (miricl-threads:make-lock)))
    (miricl-threads:with-lock-held (lock)
      (let ((worker (miricl-threads:make-thread (lambda ()
          (and (not (miricl-threads:acquire-lock lock :wait nil))
            (not (miricl-threads:acquire-lock lock :timeout 0.005)))))))
        (miricl-threads:join-thread worker))))`],
  ['atomic integer values', `(let ((integer (miricl-threads:make-atomic-integer :value 7)))
    (and (miricl-threads:atomic-integer-p integer)
      (= 7 (miricl-threads:atomic-integer-value integer))
      (not (miricl-threads:atomic-integer-compare-and-swap integer 8 99))
      (miricl-threads:atomic-integer-compare-and-swap integer 7 9)
      (= 10 (miricl-threads:atomic-integer-incf integer))
      (= 7 (miricl-threads:atomic-integer-decf integer 3))
      (= #xffffffff (setf (miricl-threads:atomic-integer-value integer) #xffffffff))
      (= 0 (miricl-threads:atomic-integer-incf integer))
      (= #xffffffff (miricl-threads:atomic-integer-decf integer))
      (handler-case (miricl-threads:make-atomic-integer :value -1) (type-error () t))))`],
  ['atomic integer contention', `(let ((integer (miricl-threads:make-atomic-integer)))
    (let ((workers (loop repeat 3 collect (miricl-threads:make-thread
        (lambda () (dotimes (i 100) (miricl-threads:atomic-integer-incf integer)
          (miricl-threads:thread-yield)))))))
      (mapc #'miricl-threads:join-thread workers)
      (= 300 (miricl-threads:atomic-integer-value integer))))`],
  ['condition notification', `(let ((lock (miricl-threads:make-lock)) (cv (miricl-threads:make-condition-variable))
        (ready (miricl-threads:make-semaphore)) (value nil))
    (let ((worker (miricl-threads:make-thread (lambda () (miricl-threads:with-lock-held (lock)
        (miricl-threads:signal-semaphore ready)
        (loop until value do (miricl-threads:condition-wait cv lock)) value)))))
      (miricl-threads:wait-on-semaphore ready)
      (miricl-threads:with-lock-held (lock) (setq value :ready) (miricl-threads:condition-notify cv))
      (eq (miricl-threads:join-thread worker) :ready)))`],
  ['condition timeout reacquires lock', `(let ((lock (miricl-threads:make-lock)) (cv (miricl-threads:make-condition-variable)))
    (miricl-threads:with-lock-held (lock) (not (miricl-threads:condition-wait cv lock :timeout 0.005))))`],
  ['semaphore count keyword', `(let ((semaphore (miricl-threads:make-semaphore)))
    (miricl-threads:signal-semaphore semaphore :count 2)
    (and (miricl-threads:wait-on-semaphore semaphore :timeout 0)
      (miricl-threads:wait-on-semaphore semaphore :timeout 0)
      (not (miricl-threads:wait-on-semaphore semaphore :timeout 0))))`],
  ['blocking timeout cleanup', `(let ((cleaned nil))
    (and (handler-case (miricl-threads:with-timeout (0.005)
        (unwind-protect (sleep 10) (setq cleaned t)))
      (miricl-threads:timeout () t)) cleaned))`],
  ['CPU timeout', '(handler-case (miricl-threads:with-timeout (0.005) (loop)) (miricl-threads:timeout () t))'],
  ['timeout scope and multiple values', `(and
    (equal (multiple-value-list (miricl-threads:with-timeout (0.005) (values 1 2))) '(1 2))
    (null (sleep 0.02)))`],
  ['standard IO binding forms', `(miricl-threads:join-thread (miricl-threads:make-thread
    (lambda () (and (eq *package* (find-package :cl-user)) (readtablep *readtable*)
      (eq *read-default-float-format* 'single-float) (random-state-p *random-state*)))
    :initial-bindings miricl-threads:*standard-io-bindings*))`],
  ['semaphore timeout', '(not (miricl-threads:wait-on-semaphore (miricl-threads:make-semaphore) :timeout 0.005))'],
  ['error isolation', `(let ((worker (miricl-threads:make-thread (lambda () (error "thread failure")))))
    (handler-case (progn (miricl-threads:join-thread worker) nil)
      (miricl-threads:abnormal-exit (c) (and (typep (miricl-threads:abnormal-exit-condition c) 'error)
        (not (miricl-threads:thread-alive-p worker))))))`],
  ['reader and multiple values survive suspension', `(let ((worker (miricl-threads:make-thread (lambda ()
      (dotimes (i 20) (read-from-string "#1=(a . #1#)") (miricl-threads:thread-yield)) :read))))
    (and (equal (multiple-value-list (multiple-value-prog1 (values 1 2 3) (miricl-threads:join-thread worker))) '(1 2 3))
      (equal (read-from-string "(4 5 6)") '(4 5 6))))`],
  ['reader callback suspends', `(let ((table (copy-readtable nil)))
    (set-macro-character #\\! (lambda (stream character)
      (declare (ignore character)) (miricl-threads:thread-yield) (read stream t nil t)) nil table)
    (let ((worker (miricl-threads:make-thread (lambda () (read-from-string "(1 !2 3)"))
        :initial-bindings (list (cons '*readtable* (list 'quote table))))))
      (dotimes (i 10) (read-from-string "#1=(a . #1#)") (miricl-threads:thread-yield))
      (equal (miricl-threads:join-thread worker) '(1 2 3))))`],
  ['formatter callback suspends', `(let ((worker (miricl-threads:make-thread (lambda ()
      (format nil (lambda (stream) (write-string "left" stream)
        (miricl-threads:thread-yield) (write-string "right" stream)))))))
    (miricl-threads:thread-yield)
    (and (string= (format nil "~D" 123) "123")
      (string= (miricl-threads:join-thread worker) "leftright")))`],
  ['next-method context across thread switches', `(progn
    (defmethod yielding-next-method ((x t)) (values (1+ x) x))
    (defmethod yielding-next-method ((x integer))
      (let ((next (lambda () (call-next-method))))
        (miricl-threads:thread-yield)
        (funcall next)))
    (let ((a (miricl-threads:make-thread (lambda () (multiple-value-list (yielding-next-method 17)))))
          (b (miricl-threads:make-thread (lambda () (multiple-value-list (yielding-next-method 23))))))
      (and (equal (miricl-threads:join-thread a) '(18 17))
           (equal (miricl-threads:join-thread b) '(24 23)))))`],
  ['interrupt sleeping thread', `(let ((ready (miricl-threads:make-semaphore)) (interrupted nil))
    (let ((worker (miricl-threads:make-thread (lambda () (miricl-threads:signal-semaphore ready) (sleep 0.02) :done))))
      (miricl-threads:wait-on-semaphore ready)
      (miricl-threads:interrupt-thread worker (lambda () (setq interrupted (eq worker (miricl-threads:current-thread)))))
      (and (eq (miricl-threads:join-thread worker) :done) interrupted)))`],
  ['destroy before startup', `(let ((ran nil))
    (let ((worker (miricl-threads:make-thread (lambda () (setq ran t)))))
      (miricl-threads:destroy-thread worker)
      (and (handler-case (progn (miricl-threads:join-thread worker) nil) (miricl-threads:abnormal-exit () t))
        (not ran))))`],
  ['destroy runs cleanup', `(let ((ready (miricl-threads:make-semaphore)) (cleaned nil))
    (let ((worker (miricl-threads:make-thread (lambda ()
        (unwind-protect (progn (miricl-threads:signal-semaphore ready) (loop (miricl-threads:thread-yield))) (setq cleaned t))))))
      (miricl-threads:wait-on-semaphore ready) (miricl-threads:destroy-thread worker)
      (handler-case (miricl-threads:join-thread worker) (miricl-threads:abnormal-exit () nil))
      (and cleaned (not (miricl-threads:thread-alive-p worker)))))`]
];
