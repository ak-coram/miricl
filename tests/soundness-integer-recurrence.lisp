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

;; The saved LENGTH bounds the whole loop. Each AREF reads live storage
;; even if a callback changes the array's dimension and fill pointer.
(let* ((worker (compile-native
                '(lambda (left right visit)
                   (declare (optimize (space 2) (speed 1) (safety 1) (debug 1)))
                   (let ((pairs nil))
                     (dotimes (index (length left) (values (nreverse pairs) :done))
                       (funcall visit index)
                       (push (cons (aref left index) (aref right index)) pairs))))))
       (left (make-array 4 :adjustable t :fill-pointer 3
                         :initial-contents '(:a :b :c :d)))
       (right (copy-seq "abcd"))
       (visited nil))
  (assert
   (equal (multiple-value-list
           (funcall worker left right
                    (lambda (index)
                      (push index visited)
                      (when (= index 1)
                        (adjust-array left 5 :fill-pointer 1 :initial-element :new)
                        (setf (aref left index) :changed)))))
          '(((:a . #\a) (:changed . #\b) (:c . #\c)) :done)))
  (assert (and (equal visited '(2 1 0)) (= (fill-pointer left) 1))))

;; Exercise both loop exits and the continuation at the default optimization
;; levels. The inputs run for several grants when batching is available.
;; The checksum and last body value are computed independently of LOOP.
(dolist (case '((2 :below 24581 3 8193 24578)
                (2 :to 24581 3 8194 24581)
                (0 :upto 8192 1 8193 8192)
                (24581 :downto 2 3 8194 2)
                (24581 :above 2 3 8193 5)
                (2 :below 2 1 0 -1)
                (2 :downto 3 1 0 -1)))
  (destructuring-bind (start end-clause limit stride trips last-index) case
    (let* ((source
             `(lambda ()
                (declare (optimize (speed 1) (space 2) (safety 1) (debug 1)))
                (let ((total 0) (last -1))
                  (declare (type (unsigned-byte 16) total)
                           (type (integer -1 24582) last))
                  (loop :for index :from ,start ,end-clause ,limit :by ,stride
                        :do (setq total (logand (+ total index) 65535) last index)
                        :finally (return (values total last))))))
           (checksum (logand (truncate (* trips (+ start last-index)) 2) 65535)))
      (assert (equal (multiple-value-list (funcall (compile-native source)))
                     (list checksum last-index))))))

;; A checked run-time start can reach the exclusive exit from either setup
;; or the successor check. Preserve the loop variable in both cases.
(let ((worker
        (compile-native
         '(lambda (start)
            (declare (optimize (speed 1) (space 2) (safety 1) (debug 1)))
            (if (typep start '(integer -1 40000))
                (let ((total 0))
                  (loop :for index :from start :above 0 :by 3
                        :do (setq total (logand (+ total index) 65535))
                        :finally (return (values total index))))
                :invalid)))))
  (assert (equal (multiple-value-list (funcall worker 30001))
                 (list (logand (* 10001 15001) 65535) 1)))
  (assert (equal (multiple-value-list (funcall worker 0)) '(0 0)))
  (assert (eq (funcall worker :invalid) :invalid)))

;; The same modular recurrence can leave through a run-time predicate in
;; either structured source or a TAGBODY backedge.
(dolist (body '((do ((position 0 (logand (+ position 3) 65535)))
                    ((>= position 65530) (values position :reached)))
                (let ((position 0))
                  (tagbody next
                    (when (< position 65530)
                      (setq position (logand (+ position 3) 65535))
                      (go next)))
                  (values position :reached))))
  (let ((worker (compile-native
                 `(lambda ()
                    (declare (optimize (speed 1) (space 2) (safety 1) (debug 1)))
                    ,body))))
    (assert (equal (multiple-value-list (funcall worker)) '(65532 :reached)))))

;; A callback can replace the function called by the next trip. Its change
;; must survive ordinary LOOP setup and FINALLY must see the completed body.
(let* ((name 'soundness-integer-loop-step)
       (worker
         (compile-native
          '(lambda (setup)
             (declare (notinline soundness-integer-loop-step)
                      (optimize (speed 3) (space 1) (safety 1) (debug 1)))
             (let ((trace nil) (last nil))
               (loop :for index :by (funcall setup :stride 2)
                     :from (funcall setup :start 1) :to (funcall setup :limit 5)
                     :do (push (soundness-integer-loop-step index) trace)
                         (setq last index)
                     :finally (return (values (nreverse trace) last)))))))
       (setup-events nil))
  (unwind-protect
       (progn
         (setf (fdefinition name)
               (lambda (index)
                 (setf (fdefinition name) (lambda (next) (list :changed next)))
                 (list :initial index)))
         (assert
          (equal (multiple-value-list
                   (funcall worker (lambda (part value) (push part setup-events) value)))
                 '(((:initial 1) (:changed 3) (:changed 5)) 5)))
         (assert (equal (reverse setup-events) '(:stride :start :limit))))
    (fmakunbound name)))

;; A dynamic bound can carry the induction variable through the target's
;; FIXNUM limit. The body and FINALLY must still see exact integers.
(let* ((worker
         (compile-native
          '(lambda (start limit)
             (declare (type integer start limit)
                      (optimize (speed 3) (space 1) (safety 1) (debug 1)))
             (let ((total 0) (last nil))
               (loop :for index :from start :upto limit :by 2
                     :do (incf total index) (setq last index)
                     :finally (return (values total last)))))))
       (edge most-positive-fixnum))
  (assert (equal (multiple-value-list (funcall worker (- edge 1) (+ edge 4)))
                 (list (+ (* 3 edge) 3) (+ edge 3)))))
