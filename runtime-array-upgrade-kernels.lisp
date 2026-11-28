;;;; Target array element specialization, above checked type services.
(in-package #:miricl-runtime)
(export '(upgraded-array-kind-value))

;; Private closed workers carry per-activation state explicitly. The context is
;; (remaining metadata environment minimum maximum lower upper . integer-type).
;; Only fuel and the final result triple are mutable; recursion shares fuel and
;; snapshots each child result before descending again. Primitive cons storage
;; avoids recursion through MAKE-ARRAY/UAET.

;; The closed worker ABI is single-valued. The final three context fields carry
;; the other WALK results; callers copy them before any subsequent callback.
(defun %array-upgrade-store-result (lower upper integer-type context)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr rplaca rplacd))
  (let* ((bounds (cdr (the cons (cdr (the cons (cdr (the cons context)))))))
         (output (cdr (the cons (cdr (the cons bounds))))))
    (rplaca (the cons output) lower)
    (rplaca (the cons (cdr (the cons output))) upper)
    (rplacd (the cons (cdr (the cons output))) integer-type)
    nil))

(defun %array-upgrade-range-kind (lower upper context)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons rplaca rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))
  (let* ((metadata (car (the cons (cdr (the cons context)))))
         (bounds (cdr (the cons (cdr (the cons (cdr (the cons context)))))))
         (minimum (car (the cons bounds)))
         (maximum (car (the cons (cdr (the cons bounds))))))
    (if (and lower upper)
        (if (> (the integer (funcall metadata 2 lower upper)) 0)
            3
            (if (and (>= (the integer (funcall metadata 2 lower 0)) 0)
                     (<= (the integer (funcall metadata 2 upper 1)) 0))
                2
                (if (and (>= (the integer (funcall metadata 2 lower 0)) 0)
                         (<= (the integer (funcall metadata 2 upper 255)) 0))
                    6
                    (if (and (>= (the integer (funcall metadata 2 lower minimum)) 0)
                             (<= (the integer (funcall metadata 2 upper maximum)) 0))
                        4
                        0))))
        0)))

(defun %array-upgrade-integer-kind-p (kind)
  (declare (type (integer 0 6) kind) (optimize (safety 0) (debug 0)) (inline =))
  (or (= kind 2) (= kind 4) (= kind 6)))

(defun %array-upgrade-join-kind (a b)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons rplaca rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))

  (if (= a 3)
      b
      (if (= b 3)
          a
          (if (= a b)
              a
              (if (and (%array-upgrade-integer-kind-p a) (%array-upgrade-integer-kind-p b))
                  (if (or (= a 4) (= b 4)) 4 6)
                  0)))))

(defun %array-upgrade-meet-kind (a b)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons rplaca rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))

  (if (or (= a 3) (= b 3))
      3
      (if (= a 0)
          b
          (if (= b 0)
              a
              (if (= a b)
                  a
                  (if (and (%array-upgrade-integer-kind-p a) (%array-upgrade-integer-kind-p b))
                      (if (or (= a 2) (= b 2)) 2 6)
                      3))))))

(defun %array-upgrade-fixed-kind (specifier)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons rplaca rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))

  (case specifier
    ((t integer signed-byte unsigned-byte) 0)
    ((character base-char standard-char) 1)
    (bit 2) ((nil extended-char) 3) (fixnum 4) ((double-float long-float) 5) ((single-float short-float) 7)))

(defun %array-upgrade-fixed-bound (bound)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons rplaca rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))

  ;; Only a scalar integer or its singleton exclusive wrapper. The
  ;; constant-size copy cannot follow an untrusted or cyclic spine.
  (if (or (integerp bound) (eq bound '*))
      bound
      (if (and (consp bound) (null (cdr bound)) (integerp (car bound)))
          (cons (car bound) nil)
          nil)))

(defun %array-upgrade-fixed-integer-form (specifier)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons rplaca rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))

  ;; Standard integer constructors need no derived-type callbacks.
  ;; Copy their bounded syntax before asking the target interval
  ;; service, just as FIXED-LOGICAL owns its argument spine.
  (if (consp specifier)
      (let ((name (car specifier)) (arguments (cdr specifier)))
        (case name
          ((signed-byte unsigned-byte)
           (if (null arguments)
               (cons name nil)
               (if (and (consp arguments) (null (cdr arguments))
                        (or (integerp (car arguments)) (eq (car arguments) '*)))
                   (cons name (cons (car arguments) nil))
                   nil)))
          (integer
           (if (null arguments)
               (cons name nil)
               (if (consp arguments)
                   (let ((lower (%array-upgrade-fixed-bound (car arguments))) (tail (cdr arguments)))
                     (if lower
                         (if (null tail)
                             (cons name (cons lower nil))
                             (if (and (consp tail) (null (cdr tail)))
                                 (let ((upper (%array-upgrade-fixed-bound (car tail))))
                                   (if upper
                                       (cons name (cons lower (cons upper nil)))))
                                 nil))))
                   nil)))))))

(defun %array-upgrade-snapshot-parts (arguments context)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons rplaca rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))
  (let* ((metadata (car (the cons (cdr (the cons context))))))
    (let ((head nil) (tail nil))
      (if (funcall metadata 3 arguments nil) nil (funcall metadata 4 arguments 3))
      ;; The inline spine copy invokes no callback and owns its conses.
      ;; Later metadata calls cannot mutate its iteration state.
      (let ((scan arguments))
        (tagbody upgrade-next-1
          (if (null scan) (go upgrade-done-1))
          (let ((cell (cons (car (the cons scan)) nil)))
            (if tail (rplacd (the cons tail) cell) (setq head cell)) (setq tail cell))
          (setq scan (cdr (the cons scan)))
          (go upgrade-next-1)
         upgrade-done-1))
      head)))

(defun %array-upgrade-fixed-logical (specifier context)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons rplaca rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))
  (let* ((metadata (car (the cons (cdr (the cons context)))))
         (rest (cdr (the cons (cdr (the cons context)))))
         (environment (car (the cons rest)))
         (bounds (cdr (the cons rest)))
         (minimum (car (the cons bounds)))
         (maximum (car (the cons (cdr (the cons bounds))))))
    ;; Finite fixed atoms and bounded integer constructor syntax are
    ;; ordinary linear semantic work, not optional recursive proof effort.
    (if (and (consp specifier) (or (eq (car specifier) 'and) (eq (car specifier) 'or)))
        (let* ((arguments (cdr specifier)) (head (%array-upgrade-snapshot-parts arguments context)))
          (if (eq arguments (cdr specifier)) nil
              (return-from %array-upgrade-fixed-logical nil))
          (let* ((unionp (eq (car specifier) 'or))
                 (kind (if unionp 3 0)) (integers unionp)
                 (lower nil) (upper nil) (first t))
            (if (or unionp (eq (car specifier) 'and)) nil
                (return-from %array-upgrade-fixed-logical nil))
            (let ((parts head))
              (tagbody upgrade-next-2
                (if (null parts) (go upgrade-done-2))
                (let ((part (car parts)))
                  (let* ((other (%array-upgrade-fixed-kind part))
                        (integer-type (or (eq part 'fixnum) (eq part 'bit) (eq part 'integer)
                                          (eq part 'signed-byte) (eq part 'unsigned-byte)))
                        (form (and (null other) (%array-upgrade-fixed-integer-form part)))
                        (interval (and form (funcall metadata 1 form environment))))
                    (if (or other form) nil
                        (return-from %array-upgrade-fixed-logical nil))
                    (if interval
                        (setq other (if (= (svref (the simple-vector interval) 2) 2) 3
                                      (%array-upgrade-range-kind (svref (the simple-vector interval) 0)
                                                  (svref (the simple-vector interval) 1) context))
                              integer-type t))
                    (let ((lo (if (= other 2)
                                  0
                                  (if (= other 4)
                                      minimum
                                      (if (or (= other 1) (= other 3))
                                          1
                                          (if (eq part 'unsigned-byte)
                                              0
                                              nil)))))
                          (hi (if (= other 2)
                                  1
                                  (if (= other 4)
                                      maximum
                                      (if (or (= other 1) (= other 3))
                                          0
                                          nil)))))
                      ;; Retain exact endpoints, rather than widening each
                      ;; byte leaf to its storage kind before an intersection.
                      (if interval
                          (setq lo (if (= other 3) 1 (svref (the simple-vector interval) 0))
                                hi (if (= other 3) 0 (svref (the simple-vector interval) 1))))
                      (if unionp
                          (progn (setq kind (%array-upgrade-join-kind kind other))
                            (if (= other 3) nil
                                (setq integers (and integers integer-type)))
                            (if (or (= other 1) (= other 3)) nil
                                (if first
                                    (setq lower lo upper hi first nil)
                                    (progn
                                      (setq lower (and lower lo (if (< (the integer (funcall metadata 2 lower lo)) 0) lower lo)))
                                      (setq upper (and upper hi (if (> (the integer (funcall metadata 2 upper hi)) 0) upper hi)))))))
                          (progn (setq kind (%array-upgrade-meet-kind kind other))
                            (if integer-type (setq integers t))
                            (if (and lo (or (null lower) (> (the integer (funcall metadata 2 lo lower)) 0))) (setq lower lo))
                            (if (and hi (or (null upper) (< (the integer (funcall metadata 2 hi upper)) 0))) (setq upper hi)))))))
                (setq parts (cdr parts))
                (go upgrade-next-2)
               upgrade-done-2))
            (if (and unionp first) (setq lower 1 upper 0))
            (if integers (setq kind (%array-upgrade-meet-kind kind (%array-upgrade-range-kind lower upper context))))
            (if (and integers (= kind 1)) (setq kind 3))
            (let* ((result-kind kind) (result-integers integers)
                   (result-known t) (result-lower lower) (result-upper upper))
              (declare (ignore result-known))
              (%array-upgrade-store-result result-lower result-upper result-integers context)
              result-kind))))))

(defun %array-upgrade-walk (specifier depth context)
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons rplaca rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))
  (let* ((metadata (car (the cons (cdr (the cons context)))))
         (rest (cdr (the cons (cdr (the cons context)))))
         (environment (car (the cons rest)))
         (bounds (cdr (the cons rest)))
         (minimum (car (the cons bounds)))
         (maximum (car (the cons (cdr (the cons bounds)))))
         (output (cdr (the cons (cdr (the cons bounds))))))
    (let ((output-lower nil) (output-upper nil) (output-integers nil))
      ;; Every exit computes its fields into activation-local temporaries. One
      ;; epilogue publishes them, avoiding repeated captured helper references.
      (let ((kind
              (block %array-upgrade-walk
                ;; Even exhausted recursive analysis retains exact fixed target
                ;; facts. A surrounding AND can still use a known storage cover.
                (case specifier
                  ((double-float long-float) (return-from %array-upgrade-walk 5))
                  ((single-float short-float) (return-from %array-upgrade-walk 7))
                  (fixnum (return-from %array-upgrade-walk (let* ((result-kind 4)
                             (result-lower minimum) (result-upper maximum) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                  (bit (return-from %array-upgrade-walk (let* ((result-kind 2)
                             (result-lower 0) (result-upper 1) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                  (integer (return-from %array-upgrade-walk (let* ((result-kind 0)
                             (result-lower nil) (result-upper nil) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                  ((character base-char standard-char) (return-from %array-upgrade-walk (let* ((result-kind 1)
                             (result-lower 1) (result-upper 0) (result-integers nil))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind))))
                (if specifier nil (return-from %array-upgrade-walk (let* ((result-kind 3)
                             (result-lower 1) (result-upper 0) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                (setq specifier (funcall metadata 0 specifier environment))
                (if specifier nil (return-from %array-upgrade-walk (let* ((result-kind 3)
                             (result-lower nil) (result-upper nil) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                (if (or (symbolp specifier) (consp specifier)) nil
                    (let ((class (funcall metadata 5 specifier nil)))
                      (if class (setq specifier (car class)))))
                (case specifier
                  ((double-float long-float) (return-from %array-upgrade-walk 5))
                  ((single-float short-float) (return-from %array-upgrade-walk 7))
                  (fixnum (return-from %array-upgrade-walk (let* ((result-kind 4)
                             (result-lower minimum) (result-upper maximum) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                  (bit (return-from %array-upgrade-walk (let* ((result-kind 2)
                             (result-lower 0) (result-upper 1) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                  (integer (return-from %array-upgrade-walk (let* ((result-kind 0)
                             (result-lower nil) (result-upper nil) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                  ((character base-char standard-char) (return-from %array-upgrade-walk (let* ((result-kind 1)
                             (result-lower 1) (result-upper 0) (result-integers nil))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                  (extended-char (return-from %array-upgrade-walk (let* ((result-kind 3)
                             (result-lower nil) (result-upper nil) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind))))
                (let ((kind (%array-upgrade-fixed-logical specifier context)))
                  (if kind (return-from %array-upgrade-walk (let* ((result-kind kind)
                             (result-lower (car (the cons output))) (result-upper (car (the cons (cdr (the cons output))))) (result-integers (cdr (the cons (cdr (the cons output))))))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind))))
                (if (or (<= (car (the cons context)) 0) (>= depth 128))
                    (return-from %array-upgrade-walk (let* ((result-kind 0)
                             (result-lower nil) (result-upper nil) (result-integers nil))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                (rplaca (the cons context) (1- (car (the cons context))))
                (let ((name specifier) (arguments nil) (count 0))
                  (if (consp specifier)
                      (progn
                        (setq count (funcall metadata 3 (cdr specifier) nil))
                        (setq name (car specifier) arguments (cdr specifier))
                        (if count nil (funcall metadata 4 specifier 3))
                        (if (case name
                              (integer (<= (the integer count) 2))
                              ((signed-byte unsigned-byte) (<= (the integer count) 1))
                              ((mod eql not) (= (the integer count) 1))
                              (otherwise t))
                            nil (funcall metadata 4 specifier 1))))
                  ;; Existing target intervals also retain symbolic huge endpoints.
                  (let* ((interval (funcall metadata 1 specifier environment))
                         (lower (svref (the simple-vector interval) 0))
                         (upper (svref (the simple-vector interval) 1))
                         (status (svref (the simple-vector interval) 2)))
                    (if (= (the (integer 0 2) status) 2)
                        (return-from %array-upgrade-walk (let* ((result-kind 3)
                             (result-lower nil) (result-upper nil) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                    (if (= (the (integer 0 2) status) 1)
                        (return-from %array-upgrade-walk (let* ((result-kind (%array-upgrade-range-kind lower upper context))
                             (result-lower lower) (result-upper upper) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind))))
                  (case name
                    ((double-float long-float single-float short-float)
                     (return-from %array-upgrade-walk (funcall metadata 6 specifier name)))
                    ((member eql)
                     (let ((scan (%array-upgrade-snapshot-parts arguments context)) (kind 3) (lower nil) (upper nil)
                           (integers t) (value nil) (candidate 0))
                       (tagbody next
                          (if scan nil (go done))
                          ;; Finite literal classification is linear semantic
                          ;; work, just like the fixed logical form fallback.
                          (setq value (car scan) candidate 0)
                          (if (characterp value)
                              (setq candidate 1 integers nil)
                              (if (integerp value)
                                  (progn
                                    (setq candidate (%array-upgrade-range-kind value value context))
                                    (if (or (null lower) (< (the integer (funcall metadata 2 value lower)) 0))
                                        (setq lower value))
                                    (if (or (null upper) (> (the integer (funcall metadata 2 value upper)) 0))
                                        (setq upper value)))
                                  (setq integers nil candidate (if (typep value 'double-float) 5
                                                               (if (typep value 'single-float) 7 0)))))
                          (setq kind (%array-upgrade-join-kind kind candidate) scan (cdr scan))
                          (go next)
                         done)
                       ;; The endpoints cover the integer PART even when the
                       ;; MEMBER type also contains nonintegers. An AND with an
                       ;; integer type can use this cover without retaining them.
                       (return-from %array-upgrade-walk (let* ((result-kind kind)
                             (result-lower (if lower lower 1)) (result-upper (if upper upper 0)) (result-integers integers))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind))))
                    ((and or)
                     ;; Reserve this frame's complete scan and child-visit work
                     ;; before descending, so a child cannot consume its credits.
                     (if (> (the integer count) (car (the cons context)))
                         (return-from %array-upgrade-walk (let* ((result-kind 0)
                             (result-lower nil) (result-upper nil) (result-integers nil))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                     (let ((cost (truncate (* count (+ count 3)) 2)))
                       (if (> cost (car (the cons context))) (return-from %array-upgrade-walk (let* ((result-kind 0)
                             (result-lower nil) (result-upper nil) (result-integers nil))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))
                       (rplaca (the cons context) (- (car (the cons context)) cost)))
                     (let ((unionp (eq name 'or)) (first t)
                           (kind (if (eq name 'or) 3 0))
                           (lower nil) (upper nil) (integers (eq name 'or)) (left count))
                       (let ((scan arguments))
                         (tagbody upgrade-next-3
                           (if (null scan) (go upgrade-done-3))
                           ;; Expansion can mutate a previously checked spine.
                           (let ((current (funcall metadata 3 scan nil)))
                             (if current nil (funcall metadata 4 scan 3))
                             (if (> (the integer current) left)
                                 (return-from %array-upgrade-walk (let* ((result-kind 0)
                             (result-lower nil) (result-upper nil) (result-integers nil))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind))))
                           (setq left (1- left))
                           (let* ((other (%array-upgrade-walk (car scan) (1+ depth) context))
                                  (lo (car (the cons output)))
                                  (hi (car (the cons (cdr (the cons output)))))
                                  (integer-type (cdr (the cons (cdr (the cons output))))))
                             (if unionp
                                 (progn
                                   (setq kind (%array-upgrade-join-kind kind other))
                                   (if (= other 3) nil
                                       (progn
                                         (setq integers (and integers integer-type))
                                         (if (and lo hi (> (the integer (funcall metadata 2 lo hi)) 0)) nil
                                             (if first
                                                 (setq lower lo upper hi first nil)
                                                 (progn
                                                   (setq lower (and lower lo
                                                     (if (< (the integer (funcall metadata 2 lower lo)) 0) lower lo)))
                                                   (setq upper (and upper hi
                                                     (if (> (the integer (funcall metadata 2 upper hi)) 0) upper hi)))))))))
                                 (progn
                                   (setq kind (%array-upgrade-meet-kind kind other))
                                   (if integer-type (setq integers t))
                                   (if (and lo (or (null lower)
                                       (> (the integer (funcall metadata 2 lo lower)) 0))) (setq lower lo))
                                   (if (and hi (or (null upper)
                                       (< (the integer (funcall metadata 2 hi upper)) 0))) (setq upper hi)))))
                           (setq scan (cdr scan))
                           (go upgrade-next-3)
                          upgrade-done-3))
                       ;; Unknown conjuncts restrict a known integer cover. They
                       ;; cannot widen it or justify calling a user predicate.
                       (if (and unionp first) (setq lower 1 upper 0))
                       (if integers
                           (setq kind (%array-upgrade-meet-kind kind (%array-upgrade-range-kind lower upper context))))
                       (if (and integers (= kind 1)) (setq kind 3))
                       (return-from %array-upgrade-walk (let* ((result-kind kind)
                             (result-lower lower) (result-upper upper) (result-integers integers))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind))))
                    (not (if (eq (car arguments) t) (return-from %array-upgrade-walk (let* ((result-kind 3)
                             (result-lower nil) (result-upper nil) (result-integers t))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))))
                  (let* ((result-kind 0)
                             (result-lower nil) (result-upper nil) (result-integers nil))
                        (setq output-lower result-lower output-upper result-upper output-integers result-integers)
                        result-kind)))))
        (%array-upgrade-store-result output-lower output-upper output-integers context)
        kind))))

(defun upgraded-array-kind-value (type environment metadata)
  "Return target storage kind: object0, character1, bit2, empty3, fixnum4, binary64 kind 5, octet6, binary32 kind 7.
METADATA supplies target expansion and integer endpoints. This bounded walk
never executes SATISFIES and never depends on host integer representation."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr cons rplacd null consp eq eql characterp integerp symbolp = < > <= >= svref))
  ;; Immutable standard atoms need neither target endpoints nor scratch state.
  (case type
    ((t) (return-from upgraded-array-kind-value 0))
    ((nil) (return-from upgraded-array-kind-value 3))
    (bit (return-from upgraded-array-kind-value 2))
    (character (return-from upgraded-array-kind-value 1))
    (fixnum (return-from upgraded-array-kind-value 4))
    ((double-float long-float) (return-from upgraded-array-kind-value 5))
    ((single-float short-float) (return-from upgraded-array-kind-value 7)))
  (let* ((target (funcall metadata 1 'fixnum environment))
         (minimum (svref (the simple-vector target) 0))
         (maximum (svref (the simple-vector target) 1))
         (context (cons 8192 (cons metadata (cons environment
                    (cons minimum (cons maximum (cons nil (cons nil nil)))))))))
    (values (%array-upgrade-walk type 0 context))))
