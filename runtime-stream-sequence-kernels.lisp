;;;; Portable stream-sequence algorithms. Checked native boundaries retain
;;;; stream representation/platform operations and canonical callback objects.
(in-package #:miricl-runtime)
(export '(stream-sequence-control-values stream-sequence-text-value
          stream-sequence-transfer-value stream-broadcast-sequence-value))

(defun stream-sequence-control-values (arguments mode operations)
  "Return #(sequence stream first stop), or NIL for a malformed keyword packet.
OPERATIONS captures canonical LENGTH and ERROR, independently of public bindings. Native callers
retain their established arity reporter for a NIL result."
  (declare (type (integer 0 3) mode) (type simple-vector operations)
           (optimize (safety 0) (debug 0))
           (inline consp car cdr checked-path-car checked-path-cdr eq >= < = -
                   integerp > cons stringp make-array svref (setf svref)))
  (values
   (let ((size (svref operations 0)) (fail (svref operations 14)))
    (declare (type function size fail))
    (block rejected
     (if arguments
         (if (consp arguments) nil (funcall fail 'type-error :datum arguments :expected-type 'list)))
     ;; Validate the complete spine before any unchecked access, including when
     ;; an early keyword or arity would otherwise end parsing.
     (let ((count (funcall size arguments)))
       (declare (type (integer 0 #x7fffffff) count))
       (if (< count (if (< mode 2) 2 1)) (return-from rejected nil)))
     (let* ((sequence (checked-path-car arguments))
            (scan (checked-path-cdr arguments))
            (stream (if scan (checked-path-car scan)))
            (first 0) (stop nil) (seen-start nil) (seen-end nil)
            (seen-allow nil) (allow nil) (unknown nil))
       (if scan (setq scan (checked-path-cdr scan)))
       (tagbody key
          (if scan nil (go bounded))
          (let ((keyword (checked-path-car scan)))
            (setq scan (checked-path-cdr scan))
            (if scan nil (return-from rejected nil))
            (let ((value (checked-path-car scan)))
              (setq scan (checked-path-cdr scan))
              (if (eq keyword :start)
                  (if seen-start nil (setq first value seen-start t))
                  (if (eq keyword :end)
                      (if seen-end nil (setq stop value seen-end t))
                      (if (eq keyword :allow-other-keys)
                          (if seen-allow nil (setq allow value seen-allow t))
                          (setq unknown t))))))
          (go key)
         bounded)
       (if unknown (if allow nil (return-from rejected nil)))
       (if (>= mode 2)
           (if (stringp sequence) nil (funcall fail 'type-error :datum sequence :expected-type 'string)))
       (let* ((limit (funcall size sequence)) (value stop) (startp nil)
              (result (make-array 4)))
         (declare (type simple-vector result))
         (tagbody bound
            (if startp nil (if value nil (setq value limit)))
            (if (integerp value)
                (if (< value 0) (go bad)
                    (if (> value limit) (go bad) (go valid)))
                (go bad))
           bad
            (funcall fail 'type-error :datum value :expected-type (cons 'integer (cons 0 (cons limit nil))))
           valid
            (if startp (progn (setq first value) (go ready)))
            (setq stop value limit value value first startp t)
            (go bound)
           ready)
         (setf (svref result 0) sequence)
         (setf (svref result 1) stream)
         (setf (svref result 2) first)
         (setf (svref result 3) stop)
         result))))))

(defun stream-sequence-text-value (sequence first count operations)
  "Materialize a checked region as character text before any output occurs."
  (declare (type (integer 0 #x1ffffffe) first count) (type simple-vector operations)
           (optimize (safety 0) (debug 0))
           (inline svref vectorp row-major-aref make-string char (setf char) characterp consp car cdr
                   checked-path-car checked-path-cdr = 1+ +))
  (values
   (let ((result (make-string count)) (scan sequence) (index 0) (skip 0)
         (indexed (vectorp sequence)) (fail (svref operations 14)))
     (declare (type function fail) (type simple-string result)
              (type (integer 0 #x1ffffffe) index skip))
     (if indexed nil
         (tagbody position
            (if (= skip first) (go positioned))
            (setq scan (checked-path-cdr scan) skip (the (integer 0 #x1ffffffe) (1+ skip)))
            (go position)
           positioned))
     (tagbody next
        (if (= index count) (go done))
        (let ((value (if indexed (row-major-aref sequence (+ first index)) (checked-path-car scan))))
          (if (characterp value) (setf (char result index) (the character value))
              (funcall fail 'type-error :datum value :expected-type 'character)))
        (if indexed nil (setq scan (checked-path-cdr scan)))
        (setq index (the (integer 0 #x1ffffffe) (1+ index)))
        (go next)
       done)
     result)))

(defun stream-sequence-transfer-value (sequence peer state operations)
  "Checked traversal. STATE = #(first stop source-first mode binary).
MODE is read stream=0, write bytes=1, forward array copy=2, write characters=3.
OPERATIONS captures READ-BYTE, READ-CHAR, WRITE-BYTE and WRITE-CHAR."
  (declare (type simple-vector state operations) (optimize (safety 0) (debug 0))
           (inline svref vectorp row-major-aref (setf row-major-aref) consp car cdr rplaca
                   checked-path-car checked-path-cdr = < 1+ +))
  (values
   (block done
     (let* ((first (svref state 0)) (stop (svref state 1))
            (from (svref state 2)) (mode (svref state 3)) (binary (svref state 4))
            (read (svref operations (if binary 1 2)))
            (write (svref operations (if (= mode 3) 4 3)))
            (indexed (vectorp sequence)) (scan sequence) (skip 0) (index first)
            (fail (svref operations 14)))
       (declare (type (integer 0 #x1ffffffe) first stop from skip index)
                (type (integer 0 3) mode) (type function read write fail))
       (if indexed nil
           (tagbody position
              (if (= skip first) (go positioned))
              (setq scan (checked-path-cdr scan) skip (the (integer 0 #x1ffffffe) (1+ skip)))
              (go position)
             positioned))
       (tagbody next
          (if (= index stop) (return-from done index))
          (if (if (= mode 1) t (= mode 3))
              (funcall write
                       (if indexed (row-major-aref sequence index) (checked-path-car scan)) peer)
              (let ((value (if (= mode 2) (row-major-aref peer from)
                               (funcall read peer nil nil))))
                (if (= mode 0) (if value nil (return-from done index)))
                (if indexed (locally (declare (optimize (safety 3)))
                              (setf (row-major-aref sequence index) value))
                    (if (consp scan) (rplaca (the cons scan) value)
                        (funcall fail 'type-error :datum scan :expected-type 'cons)))))
          (if indexed nil (setq scan (checked-path-cdr scan)))
          (setq index (the (integer 0 #x1ffffffe) (1+ index)))
          (if (= mode 2) (setq from (the (integer 0 #x1ffffffe) (1+ from))))
          (go next))))))

(defun stream-broadcast-sequence-value (sequence stream state operations)
  "Depth-first broadcast/alias traversal with active-path cycle rejection.
FIELDS is a checked canonical one-step metadata/leaf callback. STATE is
#(sequence first count mode); leaf emission retains the native I/O substrate."
  (declare (ignore sequence) (type simple-vector state operations)
           (optimize (safety 0) (debug 0))
           (inline cons car cdr rplaca rplacd eq = svref checked-path-car checked-path-cdr))
  (values
   (let* ((fields (svref operations 8)) (make-hash (svref operations 9))
          (lookup (svref operations 10)) (put (svref operations 11)) (remove (svref operations 12))
          (fail (svref operations 14))
          (work (cons (cons stream (cons nil nil)) nil))
          (active (funcall make-hash :test (svref operations 13))))
     (declare (type function fields make-hash lookup put remove fail))
     (tagbody next
        (if work nil (go done))
        (let* ((frame (car (the cons work))) (node (car (the cons frame)))
               (rest (cdr (the cons frame))) (phase (cdr (the cons rest))))
          (tagbody
          (if phase
              (if (eq phase t)
                  (let ((children (car (the cons rest))))
                    (if children
                        (let ((child (checked-path-car children)))
                          (rplaca (the cons rest) (checked-path-cdr children))
                          (setq work (cons (cons child (cons nil nil)) work))
                          (go next))
                        (go leave)))
                  (go leave))
              (progn
                (if (funcall lookup node active) (funcall fail 'stream-error :stream node))
                (funcall put t node active)
                (let ((kind (the (integer 0 46) (funcall fields node 0 nil))))
                  (if (= kind 43)
                      (progn
                        (rplaca (the cons rest) (funcall fields node 2 nil))
                        (rplacd (the cons rest) t)
                        (go next)))
                  (if (if (= kind 40) t (if (= kind 41) t (= kind 46)))
                      (let ((child (funcall fields node 1 nil)))
                        (rplacd (the cons rest) 1)
                        (setq work (cons (cons child (cons nil nil)) work))
                        (go next)))
                  (funcall fields node 3 state))))
         leave
          (funcall remove node active)
          (setq work (cdr (the cons work)))))
        (go next)
       done)
     nil)))
