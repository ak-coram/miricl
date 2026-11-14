;;;; Canonical character traversal, above validated string/region boundaries.
(in-package #:miricl-runtime)
(export '(scan-character-region mismatch-character-regions search-character-regions
          compare-character-regions bootstrap-text-equal-p))

(defun bootstrap-text-equal-p (left right)
  "Flat bootstrap strings, before ordinary function bindings are installed."
  (declare (type simple-string left right) (optimize (safety 0) (debug 0))
           (inline eq length = 1+ char char-code))
  (values
   (block result
     ;; LEFT is a true generalized boolean available before T is interned.
     (if (eq left right) (return-from result left))
     (let ((size (the (integer 0 #x7fffffff) (length left)))
           (other-size (the (integer 0 #x7fffffff) (length right))) (index 0))
       (declare (type (integer 0 #x7fffffff) size other-size index))
       (if (= size other-size) nil (return-from result nil))
       (tagbody next
          (if (= index size) (return-from result left))
          (if (= (char-code (char left index)) (char-code (char right index))) nil
              (return-from result nil))
          (setq index (the (integer 0 #x7fffffff) (1+ index)))
          (go next))))))

(defun compare-character-regions (a b state)
  "Lexicographic string relation. STATE is #(first1 stop1 first2 stop2 op fold).
OP is equality=0, unequal=1, less=2, greater=3, not-greater=4, not-less=5."
  (declare (type string a b) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref eq char char-code = < > <= >= - 1+))
  (values
   (let* ((i (svref state 0)) (stop1 (svref state 1))
          (j (svref state 2)) (stop2 (svref state 3))
          (op (svref state 4)) (fold (svref state 5)) (comparison 0))
     (declare (type (integer 0 #x7fffffff) i j stop1 stop2)
              (type (integer 0 5) op) (type (integer -1 1) comparison))
     (block compared
       (if (eq a b)
           (if (= i j) (if (= stop1 stop2) (progn (setq i stop1) (return-from compared nil)))))
       (tagbody next
          (if (= i stop1)
              (progn (if (= j stop2) nil (setq comparison -1)) (return-from compared nil)))
          (if (= j stop2) (progn (setq comparison 1) (return-from compared nil)))
          (let ((left (char a i)) (right (char b j)))
            (declare (type character left right))
            (if fold
                (if (= (char-code left) (char-code right)) nil
                    (setq left (funcall fold left) right (funcall fold right))))
            (if (= (char-code left) (char-code right)) nil
                (progn (setq comparison (if (< (char-code left) (char-code right)) -1 1))
                       (return-from compared nil))))
          (setq i (the (integer 0 #x7fffffff) (1+ i))
                j (the (integer 0 #x7fffffff) (1+ j)))
          (go next)))
     (if (= op 0) (= comparison 0)
         (if (if (= op 1) (if (= comparison 0) nil t)
                 (if (= op 2) (< comparison 0)
                     (if (= op 3) (> comparison 0)
                         (if (= op 4) (<= comparison 0) (>= comparison 0)))))
             i nil)))))

(defun scan-character-region (string state)
  "STATE is #(first stop needle flags fold). Flags: negate=1, reverse=2, count=4.
NEEDLE is an already folded code, or -1 for a noncharacter EQL operand.
FOLD is the canonical Unicode uppercase operation, independently of bindings."
  (declare (type string string) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref char char-code < = - + 1+ 1- logand zerop not))
  (values
   (block result
     (let* ((first (svref state 0)) (stop (svref state 1)) (needle (svref state 2))
            (flags (svref state 3)) (fold (svref state 4))
            (reverse (not (zerop (logand flags 2))))
            (countp (not (zerop (logand flags 4))))
            (negate (not (zerop (logand flags 1))))
            (index (if reverse stop first)) (left (- stop first)) (count 0))
       (declare (type (integer 0 #x7fffffff) first stop index left count)
                (type (integer -1 #x10ffff) needle) (type (integer 0 7) flags))
       (if (= left 0) (return-from result (if countp 0 -1)))
       (if (< needle 0)
           (return-from result (if negate (if countp left (if reverse (1- index) index))
                                  (if countp 0 -1))))
       (tagbody next
          (if reverse (setq index (the (integer 0 #x7fffffff) (1- index))))
          (let ((match (= (char-code (if fold (funcall fold (char string index)) (char string index))) needle)))
            (if negate (setq match (not match)))
            (if match
                (if countp (setq count (the (integer 0 #x7fffffff) (1+ count)))
                    (return-from result index))))
          (setq left (the (integer 0 #x7fffffff) (1- left)))
          (if (= left 0) (return-from result (if countp count -1)))
          (if reverse nil (setq index (the (integer 0 #x7fffffff) (1+ index))))
          (go next))))))

(defun mismatch-character-regions (a b state)
  "STATE is #(first1 stop1 first2 stop2 flags fold); negate=1, reverse=2."
  (declare (type string a b) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref char char-code = < - 1+ 1- logand zerop not))
  (values
   (block result
     (let* ((first1 (svref state 0)) (stop1 (svref state 1))
            (first2 (svref state 2)) (stop2 (svref state 3))
            (flags (svref state 4)) (fold (svref state 5))
            (reverse (not (zerop (logand flags 2))))
            (count1 (- stop1 first1)) (count2 (- stop2 first2))
            (left (if (< count1 count2) count1 count2))
            (i (if reverse stop1 first1)) (j (if reverse stop2 first2)))
       (declare (type (integer 0 #x7fffffff) first1 stop1 first2 stop2 count1 count2 left i j)
                (type (integer 0 3) flags))
       (if (= left 0) (return-from result (if (= count1 count2) nil i)))
       (tagbody next
          (if reverse (setq i (the (integer 0 #x7fffffff) (1- i))
                           j (the (integer 0 #x7fffffff) (1- j))))
          (let ((match (= (char-code (if fold (funcall fold (char a i)) (char a i))) (char-code (if fold (funcall fold (char b j)) (char b j))))))
            (if (zerop (logand flags 1)) nil (setq match (not match)))
            (if match nil (return-from result (if reverse (1+ i) i))))
          (if reverse nil (setq i (the (integer 0 #x7fffffff) (1+ i))
                               j (the (integer 0 #x7fffffff) (1+ j))))
          (setq left (the (integer 0 #x7fffffff) (1- left)))
          (if (= left 0) (return-from result (if (= count1 count2) nil i)))
          (go next))))))

(defun search-character-regions (pattern source state)
  "KMP over validated character regions, using only canonical equivalences.
STATE is #(first1 stop1 first2 stop2 reverse fold). Reverse scanning returns
the leftmost element of the rightmost match, including overlapping matches."
  (declare (type string pattern source) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) char char-code make-array = < > + - 1+ 1-))
  (values
   (block result
     (let* ((first1 (svref state 0)) (stop1 (svref state 1))
            (first2 (svref state 2)) (stop2 (svref state 3))
            (reverse (svref state 4)) (fold (svref state 5))
            (length (- stop1 first1)) (left (- stop2 first2))
            (i 0) (j 0) (index (if reverse stop2 first2)))
       (declare (type (integer 0 #x7fffffff) first1 stop1 first2 stop2 length left i j index))
       (if (= length 0) (return-from result (if reverse stop2 first2)))
       (if (> length left) (return-from result nil))
       (if (= length 1)
           (let ((needle (char-code (if fold (funcall fold (char pattern first1)) (char pattern first1)))))
             (declare (type (integer 0 #x10ffff) needle))
             (tagbody single
                (if (= left 0) (return-from result nil))
                (if reverse (setq index (the (integer 0 #x7fffffff) (1- index))))
                (if (= needle (char-code (if fold (funcall fold (char source index)) (char source index)))) (return-from result index))
                (if reverse nil (setq index (the (integer 0 #x7fffffff) (1+ index))))
                (setq left (the (integer 0 #x7fffffff) (1- left)))
                (go single))))
       ;; Keep normalized codes separate from text so no character objects
       ;; survive in the prefix table. Both arrays are private to this call.
       (let ((codes (make-array length :initial-element 0))
             (table (make-array length :initial-element 0)))
         (declare (type simple-vector codes table))
         (tagbody copy
            (if (= i length) (go copied))
            (setf (svref codes i)
                  (char-code (if fold (funcall fold (char pattern (if reverse (- stop1 i 1) (+ first1 i)))) (char pattern (if reverse (- stop1 i 1) (+ first1 i))))))
            (setq i (the (integer 0 #x7fffffff) (1+ i))) (go copy)
           copied)
         (setq i 1)
         (tagbody prefix
            (if (= i length) (go built))
            (let ((code (the (integer 0 #x10ffff) (svref codes i))))
              (declare (type (integer 0 #x10ffff) code))
              (tagbody fallback
                 (if (= j 0) (go ready))
                 (if (= code (the (integer 0 #x10ffff) (svref codes j))) (go ready))
                 (setq j (the (integer 0 #x7fffffff) (svref table (1- j)))) (go fallback)
                ready)
              (if (= code (the (integer 0 #x10ffff) (svref codes j))) (setq j (the (integer 0 #x7fffffff) (1+ j)))))
            (setf (svref table i) j)
            (setq i (the (integer 0 #x7fffffff) (1+ i))) (go prefix)
           built)
         (setq j 0)
         (tagbody scan
            (if (= left 0) (return-from result nil))
            (if reverse (setq index (the (integer 0 #x7fffffff) (1- index))))
            (let ((code (char-code (if fold (funcall fold (char source index)) (char source index)))))
              (declare (type (integer 0 #x10ffff) code))
              (tagbody fallback
                 (if (= j 0) (go ready))
                 (if (= code (the (integer 0 #x10ffff) (svref codes j))) (go ready))
                 (setq j (the (integer 0 #x7fffffff) (svref table (1- j)))) (go fallback)
                ready)
              (if (= code (the (integer 0 #x10ffff) (svref codes j))) (setq j (the (integer 0 #x7fffffff) (1+ j)))))
            (if (= j length) (return-from result (if reverse index (- (1+ index) length))))
            (if reverse nil (setq index (the (integer 0 #x7fffffff) (1+ index))))
            (setq left (the (integer 0 #x7fffffff) (1- left))) (go scan)))))))
