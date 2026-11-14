;;;; Marked sequence editing. Native adapters supply checked initial regions
;;;; and bit-vector views sharing their private packed marker storage.
(in-package #:miricl-runtime)
(export '(remove-marked-values substitute-marked-values))

(defun remove-marked-values (sequence marks state)
  "Copy/remove marked positions after callbacks have finished.
STATE is #(initial-length first stop removed-count destructive-p). The caller
validates the original sequence and marker bounds before this private worker."
  (declare (type simple-bit-vector marks) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline vectorp svref sbit row-major-aref (setf row-major-aref) = < >= - 1+ zerop
                   checked-path-car checked-path-cdr consp car cdr cons rplacd))
  (values
   (block result
     (let* ((size (svref state 0)) (first (svref state 1)) (stop (svref state 2))
            (removed (svref state 3)) (write (svref state 4))
            (indexed (vectorp sequence)) (scan sequence) (head nil) (tail nil)
            (index 0) (out 0))
       (declare (type (integer 0 #x1ffffffe) size first stop removed index out))
       (if (= removed 0) (return-from result sequence))
       (if indexed (setq head (make-array (- size removed) :element-type (array-element-type sequence))))
       (tagbody next
          (if (= index size) (go finish))
          (let ((keep t) (next nil))
            (if (>= index first)
                (if (< index stop) (setq keep (zerop (sbit marks (- index first))))))
            ;; Read the next spine before destructive relinking can change it.
            (if indexed nil (setq next (checked-path-cdr scan)))
            (if keep
                (if indexed
                    (progn
                      (setf (row-major-aref head out) (row-major-aref sequence index))
                      (setq out (the (integer 0 #x1ffffffe) (1+ out))))
                    (let ((cell (if write scan (cons (checked-path-car scan) nil))))
                      (if (consp cell) nil (error 'type-error :datum cell :expected-type 'cons))
                      (if tail (rplacd (the cons tail) cell) (setq head cell))
                      (setq tail cell))))
            (setq scan next))
          (setq index (the (integer 0 #x1ffffffe) (1+ index)))
          (go next)
         finish)
       (if indexed (return-from result head))
       (if tail (rplacd (the cons tail) nil))
       head))))

(defun substitute-marked-values (sequence marks new state)
  "Write marked positions in a prepared result, preserving list spines.
The native adapter reuses the shared compiled subsequence copier when a fresh
result is required. STATE is #(first stop), checked before this private worker."
  (declare (type simple-bit-vector marks) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref sbit vectorp (setf row-major-aref) = < - 1+ zerop
                   checked-path-car checked-path-cdr consp rplaca))
  (values
   (let* ((first (svref state 0)) (stop (svref state 1))
          (indexed (vectorp sequence)) (scan sequence) (index 0))
     (declare (type (integer 0 #x1ffffffe) first stop index))
     (if indexed nil
         (tagbody skip
            (if (= index first) (go skipped))
            (setq scan (checked-path-cdr scan) index (the (integer 0 #x1ffffffe) (1+ index)))
            (go skip)
           skipped))
     (setq index first)
     (tagbody next
        (if (= index stop) (go finish))
        (if (zerop (sbit marks (- index first))) nil
            (if indexed (setf (row-major-aref sequence index) new)
                (progn
                  (if (consp scan) nil (error 'type-error :datum scan :expected-type 'cons))
                  (rplaca (the cons scan) new))))
        (if indexed nil (setq scan (checked-path-cdr scan)))
        (setq index (the (integer 0 #x1ffffffe) (1+ index)))
        (go next)
       finish)
     sequence)))
