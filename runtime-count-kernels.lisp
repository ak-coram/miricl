;;;; Count traversal over sequence bounds and callables checked at the public
;;;; native boundary. Ordinary CL callbacks never reenter public COUNT.
(in-package #:miricl-runtime)
(export '(count-sequence-value))

(defun count-sequence-value (item sequence function state)
  "Count matches in a validated region. STATE is #(key first stop flags).
Bits 0..1 select COUNT/COUNT-IF/COUNT-IF-NOT, bit 2 selects TEST-NOT, and bit 3
selects reverse traversal. Reverse lists retain cursors, not element copies."
  (declare (type function function) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) checked-path-car checked-path-cdr
                   vectorp row-major-aref = 1+ 1- - logand zerop not))
  (values
   (block result
     (let* ((key (svref state 0)) (first (svref state 1)) (stop (svref state 2))
            (flags (svref state 3)) (mode (logand flags 3))
            (reverse (not (zerop (logand flags 8))))
            (indexed (vectorp sequence)) (remaining (- stop first))
            (index first) (cursor sequence) (buffer nil) (offset 0) (count 0))
       (declare (type (integer 0 #x7fffffff) first stop remaining index offset count)
                (type (integer 0 15) flags) (type (integer 0 2) mode))
       (if (= remaining 0) (return-from result 0))
       (if indexed nil
           (progn
             (tagbody skip
                (if (= offset first) (go skipped))
                (setq cursor (checked-path-cdr cursor)
                      offset (the (integer 0 #x7fffffff) (1+ offset)))
                (go skip)
               skipped)
             (if reverse
                 (progn
                   (setq buffer (make-array remaining) offset 0)
                   (let ((scan cursor))
                     (tagbody save
                        (if (= offset remaining) (go saved))
                        (setf (svref (the simple-vector buffer) offset) scan)
                        (setq scan (checked-path-cdr scan)
                              offset (the (integer 0 #x7fffffff) (1+ offset)))
                        (go save)
                       saved))))))
       (if reverse (setq index (the (integer 0 #x7fffffff) (1- stop))))
       (tagbody next
          (let* ((element (if indexed (row-major-aref sequence index)
                              (checked-path-car
                               (if reverse (svref (the simple-vector buffer) (- index first)) cursor))))
                 (value (if key (funcall key element) element))
                 (match (if (= mode 0) (funcall function item value) (funcall function value))))
            (if (= mode 0)
                (if (zerop (logand flags 4)) nil (setq match (not match)))
                (if (= mode 2) (setq match (not match))))
            (if match (setq count (the (integer 0 #x7fffffff) (1+ count)))))
          ;; Subsequent reads remain checked after arbitrary callback effects.
          ;; The original finite region also prevents callback-created cycles
          ;; from introducing an unbounded traversal.
          (if indexed nil (if reverse nil (setq cursor (checked-path-cdr cursor))))
          (setq remaining (the (integer 0 #x7fffffff) (1- remaining)))
          (if (= remaining 0) (return-from result count))
          (setq index (the (integer 0 #x7fffffff) (if reverse (1- index) (1+ index))))
          (go next))))))
