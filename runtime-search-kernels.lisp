;;;; Pairwise SEARCH traversal above the checked native sequence boundary.
(in-package #:miricl-runtime)
(export '(search-sequence-value))

(defun search-sequence-value (pattern source function state)
  "Search validated regions without assuming callbacks are equivalence tests.
STATE is #(key first1 stop1 first2 stop2 flags). Bit 0 negates the test;
bit 1 requests the rightmost match. List cursors and all array reads remain
live after callbacks. Initial finite bounds limit callback-created cycles."
  (declare (type function function) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref vectorp row-major-aref checked-path-car checked-path-cdr
                   = > - + 1+ logand zerop not))
  (values
   (block result
     (let* ((key (svref state 0)) (first1 (svref state 1)) (stop1 (svref state 2))
            (first2 (svref state 3)) (stop2 (svref state 4)) (flags (svref state 5))
            (length (- stop1 first1)) (last 0) (candidate first2)
            (indexed1 (vectorp pattern)) (indexed2 (vectorp source))
            (head1 pattern) (head2 source) (skip 0) (found nil))
       (declare (type (integer 0 #x7fffffff) first1 stop1 first2 stop2 length last candidate skip)
                (type (integer 0 3) flags))
       (if (= length 0)
           (return-from result (if (zerop (logand flags 2)) first2 stop2)))
       (if (> length (- stop2 first2)) (return-from result nil))
       (if indexed1 nil
           (tagbody position1
              (if (= skip first1) (go positioned1))
              (setq head1 (checked-path-cdr head1) skip (the (integer 0 #x7fffffff) (1+ skip)))
              (go position1)
             positioned1))
       (setq skip 0)
       (if indexed2 nil
           (tagbody position2
              (if (= skip first2) (go positioned2))
              (setq head2 (checked-path-cdr head2) skip (the (integer 0 #x7fffffff) (1+ skip)))
              (go position2)
             positioned2))
       (setq last (the (integer 0 #x7fffffff) (- stop2 length)))
       (tagbody next
          (if (> candidate last) (return-from result found))
          (let ((i first1) (j candidate) (scan1 head1) (scan2 head2))
            (declare (type (integer 0 #x7fffffff) i j))
            (block different
              (tagbody compare
                 (if (= i stop1)
                     (progn (setq found candidate)
                            (if (zerop (logand flags 2)) (return-from result found))
                            (return-from different nil)))
                 ;; Read both elements before either KEY call, matching the
                 ;; native traversal's behavior when a callback mutates data.
                 (let* ((value1 (if indexed1 (row-major-aref pattern i) (checked-path-car scan1)))
                        (value2 (if indexed2 (row-major-aref source j) (checked-path-car scan2)))
                        (left (if key (funcall key value1) value1))
                        (right (if key (funcall key value2) value2))
                        (match (funcall function left right)))
                   (if (zerop (logand flags 1)) nil (setq match (not match)))
                   (if match nil (return-from different nil)))
                 (if indexed1 nil (setq scan1 (checked-path-cdr scan1)))
                 (if indexed2 nil (setq scan2 (checked-path-cdr scan2)))
                 (setq i (the (integer 0 #x7fffffff) (1+ i))
                       j (the (integer 0 #x7fffffff) (1+ j)))
                 (go compare))))
          (if indexed2 nil (setq head2 (checked-path-cdr head2)))
          (setq candidate (the (integer 0 #x7fffffff) (1+ candidate)))
          (go next))))))
