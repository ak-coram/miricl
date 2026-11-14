;;;; Reduction traversal over bounds and callable definitions checked by the
;;;; native public entry. The worker never calls the public REDUCE function.
(in-package #:miricl-runtime)
(export '(reduce-sequence-value))

(defun reduce-sequence-value (function sequence key state)
  "Reduce a validated region. STATE is #(first stop initial flags).
Bit 0 selects right-to-left traversal; bit 1 distinguishes a supplied initial
value, including NIL. Reverse list traversal saves cursors, preserving callback
visibility of later CAR mutations. The native boundary validates proper lists,
sequence bounds and callable definitions before supplying this private state."
  (declare (type function function) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) checked-path-car checked-path-cdr
                   vectorp row-major-aref = 1+ 1- - logand zerop not))
  (values
   (block result
     (let* ((first (svref state 0)) (stop (svref state 1))
            (accumulated (svref state 2)) (flags (svref state 3))
            (reverse (not (zerop (logand flags 1))))
            (ready (not (zerop (logand flags 2))))
            (indexed (vectorp sequence)) (remaining (- stop first))
            (index first) (cursor sequence) (buffer nil) (offset 0))
       (declare (type (integer 0 #x7fffffff) first stop remaining index offset)
                (type (integer 0 3) flags))
       (if (= remaining 0)
           (return-from result (if ready accumulated (funcall function))))
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
                 (value (if key (funcall key element) element)))
            (if ready
                (setq accumulated (if reverse (funcall function value accumulated)
                                      (funcall function accumulated value)))
                (setq accumulated value ready t)))
          ;; Callbacks can change either visited cells or vector fill pointers.
          ;; Read physical array slots within the initially validated region,
          ;; and validate each list cursor before its next use.
          (if indexed nil (if reverse nil (setq cursor (checked-path-cdr cursor))))
          (setq remaining (the (integer 0 #x7fffffff) (1- remaining)))
          (if (= remaining 0) (return-from result accumulated))
          (setq index (the (integer 0 #x7fffffff) (if reverse (1- index) (1+ index))))
          (go next))))))
