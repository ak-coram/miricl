;;;; Coordinate intersection and vector stack policy above checked storage services.
(in-package #:miricl-runtime)
(export '(array-adjust-value array-adjust-copy-value array-adjust-displacement-check-value array-adjust-shape-value
          vector-stack-value))

(defun array-adjust-shape-value (dimensions rank metadata)
  "Validate finite target dimensions before entering the allocation substrate."
  (declare (type (integer 0 255) rank) (type function metadata)
           (optimize (safety 0) (debug 0))
           (inline integerp consp car cdr = < > <= + * zerop truncate))
  (let* ((scalar (integerp dimensions))
         (count (if scalar 1 (funcall metadata 12 dimensions nil)))
         (scan dimensions) (total 1) (overflow nil))
    (declare (type (integer 0 536870910) total))
    (if count nil (funcall metadata 9 dimensions 0))
    (if (= (the integer count) rank) nil (funcall metadata 9 dimensions 0))
    (tagbody next
       (if scalar nil (if scan nil (go done)))
       (let ((dimension (if scalar dimensions (car scan))))
         (if (integerp dimension) nil (funcall metadata 9 dimension 0))
         (if (< (the integer dimension) 0) (funcall metadata 9 dimension 0))
         (if (> (the integer dimension) 536870910) (funcall metadata 9 dimension 0))
         (let ((dimension (the (integer 0 536870910) dimension)))
           (if (zerop dimension) (setq total 0 overflow nil)
               (if overflow nil
                   (if (> total (truncate 536870910 dimension)) (setq overflow t)
                       (setq total (* total dimension)))))))
       (if scalar (go done))
       (setq scan (cdr scan)) (go next)
      done)
    (if overflow (funcall metadata 9 dimensions 3))
    (values total)))

(defun array-adjust-displacement-check-value (old target metadata)
  "Reject a displacement through OLD or a circular target chain before mutation."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline eq))
  (let ((slow target) (fast target))
    (tagbody next
       (if fast nil (go done))
       (if (eq fast old) (funcall metadata 9 target 0))
       (setq fast (funcall metadata 2 fast nil))
       (if fast nil (go done))
       (if (eq fast old) (funcall metadata 9 target 0))
       (setq fast (funcall metadata 2 fast nil)
             slow (funcall metadata 2 slow nil))
       (if fast (if (eq fast slow) (funcall metadata 9 target 0)))
       (go next)
      done)
    (values nil)))

(defun array-adjust-copy-value (old new metadata)
  "Copy the coordinate intersection, coalescing the innermost dimension.
Arrays are checked by the metadata boundary; indices and strides are derived
from their target dimensions. There is no traversal of host storage."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline svref (setf svref) = < > + - * 1+ 1- zerop array-rank
                   array-total-size array-dimension make-array))
  (values
   (block copy
     (if (zerop (array-total-size old)) (return-from copy new))
     (if (zerop (array-total-size new)) (return-from copy new))
     (let* ((rank (array-rank old)) (packet (make-array 4)))
       (declare (type (integer 0 255) rank) (type simple-vector packet))
       (setf (svref packet 0) old)
       (if (< rank 2)
           (let ((count 1))
             (declare (type (integer 1 536870910) count))
             (if (= rank 1)
                 (let ((a (array-dimension old 0)) (b (array-dimension new 0)))
                   (setq count (if (< a b) a b))))
             (setf (svref packet 1) 0)
             (setf (svref packet 2) 0)
             (setf (svref packet 3) count)
             (funcall metadata 4 new packet)
             (return-from copy new)))
       (let* ((last (1- rank))
            (count (make-array rank)) (coordinates (make-array rank :initial-element 0))
            (old-strides (make-array rank)) (new-strides (make-array rank))
            (old-stride 1) (new-stride 1) (axis last)
            (from 0) (to 0))
       (declare (type (integer -1 255) last axis)
                (type (integer 0 536870910) old-stride new-stride from to)
                (type simple-vector count coordinates old-strides new-strides packet))
       (tagbody strides
          (if (< axis 0) (go copied-strides))
          (let ((a (array-dimension old axis)) (b (array-dimension new axis)))
            (declare (type (integer 1 536870910) a b))
            (setf (svref count axis) (if (< a b) a b))
            (setf (svref old-strides axis) old-stride)
            (setf (svref new-strides axis) new-stride)
            (setq old-stride (* old-stride a) new-stride (* new-stride b)))
          (setq axis (1- axis)) (go strides)
         copied-strides
          (setf (svref packet 3) (svref count last))
         row
          (setf (svref packet 1) to)
          (setf (svref packet 2) from)
          (funcall metadata 4 new packet)
          (setq axis (1- last))
         advance
          (if (< axis 0) (return-from copy new))
          (let ((coordinate (1+ (the (integer 0 536870910) (svref coordinates axis))))
                (a (the (integer 0 536870910) (svref old-strides axis)))
                (b (the (integer 0 536870910) (svref new-strides axis))))
            (declare (type (integer 1 536870910) coordinate))
            (if (< coordinate (the (integer 1 536870910) (svref count axis)))
                (progn
                  (setf (svref coordinates axis) coordinate)
                  (setq from (+ from a) to (+ to b))
                  (go row)))
            ;; Reset an exhausted axis without ever forming an out of bounds
            ;; one-past row offset (important near the target size limit).
            (setq from (- from (* a (the (integer 0 536870910) (svref coordinates axis))))
                  to (- to (* b (the (integer 0 536870910) (svref coordinates axis)))))
            (setf (svref coordinates axis) 0))
          (setq axis (1- axis)) (go advance)))))))

(defun array-adjust-value (old state metadata)
  "STATE is #(dimensions element initial contents supplied fill target offset).
Allocation completes before intersection copying; only a completed replacement
is committed to an actually adjustable array."
  (declare (type simple-vector state) (type function metadata)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) = < >= logand zerop array-rank array-total-size))
  (values
   (let* ((info (the simple-vector (funcall metadata 0 old nil)))
          (kind (the (integer 0 4) (svref info 0)))
          (rank (the (integer 0 255) (svref info 1)))
          (adjustable (svref info 2))
          (fill (the (integer -1 536870910) (svref info 3))))
     (if (= kind (the (integer 0 4) (funcall metadata 1 (svref state 1) nil)))
         nil (funcall metadata 9 old 0))
     ;; Target DEFTYPE expansion can have effects, including changing the
     ;; original fill pointer. Preserve it after that expansion, as ADJUST-ARRAY
     ;; requires, rather than carrying a stale header fact across arbitrary code.
     (setq fill (the (integer -1 536870910) (funcall metadata 0 old t)))
     (if (svref state 5)
         (if (< fill 0) (funcall metadata 9 old 0))
         (if (>= fill 0) (setf (svref state 5) fill)))
     (if adjustable
         (array-adjust-displacement-check-value old (svref state 6) metadata))
     (let ((new (funcall metadata 3 old state)))
       (if (= (array-rank new) rank) nil (funcall metadata 9 new 0))
       (if (= kind 3) nil
           (if (zerop (array-total-size old)) nil
               (if (zerop (array-total-size new)) nil
                   (if (zerop (logand (the (integer 0 7) (svref state 4)) 2))
                       (if (svref state 6) nil (array-adjust-copy-value old new metadata))))))
       (if adjustable (funcall metadata 5 old new) new)))))

(defun vector-stack-value (mode args metadata)
  "MODE is 0 POP, 1 PUSH, 2 PUSH-EXTEND. ARGS is a checked finite packet."
  (declare (type (integer 0 2) mode) (type list args) (type function metadata)
           (optimize (safety 0) (debug 0))
           (inline car cdr = < <= > + - 1+ 1- integerp array-total-size fill-pointer
                   svref (setf svref) make-array row-major-aref))
  (values
   (block stack
     (let* ((value (if (= mode 0) nil (car args)))
            (array (if (= mode 0) (car args) (car (cdr args))))
            (info (the simple-vector (funcall metadata 6 array nil)))
            (fill (the (integer 0 536870910) (svref info 3)))
            (length (array-total-size array)))
       (declare (type (integer 0 536870910) length))
       (if (= mode 0)
           (progn
             (if (= fill 0) (funcall metadata 9 array 1))
             (setq fill (1- fill))
             (funcall metadata 7 array fill)
             (return-from stack (row-major-aref array fill))))
       (let ((extension (car (cdr (cdr args)))))
         (if (= mode 2)
             (if (cdr (cdr args))
                 (if (integerp extension)
                     (if (> extension 0) nil (funcall metadata 9 extension 4))
                     (funcall metadata 9 extension 4))))
         (if (= fill length)
             (progn
               (if (= mode 1) (return-from stack nil))
               (if (svref info 2) nil (funcall metadata 9 array 2))
               ;; Validate before allocating or committing growth.
               (funcall metadata 8 array value)
               (let ((growth (if extension extension (if (= length 0) 1 length))))
                 (declare (type integer growth))
                 (if (> growth (- 536870910 length)) (funcall metadata 9 growth 3))
                 (let ((state (make-array 8 :initial-element nil)))
                   (declare (type simple-vector state))
                   (setf (svref state 0) (+ length growth))
                   (setf (svref state 1) (funcall metadata 10 array nil))
                   (setf (svref state 4) 0)
                   (setf (svref state 7) 0)
                   (array-adjust-value array state metadata)))))
         (funcall metadata 11 array (cons fill value))
         (funcall metadata 7 array (1+ fill))
         fill)))))
