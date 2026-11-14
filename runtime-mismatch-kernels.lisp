;;;; Element-wise comparison over independently validated sequence regions.
(in-package #:miricl-runtime)
(export '(mismatch-sequence-value))

(defun mismatch-sequence-value (a b function state)
  "Compare validated regions, returning a bounding index into A or NIL.
STATE is #(key first1 stop1 first2 stop2 flags). Bit 0 negates the test;
bit 1 compares right-to-left and returns one past the mismatching element.
Reverse list buffers retain original cons cells, preserving live CAR reads."
  (declare (type function function) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) vectorp row-major-aref checked-path-car checked-path-cdr
                   = < - 1+ 1- logand zerop not))
  (values
   (block result
     (let* ((key (svref state 0)) (first1 (svref state 1)) (stop1 (svref state 2))
            (first2 (svref state 3)) (stop2 (svref state 4)) (flags (svref state 5))
            (reverse (not (zerop (logand flags 2))))
            (count1 (- stop1 first1)) (count2 (- stop2 first2))
            (left (if (< count1 count2) count1 count2))
            (indexed1 (vectorp a)) (indexed2 (vectorp b))
            (i (if reverse stop1 first1)) (j (if reverse stop2 first2))
            (cursor1 a) (cursor2 b) (buffer1 nil) (buffer2 nil) (skip 0))
       (declare (type (integer 0 #x7fffffff) first1 stop1 first2 stop2 count1 count2 left i j skip)
                (type (integer 0 3) flags))
       (if (= left 0)
           (return-from result (if (= count1 count2) nil i)))
       (if indexed1 nil
           (progn
             (tagbody position1
                (if (= skip first1) (go positioned1))
                (setq cursor1 (checked-path-cdr cursor1)
                      skip (the (integer 0 #x7fffffff) (1+ skip)))
                (go position1)
               positioned1)
             (if reverse
                 (progn
                   (setq buffer1 (make-array count1) skip 0)
                   (let ((scan cursor1))
                     (tagbody save1
                        (if (= skip count1) (go saved1))
                        (setf (svref (the simple-vector buffer1) skip) scan)
                        (setq scan (checked-path-cdr scan)
                              skip (the (integer 0 #x7fffffff) (1+ skip)))
                        (go save1)
                       saved1))))))
       (setq skip 0)
       (if indexed2 nil
           (progn
             (tagbody position2
                (if (= skip first2) (go positioned2))
                (setq cursor2 (checked-path-cdr cursor2)
                      skip (the (integer 0 #x7fffffff) (1+ skip)))
                (go position2)
               positioned2)
             (if reverse
                 (progn
                   (setq buffer2 (make-array count2) skip 0)
                   (let ((scan cursor2))
                     (tagbody save2
                        (if (= skip count2) (go saved2))
                        (setf (svref (the simple-vector buffer2) skip) scan)
                        (setq scan (checked-path-cdr scan)
                              skip (the (integer 0 #x7fffffff) (1+ skip)))
                        (go save2)
                       saved2))))))
       (tagbody next
          (if reverse
              (setq i (the (integer 0 #x7fffffff) (1- i))
                    j (the (integer 0 #x7fffffff) (1- j))))
          ;; Preserve both element reads before either KEY callback.
          (let* ((value1 (if indexed1 (row-major-aref a i)
                             (checked-path-car (if reverse
                               (svref (the simple-vector buffer1) (- i first1)) cursor1))))
                 (value2 (if indexed2 (row-major-aref b j)
                             (checked-path-car (if reverse
                               (svref (the simple-vector buffer2) (- j first2)) cursor2))))
                 (left-value (if key (funcall key value1) value1))
                 (right-value (if key (funcall key value2) value2))
                 (match (funcall function left-value right-value)))
            (if (zerop (logand flags 1)) nil (setq match (not match)))
            (if match nil
                (return-from result (if reverse (the (integer 0 #x7fffffff) (1+ i)) i))))
          (if reverse nil
              (progn
                (if indexed1 nil (setq cursor1 (checked-path-cdr cursor1)))
                (if indexed2 nil (setq cursor2 (checked-path-cdr cursor2)))
                (setq i (the (integer 0 #x7fffffff) (1+ i))
                      j (the (integer 0 #x7fffffff) (1+ j)))))
          (setq left (the (integer 0 #x7fffffff) (1- left)))
          (if (= left 0) (return-from result (if (= count1 count2) nil i)))
          (go next))))))
