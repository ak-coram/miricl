;;;; Duplicate marking and list-set traversal above the representation substrate.
(in-package #:miricl-runtime)
(export '(duplicate-sequence-keys mark-duplicate-values list-set-values mark-sequence-values))

(defun duplicate-sequence-keys (sequence key first stop)
  "Extract each selected key once before duplicate selection callbacks run.
The native boundary checks the original proper sequence and scalar bounds."
  (declare (type (integer 0 #x1ffffffe) first stop)
           (optimize (safety 0) (debug 0))
           (inline vectorp svref (setf svref) row-major-aref = - 1+
                   checked-path-car checked-path-cdr))
  (values
   (let* ((count (- stop first)) (keys (make-array count))
          (indexed (vectorp sequence)) (scan sequence) (index 0))
     (declare (type simple-vector keys)
              (type (integer 0 #x1ffffffe) count index))
     (if indexed nil
         (tagbody skip
            (if (= index first) (go skipped))
            (setq scan (checked-path-cdr scan)
                  index (the (integer 0 #x1ffffffe) (1+ index)))
            (go skip)
           skipped))
     (setq index 0)
     (tagbody extract
        (if (= index count) (go finished))
        (let ((value (if indexed (row-major-aref sequence first)
                         (checked-path-car scan))))
          (setf (svref keys index) (if key (funcall key value) value)))
        ;; Read each list cursor after the callback, but retain the initial
        ;; finite bound so callback-created cycles cannot extend the work.
        (if indexed nil (setq scan (checked-path-cdr scan)))
        (setq index (the (integer 0 #x1ffffffe) (1+ index))
              first (the (integer 0 #x1ffffffe) (1+ first)))
        (go extract)
       finished)
     keys)))

(defun mark-duplicate-values (keys marks function state)
  "Mark duplicates in KEYS and return the number of removed elements.
STATE is #(negate from-end canonical-hash-test). MARKS is the native marker
storage viewed as a simple bit vector. Canonical identity is checked by the
adapter; arbitrary predicates keep ordered left/right comparisons."
  (declare (type simple-vector keys state) (type simple-bit-vector marks)
           (type function function) (optimize (safety 0) (debug 0))
           (inline svref sbit (setf row-major-aref) = < >= - 1+ 1- zerop not))
  (values
   (let* ((negate (svref state 0)) (from-end (svref state 1))
          (hash-test (svref state 2))
          (table (if hash-test (make-hash-table :test hash-test) nil))
          (count (length keys)) (left count)
          (index (if table (if from-end 0 (1- count)) 0))
          (removed 0) (other 0) (limit 0))
     (declare (type (integer 0 #x1ffffffe) count left removed other limit)
              (type (integer -1 #x1ffffffe) index))
     (tagbody next
        (if (= left 0) (go finished))
        (let ((value (svref keys index)) (match nil))
          (if table
              (progn
                (setq match (gethash value table))
                (if match nil (setf (gethash value table) t)))
              (progn
                (setq other (if from-end 0 (1+ index))
                      limit (if from-end index count))
                (tagbody search
                   (if (>= other limit) (go searched))
                   (if (zerop (sbit marks other))
                       (let ((truth (funcall function value (svref keys other))))
                         (setq match (if negate (not truth) truth))))
                   (if match (go searched))
                   (setq other (the (integer 0 #x1ffffffe) (1+ other)))
                   (go search)
                  searched)))
          (if match
              (progn
                (setf (row-major-aref marks index) 1)
                (setq removed (the (integer 0 #x1ffffffe) (1+ removed))))))
        (setq left (the (integer 0 #x1ffffffe) (1- left)))
        (if (= left 0) (go finished))
        (setq index (the (integer -1 #x1ffffffe)
                        (if table (if from-end (1+ index) (1- index)) (1+ index))))
        (go next)
       finished)
     removed)))

(defun list-set-values (left right key state)
  "ADJOIN/SUBSETP/INTERSECTION/SET-DIFFERENCE/UNION traversal.
STATE is #(mode resolved-test resolved-test-not canonical-hash-test). The
adapter admits hash indexing only for canonical equality and callback-free
key extraction. List spines are checked before SAFETY 0 cursor operations."
  (declare (type simple-vector state) (optimize (safety 0) (debug 0))
           (inline svref = < >= 1+ zerop not consp car cdr cons
                   checked-path-car checked-path-cdr))
  (values
   (block result
     (let* ((mode (svref state 0)) (test-not (svref state 2))
            (function (if test-not test-not (svref state 1)))
            (hash-test (svref state 3))
            (right-count (list-length right))
            (left-count (if (= mode 0) 1 (list-length left)))
            (table nil) (scan left) (index 0) (output nil))
       (declare (type (integer 0 4) mode)
                (type (integer 0 #x1ffffffe) index))
       (if right-count nil (error 'type-error :datum right :expected-type (list 'satisfies 'list-length)))
       (if left-count nil (error 'type-error :datum left :expected-type (list 'satisfies 'list-length)))
       (if (= mode 1) (setq output t))
       (if (= mode 4) (setq output right))
       (if hash-test
           (if (>= left-count 32)
               (if (>= right-count 32)
                   (progn
                     (setq table (make-hash-table :test hash-test))
                     (let ((cursor right) (remaining right-count))
                       (tagbody index-right
                          (if (= remaining 0) (go indexed))
                          (let ((value (checked-path-car cursor)))
                            (if key (setq value (funcall key value)))
                            (setf (gethash value table) t))
                          (setq cursor (checked-path-cdr cursor) remaining (1- remaining))
                          (go index-right)
                         indexed))))))
       (tagbody next
          (if (= index left-count) (return-from result output))
          (let* ((element (if (= mode 0) left (checked-path-car scan)))
                 (value (if key (funcall key element) element)) (match nil))
            (if table (setq match (gethash value table))
                (let ((cursor right) (remaining right-count))
                  (tagbody search
                     (if (= remaining 0) (go searched))
                     (let* ((other (checked-path-car cursor))
                            (keyed (if key (funcall key other) other))
                            (truth (funcall function value keyed)))
                       (if (if test-not (not truth) truth)
                           (progn (setq match t) (go searched))))
                     (setq cursor (checked-path-cdr cursor) remaining (1- remaining))
                     (go search)
                    searched)))
            (if (= mode 0) (return-from result (if match right (cons left right))))
            (if (= mode 1)
                (if match nil (return-from result nil))
                (if (if (= mode 2) match (not match))
                    (setq output (cons (checked-path-car scan) output)))))
          (setq scan (checked-path-cdr scan)
                index (the (integer 0 #x1ffffffe) (1+ index)))
          (go next))))))

(defun mark-sequence-values (item sequence marks state)
  "Predicate selection shared by removal/deletion and substitution.
STATE is #(mode key function negate first stop reverse limit character-kind
needle fold). All original sequence bounds and callables have been checked.
Reverse lists retain cursor cells so later callback mutations remain visible."
  (declare (type simple-bit-vector marks) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) (setf row-major-aref) row-major-aref
                   char char-code vectorp = - 1+ 1- zerop not
                   checked-path-car checked-path-cdr))
  (values
   (block result
     (let* ((mode (svref state 0)) (key (svref state 1)) (function (svref state 2))
            (negate (svref state 3)) (first (svref state 4)) (stop (svref state 5))
            (reverse (svref state 6)) (limit (svref state 7))
            (character-kind (svref state 8)) (needle (svref state 9)) (fold (svref state 10))
            (remaining (- stop first)) (index first) (offset 0) (selected 0)
            (indexed (vectorp sequence)) (scan sequence) (buffer nil))
       (declare (type (integer 0 2) mode) (type (integer 0 3) character-kind)
                (type (integer -1 #x10ffff) needle)
                (type (integer 0 #x1ffffffe) first stop limit remaining index offset selected))
       (if (if (= remaining 0) t (= limit 0)) (return-from result 0))
       (if indexed nil
           (progn
             (tagbody skip
                (if (= offset first) (go skipped))
                (setq scan (checked-path-cdr scan)
                      offset (the (integer 0 #x1ffffffe) (1+ offset)))
                (go skip)
               skipped)
             (if reverse
                 (progn
                   (setq buffer (make-array remaining) offset 0)
                   (let ((cursor scan))
                     (tagbody save
                        (if (= offset remaining) (go saved))
                        (setf (svref (the simple-vector buffer) offset) cursor)
                        (setq cursor (checked-path-cdr cursor)
                              offset (the (integer 0 #x1ffffffe) (1+ offset)))
                        (go save)
                       saved))))))
       (if reverse (setq index (the (integer 0 #x1ffffffe) (1- stop))))
       (tagbody next
          (let ((match nil))
            (if (zerop character-kind)
                (let* ((element (if indexed (row-major-aref sequence index)
                                    (checked-path-car
                                     (if reverse (svref (the simple-vector buffer) (- index first)) scan))))
                       (value (if key (funcall key element) element)))
                  (setq match (if (= mode 0) (funcall function item value) (funcall function value)))
                  (if (= mode 0)
                      (if negate (setq match (not match)))
                      (if (= mode 2) (setq match (not match)))))
                (let* ((character (char (the string sequence) index))
                       (code (char-code (if fold (funcall fold character) character))))
                  (setq match (= needle code))
                  (if negate (setq match (not match)))))
            (if match
                (progn
                  (setf (row-major-aref marks (- index first)) 1)
                  (setq selected (the (integer 0 #x1ffffffe) (1+ selected))))))
          (if indexed nil (if reverse nil (setq scan (checked-path-cdr scan))))
          (setq remaining (the (integer 0 #x1ffffffe) (1- remaining)))
          (if (if (= remaining 0) t (= selected limit)) (return-from result selected))
          (setq index (the (integer 0 #x1ffffffe) (if reverse (1- index) (1+ index))))
          (go next))))))
