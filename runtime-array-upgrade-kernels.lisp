;;;; Target array element specialization, above checked type services.
(in-package #:miricl-runtime)
(export '(upgraded-array-kind-value))

(defun upgraded-array-kind-value (type environment metadata)
  "Return target storage kind: object 0, character 1, bit 2, empty 3.
METADATA expands target derived types and supplies target integer intervals.
No host subtype relationships or host array representation enter this decision."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr consp eq eql characterp integerp = < <= >= svref))
  (values
   (block upgrade
     (setq type (funcall metadata 0 type environment))
     (if type nil (return-from upgrade 3))
     (case type
       ((character base-char standard-char) (return-from upgrade 1))
       (extended-char (return-from upgrade 3))
       (bit (return-from upgrade 2)))
     (if (consp type) nil (return-from upgrade 0))
     (let ((name (car type)) (count (funcall metadata 3 (cdr type) nil)))
       (if count nil (funcall metadata 4 type 3))
       (if (case name
             (integer (<= (the integer count) 2))
             ((signed-byte unsigned-byte) (<= (the integer count) 1))
             ((mod eql not) (= (the integer count) 1))
             (otherwise t))
           nil (funcall metadata 4 type 1)))
     (case (car type)
       ((integer mod signed-byte unsigned-byte)
        (let* ((interval (funcall metadata 1 type environment))
               (lower (svref (the simple-vector interval) 0))
               (upper (svref (the simple-vector interval) 1)))
          (if (= (the (integer 0 2) (svref (the simple-vector interval) 2)) 2)
              (return-from upgrade 3))
          (if lower
              (if upper
                  (progn
                    (if (> (the integer (funcall metadata 2 lower upper)) 0)
                        (return-from upgrade 3))
                    (if (>= (the integer (funcall metadata 2 lower 0)) 0)
                        (if (<= (the integer (funcall metadata 2 upper 1)) 0)
                            (return-from upgrade 2)))))))
        (return-from upgrade 0))
       ((member eql)
        (let ((scan (cdr type)) (kind 3) (candidate 0) (value nil))
          (declare (type (integer 0 3) kind candidate))
          (tagbody next
             (if scan nil (go done))
             (setq value (car scan) candidate 0)
             (if (characterp value) (setq candidate 1))
             (if (integerp value)
                 (if (<= 0 value) (if (<= value 1) (setq candidate 2))))
             (if (= candidate 0) (return-from upgrade 0))
             (if (= kind 3) nil (if (= kind candidate) nil (return-from upgrade 0)))
             (setq kind candidate scan (cdr scan))
             (go next)
            done)
          (return-from upgrade kind)))
       (or (if (cdr type) nil (return-from upgrade 3)))
       (not (if (eq (car (cdr type)) t) (return-from upgrade 3))))
     0)))
