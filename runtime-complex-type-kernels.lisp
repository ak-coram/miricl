;;;; Target complex representations and subtype decisions above checked services.
(in-package #:miricl-runtime)
(export '(upgraded-complex-part-value complex-type-mask-value complex-subtype-query-value))

(defun upgraded-complex-part-value (type environment metadata)
  "Choose the target's rational/single/double complex component representation.
Expansion and general subtype fallback are target services, never host contracts."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr cons consp eq = 1+))
  (values
   (block upgrade
     (let ((expanded nil) (class nil) (name nil) (position 0) (candidate nil))
       (declare (type (integer 0 6) position))
       (tagbody again
          (case type
            ((nil) (return-from upgrade nil))
            ((integer rational ratio fixnum bignum bit signed-byte unsigned-byte)
             (return-from upgrade 'rational))
            ((short-float single-float) (return-from upgrade 'single-float))
            ((double-float long-float) (return-from upgrade 'double-float))
            ((float real) (return-from upgrade type)))
          (setq expanded (funcall metadata 0 type environment))
          (if (eq expanded type) nil (progn (setq type expanded) (go again)))
          (setq class (funcall metadata 1 type nil))
          (if class
              (progn (setq name (car class))
                (if (eq name type) nil (progn (setq type name) (go again)))))
          (setq name (if (consp type) (car type) nil))
          (case name
            ((integer rational ratio fixnum bignum bit signed-byte unsigned-byte mod
              real float short-float single-float double-float long-float)
             (setq position 0)
             (tagbody next
                (setq candidate (case position
                                  (0 nil) (1 'rational) (2 'single-float)
                                  (3 'double-float) (4 'float) (5 'real)))
                (if (= (the integer (funcall metadata 2 type (cons candidate environment))) 1)
                    (return-from upgrade candidate))
                (setq position (1+ position)) (if (= position 6) (go done)) (go next)
               done)))
          (return-from upgrade (funcall metadata 5 type environment)))))))

(defun complex-type-mask-value (type environment metadata)
  "Mask 1 rational,2 single,4 double; 8 empty; 0 unrelated. Controls are checked."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr consp eq = >))
  (values
   (block mask
     (let ((class (funcall metadata 1 type nil)) (arguments nil) (count nil) (part nil))
       (if class (setq type (car class)))
       (if (eq type 'complex) nil
           (progn
             (if (consp type)
                 (if (eq (car type) 'complex) nil (return-from mask 0))
                 (return-from mask 0))
             (setq arguments (cdr type) count (funcall metadata 3 arguments nil))
             (if count nil (funcall metadata 4 type 3))
             (if (> (the integer count) 1) (funcall metadata 4 type 1))))
       (if arguments nil (return-from mask 7))
       (setq part (car arguments))
       (if (eq part '*) (return-from mask 7))
       (setq part (upgraded-complex-part-value part environment metadata))
       (case part
         ((nil) 8) (rational 1) (single-float 2) (double-float 4) (float 6) (real 7)
         (otherwise 0))))))

(defun complex-subtype-query-value (a state metadata)
  "STATE is #(target environment). Return 0 unknown,1 subtype,2 counterexample.
Unequal upgraded masks require containment of the ORIGINAL component types."
  (declare (type simple-vector state) (type function metadata)
           (optimize (safety 0) (debug 0))
           (inline car cdr cons svref eq = logand))
  (values
   (block query
     (let* ((b (svref state 0)) (environment (svref state 1))
            (am (complex-type-mask-value a environment metadata))
            (bm (complex-type-mask-value b environment metadata))
            (other nil) (class nil) (scan nil) (result 0))
       (declare (type integer am bm result))
       (if (= am 8) (return-from query 1))
       (if (= am 0) nil
           (if (= bm 0) nil
               (progn
                 (if (= am bm) (return-from query 1))
                 (if (= (logand am bm) am) nil (return-from query 2))
                 (return-from query
                   (funcall metadata 6 (complex-component-type a)
                     (cons (complex-component-type b) environment))))))
       (if (= am 0) (if (= bm 0) (return-from query 0)))
       (setq other (if (= am 0) a b))
       (if other nil (return-from query 2))
       (setq class (funcall metadata 7 other nil))
       (if class
           (if (= am 0)
               (if (svref (the simple-vector class) 2) (return-from query 2))
               (progn
                 (setq scan (svref (the simple-vector class) 1))
                 (tagbody next
                    (if scan nil (go done))
                    (if (eq (svref (the simple-vector class) 0) (car scan)) (return-from query 1))
                    (setq scan (cdr scan)) (go next)
                   done)
                 (return-from query 2))))
       (if (eq other 'atom) (return-from query (if (= am 0) 2 1)))
       (setq result (funcall metadata 2 other (cons nil environment)))
       (if (= result 0) (return-from query 0))
       (if (= am 0) (if (= result 1) 1 2) 2)))))
