;;;; Reader token algorithms. Native code owns source cursors and raw storage;
;;;; these workers consume ordinary CL strings and explicit reader settings.
(in-package #:miricl-runtime)
(export '(small-reader-integer-value reader-float-format-code parse-integer-region-value))

(defun reader-float-format-code (format)
  "Map the target's supported default float format to its representation tag."
  (declare (optimize (safety 0) (debug 0)) (inline eq))
  (values (if (eq format 'short-float) 3
              (if (eq format 'single-float) 3
                  (if (eq format 'double-float) 4
                      (if (eq format 'long-float) 4
                          (error 'type-error :datum format
                                 :expected-type (list 'member 'short-float 'single-float 'double-float 'long-float))))))))

(defun small-reader-integer-value (text size radix)
  "Recognize a signed 32-bit integer token; NIL defers to the general reader.
Only a decimal token may end in a dot. Invalid bounds are errors, while an
unrecognized token or a value outside this fast path simply supplies no result."
  (declare (optimize (safety 0) (debug 0))
           (inline stringp integerp < > <= = + - * length char char-code
                   ascii-digit-value))
  ;; This worker is also useful directly from host tools. Check every public
  ;; input before introducing the private scalar/range declarations below.
  (if (stringp text) nil (error 'type-error :datum text :expected-type 'string))
  (if (integerp size) nil (error 'type-error :datum size :expected-type (list 'integer 0 #x7fffffff)))
  (if (<= 0 size (length text)) nil
      (error 'type-error :datum size :expected-type (list 'integer 0 (length text))))
  (if (<= size #x7fffffff) nil (error 'type-error :datum size :expected-type (list 'integer 0 #x7fffffff)))
  (if (integerp radix) nil (error 'type-error :datum radix :expected-type (list 'integer 2 36)))
  (if (<= 2 radix 36) nil (error 'type-error :datum radix :expected-type (list 'integer 2 36)))
  (values
   (block result
     (if (= size 0) (return-from result nil))
     (let ((index 0) (negative nil) (magnitude 0))
       (declare (type (integer 0 #x7fffffff) index)
                (type (integer 0 #x80000000) magnitude))
       (let ((code (char-code (char text 0))))
         (if (if (= code 43) t (= code 45))
             (setq index 1 negative (= code 45))))
       (let ((first index) (limit (if negative #x80000000 #x7fffffff)))
         (declare (type (integer 0 1) first)
                  (type (integer #x7fffffff #x80000000) limit))
         (if (= index size) (return-from result nil))
         (tagbody next
            (if (= index size) (return-from result (if negative (- magnitude) magnitude)))
            (let ((code (char-code (char text index))))
              (if (if (= radix 10) (if (= code 46) (if (> index first) (= (+ index 1) size))))
                  (return-from result (if negative (- magnitude) magnitude)))
              (let ((digit (ascii-digit-value code)))
                (if (>= digit radix) (return-from result nil))
                (let ((next (+ (* magnitude radix) digit)))
                  (if (> next limit) (return-from result nil))
                  (setq magnitude next))))
            (setq index (+ index 1))
            (go next)))))))

(defun parse-integer-region-value (text first stop control)
  "Return (integer . delimiter-index) for a checked PARSE-INTEGER region.
CONTROL is (radix . junk-allowed). The public adapter signals PARSE-ERROR when
junk is forbidden and the result has no integer or does not reach STOP."
  (declare (optimize (safety 0) (debug 0))
           (inline stringp consp integerp length char char-code car cdr cons
                   = < > <= 1+ + - * zerop ascii-white-p))
  (if (stringp text) nil (error 'type-error :datum text :expected-type 'string))
  (if (consp control) nil (error 'type-error :datum control :expected-type 'cons))
  (if (integerp stop) nil (error 'type-error :datum stop :expected-type 'integer))
  (if (<= 0 stop (length text)) nil
      (error 'type-error :datum stop :expected-type (list 'integer 0 (length text))))
  (if (<= stop #x7fffffff) nil
      (error 'type-error :datum stop :expected-type (list 'integer 0 #x7fffffff)))
  (if (integerp first) nil (error 'type-error :datum first :expected-type 'integer))
  (if (<= 0 first stop) nil
      (error 'type-error :datum first :expected-type (list 'integer 0 stop)))
  (let ((radix (car control)) (junk (cdr control)))
    (if (integerp radix) nil (error 'type-error :datum radix :expected-type 'integer))
    (if (<= 2 radix 36) nil
        (error 'type-error :datum radix :expected-type (list 'integer 2 36)))
    (values
     (let ((index first) (digits-start first) (negative nil) (value nil)
           (accumulated 0) (chunk 0) (scale 1) (threshold (floor #x3fffffff radix)))
       (declare (type (integer 0 #x7fffffff) index digits-start)
                (type (integer 0 #x3fffffff) chunk scale threshold)
                (type unsigned-byte accumulated))
       (tagbody whitespace
          (if (= index stop) (go sign))
          (if (ascii-white-p (char-code (char text index))) nil (go sign))
          (setq index (the (integer 0 #x7fffffff) (1+ index)))
          (go whitespace)
         sign
          (if (= index stop) (go begin-digits))
          (let ((code (char-code (char text index))))
            (if (if (= code 43) t (= code 45))
                (setq negative (= code 45) index (the (integer 0 #x7fffffff) (1+ index)))))
         begin-digits
          (setq digits-start index)
         digits
          (if (= index stop) (go finish-digits))
          (let ((digit (digit-char-p (char text index) radix)))
            (if digit nil (go finish-digits))
            ;; Flush radix-sized chunks rather than multiplying a bignum for
            ;; every digit. These scalar intermediates never exceed 30 bits.
            (if (> scale threshold)
                (setq accumulated (+ (* accumulated scale) chunk) chunk 0 scale 1))
            (setq chunk (the (integer 0 #x3fffffff) (+ (* chunk radix) digit))
                  scale (the (integer 0 #x3fffffff) (* scale radix))))
          (setq index (the (integer 0 #x7fffffff) (1+ index)))
          (go digits)
         finish-digits
          (if (= index digits-start) nil
              (progn
                (setq value (if (zerop accumulated) chunk (+ (* accumulated scale) chunk)))
                (if negative (setq value (- value)))))
          (if junk (go finish))
         trailing
          (if (= index stop) (go finish))
          (if (ascii-white-p (char-code (char text index))) nil (go finish))
          (setq index (the (integer 0 #x7fffffff) (1+ index)))
          (go trailing)
         finish)
       (cons value index)))))
