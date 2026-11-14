;;;; Character table algorithms. Native wrappers supply immutable packed
;;;; code-point tables and validated scalar codes, independently of host CL's
;;;; implementation-defined alphabetic and digit repertoire.
(in-package #:miricl-runtime)
(export '(character-digit-code-value character-alpha-code-p unicode-character-name-code
          character-case-code-value))

(defun character-digit-code-value (code ranges)
  "Return a radix-36 weight, or -1. RANGES has START, END, DELTA triples."
  (declare (type (integer 0 #x10ffff) code) (type simple-string ranges)
           (optimize (safety 0) (debug 0))
           (inline <= < > + - * ash length char char-code))
  (values
   (block result
     (if (<= 48 code 57) (return-from result (- code 48)))
     (if (<= 65 code 90) (return-from result (- code 55)))
     (if (<= 97 code 122) (return-from result (- code 87)))
     (let ((low 0) (high (floor (length ranges) 3)))
       (declare (type (integer 0 #x7fffffff) low high))
       (tagbody search
          (if (< low high) nil (return-from result -1))
          (let* ((mid (ash (+ low high) -1)) (offset (* mid 3))
                 (start (char-code (char ranges offset)))
                 (end (char-code (char ranges (+ offset 1)))))
            (declare (type (integer 0 #x7fffffff) mid offset))
            (if (< code start) (setq high mid)
                (if (> code end) (setq low (+ mid 1))
                    (return-from result (- code (char-code (char ranges (+ offset 2))))))))
          (go search))))))

(defun character-alpha-code-p (code ranges)
  "Search START, END pairs after the ASCII alphabetic fast path."
  (declare (type (integer 0 #x10ffff) code) (type simple-string ranges)
           (optimize (safety 0) (debug 0))
           (inline <= < > + ash length char char-code))
  (values
   (block result
     (if (< code 128)
         (return-from result (if (<= 65 code 90) t (<= 97 code 122))))
     (let ((low 0) (high (ash (length ranges) -1)))
       (declare (type (integer 0 #x7fffffff) low high))
       (tagbody search
          (if (< low high) nil (return-from result nil))
          (let* ((mid (ash (+ low high) -1)) (offset (ash mid 1))
                 (start (char-code (char ranges offset)))
                 (end (char-code (char ranges (+ offset 1)))))
            (declare (type (integer 0 #x7fffffff) mid offset))
            (if (< code start) (setq high mid)
                (if (> code end) (setq low (+ mid 1)) (return-from result t))))
          (go search))))))

(defun unicode-character-name-code (name)
  "Parse the target's UHHHH / U+HHHH extension; return -1 when unrecognized.
Four to six ASCII hexadecimal digits are accepted, independently of readtable
case, and codes at or above the Unicode limit are rejected. Surrogate handling
belongs to the character repertoire boundary, as with the native predecessor."
  (declare (type string name) (optimize (safety 0) (debug 0))
           (inline length char char-code = < > <= + - ash logior))
  (values
   (block result
     (let ((size (length name)) (position 1) (code 0))
       (declare (type (integer 0 #x7fffffff) size position)
                (type (integer 0 #xffffff) code))
       (if (< size 5) (return-from result -1))
       (if (= (logior (char-code (char name 0)) 32) 117) nil (return-from result -1))
       (if (= (char-code (char name 1)) 43) (setq position 2))
       (if (<= 4 (- size position) 6) nil (return-from result -1))
       (tagbody next
          (if (= position size) (return-from result (if (< code #x110000) code -1)))
          (let* ((raw (char-code (char name position)))
                 (digit (if (<= 48 raw 57) (- raw 48)
                            (if (<= 65 raw 70) (- raw 55)
                                (if (<= 97 raw 102) (- raw 87) -1)))))
            (if (< digit 0) (return-from result -1))
            (setq code (+ (ash code 4) digit)))
          (setq position (+ position 1))
          (go next))))))

(defun character-case-code-value (code down ranges ansi)
  "Search a validated START, END, ZigZag(DELTA), FLAGS character table.
The native wrappers provide immutable simple strings produced by the checked
table encoder. ANSI excludes nonreciprocal case mappings; Unicode folding
retains them. Negative reader sentinels pass through unchanged."
  (declare (type (signed-byte 32) code) (type simple-string ranges)
           (optimize (safety 0) (debug 0))
           (inline < <= > = + - ash logand length char char-code zerop))
  (values
   (block result
     (if (< code 128)
         (return-from result
           (if down (if (<= 65 code 90) (+ code 32) code)
               (if (<= 97 code 122) (- code 32) code))))
     (let ((low 0) (high (ash (the (integer 0 #x1ffffffe) (length ranges)) -2)))
       (declare (type (integer 0 #x1fffffff) low high))
       (tagbody search
          (if (< low high) nil (return-from result code))
          (let* ((mid (ash (the (integer 0 #x3ffffffe) (+ low high)) -1)) (offset (ash mid 2))
                 (start (char-code (char ranges offset)))
                 (end (char-code (char ranges (+ offset 1)))))
            (declare (type (integer 0 #x1fffffff) mid)
                     (type (integer 0 #x7ffffffc) offset))
            (if (< code start) (setq high mid)
                (if (> code end) (setq low (+ mid 1))
                    (let ((flags (char-code (char ranges (+ offset 3)))))
                      (if (zerop (logand (- code start) (logand flags 1))) nil
                          (return-from result code))
                      (if ansi (if (zerop (logand flags 2)) nil (return-from result code)))
                      (let* ((encoded (char-code (char ranges (+ offset 2))))
                             (magnitude (ash encoded -1)))
                        (return-from result
                          (+ code (if (zerop (logand encoded 1)) magnitude (- -1 magnitude)))))))))
          (go search))))))
