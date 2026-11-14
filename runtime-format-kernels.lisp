;;;; Portable FORMAT algorithms; raw stream capture and value-packet handling
;;;; remain in the representation adapters in runtime-format.lisp.
(in-package #:miricl-runtime)
(export '(format-slash-position format-leading-zero-text format-rounded-integer
          format-decimal-parts format-constant-text-p))

(defun format-slash-position (text start end)
  "Validated flat string region. Return -1 for a missing slash."
  (declare (type simple-string text) (type (integer 0 #x7fffffff) start end)
           (optimize (safety 0) (debug 0)) (inline char char-code = >= 1+))
  (values
   (block found
     (tagbody next
        (if (>= start end) (return-from found -1))
        (if (= (char-code (char text start)) 47) (return-from found start))
        (setq start (the (integer 0 #x7fffffff) (1+ start)))
        (go next)))))

(defun format-constant-text-p (text)
  (declare (type simple-string text) (optimize (safety 0) (debug 0))
           (inline length char char-code = 1+))
  (values
   (block valid
     (let ((i 0) (n (length text)))
       (declare (type (integer 0 #x7fffffff) i n))
       (tagbody next
          (if (= i n) (return-from valid t))
          (if (= (char-code (char text i)) 126) (return-from valid nil))
          (setq i (the (integer 0 #x7fffffff) (1+ i)))
          (go next))))))

(defun format-leading-zero-text (text width supplied)
  "Preserve identity unless a supplied field width permits omitting the zero."
  (declare (type simple-string text) (type (integer 0 #x7fffffff) width)
           (optimize (safety 0) (debug 0))
           (inline length char (setf char) char-code = < > <= + - 1+ 1- make-string))
  (values
   (let ((n (length text)) (offset 0))
     (declare (type (integer 0 #x7fffffff) n offset))
     (if (if supplied (if (> n width) (> n 1)))
         (progn
           (if (if (= (char-code (char text 0)) 45) t (= (char-code (char text 0)) 43))
               (setq offset 1))
           (if (if (> n (+ offset 1))
                   (if (= (char-code (char text offset)) 48)
                       (if (= (char-code (char text (+ offset 1))) 46)
                           (if (= width (1- n)) t
                               (if (> n (+ offset 2))
                                   (<= 48 (char-code (char text (+ offset 2))) 57))))))
               (let ((result (make-string (1- n))) (i 0))
                 (declare (type simple-string result) (type (integer 0 #x7fffffff) i))
                 (tagbody next
                    (if (= i (1- n)) (return-from format-leading-zero-text (values result)))
                    (setf (char result i) (char text (if (< i offset) i (1+ i))))
                    (setq i (the (integer 0 #x7fffffff) (1+ i)))
                    (go next))))))
     text)))

(defun format-rounded-integer (number scale even)
  "Absolute exact decimal rounding. SCALE has already been bounded to ±2000."
  (declare (type real number) (type (integer -2000 2000) scale)
           (optimize (safety 0) (debug 0))
           (inline rational numerator denominator abs expt truncate >= > = * - 1+ oddp))
  (let* ((value (rational number)) (a (abs (numerator value)))
         (b (denominator value)))
    (if (>= scale 0) (setq a (* a (expt 10 scale)))
        (setq b (* b (expt 10 (- scale)))))
    (let* ((quotient (truncate a b)) (remainder (- a (* quotient b)))
           (twice (* remainder 2)))
      (values (if (if (> twice b) t (if (= twice b) (if even (oddp quotient) t)))
                  (1+ quotient) quotient)))))

(defun format-decimal-parts (text)
  "Parse the native finite decimal printer's text into #(exponent precision)."
  (declare (type simple-string text) (optimize (safety 0) (debug 0))
           (inline length char char-code = < > >= + - * 1+ make-array svref (setf svref)))
  (let ((i 0) (whole 0) (point nil) (count 0) (first -1) (last 0)
        (exponent 0) (negative nil) (n (length text)))
    (declare (type (integer 0 #x7fffffff) i whole count last n)
             (type (integer -1 #x7fffffff) first) (type integer exponent))
    (block mantissa
      (tagbody next
         (if (>= i n) (return-from mantissa nil))
         (let ((c (char-code (char text i))))
           (setq i (the (integer 0 #x7fffffff) (1+ i)))
           (if (= c 45) (go next))
           (if (= c 46) (progn (setq point t) (go next)))
           (if (if (< c 48) t (> c 57)) (return-from mantissa nil))
           (if point nil (setq whole (the (integer 0 #x7fffffff) (1+ whole))))
           (if (= c 48) nil (progn (if (< first 0) (setq first count)) (setq last count)))
           (setq count (the (integer 0 #x7fffffff) (1+ count))))
         (go next)))
    (tagbody next
       (if (>= i n) (go done))
       (let ((c (char-code (char text i))))
         (setq i (the (integer 0 #x7fffffff) (1+ i)))
         (if (= c 45) (progn (setq negative t) (go next)))
         (setq exponent (+ (* exponent 10) (- c 48))))
       (go next) done)
    (if negative (setq exponent (- exponent)))
    (let ((result (make-array 2)))
      (declare (type simple-vector result))
      (setf (svref result 0) (if (< first 0) 0 (+ exponent (- whole 1 first))))
      (setf (svref result 1) (if (< first 0) 1 (+ (- last first) 1)))
      (values result))))

(export '(format-control-values))
(defun format-control-values (text arguments state)
  "Parse one directive, or scan a nested FORMAT body with the same parser.
STATE is #(start end scan close separator); CLOSE=0 selects directive parsing.
Return #(after directive colon at parameters remaining-arguments count), or
#(tilde after directive colon) for a matching close/separator. NIL denotes
malformed syntax; integer zero denotes a missing V argument. Checked native
adapters map those private results to the established Lisp conditions."
  (declare (type simple-string text) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) char char-code ascii-upper-code
                   length list-length consp car cdr make-array = < > <= >= + - * 1+))
  (values
   (block result
     (let* ((i (svref state 0)) (end (svref state 1)) (scan (svref state 2))
            (close (svref state 3)) (separator (svref state 4))
            (mark i) (depth 0) (code 0) (colon 0) (at 0) (slot 0) (count 0)
            (small 0) (sign 1) (large nil) (value nil) (params (make-array 8 :initial-element nil)))
       (declare (type (integer 0 #x7fffffff) i end mark depth slot count)
                (type (integer 0 #x10ffff) code close)
                (type (integer 0 1) colon at)
                (type (integer 0 1000000009) small)
                (type (integer -1 1) sign)
                (type simple-vector params))
       (tagbody
          (if (= close 0) (go parse))
        search
          (if (>= i end) (return-from result nil))
          (setq mark i code (char-code (char text i))
                i (the (integer 0 #x7fffffff) (1+ i)))
          (if (= code 126) (go parse) (go search))
        parse
          (setq colon 0 at 0 slot 0 count 0)
        parameter
          (if (>= i end) (return-from result nil))
          (setq code (ascii-upper-code (char-code (char text i))))
          (if (if (= code 58) t (= code 64)) (go modifier))
          (setq value nil)
          (if (= code 44) (go store))
          (if (= code 39)
              (progn
                (setq i (the (integer 0 #x7fffffff) (1+ i)))
                (if (>= i end) (return-from result nil))
                (setq value (char text i) i (the (integer 0 #x7fffffff) (1+ i)))
                (go store)))
          (if (= code 35)
              (progn
                (setq value (list-length arguments))
                (if value nil (return-from result nil))
                (setq i (the (integer 0 #x7fffffff) (1+ i)))
                (go store)))
          (if (= code 86)
              (progn
                (if scan nil
                    (if arguments
                        (if (consp arguments)
                            (setq value (car (the cons arguments)) arguments (cdr (the cons arguments)))
                            (return-from result nil))
                        (return-from result 0)))
                (setq i (the (integer 0 #x7fffffff) (1+ i)))
                (go store)))
          (setq sign 1)
          (if (if (= code 43) t (= code 45))
              (progn
                (if (= code 45) (setq sign -1))
                (setq i (the (integer 0 #x7fffffff) (1+ i)))
                (if (>= i end) (return-from result nil))
                (setq code (char-code (char text i)))
                (if (if (< code 48) t (> code 57)) (return-from result nil))))
          (if (if (< code 48) t (> code 57)) (go modifier))
          (setq small 0 large nil)
        number
          (if (>= i end) (go number-done))
          (setq code (char-code (char text i)))
          (if (if (< code 48) t (> code 57)) (go number-done))
          (if (if large nil (<= small 100000000))
              (setq small (the (integer 0 1000000009) (+ (* small 10) (- code 48))))
              (progn
                (if large nil (setq large small))
                (setq large (+ (* large 10) (- code 48)))))
          (setq i (the (integer 0 #x7fffffff) (1+ i)))
          (go number)
        number-done
          (setq value (if large (if (< sign 0) (- large) large) (* small sign)))
        store
          (if (>= slot (length params))
              (let ((grown (make-array (* (length params) 2) :initial-element nil)) (j 0) (n (length params)))
                (declare (type simple-vector grown) (type (integer 0 #x7fffffff) j n))
                (tagbody copy
                   (if (= j n) (go copied))
                   (setf (svref grown j) (svref params j))
                   (setq j (the (integer 0 #x7fffffff) (1+ j)))
                   (go copy)
                 copied)
                (setq params grown)))
          (setf (svref params slot) value)
          (setq count (the (integer 0 #x7fffffff) (1+ slot)))
          (if (>= i end) (return-from result nil))
          (if (= (char-code (char text i)) 44)
              (progn (setq slot (the (integer 0 #x7fffffff) (1+ slot))
                           i (the (integer 0 #x7fffffff) (1+ i)))
                     (go parameter)))
        modifier
          (if (>= i end) (return-from result nil))
          (setq code (ascii-upper-code (char-code (char text i)))
                i (the (integer 0 #x7fffffff) (1+ i)))
          (if (= code 58)
              (if (= colon 0) (progn (setq colon 1) (go modifier)) (return-from result nil)))
          (if (= code 64)
              (if (= at 0) (progn (setq at 1) (go modifier)) (return-from result nil)))
          (if (= close 0)
              (let ((packet (make-array 7)))
                (declare (type simple-vector packet))
                (setf (svref packet 0) i)
                (setf (svref packet 1) code)
                (setf (svref packet 2) colon)
                (setf (svref packet 3) at)
                (setf (svref packet 4) params)
                (setf (svref packet 5) arguments)
                (setf (svref packet 6) count)
                (return-from result packet)))
          ;; Slash names contain literal control characters, including tildes.
          (if (= code 47)
              (progn
                (tagbody slash
                   (if (>= i end) (return-from result nil))
                   (setq code (char-code (char text i))
                         i (the (integer 0 #x7fffffff) (1+ i)))
                   (if (= code 47) (go slash-done))
                   (go slash)
                 slash-done)
                (setq code 47)))
          (if (= depth 0)
              (if (if (= code close) t (if separator (= code 59)))
                  (let ((packet (make-array 4)))
                    (declare (type simple-vector packet))
                    (setf (svref packet 0) mark)
                    (setf (svref packet 1) i)
                    (setf (svref packet 2) code)
                    (setf (svref packet 3) colon)
                    (return-from result packet))))
          (if (if (= code 60) t (if (= code 123) t (if (= code 91) t (= code 40))))
              (setq depth (the (integer 0 #x7fffffff) (1+ depth))))
          (if (if (= code 62) t (if (= code 125) t (if (= code 93) t (= code 41))))
              (if (= depth 0) (return-from result nil)
                  (setq depth (the (integer 0 #x7fffffff) (1- depth)))))
          (go search))))))
