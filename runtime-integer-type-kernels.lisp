;;;; Host reference algorithms for integer interval and endpoint regressions.
;;;; The runtime uses the handwritten workers in runtime-integer-types.lisp.
;;;; Keep this oracle for host tests and matched SBCL comparisons; it is not a
;;;; runtime migration entry or a dependency of the shipped Wasm image.
(in-package #:miricl-runtime)
(export '(integer-power-endpoint-value integer-endpoint-width-value
          integer-endpoint-compare-value integer-type-query-value))

(defun integer-power-endpoint-value (exponent sign ones)
  (declare (type integer exponent sign) (optimize (safety 0) (debug 0)))
  (values (if (= exponent 0) (if ones 0 sign) (vector sign exponent ones))))

(defun integer-endpoint-width-value (endpoint)
  (declare (optimize (safety 0) (debug 0))
           (inline svref integerp integer-length abs +))
  (values (if (integerp endpoint)
              (integer-length (abs endpoint))
              (+ (the integer (svref (the simple-vector endpoint) 1))
                 (if (svref (the simple-vector endpoint) 2) 0 1)))))

(defun integer-endpoint-compare-value (a b)
  (declare (optimize (safety 0) (debug 0))
           (inline svref integerp signum abs integer-length + - * < = logand 1- 1+))
  (values
   (block comparison
     (if (integerp a)
         (if (integerp b) (return-from comparison (if (< a b) -1 (if (= a b) 0 1)))))
     (let* ((sa (if (integerp a) (signum a) (svref (the simple-vector a) 0)))
            (sb (if (integerp b) (signum b) (svref (the simple-vector b) 0)))
            (factor 1) (temporary nil) (width 0) (other-width 0) (answer 0))
       (declare (type integer sa sb factor width other-width answer))
       (if (= sa sb) nil (return-from comparison (if (< sa sb) -1 1)))
       (if (integerp a) (setq temporary a a b b temporary factor -1))
       (setq width (if (integerp a) (integer-length (abs a))
                       (+ (the integer (svref (the simple-vector a) 1))
                          (if (svref (the simple-vector a) 2) 0 1)))
             other-width (if (integerp b) (integer-length (abs b))
                             (+ (the integer (svref (the simple-vector b) 1))
                                (if (svref (the simple-vector b) 2) 0 1))))
       (if (= width other-width) nil
           (return-from comparison (* factor sa (if (< width other-width) -1 1))))
       (if (integerp b)
           (setq answer
             (if (svref (the simple-vector a) 2)
                 (let ((magnitude (abs b)))
                   (if (= (logand magnitude (1+ magnitude)) 0) 0 1))
                 (let ((magnitude (abs b)))
                   (if (= (logand magnitude (1- magnitude)) 0) 0 -1))))
           (progn
             (if (eq (svref (the simple-vector a) 2) (svref (the simple-vector b) 2))
                 (return-from comparison 0))
             (if (= width 1) (return-from comparison 0))
             (setq answer (if (svref (the simple-vector a) 2) 1 -1))))
       (* factor sa answer)))))

(defun integer-type-query-value (a b state metadata)
  "STATE is #(mode coverage environment): mode 0 describes integer points;
mode 1 proves subtype, returning 0 unknown / 1 true / 2 false. Descriptors are
#(lower upper kind), where kind 0 unknown / 1 interval / 2 empty. Symbolic
endpoints are #(sign exponent ones); no power of a huge exponent is allocated.
METADATA supplies target expansion, class names and finite-spine checking."
  (declare (type simple-vector state) (type function metadata)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) car cdr cons consp eq eql symbolp integerp
                   = < > <= >= + - * 1+ 1- logand make-array))
  (values
   (block query
     (let ((mode (svref state 0)) (coverage (svref state 1)) (env (svref state 2))
           (type a) (name nil) (arguments nil) (class nil) (width nil)
           (lower nil) (upper nil) (kind 0) (unsigned nil)
           (stack nil) (frame nil) (scan nil) (part nil) (path nil)
           (source-lower nil) (source-upper nil) (phase 0)
           (compare-a nil) (compare-b nil) (compare-result 0) (continuation 0)
           (sa 0) (sb 0) (factor 1) (temporary nil) (endpoint-width 0) (other-width 0)
           (bound nil) (bound-lower nil) (exclusive nil) (number 0))
       (declare (type (integer 0 1) mode phase) (type (integer 0 2) kind) (type integer compare-result continuation sa sb factor endpoint-width other-width))
       (tagbody
        parse
          (setq type (funcall metadata 0 type env))
          (setq lower nil upper nil kind 0 arguments nil)
          (if type nil (progn (setq kind 2) (go descriptor)))
          (setq name type)
          (if (consp type)
              (setq name (car type) arguments (cdr type))
              (if (symbolp type) nil
                  (progn (setq class (funcall metadata 2 type nil))
                         (if class (setq name (car class))))))
          (if (eq name 'and)
              (if (consp type)
                  (progn
                    ;; Reject recursive expansion controls while allowing shared
                    ;; DAGs and arbitrary circular data in opaque parameters.
                    (setq path stack)
                    (tagbody active
                       (if path nil (go checked))
                       (if (eq type (svref (the simple-vector (car path)) 0))
                           (funcall metadata 4 type 3))
                       (setq path (cdr path)) (go active)
                      checked)
                    ;; type, scan, pending, lo, hi, integer, empty, unknown,
                    ;; coverage, continuation phase, original part.
                    (setq frame (vector type arguments nil nil nil nil nil nil coverage 0 nil))
                    (setq stack (cons frame stack))
                    (go next-part))))
          (case name
            (fixnum (setq lower -9223372036854775808 upper 9223372036854775807 kind 1) (go descriptor))
            (bit (setq lower 0 upper 1 kind 1) (go descriptor))
            (mod
             (setq width (car arguments))
             (if (integerp width) nil (error 'type-error :datum width :expected-type 'integer))
             (if (> width 0) nil (error 'type-error :datum width :expected-type (list 'integer 1 '*)))
             (setq lower 0 upper (1- width) kind 1) (go descriptor)))
          (if (if (eq name 'integer) t
                  (if coverage (if (eq name 'real) t (eq name 'rational))))
              (progn
                (setq kind 1)
                (if arguments nil (go descriptor))
                (setq bound (car arguments) bound-lower t) (go round-bound)))
          (if coverage
              (case name
                ((t number atom) (setq kind 1) (go descriptor))
                ((float single-float double-float short-float long-float ratio complex null boolean keyword
                  symbol character base-char standard-char extended-char cons list sequence array vector
                  simple-array simple-vector string simple-string base-string simple-base-string bit-vector simple-bit-vector
                  function compiled-function)
                 (setq kind 2) (go descriptor))))
          (if (if (eq name 'signed-byte) t (eq name 'unsigned-byte)) nil (go descriptor))
          (setq unsigned (eq name 'unsigned-byte))
          (if unsigned (setq lower 0))
          (setq width (car arguments) kind 1)
          (if (if (null arguments) t (eq width '*)) (go descriptor))
          (if (integerp width) nil (error 'type-error :datum width :expected-type 'integer))
          (if (if (< width 0) t (if (null unsigned) (= width 0)))
              (error 'type-error :datum width :expected-type (list 'integer (if unsigned 0 1) '*)))
          (if unsigned nil
              (progn (setq width (1- width))
                     (setq lower (if (= width 0) -1 (vector -1 width nil)))))
          (setq upper (if (= width 0) 0 (vector 1 width t)))
        descriptor
          (if stack (go merge-part))
          (if (= mode 0) (return-from query (vector lower upper kind)))
          (if (= phase 1) (go prove-target))
          (if (= kind 0) (return-from query 0))
          (if (= kind 2) (return-from query 1))
          (if lower (if upper
              (progn (setq compare-a lower compare-b upper continuation 0) (go compare-endpoints))))
        source-after-empty
          (setq source-lower lower source-upper upper)
          (setq name b)
          (if (symbolp b) nil
              (progn (setq class (funcall metadata 2 b nil)) (if class (setq name (car class)))))
          (if (eq name 'bignum)
              (progn
                (if upper (progn (setq compare-a upper compare-b -9223372036854775808 continuation 1)
                                 (go compare-endpoints)))
                (go bignum-lower)))
          (setq phase 1 type b coverage t) (go parse)
        next-part
          (setq frame (car stack) scan (svref (the simple-vector frame) 1))
          (if scan nil
              (if (svref (the simple-vector frame) 2)
                  (progn
                    (setq scan (car (svref (the simple-vector frame) 2)))
                    (setf (svref (the simple-vector frame) 2) (cdr (svref (the simple-vector frame) 2)))
                    (setf (svref (the simple-vector frame) 1) scan)
                    (if scan nil (go next-part)))
                  (go finish-and)))
          ;; Expansion can mutate a spine validated before its first call.
          (if (funcall metadata 3 scan nil) nil
              (funcall metadata 4 scan 3))
          (setq part (funcall metadata 0 (car scan) env))
          (setf (svref (the simple-vector frame) 1) (cdr scan))
          (if (consp part)
              (if (eq (car part) 'and)
                  (progn
                    (setf (svref (the simple-vector frame) 2)
                          (cons (svref (the simple-vector frame) 1) (svref (the simple-vector frame) 2)))
                    (setf (svref (the simple-vector frame) 1) (cdr part))
                    (go next-part))))
          (setf (svref (the simple-vector frame) 10) part)
          (setf (svref (the simple-vector frame) 9) 1)
          (setq type part coverage t) (go parse)
        merge-part
          (setq frame (car stack))
          (if (= (the integer (svref (the simple-vector frame) 9)) 2)
              (progn (setf (svref (the simple-vector frame) 5) (if (= kind 0) nil t))
                     (go next-part)))
          (if (= kind 0) (setf (svref (the simple-vector frame) 7) t))
          (if (= kind 2) (setf (svref (the simple-vector frame) 6) t))
          (if lower
              (if (svref (the simple-vector frame) 3)
                  (progn (setq compare-a lower compare-b (svref (the simple-vector frame) 3) continuation 3)
                         (go compare-endpoints))
                  (setf (svref (the simple-vector frame) 3) lower)))
        merge-upper
          (if upper
              (if (svref (the simple-vector frame) 4)
                  (progn (setq compare-a upper compare-b (svref (the simple-vector frame) 4) continuation 4)
                         (go compare-endpoints))
                  (setf (svref (the simple-vector frame) 4) upper)))
        merge-integer
          (if (if (svref (the simple-vector frame) 8) t (svref (the simple-vector frame) 5))
              (go next-part))
          (setf (svref (the simple-vector frame) 9) 2)
          (setq type (svref (the simple-vector frame) 10) coverage nil) (go parse)
        finish-and
          (setq lower (svref (the simple-vector frame) 3) upper (svref (the simple-vector frame) 4)
                coverage (svref (the simple-vector frame) 8) kind 1)
          (if (if coverage t (svref (the simple-vector frame) 5)) nil
              (progn (setq lower nil upper nil kind 0) (go and-done)))
          (if lower (if upper
              (progn (setq compare-a lower compare-b upper continuation 5) (go compare-endpoints))))
        and-after-empty
          (if (svref (the simple-vector frame) 6) (setq lower nil upper nil kind 2)
              (if (svref (the simple-vector frame) 7) (setq lower nil upper nil kind 0)))
        and-done
          (setq stack (cdr stack)) (go descriptor)
        prove-target
          (if (= kind 0) (return-from query 0))
          (if (= kind 2) (return-from query 2))
          (if lower
              (if source-lower
                  (progn (setq compare-a source-lower compare-b lower continuation 6) (go compare-endpoints))
                  (return-from query 2)))
        prove-upper
          (if upper
              (if source-upper
                  (progn (setq compare-a source-upper compare-b upper continuation 7) (go compare-endpoints))
                  (return-from query 2)))
          (return-from query 1)
        bignum-lower
          (if source-lower
              (progn (setq compare-a source-lower compare-b 9223372036854775807 continuation 2)
                     (go compare-endpoints)))
          (return-from query 2)
        round-bound
          (setq exclusive nil)
          (if (eq bound '*) (setq number nil)
              (progn
                (if (consp bound)
                    (progn
                           (setq temporary (funcall metadata 3 bound nil))
                           (if temporary nil (funcall metadata 4 bound 3))
                           (if (= temporary 1) nil (error 'program-error))
                           (setq bound (car bound) exclusive t)))
                (setq number (rational bound))
                (if (integerp number)
                    (if exclusive (setq number (if bound-lower (1+ number) (1- number))))
                    (setq number (if bound-lower (ceiling number) (floor number))))))
          (if bound-lower
              (progn (setq lower number)
                     (if (cdr arguments)
                         (progn (setq bound (car (cdr arguments)) bound-lower nil) (go round-bound))))
              (setq upper number))
          (go descriptor)
        compare-endpoints
          ;; One shared comparison site keeps sealed worker growth bounded.
          (setq factor 1)
          (if (integerp compare-a)
              (if (integerp compare-b)
                  (progn (setq compare-result (if (< compare-a compare-b) -1 (if (= compare-a compare-b) 0 1)))
                         (go compared))))
          (setq sa (if (integerp compare-a) (signum compare-a) (svref (the simple-vector compare-a) 0))
                sb (if (integerp compare-b) (signum compare-b) (svref (the simple-vector compare-b) 0)))
          (if (= sa sb) nil
              (progn (setq compare-result (if (< sa sb) -1 1)) (go compared)))
          (if (integerp compare-a)
              (setq temporary compare-a compare-a compare-b compare-b temporary factor -1))
          (setq endpoint-width (if (integerp compare-a) (integer-length (abs compare-a))
                                   (+ (the integer (svref (the simple-vector compare-a) 1))
                                      (if (svref (the simple-vector compare-a) 2) 0 1)))
                other-width (if (integerp compare-b) (integer-length (abs compare-b))
                                (+ (the integer (svref (the simple-vector compare-b) 1))
                                   (if (svref (the simple-vector compare-b) 2) 0 1))))
          (if (= endpoint-width other-width) nil
              (progn (setq compare-result (* factor sa (if (< endpoint-width other-width) -1 1)))
                     (go compared)))
          (if (integerp compare-b)
              (progn
                (setq temporary (abs compare-b))
                (setq compare-result
                  (if (svref (the simple-vector compare-a) 2)
                      (if (= (logand temporary (1+ temporary)) 0) 0 1)
                      (if (= (logand temporary (1- temporary)) 0) 0 -1))))
              (progn
                (setq compare-result 0)
                (if (eq (svref (the simple-vector compare-a) 2) (svref (the simple-vector compare-b) 2))
                    (go compared))
                (if (= endpoint-width 1) (go compared))
                (setq compare-result (if (svref (the simple-vector compare-a) 2) 1 -1))))
          (setq compare-result (* factor sa compare-result))
        compared
          (case continuation
            (0 (if (> compare-result 0) (return-from query 1)) (go source-after-empty))
            (1 (if (< compare-result 0) (return-from query 1)) (go bignum-lower))
            (2 (return-from query (if (> compare-result 0) 1 2)))
            (3 (if (> compare-result 0) (setf (svref (the simple-vector frame) 3) lower)) (go merge-upper))
            (4 (if (< compare-result 0) (setf (svref (the simple-vector frame) 4) upper)) (go merge-integer))
            (5 (if (> compare-result 0) (setf (svref (the simple-vector frame) 6) t)) (go and-after-empty))
            (6 (if (< compare-result 0) (return-from query 2)) (go prove-upper))
            (7 (return-from query (if (> compare-result 0) 2 1)))))))))
