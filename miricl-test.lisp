(defpackage #:miricl-test
  (:use #:cl #:fiveam #:cffi)
  (:local-nicknames (#:wt #:wasmtime) (#:m #:miricl)))
(in-package #:miricl-test)
(named-readtables:in-readtable :miricl-wat)
(def-suite :miricl)
(in-suite :miricl)

(defun sequence-test-bounds (sequence)
  "Representative empty, full, prefix, suffix and interior ranges.
The exhaustive start/end matrix is covered by upstream ANSI tests."
  (let ((size (length sequence)))
    (remove-duplicates
      (list (list 0 size) '(0 0) (list size size)
            (list (min 1 size) size) (list 0 (max 0 (1- size)))
            (list (min 1 size) (max (min 1 size) (1- size)))) :test #'equal)))

(defun reference-remove-duplicates (sequence &key from-end (start 0) end key
                                               (test #'eql) (test-not nil negate))
  ;; Use pairwise selection rather than relying on host implementations of
  ;; :TEST-NOT with :FROM-END (ECL 26.5.5 gives different results).
  (let* ((items (coerce sequence 'vector)) (stop (or end (length items)))
         (kept nil) (result nil))
    (labels ((matches (a b)
               (let ((a (if key (funcall key a) a)) (b (if key (funcall key b) b)))
                 (if negate (not (funcall test-not a b)) (funcall test a b)))))
      (dotimes (i (length items))
        (let ((item (aref items i)))
          (unless (and (<= start i) (< i stop)
                       (if from-end
                           (some (lambda (earlier) (matches item earlier)) kept)
                           (loop for j from (1+ i) below stop
                                 thereis (matches item (aref items j)))))
            (push item result)
            (when (<= start i) (push item kept))))))
    (coerce (nreverse result) (if (stringp sequence) 'string
                                (if (vectorp sequence) 'vector 'list)))))

(test trivial-compile
  (is (equalp (wt:wat-to-wasm "(module)") #(0 97 115 109 1 0 0 0))))
(test trivial-compile-error (signals wt:wasmtime-error (wt:wat-to-wasm "(module")))

(test host-integer-transport
  (m:with-interpreter (vm)
    (dolist (number (list (1- most-negative-fixnum) most-negative-fixnum
                         (1+ most-negative-fixnum) most-positive-fixnum
                         (1+ most-positive-fixnum)))
      (let ((result (m:evaluate vm number)))
        (is (eql number result))
        (is (eq (typep number 'fixnum) (typep result 'fixnum)))))))

(test host-character-reader
  (m:with-interpreter (vm)
    (dolist (character '(#\A #\a #\w #\Space #\Newline #\( #\) #\\))
      (let ((source (write-to-string character :readably t)))
        (is (eql character (m:evaluate-string vm source)))))))

(test wasm-debug-name-removal
  (let* ((raw (wt:wat-to-wasm "(module $named (func $answer (export \"answer\") (result i32) (i32.const 42)))"))
         (stripped (m::wasm-without-debug-names raw))
         (metadata #(0 7 3 102 111 111 1 2 3))
         (with-metadata (concatenate '(vector (unsigned-byte 8)) (subseq raw 0 8) metadata (subseq raw 8)))
         (expected (concatenate '(vector (unsigned-byte 8)) (subseq stripped 0 8) metadata (subseq stripped 8))))
    (is (< (length stripped) (length raw)))
    (is (equalp stripped (m::wasm-without-debug-names stripped)))
    (is (equalp expected (m::wasm-without-debug-names with-metadata)))
    (wt:with-engine (engine)
      (wt:with-store ((store context) engine)
        (let ((module (wt:compile-module engine expected)))
          (unwind-protect
            (wt:with-instance (instance context module)
              (is (= 42 (wt:instance-call context instance "answer"))))
            (wt::wasmtime-module-delete module)))))
    (dolist (bytes '(#() #(0 97 115 109 1 0 0 0 0)
                     #(0 97 115 109 1 0 0 0 0 127)
                     #(0 97 115 109 1 0 0 0 0 1 4)
                     #(0 97 115 109 1 0 0 0 0 128 128 128 128 128)))
      (signals error (m::wasm-without-debug-names bytes))))
  (let ((debug (m:interpreter-wasm :debug-names t)) (compact (m:interpreter-wasm)))
    (is (equalp compact (m::wasm-without-debug-names debug)))
    (is (< (length compact) (length debug)))
    (setf (aref compact 0) 255)
    (is (= 0 (aref (m:interpreter-wasm) 0)))))

(test bootstrap-string-references-copy
  (let* ((strings (loop for i below 140 collect (format nil "bootstrap entry ~D" i)))
         (payload (append strings
                    (list (copy-seq (first strings)) (copy-seq (nth 127 strings))
                          (copy-seq (nth 139 strings)) "" (copy-seq "") "é𝄞" (copy-seq "é𝄞"))))
         (fixture (list 'set (list 'intern "BOOTSTRAP-STRING-FIXTURE" "CL-USER") (list 'quote payload)))
         (m::*interpreter-library* (append m::*interpreter-library* (list fixture)))
         (m::*interpreter-wasm* (wt:wat-to-wasm (m::interpreter-wat :compiler nil))))
    ;; The codec shares encoded contents, including wide reference indices,
    ;; while retaining separate empty, ASCII and Unicode string objects.
    (m:with-interpreter (vm)
      (is (eq t (m:evaluate-string vm
        "(let ((items cl-user::bootstrap-string-fixture))
           (and (= (length items) 147)
                (string= (first items) (nth 140 items))
                (not (eq (first items) (nth 140 items)))
                (string= (nth 127 items) (nth 141 items))
                (string= (nth 139 items) (nth 142 items))
                (string= (nth 143 items) (nth 144 items))
                (not (eq (nth 143 items) (nth 144 items)))
                (string= (nth 145 items) (nth 146 items))
                (not (eq (nth 145 items) (nth 146 items)))))"))))))

(test bootstrap-constant-tags
  (let* ((numbers '(0 1 -1 63 64 -64 -65 127 128 -128
                   -134217727 134217727 -134217728 134217728
                   -9223372036854775808 9223372036854775807))
         (payload (append (loop repeat 24 append numbers)
                          (list (cons -1 1) '(nil 1 -1) '(quote 0))))
         (fixture (list 'set (list 'intern "BOOTSTRAP-CONSTANT-FIXTURE" "CL-USER") (list 'quote payload)))
         (m::*interpreter-library* (append m::*interpreter-library* (list fixture)))
         (m::*interpreter-wasm* (wt:wat-to-wasm (m::interpreter-wat :compiler nil))))
    ;; Frequent integers and syntax lists share the tag bank. Boundary values
    ;; outside the packed table's range still use the full 64-bit encoding;
    ;; dotted tails and NIL-headed lists must retain their original structure.
    (m:with-interpreter (vm)
      (is (eq t (m:evaluate-string vm
        (format nil "(equalp cl-user::bootstrap-constant-fixture '~S)" payload)))))))

(test runtime-function-layout
  ;; Cross the one-byte function-index boundary, including recursive calls and
  ;; exported functions. Both modules must retain identical observable results.
  (let* ((m::*runtime-function-definitions* (make-array 0 :adjustable t :fill-pointer 0))
         (hot 'm::$layout-hot)
         (answer 'm::$layout-answer))
    (dotimes (i 128)
      (let ((name (make-symbol (format nil "$cold-~D" i))))
        (m::runtime-function-definition name
          (m:wat-fragment (m:wat-string `(func ,name))))))
    (m::runtime-function-definition hot (m:wat-fragment
      #@(func $layout-hot (param $n i32) (result i32)
        (if (result i32) (local.get $n)
          (then (call $layout-hot (i32.sub (local.get $n) (i32.const 1))))
          (else (i32.const 42))))))
    (m::runtime-function-definition answer (m:wat-fragment
      #@(func $layout-answer (export "answer") (result i32)
        ,@(loop repeat 64 collect '(drop (call $layout-hot (i32.const 2))))
        (call $layout-hot (i32.const 3)))))
    (let* ((definitions m::*runtime-function-definitions*)
           (original (copy-seq definitions))
           (source (apply #'m::runtime-wat-sequence (map 'list #'cdr definitions)))
           (layout (m::runtime-function-layout definitions))
           (before (wt:wat-to-wasm #@(module ,source)))
           (after (wt:wat-to-wasm #@(module ,layout))))
      (is (< (length after) (length before)))
      (is (equalp original definitions))
      (is (equal (m::wat-fragment-text layout)
                 (m::wat-fragment-text (m::runtime-function-layout definitions))))
      (wt:with-engine (engine)
        (wt:with-store ((store context) engine)
          (dolist (bytes (list before after))
            (let ((module (wt:compile-module engine bytes)))
              (unwind-protect
                   (wt:with-instance (instance context module)
                     (is (= 42 (wt:instance-call context instance "answer"))))
                (wt::wasmtime-module-delete module)))))))
    ;; The DSL still renders definitions when called outside module assembly.
    (let ((m::*runtime-function-definitions* nil))
      (is (search "(func $bool " (m::wat-fragment-text (m::$bool)))))))

(test static-printer-literals
  ;; Exercise the actual bulk emitter across a memory growth boundary, with
  ;; UTF-8, embedded NUL, single characters, empty text, and reused fragments.
  (let* ((m::*runtime-output-literals* (make-hash-table :test 'equal))
         (m::*runtime-output-bytes* (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (large (make-string 70000 :initial-element #\x))
         (pieces (list "" "a" "λ" (string (code-char 0)) "🙂λ" large "🙂λ" large))
         (forms (mapcar #'m::emit-text pieces))
         (expected (apply #'concatenate 'string pieces)))
    (is (= 2 (hash-table-count m::*runtime-output-literals*)))
    (wt:with-engine (engine)
      (wt:with-store ((store context) engine)
        (wt:with-module (module engine #@(module
          (memory (export "memory") 1)
          (global $cursor (mut i32) (i32.const 65530))
          (func $fail (param i32) (unreachable))
          ,(m::$reserve) ,(m::$emit-byte) ,(m::$emit-char) ,(m::$emit-literal)
          (func (export "run") (result i32) ,@forms (global.get $cursor))
          (data $output-literal-data ,(m::runtime-library-data m::*runtime-output-bytes*))))
          (wt:with-instance (instance context module)
            (let ((end (wt:instance-call context instance "run")))
              (wt:with-export (export context instance "memory" 3)
                (let* ((memory (wt:extern-payload export 'wt::memory))
                       (data (wt:wasmtime-memory-data context memory)))
                  (is (< 65536 (wt:wasmtime-memory-data-size context memory)))
                  (is (equal expected (foreign-string-to-lisp (inc-pointer data 65530)
                    :count (- end 65530) :encoding :utf-8))))))))))))

(test static-text-literals-copy
  (let* ((m::*runtime-output-literals* (make-hash-table :test 'equal))
         (m::*runtime-output-bytes* (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (pieces (list "" "short" "ASCII literal" (make-string 70000 :initial-element #\x)
                       (concatenate 'string "alpha" (string (code-char 0)) "omega") "λ🙂 Unicode literal"))
         (checks
           (loop for string in pieces for literal = (m::wat-text-literal string) append
             `((local.set $first ,literal) (local.set $second ,literal)
               (if (ref.eq (local.get $first) (local.get $second)) (then (return (i32.const 0))))
               ,@(loop for index in (remove-duplicates (list 0 5 (floor (length string) 2) (1- (length string))))
                       when (< -1 index (length string)) collect
                         `(if (i32.ne (array.get $digits
                           (struct.get $text-box 0 (ref.cast (ref $text-box) (local.get $second)))
                           (i32.const ,index)) (i32.const ,(char-code (char string index))))
                           (then (return (i32.const 0)))))
               ,@(when (plusp (length string))
                   `((array.set $digits (struct.get $text-box 0 (ref.cast (ref $text-box) (local.get $first)))
                       (i32.const 0) (i32.const 0))
                     (if (i32.ne (array.get $digits
                       (struct.get $text-box 0 (ref.cast (ref $text-box) (local.get $second))) (i32.const 0))
                       (i32.const ,(char-code (char string 0)))) (then (return (i32.const 0))))))))))
    (wt:with-engine (engine)
      (wt:with-store ((store context) engine)
        (wt:with-module (module engine #@(module
          (type $digits (array (mut i32))) (type $bootstrap-bytes (array i8))
          (type $text-box (struct (field (ref $digits))))
          (global $literal-text-bytes (mut (ref $bootstrap-bytes)) (array.new_default $bootstrap-bytes (i32.const 0)))
          (global $output-literal-length i32 (i32.const ,(length m::*runtime-output-bytes*)))
          (func $text (param (ref $digits)) (result (ref eq)) (struct.new $text-box (local.get 0)))
          ,(m::$ascii-text-literal)
          (func (export "run") (result i32) (local $first (ref null eq)) (local $second (ref null eq))
            ,(m::runtime-wat-forms checks) (i32.const 1))
          (data $output-literal-data ,(m::runtime-library-data m::*runtime-output-bytes*))))
          (wt:with-instance (instance context module)
            (is (= 1 (wt:instance-call context instance "run")))))))))

(test numeric-bindings
  (wt:with-engine (engine)
    (wt:with-store ((store context) engine)
      (wt:with-module (module engine
                        #@(module
                           (func (export "echo") (param i32 i64 f32 f64) (result i32 i64 f32 f64)
                             (local.get 0) (local.get 1) (local.get 2) (local.get 3))
                           (func (export "trap") (unreachable))
                           (memory (export "memory") 1)))
        (wt:with-instance (instance context module)
          (is (equalp (multiple-value-list (wt:instance-call context instance "echo" -17 (- (expt 2 62)) 1.25 2.5d0))
                      (list -17 (- (expt 2 62)) 1.25 2.5d0)))
          (signals error (wt:instance-call context instance "echo" 1))
          (signals error (wt:instance-call context instance "missing"))
          (signals wt:wasm-trap (wt:instance-call context instance "trap"))
          (signals error (wt:instance-call context instance "memory")))))))

(defcallback add-callback wt::p-wasm-trap
    ((env :pointer) (caller :pointer) (args wt::p-wasmtime-val) (nargs wt::size)
     (results wt::p-wasmtime-val) (nresults wt::size))
  (declare (ignore env caller nargs nresults))
  (wt:write-value results 1
                  (+ (wt:read-value args) (wt:read-value (mem-aptr args '(:struct wt::wasmtime-val) 1))))
  (null-pointer))
(test typed-callback
  (wt:with-engine (engine)
    (wt:with-store ((store context) engine)
      (wt:with-module (module engine #@(module
        (func $add (import "" "add") (param i64 i64) (result i64))
        (func (export "run") (result i64) (call $add (i64.const 40) (i64.const 2)))))
        (wt:with-callback-func (func context (callback add-callback) :parameters '(:i64 :i64) :results '(:i64))
          (let ((instance (wt:func-instance context module func)))
            (unwind-protect (is (= 42 (wt:instance-call context instance "run"))) (foreign-free instance))))))))

(test wat-templates
  (let ((value 42) (include t))
    (let ((wat #@(module
                  ;; A WAT comment and a nested (; block (; comment ;) ;).
                  (func (export "answer") (result i32)
                    ,@(when include `((i32.const ,value)))))))
      (wt:with-engine (engine)
        (wt:with-store ((store context) engine)
          (wt:with-module (module engine wat)
            (wt:with-instance (instance context module)
              (is (= 42 (wt:instance-call context instance "answer"))))))))))
(test wat-lexemes-and-strings
  (is (equal (m:wat-string '(module (export "hello\"\\"))) "(module (export \"hello\\\"\\\\\"))"))
  (is (equal #@(module (data (i32.const 0) "\00\ff\"\\"))
             "(module (data (i32.const 0) \"\\00\\ff\\\"\\\\\"))"))
  (is (equal #@(f64.const 0x1.8p+2) "(f64.const 0x1.8p+2)")))

(test numeric-interpreter
  (m:with-interpreter (vm)
    (dolist (form '(42 (+) (*) (+ 1 2) (- 7) (/ 3 4) (+ 1/3 1/6)
                    (+ #c(1 2) #c(3 4)) (* #c(1 2) #c(3 4)) (/ #c(1 2) #c(3 4))
                    (+ 12345678901234567890 98765432109876543210)
                    (* 12345678901234567890 -98765432109876543210)
                    (/ 12345678901234567890 -98765432109876543210)
                    (+ 1/3 0.5) (+ 1.0 2d0) (expt 2 100) (expt 2 -10)
                    (= 1 1.0 1d0) (< -2 0 1/2) (/= 1 2 1)
                    (float 1/3 1d0) (complex 1 0) (complex 1 2d0)
                    (numerator -4/6) (denominator -4/6) (gcd 200 300)))
      (is (eql (eval form) (m:evaluate vm form)) "Mismatch for ~S" form))))

(test persistent-definitions
  (m:with-interpreter (vm)
    (is (eq 'factorial (m:evaluate vm '(defun factorial (n)
      (declare (type (integer 0 *) n))
      (if (zerop n) 1 (* n (factorial (1- n))))))))
    (is (= (loop for n from 1 to 30 for result = n then (* result n) finally (return result))
           (m:evaluate vm '(factorial 30))))
    (signals m:interpreter-error (m:evaluate vm '(factorial -1)))
    (signals m:interpreter-error (m:evaluate vm '(factorial 1/2)))
    (is (= 42 (m:evaluate vm '(let ((x 40)) (+ x 2)))))
    (is (= 42 (m:evaluate vm '(let ((x 40)) (declare (integer x)) (setq x 42) x))))
    (signals m:interpreter-error (m:evaluate vm '(let ((x 1)) (declare (integer x)) (setq x 1.0))))
    (is (= 3/4 (m:evaluate vm '(the rational (+ 1/4 1/2)))))
    (signals m:interpreter-error (m:evaluate vm '(the integer 3/4)))
    (is (= 42 (m:evaluate vm '(let ((x 40)) (funcall (function (lambda (y) (+ x y))) 2)))))))

(defun same-number-p (expected actual)
  ;; Complex float algorithms may round differently across CL implementations.
  (cond ((complexp expected)
         (and (complexp actual) (same-number-p (realpart expected) (realpart actual))
              (same-number-p (imagpart expected) (imagpart actual))))
        ((floatp expected)
         (and (typep actual (type-of expected))
              (<= (abs (- expected actual))
                  (* 8 (if (typep expected 'single-float) single-float-epsilon double-float-epsilon)
                     (max (abs expected) least-positive-normalized-double-float)))))
        (t (eql expected actual))))

(test native-numeric-comparisons
  (m:with-interpreter (vm)
    ;; ANSI exercises the comparison matrix. Check guest integer digit and
    ;; IEEE precision boundaries across the host/Wasm transport here.
    (dolist (pair (list (list 32767 32768) (list -32768 -32769)
                       (list (1- (expt 2 45)) (expt 2 45))
                       (list (1+ (expt 2 53)) (float (expt 2 53) 1d0))
                       (list (+ (expt 2 100) 1) (float (expt 2 100) 1d0))
                       (list (- (+ (expt 2 100) 1)) (- (float (expt 2 100) 1d0)))
                       (list 0.0 -0d0) (list 1/10 0.1) (list 1/10 0.1d0)
                       (list 1/2 0.5d0) (list -5/7 -0.5)))
      (dolist (values (list pair (reverse pair)))
        (dolist (operator '(= /= < > <= >=))
          (let ((form (list operator (first values) (second values))))
            (is (eql (eval form) (m:evaluate vm form)) "Mismatch for ~S" form)))))
    (dolist (form '((= #c(1.0 0.0) 1) (= #c(1.0 2.0) #c(1 2))
                    (/= #c(1.0 2.0) #c(1 3)) (< -2 -1 0 1 2)
                    (> 2 1 0 -1 -2) (<= -0.0 0 0d0) (>= -0.0 0 0d0)))
      (is (eql (eval form) (m:evaluate vm form))))))

(test exact-float-rationals
  (m:with-interpreter (vm :fuel 50000000)
    (dolist (number (list 0.0 -0.0 0d0 -0d0 0.1 0.1d0 0.5 -0.75d0
                         32767d0 32768d0 1073741823d0 1073741824d0
                         2147483647d0 2147483648d0 -2147483648d0 -2147483649d0
                         least-positive-single-float least-positive-double-float
                         least-positive-normalized-single-float least-positive-normalized-double-float
                         most-positive-single-float most-positive-double-float
                         (- least-positive-double-float) (- most-positive-double-float)))
      (let ((form `(rational ,number)))
        (is (eql (eval form) (m:evaluate vm form))))
      (is (eq t (m:evaluate vm `(= (rational ,number) ,number)))))
    (dolist (form '((let ((x 1/10)) (eq x (rational x)))
                    (let ((x (+ (expt 2 100) 1))) (eq x (rational x)))
                    (multiple-value-list (rational (values 0.1d0 99)))
                    (let ((x (+ (expt 2 100) 1)))
                      (let ((y (+ x 32768))) (list x y (* x 32768) x)))
                    (let ((x (rational 0.1d0))) (list x (+ x 1) (* x x) x))))
      (is (equal (eval form) (m:evaluate vm form))))))

(test native-numeric-extrema
  (m:with-interpreter (vm)
    (let ((numbers (list 0 1 -1 1/2 -5/7 0.0 -0.0 0d0 -0d0 0.1 0.1d0
                         (1+ (expt 2 53)) (float (expt 2 53) 1d0)
                         (expt 2 100) (- (expt 2 100)))))
      (dolist (operator '(max min))
        (dolist (a numbers)
          (is (eql a (m:evaluate vm `(,operator ,a))))
          (dolist (b numbers)
            ;; MAX/MIN may choose an input unchanged or apply contagion.
            ;; This implementation deliberately retains the first tied input.
            (let ((expected (if (funcall (if (eq operator 'max) #'> #'<) b a) b a)))
              (is (eql expected (m:evaluate vm `(,operator ,a ,b)))))))
        (is (eq t (m:evaluate vm
          `(let ((value (+ (expt 2 100) 1))) (eq value (,operator value))))))
        (is (equal '(3 1) (m:evaluate vm
          `(let ((seen nil))
             (list (length (progn
               (,operator (progn (push 1 seen) 3) (progn (push 2 seen) 2) (progn (push 3 seen) 1))
               seen)) (length (multiple-value-list (,operator 1 2 3))))))))
        (dolist (bad '(nil t #\a "a" (1) #c(1 2)))
          (dolist (form `((,operator ',bad) (,operator 0 ',bad) (,operator ',bad 0)))
            (is (eq t (m:evaluate vm `(handler-case ,form (type-error () t)))))))
        (is (eq t (m:evaluate vm `(handler-case (,operator) (program-error () t)))))))))

(test numeric-boundaries
  (m:with-interpreter (vm :fuel 50000000)
    (dolist (form `((= 16777217 16777216.0) (> 16777217 16777216.0)
                    (= 1/10 0.1) (= 1/2 0.5)
                    (< 1/10 0.1) (> 1/10 0.1d0)
                    (float ,(+ (expt 2 80) (expt 2 56) 1))
                    (float ,(+ (expt 2 80) (expt 2 56) -1))
                    (float ,(+ (expt 2 100) (expt 2 47) 1) 1d0)
                    (float ,(/ (+ (expt 2 1100) 1) (- (expt 2 1100) 1)) 1d0)
                    (float ,(/ 1 (expt 2 149)))
                    (float ,(/ 1 (expt 2 1074)) 1d0)
                    (typep ,(1- (expt 2 63)) 'fixnum)
                    (typep ,(expt 2 63) 'bignum)
                    (typep ,(- (expt 2 63)) 'fixnum)
                    (typep ,(- -1 (expt 2 63)) 'bignum)
                    (typep 255 '(unsigned-byte 8)) (typep 256 '(unsigned-byte 8))
                    (typep -128 '(signed-byte 8)) (typep 128 '(signed-byte 8))
                    (typep 1/3 '(rational (0) 1))
                    (typep #c(1d0 2d0) '(complex double-float))))
      ;; FIXNUM/BIGNUM ranges belong to the guest, not to the host.
      (let ((expected
              (cond ((and (eq (car form) 'typep) (member (second (third form)) '(fixnum bignum))) t)
                    ;; Use exact IEEE results: some hosts double-round large integer coercions.
                    ((equal form `(float ,(+ (expt 2 80) (expt 2 56) 1))) (scale-float 8388609.0 57))
                    ((equal form `(float ,(+ (expt 2 80) (expt 2 56) -1))) (scale-float 1.0 80))
                    ((equal form `(float ,(+ (expt 2 100) (expt 2 47) 1) 1d0))
                     (scale-float 4503599627370497d0 48))
                    (t (eval form)))))
        (is (eql expected (m:evaluate vm form)) "Mismatch for ~S" form)))))

(test interpreter-errors-and-recovery
  (m:with-interpreter (vm)
    (dolist (form '((/ 1 0) (+ 'x 1) (unknown-function 1) unbound-variable
                    (if t) (the single-float 1) (typep 1 'unknown-type)))
      (signals m:interpreter-error (m:evaluate vm form))
      (is (= 3 (m:evaluate vm '(+ 1 2)))))
    (is (eq :hello (m:evaluate vm :hello)))
    (is (equal '(a . b) (m:evaluate vm '(quote (a . b)))))
    (is (equal "hello" (m:evaluate vm "hello"))))
  (m:with-interpreter (vm :fuel 100000)
    (m:evaluate vm '(defun spin () (spin)))
    (signals m:interpreter-error (m:evaluate vm '(spin)))
    (is (= 3 (m:evaluate vm '(+ 1 2))))))

(test environments
  (m:with-interpreter (vm)
    (is (= 1 (m:evaluate vm '(let ((x 1)) (let ((x 2) (y x)) y)))))
    (is (= 2 (m:evaluate vm '(let ((x 1)) (let* ((x 2) (y x)) y)))))
    (is (= 1/2 (m:evaluate vm '(let ((x 1)) (locally (declare (integer x)) (setq x 2)) (setq x 1/2)))))
    (m:evaluate vm '(setq value 41))
    (is (= 42 (m:evaluate vm '(1+ value))))
    (m:evaluate vm '(defun sum (a b) (+ a b)))
    (is (= 3 (m:evaluate vm '(sum 1 2))))
    (m:evaluate vm '(defun sum (a b) (- a b)))
    (is (= -1 (m:evaluate vm '(sum 1 2))))
    (is (= 3 (m:evaluate vm '(let ((sum 100)) (sum 4 1)))))))

(test terminal-repl
  (let ((output (make-string-output-stream)))
    (m:repl :input (make-string-input-stream "(defun square (x) (* x x)) (square 12) (/ 1 0) (+ 1 2) :quit")
            :output output)
    (let ((text (get-output-stream-string output)))
      (is (search "144" text)) (is (search "Division by zero" text)) (is (search "3" text)))))

(test memory-global-table-bindings
  (wt:with-engine (engine)
    (wt:with-store ((store context) engine)
      (wt:with-module (module engine #@(module
        (memory (export "memory") 1 3)
        (global (export "counter") (mut i64) (i64.const 12))
        (table (export "table") 1 3 funcref)))
        (wt:with-instance (instance context module)
          (wt:with-export (export context instance "memory" 3)
            (let ((memory (wt:extern-payload export 'wt::memory)))
              (is (= 65536 (wt:wasmtime-memory-data-size context memory)))
              (with-foreign-object (previous :uint64)
                (wt:check-error (wt::wasmtime-memory-grow context memory 1 previous))
                (is (= 1 (mem-ref previous :uint64)))
                (is (= 131072 (wt:wasmtime-memory-data-size context memory)))
                (signals wt:wasmtime-error (wt:check-error (wt::wasmtime-memory-grow context memory 2 previous))))))
          (wt:with-export (export context instance "counter" 1)
            (let ((global (wt:extern-payload export 'wt::global)))
              (with-foreign-object (value '(:struct wt::wasmtime-val))
                (wt::wasmtime-global-get context global value)
                (is (= 12 (wt:read-value value)))
                (wt:write-value value 1 (- (expt 2 60)))
                (wt:check-error (wt::wasmtime-global-set context global value))
                (wt::wasmtime-global-get context global value)
                (is (= (- (expt 2 60)) (wt:read-value value))))))
          (wt:with-export (export context instance "table" 2)
            (let ((table (wt:extern-payload export 'wt::table)))
              (with-foreign-object (value '(:struct wt::wasmtime-val))
                (is (wt::wasmtime-table-get context table 0 value))
                (is (= 5 (foreign-slot-value value '(:struct wt::wasmtime-val) 'wt::kind)))
                (wt::wasmtime-val-unroot value)
                (is (not (wt::wasmtime-table-get context table 1 value)))))))))))

(test gc-reference-bindings
  (wt:with-engine (engine)
    (wt:with-store ((store context) engine)
      (with-foreign-objects ((ref '(:struct wt::wasmtime-ref))
                            (clone '(:struct wt::wasmtime-ref)) (result :int32) (data :int32))
        (wt::wasmtime-anyref-from-i31 context #x7fffffff ref)
        (unwind-protect
             (progn
               (wt::wasmtime-anyref-clone ref clone)
               (unwind-protect
                    (progn
                      (wt:check-error (wt::wasmtime-context-gc context))
                      (is (wt::wasmtime-anyref-i31-get-s context clone result))
                      (is (= -1 (mem-ref result :int32))))
                 (wt::wasmtime-anyref-unroot clone)))
          (wt::wasmtime-anyref-unroot ref))
        (setf (mem-ref data :int32) 42)
        (is (wt::wasmtime-externref-new context data (null-pointer) ref))
        (unwind-protect
             (progn (wt::wasmtime-externref-clone ref clone)
                    (unwind-protect
                         (progn (wt:check-error (wt::wasmtime-context-gc context))
                                (is (= 42 (mem-ref (wt::wasmtime-externref-data context clone) :int32))))
                      (wt::wasmtime-externref-unroot clone)))
          (wt::wasmtime-externref-unroot ref))))))

(test wat-template-boundaries
  (let ((value 1.25) (name "name\"\\"))
    (is (search "1.25e0" #@(f32.const ,value)))
    (is (equal (m:wat-string nil) "()"))
    (is (equal #@(export ,name) "(export \"name\\\"\\\\\")"))
    (is (equal #@(module ,(m:wat-fragment #@(memory 1))) "(module (memory 1))")))
  (let ((*readtable* (named-readtables:find-readtable :miricl-wat)))
    (signals end-of-file (read-from-string "#@(module"))
    (signals end-of-file (read-from-string "#@(module (; missing end"))
    (signals error (read-from-string "#@,@anything"))))

(test interpreter-lifetime-and-transport
  (m:with-interpreter (first)
    (m:with-interpreter (second)
      (m:evaluate first '(defun isolated () 42))
      (signals m:interpreter-error (m:evaluate second '(isolated)))
      (is (= 42 (m:evaluate first '(isolated))))))
  (m:with-interpreter (vm :fuel 50000000)
    (let ((text (make-string 20000 :initial-element #\a)))
      (is (equal (list text text) (m:evaluate vm `(let ((s ,text)) (list s s)))))))
  (m:with-interpreter (vm :fuel 0)
    (signals m:interpreter-error (m:evaluate vm 1)))
  (let ((vm (m:make-interpreter)))
    (m:close-interpreter vm) (m:close-interpreter vm)
    (signals error (m:evaluate vm 1))))

(test iterative-list-transport
  (m:with-interpreter (vm)
    (let ((expected (loop for i below 20000 collect i)))
      (is (equal expected (m:evaluate vm '(loop for i below 20000 collect i))))
      (is (equal expected (m:evaluate vm `(quote ,expected)))))
    (let* ((tail (list 1/3 #c(2 3) 1.5d0 #\λ "abc" nil))
           (shared (list tail tail))
           (dotted (append (make-list 10000 :initial-element tail) "end")))
      (is (equal shared (m:evaluate vm `(quote ,shared))))
      (is (equal dotted (m:evaluate vm `(quote ,dotted)))))
    (let ((cycle (list 1 2)))
      (setf (cdr (last cycle)) cycle)
      (signals error (m:evaluate vm `(quote ,cycle))))
    (let ((cycle (list nil)))
      (setf (car cycle) cycle)
      (signals error (m:evaluate vm `(quote ,cycle))))
    (is (= 42 (m:evaluate vm '(+ 20 22))))))

(test interpreter-syntax-boundaries
  (m:with-interpreter (vm)
    (dolist (form '((let () "literal") (and) (or) (and 1 2) (or nil 3)
                    (and nil (/ 1 0)) (or 1 (/ 1 0))
                    (typep 1/3 'ratio) (typep #c(1 2) '(complex *)) (rational 0.1)))
      (is (equal (eval form) (m:evaluate vm form))))
    (m:evaluate vm '(defun documented (x) "Documentation." (declare (integer x)) (+ x 1)))
    (is (= 42 (m:evaluate vm '(documented 41))))
    (dolist (form '((defun invalid (&optional &optional x) x) (defun duplicate (x x) x)
                    (defun keyword (:x) :x) (let ((:x 1)) :x)))
      (signals m:interpreter-error (m:evaluate vm form)))))
