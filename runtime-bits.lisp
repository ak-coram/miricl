;;;; Two's-complement integer operations over signed-magnitude radix-2^15 digits.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defparameter *variadic-integer-bit-operations*
  '((logand 8) (logior 14) (logxor 6) (logeqv 9)))

(defun $require-integer-cl ()
  (runtime-function-definition '$require-integer-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$require-integer-cl 'miricl-runtime:checked-integer-value))))

(define-runtime-function $require-integer (($x :nullable-object)) (:object)
  ;; Before function installation, bootstrap constants use the native type boundary.
  #@(if (global.get $cl-kernel-functions-ready)
    (then (return (ref.as_non_null (call $require-integer-cl (local.get $x))))))
  #@(if (i32.ne (call $tag (local.get $x)) (i32.const 1))
    (then (call $list-type-error (local.get $x) (i32.const ,(runtime-symbol-id 'integer)))))
  #@(ref.as_non_null (local.get $x)))

(define-runtime-function $ash-integer-digits (($value :object) ($amount :object)) (:nullable-object)
  (:locals ($digits :digits) ($result :object)
           ($sign i32) ($right i32) ($bits i32) ($offset i32) ($shift i32)
           ($length i32) ($n i32) ($i i32) ($v i32) ($carry i32) ($discarded i32))
  #@(local.set $sign (call $value-n (local.get $value)))
  #@(if (i32.or (i32.eqz (local.get $sign)) (i32.eqz (call $value-n (local.get $amount))))
    (then (return (local.get $value))))
  #@(local.set $right (i32.lt_s (call $value-n (local.get $amount)) (i32.const 0)))
  #@(if (i32.gt_u (call $bitlength (local.get $amount)) (i32.const 30))
    (then
      (if (local.get $right) (then (return (call $int (select (i32.const -1) (i32.const 0) (i32.lt_s (local.get $sign) (i32.const 0)))))))
      ,(wat-fail :allocation)))
  #@(local.set $bits (i32.or (call $digit (local.get $amount) (i32.const 0))
    (i32.shl (call $digit (local.get $amount) (i32.const 1)) (i32.const 15))))
  #@(local.set $length (call $integer-length (ref.as_non_null (local.get $value))))
  #@(local.set $offset (i32.div_u (local.get $bits) (i32.const 15)))
  #@(local.set $shift (i32.rem_u (local.get $bits) (i32.const 15)))
  #@(if (local.get $right)
    (then
      (if (i32.ge_u (local.get $bits) (call $bitlength (local.get $value)))
        (then (return (call $int (select (i32.const -1) (i32.const 0) (i32.lt_s (local.get $sign) (i32.const 0)))))))
      (local.set $n (i32.sub (local.get $length) (local.get $offset)))
      (local.set $digits (array.new_default $digits (local.get $n)))
      (block $copied (loop $copy (br_if $copied (i32.eq (local.get $i) (local.get $n)))
        (array.set $digits (local.get $digits) (local.get $i)
          (i32.and (i32.or
            (i32.shr_u (call $digit (local.get $value) (i32.add (local.get $i) (local.get $offset))) (local.get $shift))
            (i32.shl (call $digit (local.get $value) (i32.add (i32.add (local.get $i) (local.get $offset)) (i32.const 1)))
              (i32.sub (i32.const 15) (local.get $shift)))) (i32.const 32767)))
        (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $copy)))
      (local.set $result (call $integer (local.get $sign) (local.get $digits)))
      (if (i32.lt_s (local.get $sign) (i32.const 0))
        (then
          (local.set $i (i32.const 0))
          (block $scanned (loop $scan (br_if $scanned (i32.eq (local.get $i) (local.get $offset)))
            (local.set $discarded (i32.or (local.get $discarded) (call $digit (local.get $value) (local.get $i))))
            (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $scan)))
          (local.set $discarded (i32.or (local.get $discarded)
            (i32.and (call $digit (local.get $value) (local.get $offset))
              (i32.sub (i32.shl (i32.const 1) (local.get $shift)) (i32.const 1)))))
          (if (local.get $discarded) (then (local.set $result (call $iadd (local.get $result) (call $int (i32.const -1))))))))
      (return (local.get $result))))
  ;; Use a widened sum before allocating, including when the input alone
  ;; exceeds the result limit. Huge negative counts above need no buffer.
  (wat-fail-when #@(i64.gt_u (i64.add (i64.extend_i32_u (local.get $bits))
    (i64.extend_i32_u (call $bitlength (local.get $value)))) (i64.const 536870910)) :allocation)
  #@(local.set $n (i32.add (i32.add (local.get $length) (local.get $offset)) (i32.ne (local.get $shift) (i32.const 0))))
  #@(local.set $digits (array.new_default $digits (local.get $n)))
  #@(block $done (loop $next (br_if $done (i32.eq (local.get $i) (local.get $length)))
    (local.set $v (i32.or (i32.shl (call $digit (local.get $value) (local.get $i)) (local.get $shift)) (local.get $carry)))
    (array.set $digits (local.get $digits) (i32.add (local.get $i) (local.get $offset)) (i32.and (local.get $v) (i32.const 32767)))
    (local.set $carry (i32.shr_u (local.get $v) (i32.const 15)))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(if (local.get $shift) (then (array.set $digits (local.get $digits) (i32.sub (local.get $n) (i32.const 1)) (local.get $carry))))
  #@(call $integer (local.get $sign) (local.get $digits)))

(define-runtime-function $validate-bit-integer-layout (($operand (ref null any))) ()
  (:locals ($object (ref $obj)) ($digits (ref null $digits)) ($length i32)
           ($tag i32) ($sign i32) ($scalar i64))
  ;; A decoder miss can include a private GC value or a foreign host reference.
  (wat-fail-when #@(i32.eqz (i32.or
    (ref.test (ref null i31) (local.get $operand))
    (ref.test (ref $obj) (local.get $operand)))) :malformed-form)
  #@(if (ref.test (ref $obj) (local.get $operand))
    (then
      (local.set $object (ref.cast (ref $obj) (local.get $operand)))
      (local.set $tag (i32.and (struct.get $obj $tag (local.get $object)) (i32.const 63)))
      (local.set $sign (struct.get $obj $n (local.get $object)))
      (if (ref.test (ref $fixnum) (local.get $object))
        (then
          (local.set $scalar (struct.get $fixnum $value (ref.cast (ref $fixnum) (local.get $object))))
          ,(wat-fail-when #@(i32.or (i32.ne (local.get $tag) (i32.const 1))
            (i32.ne (local.get $sign) (i32.sub (i64.gt_s (local.get $scalar) (i64.const 0))
              (i64.lt_s (local.get $scalar) (i64.const 0))))) :malformed-form))
        (else
          ;; The digit worker assumes a nonempty, normalized magnitude.
          ;; A Lisp integer tag alone does not establish that storage.
          (if (i32.eq (local.get $tag) (i32.const 1))
            (then
              ,(wat-fail-when #@(i32.and (i32.ne (local.get $sign) (i32.const -1))
                (i32.ne (local.get $sign) (i32.const 1))) :malformed-form)
              (local.set $digits (struct.get $obj $digits (local.get $object)))
              ,(wat-fail-when #@(ref.is_null (local.get $digits)) :malformed-form)
              (local.set $length (array.len (local.get $digits)))
              ,(wat-fail-when #@(i32.or (i32.eqz (local.get $length))
                (i32.gt_u (local.get $length) (i32.const ,(floor #xffffffff 15)))) :malformed-form)
              ,(wat-fail-when #@(i32.ge_u
                (i32.sub (array.get $digits (local.get $digits)
                  (i32.sub (local.get $length) (i32.const 1))) (i32.const 1))
                (i32.const 32767)) :malformed-form))))))))

(define-runtime-function $require-ash-integer (($operand (ref null any))) (:object)
  #@(call $validate-bit-integer-layout (local.get $operand))
  #@(return_call $require-integer (ref.cast (ref null eq) (local.get $operand))))

(defun wat-checked-scalar-shift (value shift result overflow)
  ;; Both inputs are checked scalar integers. The caller handles identity.
  ;; Clamp before negating MIN-I64, and reverse left shifts with sign extension
  ;; so -1 shifted by 63 is accepted while overflowing shifts miss.
  (wat-fragment
    #@(if (i64.lt_s (local.get ,shift) (i64.const 0))
      (then (local.set ,result (i64.shr_s (local.get ,value)
        (if (result i64) (i64.le_s (local.get ,shift) (i64.const -63))
          (then (i64.const 63))
          (else (i64.sub (i64.const 0) (local.get ,shift)))))))
      (else
        (br_if ,overflow (i64.ge_u (local.get ,shift) (i64.const 64)))
        (local.set ,result (i64.shl (local.get ,value) (local.get ,shift)))
        (br_if ,overflow (i64.ne (local.get ,value)
          (i64.shr_s (local.get ,result) (local.get ,shift))))))))

(define-runtime-function $ash-native (($x (ref null any)) ($count (ref null any))) (:nullable-object)
  (:locals ($value :nullable-object) ($amount :nullable-object)
           ($scalar i64) ($shift i64) ($shifted i64))
  #@(block $miss
    (block $foreign (result (ref null any))
      (local.set $value (br_on_cast_fail $foreign (ref null any) (ref null eq) (local.get $x)))
      ,(wat-decode-scalar-integer '$value '$scalar '$miss)
      (local.set $amount (br_on_cast_fail $foreign (ref null any) (ref null eq) (local.get $count)))
      ,(wat-decode-scalar-integer '$amount '$shift '$miss)
      (if (i32.or (i64.eqz (local.get $scalar)) (i64.eqz (local.get $shift)))
        (then (return (local.get $value))))
      (block $overflow
        ,(wat-checked-scalar-shift '$scalar '$shift '$shifted '$overflow)
        (return_call $int64 (local.get $shifted)))
      ;; The digit path reads the stored integer signs after scalar overflow.
      (call $validate-bit-integer-layout (local.get $value))
      (call $validate-bit-integer-layout (local.get $amount))
      (return_call $ash-integer-digits
        (ref.as_non_null (local.get $value)) (ref.as_non_null (local.get $amount))))
    (drop))
  ;; Validate the physical shape and Lisp type of X before COUNT.
  #@(return_call $ash-integer-digits
    (call $require-ash-integer (local.get $x)) (call $require-ash-integer (local.get $count))))

(define-runtime-function $integer-bit-measure (($x :nullable-object) ($lengthp i32)) (:nullable-object)
  (:locals ($value :object) ($i i32) ($v i32) ($digit i32) ($borrow i32) ($result i32) ($scalar i64))
  #@(local.set $value (call $require-integer (local.get $x)))
  #@(if (call $scalar-integer-p (local.get $value))
    (then
      (local.set $scalar (call $scalar-integer-value (local.get $value)))
      (if (i64.lt_s (local.get $scalar) (i64.const 0))
        (then (local.set $scalar (i64.xor (local.get $scalar) (i64.const -1)))))
      (return (call $int (i32.wrap_i64 (if (result i64) (local.get $lengthp)
        (then (i64.sub (i64.const 64) (i64.clz (local.get $scalar))))
        (else (i64.popcnt (local.get $scalar)))))))))
  ;; For negative X, measure its complement: ABS(X)-1, without an allocation.
  #@(local.set $borrow (i32.lt_s (call $value-n (local.get $value)) (i32.const 0)))
  #@(block $done (loop $next
    (br_if $done (i32.eq (local.get $i) (call $integer-length (ref.as_non_null (local.get $value)))))
    (local.set $digit (call $digit (local.get $value) (local.get $i)))
    (local.set $v (i32.and (i32.sub (local.get $digit) (local.get $borrow)) (i32.const 32767)))
    (local.set $borrow (i32.and (local.get $borrow) (i32.eqz (local.get $digit))))
    (if (local.get $lengthp)
      (then (if (local.get $v) (then (local.set $result (i32.add (i32.mul (local.get $i) (i32.const 15))
        (i32.sub (i32.const 32) (i32.clz (local.get $v))))))))
      (else (local.set $result (i32.add (local.get $result) (i32.popcnt (local.get $v))))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(call $int (local.get $result)))

(defun $logbitp-native-cl ()
  (runtime-function-definition '$logbitp-native-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$logbitp-native-cl 'miricl-runtime:integer-bit-present-p))))

(define-runtime-function $logbitp-native (($index :nullable-object) ($x :nullable-object)) (:nullable-object)
  #@(local.set $x (call $require-integer (local.get $x)))
  #@(drop (call $natural-count (local.get $index)))
  #@(call $logbitp-native-cl (local.get $index) (local.get $x)))

(define-runtime-function $boole-word (($op i32) ($a i32) ($b i32)) (i32)
  ;; Dispatch once to the native operation instead of expanding the truth table
  ;; into four masked terms for every integer limb or packed bit-array word.
  (runtime-wat-forms
    (let* ((labels (loop for i below 16 collect (intern (format nil "$BOOLE~D" i) :miricl)))
           (a '(local.get $a)) (b '(local.get $b))
           (not-a `(i32.xor ,a (i32.const -1))) (not-b `(i32.xor ,b (i32.const -1)))
           (operations `((i32.const 0) (i32.xor (i32.or ,a ,b) (i32.const -1))
                         (i32.and ,not-a ,b) ,not-a (i32.and ,a ,not-b) ,not-b
                         (i32.xor ,a ,b) (i32.xor (i32.and ,a ,b) (i32.const -1))
                         (i32.and ,a ,b) (i32.xor (i32.xor ,a ,b) (i32.const -1)) ,b
                         (i32.or ,not-a ,b) ,a (i32.or ,a ,not-b) (i32.or ,a ,b) (i32.const -1)))
           (forms `((br_table ,@labels ,(first labels) (local.get $op)))))
      (loop for label in (reverse labels) for operation in (reverse operations)
            do (setf forms `((block ,label ,@forms) (return ,operation))))
      forms)))

(define-runtime-function $boole64-word (($op i32) ($a i64) ($b i64)) (i64)
  ;; Dispatch once to the native operation instead of expanding the truth table
  ;; into four masked terms for every integer limb or packed bit-array word.
  (runtime-wat-forms
    (let* ((labels (loop for i below 16 collect (intern (format nil "$BOOLE~D" i) :miricl)))
           (a '(local.get $a)) (b '(local.get $b))
           (not-a `(i64.xor ,a (i64.const -1))) (not-b `(i64.xor ,b (i64.const -1)))
           (operations `((i64.const 0) (i64.xor (i64.or ,a ,b) (i64.const -1))
                         (i64.and ,not-a ,b) ,not-a (i64.and ,a ,not-b) ,not-b
                         (i64.xor ,a ,b) (i64.xor (i64.and ,a ,b) (i64.const -1))
                         (i64.and ,a ,b) (i64.xor (i64.xor ,a ,b) (i64.const -1)) ,b
                         (i64.or ,not-a ,b) ,a (i64.or ,a ,not-b) (i64.or ,a ,b) (i64.const -1)))
           (forms `((br_table ,@labels ,(first labels) (local.get $op)))))
      (loop for label in (reverse labels) for operation in (reverse operations)
            do (setf forms `((block ,label ,@forms) (return ,operation))))
      forms)))

(define-runtime-function $boole-native
    (($op i32) ($x :nullable-object) ($y :nullable-object) ($predicate i32)) (:nullable-object)
  (:locals ($a :object) ($b :object) ($digits (ref null $digits))
           ($na i32) ($nb i32) ($negative-a i32) ($negative-b i32) ($carry-a i32) ($carry-b i32)
           ($n i32) ($i i32) ($av i32) ($bv i32) ($v i32) ($negative i32) ($carry i32) ($scalar i64))
  #@(local.set $a (call $require-integer (local.get $x)))
  #@(local.set $b (call $require-integer (local.get $y)))
  #@(if (i32.and (call $scalar-integer-p (local.get $a)) (call $scalar-integer-p (local.get $b)))
    (then
      (local.set $scalar (call $boole64-word (local.get $op)
        (call $scalar-integer-value (local.get $a)) (call $scalar-integer-value (local.get $b))))
      (if (local.get $predicate) (then (return (call $bool (i64.ne (local.get $scalar) (i64.const 0))))))
      (return (call $int64 (local.get $scalar)))))
  #@(local.set $na (call $integer-length (ref.as_non_null (local.get $a))))
  #@(local.set $nb (call $integer-length (ref.as_non_null (local.get $b))))
  #@(if (i32.and (i32.le_u (local.get $na) (i32.const 2)) (i32.le_u (local.get $nb) (i32.const 2)))
    (then
      (local.set $av (call $small-integer-value (local.get $a)))
      (local.set $bv (call $small-integer-value (local.get $b)))
      (local.set $v (call $boole-word (local.get $op) (local.get $av) (local.get $bv)))
      (if (local.get $predicate) (then (return (call $bool (i32.ne (local.get $v) (i32.const 0))))))
      (return (call $int (local.get $v)))))
  #@(local.set $n (i32.add (select (local.get $na) (local.get $nb) (i32.gt_u (local.get $na) (local.get $nb))) (i32.const 1)))
  #@(if (i32.eqz (local.get $predicate)) (then (local.set $digits (array.new_default $digits (local.get $n)))))
  #@(local.set $negative-a (i32.lt_s (call $value-n (local.get $a)) (i32.const 0)))
  #@(local.set $negative-b (i32.lt_s (call $value-n (local.get $b)) (i32.const 0)))
  ;; LOGTEST only needs overlapping limbs of nonnegative integers. The
  ;; remaining limbs AND with zero; no result or sign limb is constructed.
  #@(if (i32.and (local.get $predicate)
        (i32.eqz (i32.or (local.get $negative-a) (local.get $negative-b))))
    (then (local.set $n (select (local.get $na) (local.get $nb)
      (i32.lt_u (local.get $na) (local.get $nb))))))
  #@(local.set $carry-a (local.get $negative-a))
  #@(local.set $carry-b (local.get $negative-b))
  #@(block $done (loop $next (br_if $done (i32.eq (local.get $i) (local.get $n)))
    (local.set $av (call $digit (local.get $a) (local.get $i)))
    (local.set $bv (call $digit (local.get $b) (local.get $i)))
    (if (local.get $negative-a)
      (then (local.set $av (i32.add (i32.xor (local.get $av) (i32.const 32767)) (local.get $carry-a)))
        (local.set $carry-a (i32.shr_u (local.get $av) (i32.const 15)))))
    (if (local.get $negative-b)
      (then (local.set $bv (i32.add (i32.xor (local.get $bv) (i32.const 32767)) (local.get $carry-b)))
        (local.set $carry-b (i32.shr_u (local.get $bv) (i32.const 15)))))
    (local.set $v (i32.and (call $boole-word (local.get $op) (local.get $av) (local.get $bv)) (i32.const 32767)))
    (if (local.get $predicate)
      (then (if (local.get $v) (then (return (call $symbol (i32.const 1))))))
      (else (array.set $digits (local.get $digits) (local.get $i) (local.get $v))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(if (local.get $predicate) (then (return (ref.null eq))))
  #@(local.set $negative (i32.ne (local.get $v) (i32.const 0)))
  #@(if (local.get $negative)
    (then
      (local.set $carry (i32.const 1)) (local.set $i (i32.const 0))
      (block $done (loop $next (br_if $done (i32.eq (local.get $i) (local.get $n)))
        (local.set $v (i32.add (i32.xor (array.get $digits (local.get $digits) (local.get $i)) (i32.const 32767)) (local.get $carry)))
        (array.set $digits (local.get $digits) (local.get $i) (i32.and (local.get $v) (i32.const 32767)))
        (local.set $carry (i32.shr_u (local.get $v) (i32.const 15)))
        (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))))
  #@(call $integer (select (i32.const -1) (i32.const 1) (local.get $negative)) (ref.as_non_null (local.get $digits))))

(define-runtime-function $fixed-scalar-integer-bits
    (($function :nullable-object) ($id i32) ($x :nullable-object) ($y :nullable-object)
     ($z :nullable-object) ($ternary i32)) (:nullable-object)
  (:locals ($op i32) ($left i64) ($right i64) ($third i64) ($word i64))
  ;; CALL2/3 already admitted the inputs and ran the application checkpoint.
  ;; The ID gate selects only the ordinary bit builtins and bounds the slot.
  #@(block $miss
    (br_if $miss (global.get $debug-evaluating))
    (br_if $miss (i32.eqz (ref.eq (local.get $function)
      (array.get $objects (global.get $canonical-builtin-functions) (i32.shl (local.get $id) (i32.const 1))))))
    (br_if $miss (i32.ne (struct.get $obj $n (ref.cast (ref $obj) (local.get $function)))
      (i32.add (local.get $id) (i32.const 1))))
    ,(wat-decode-scalar-integer '$x '$left '$miss)
    ,(wat-decode-scalar-integer '$y '$right '$miss)
    (if (local.get $ternary)
      (then ,(wat-decode-scalar-integer '$z '$third '$miss)))
    (local.set $op (i32.and (i32.const 15)
      (i32.shr_u (i32.const ,(loop with first = (runtime-symbol-id 'logand)
                                  for (name op) in *variadic-integer-bit-operations*
                                  for i from 0
                                  do (assert (= (runtime-symbol-id name) (+ first i)))
                                  sum (ash op (* i 4))))
        (i32.shl (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'logand))) (i32.const 2)))))
    (local.set $word (call $boole64-word (local.get $op) (local.get $left) (local.get $right)))
    ;; Sign extension is preserved by each bitwise operation. Keep the
    ;; intermediate result unboxed and apply LOGEQV's complement per pair.
    (if (local.get $ternary)
      (then (local.set $word (call $boole64-word (local.get $op) (local.get $word) (local.get $third)))))
    (call $clear-values)
    (return_call $int64 (local.get $word)))
  #@(ref.null eq))

(define-runtime-function $binary-byte-operation
    (($id i32) ($a :nullable-object) ($b :nullable-object)) (:nullable-object)
  (:locals ($result :nullable-object) ($position :nullable-object))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'byte)))
    (then
      (drop (call $natural-count (local.get $a)))
      (drop (call $natural-count (local.get $b)))
      (return (call $cons (local.get $a) (local.get $b)))))
  #@(local.set $result (call $ldb-native (local.get $b)
    (call $car (local.get $a)) (call $cdr (local.get $a))
    (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'ldb-test)))))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'mask-field)))
    (then
      ;; Extraction can enter Lisp. Read the live position again before shifting.
      (local.set $position (call $cdr (local.get $a)))
      ,(wat-fail-when #@(i32.eqz (i32.or (ref.test (ref null i31) (local.get $position))
        (ref.test (ref $obj) (local.get $position)))) :malformed-form)
      (return_call $ash-native (local.get $result) (local.get $position))))
  #@(local.get $result))

(define-runtime-function $integer-bits-apply (($id i32) ($args :nullable-object)) (:nullable-object)
  (:locals ($a :nullable-object) ($b :nullable-object) ($result :nullable-object) ($op i32))
  (wat-symbol-case #@(local.get $id)
    ((byte ldb ldb-test mask-field)
      (wat-arity 2)
      #@(return_call $binary-byte-operation (local.get $id)
        (call $car (local.get $args)) (call $car (call $cdr (local.get $args)))))
    ((byte-size byte-position)
      (wat-arity 1) #@(return (call $byte-component (call $car (local.get $args))
        (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'byte-position))))))
    ((dpb deposit-field)
      (wat-arity 3) #@(return (call $deposit-native (call $car (local.get $args))
        (call $car (call $cdr (local.get $args))) (call $car (call $cdr (call $cdr (local.get $args))))
        (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'deposit-field))))))
    ((integer-length logcount)
      (wat-arity 1) #@(return (call $integer-bit-measure (call $car (local.get $args))
        (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'integer-length))))))
    (lognot (wat-arity 1) #@(return (call $negative
      (call $iadd (call $require-integer (call $car (local.get $args))) (call $int (i32.const 1))))))
    (boole (wat-arity 3) #@(return (call $boole-native
      (call $bounded-index (call $car (local.get $args)) (i32.const 15))
      (call $car (call $cdr (local.get $args))) (call $car (call $cdr (call $cdr (local.get $args)))) (i32.const 0)))))
  #@(local.set $a (call $car (local.get $args)))
  #@(local.set $b (call $car (call $cdr (local.get $args))))
  (wat-symbol-case #@(local.get $id)
    (ash (wat-arity 2) #@(return (call $ash-native (local.get $a) (local.get $b))))
    (logbitp (wat-arity 2) #@(return (call $logbitp-native (local.get $a) (local.get $b))))
    (logtest (wat-arity 2) #@(return (call $boole-native (i32.const 8) (local.get $a) (local.get $b) (i32.const 1)))))
  (runtime-wat-forms
    (loop for (name op) in (append *variadic-integer-bit-operations*
                            '((lognand 7) (lognor 1) (logandc1 2) (logandc2 4) (logorc1 11) (logorc2 13))) collect
      `(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id name))) (then (local.set $op (i32.const ,op))))))
  #@(if (i32.gt_u (local.get $id) (i32.const ,(runtime-symbol-id 'logeqv)))
    (then ,(wat-arity 2) (return (call $boole-native (local.get $op) (local.get $a) (local.get $b) (i32.const 0)))))
  #@(if (ref.is_null (local.get $args))
    (then (return (call $int (select (i32.const -1) (i32.const 0)
      (i32.or (i32.eq (local.get $op) (i32.const 8)) (i32.eq (local.get $op) (i32.const 9))))))))
  #@(local.set $result (call $require-integer (local.get $a)))
  #@(local.set $args (call $cdr (local.get $args)))
  #@(block $done (loop $next (br_if $done (ref.is_null (local.get $args)))
    (local.set $result (call $boole-native (local.get $op) (local.get $result) (call $car (local.get $args)) (i32.const 0)))
    (local.set $args (call $cdr (local.get $args))) (br $next)))
  #@(local.get $result))

(define-runtime-section integer-bits-runtime-wat ()
  ($require-integer-cl) ($require-integer) ($ash-integer-digits)
  ($validate-bit-integer-layout) ($require-ash-integer) ($ash-native)
  ($integer-bit-measure) ($logbitp-native-cl) ($logbitp-native)
  ($boole-word) ($boole64-word) ($boole-native) ($binary-byte-operation) ($integer-bits-apply) ($fixed-scalar-integer-bits))
