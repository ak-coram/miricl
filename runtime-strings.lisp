;;;; Guest strings and string designators.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

;; Bootstrap the host Lisp's single-character case pairs into compact ranges.
;; Each record is START, END, DELTA, FLAGS: bit 0 selects alternating codes;
;; bit 1 marks a Unicode mapping without a reciprocal Common Lisp case pair.
;; Comparison needs no host callbacks or temporary uppercase strings.
(defun string-case-ranges (function)
  (let* ((opposite (if (eq function #'char-upcase) #'char-downcase #'char-upcase))
         (pairs (loop for code from 128 below (min char-code-limit #x110000)
                     for char = (code-char code)
                     when (and char (char/= char (funcall function char)))
                       collect (list code (char-code (funcall function char))
                         (if (char= char (funcall opposite (funcall function char))) 0 2)))))
    (loop while pairs
          for (start upper flags) = (pop pairs)
          for delta = (- upper start)
          for end = start
          for step = (if (and pairs (= (- (cadar pairs) (caar pairs)) delta)
                              (= (caddar pairs) flags)
                              (member (- (caar pairs) start) '(1 2)))
                         (- (caar pairs) start) 1)
          do (loop while (and pairs (= (caar pairs) (+ end step))
                              (= (- (cadar pairs) (caar pairs)) delta) (= (caddar pairs) flags))
                   do (setf end (car (pop pairs))))
          append (list start end delta (logior flags (1- step))))))

(defun ansi-character-upcase (character)
  (let ((mapped (char-upcase character)))
    (if (char= character (char-downcase mapped)) mapped character)))
(defun ansi-character-downcase (character)
  (let ((mapped (char-downcase character)))
    (if (char= character (char-upcase mapped)) mapped character)))

(defparameter *string-case-ranges* (string-case-ranges #'char-upcase))
(defparameter *string-downcase-ranges* (string-case-ranges #'char-downcase))

(defun string-case-table-values (ranges)
  "Validate generated case records, then encode every field as a code point."
  ;; These host-generated tables are the checked boundary for the private CL
  ;; search. The native view shares the encoded array, without per-query copies.
  (let ((count (list-length ranges)) (previous -1) (values nil))
    (unless count (error "Circular character case table; expected finite four-field records."))
    (unless (<= count #x1ffffffe)
      (error "Character case table exceeds the target's array-size limit: ~D fields." count))
    (unless (zerop (mod count 4))
      (error "Malformed character case table: ~D fields cannot form four-field records." count))
    (loop for (start end delta flags) on ranges by #'cddddr do
      (unless (and (integerp start) (integerp end) (integerp delta) (integerp flags)
                   (<= 128 start end #x10ffff) (> start previous) (<= 0 flags 3)
                   (<= 0 (+ start delta) (+ end delta) #x10ffff))
        (error "Malformed character case record: ~A"
               (let ((*print-circle* t)) (prin1-to-string (list start end delta flags)))))
      (let ((encoded (if (minusp delta) (1- (* -2 delta)) (* 2 delta))))
        (unless (<= 0 encoded #x10ffff)
          (error "Character case delta cannot fit the packed code-point table: ~S" delta))
        (push start values) (push end values) (push encoded values) (push flags values))
      (setf previous end))
    (nreverse values)))

(defun $character-case-code-cl ()
  (runtime-function-definition '$character-case-code-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$character-case-code-cl 'miricl-runtime:character-case-code-value))))

(define-runtime-function $initialize-string-case-tables () ()
  ;; Some reader paths use case conversion before character-property startup.
  ;; Construct both immutable string views together on their first use.
  #@(if (ref.is_null (global.get $string-case-table))
    (then (global.set $string-case-table (call $text (global.get $string-case-ranges)))
          (global.set $string-downcase-table (call $text (global.get $string-downcase-ranges))))))

(defun string-case-code-wat (ansi)
  (runtime-wat-sequence
   #@(call $initialize-string-case-tables)
   #@(i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null
       (call $character-case-code-cl (call $int (local.get $c)) (call $bool (local.get $down))
         (if (result (ref null eq)) (local.get $down)
           (then (global.get $string-downcase-table)) (else (global.get $string-case-table)))
         (call $bool (i32.const ,(if ansi 1 0)))))))))

(defmacro define-runtime-case-code (name ansi)
  `(define-runtime-function ,name (($c i32) ($down i32)) (i32)
     (string-case-code-wat ,ansi)))
(define-runtime-case-code $string-case-code t)
(define-runtime-case-code $unicode-case-code nil)

(define-runtime-function $string-upcase-code (($c i32)) (i32)
  ;; Preserve Unicode simple folding for case-insensitive comparisons.
  #@(call $unicode-case-code (local.get $c) (i32.const 0)))

(define-runtime-function $text
    (($characters :digits))
    (:object)
  (wat-new-object :string
                  :digits #@(local.get $characters)))

;; The primitive-only CL comparison also works before function installation.
(defun $bootstrap-string-equal ()
  (runtime-function-definition '$bootstrap-string-equal
    (wat-fragment (miricl-cross:runtime-kernel-wat '$bootstrap-string-equal 'miricl-runtime:bootstrap-text-equal-p :result :truth))))

(defun $string-equal-cl ()
  (runtime-function-definition '$string-equal-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$string-equal-cl 'miricl-runtime:text-equal-p :result :truth))))

(define-runtime-function $string-equal (($a :nullable-object) ($b :nullable-object)) (i32)
  #@(if (i32.eqz (global.get $cl-kernel-functions-ready))
    (then (return (call $bootstrap-string-equal (local.get $a) (local.get $b)))))
  #@(if (i32.ne (call $tag (local.get $a)) (i32.const 10))
    (then (call $list-type-error (local.get $a) (i32.const ,(runtime-symbol-id 'string)))))
  #@(if (i32.ne (call $tag (local.get $b)) (i32.const 10))
    (then (call $list-type-error (local.get $b) (i32.const ,(runtime-symbol-id 'string)))))
  #@(call $string-equal-cl (local.get $a) (local.get $b)))

(defun $string-fold-equal-cl ()
  (runtime-function-definition '$string-fold-equal-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$string-fold-equal-cl 'miricl-runtime:text-fold-equal-p :result :truth))))

(define-runtime-function $string-fold-equal (($a :nullable-object) ($b :nullable-object)) (i32)
  #@(if (i32.ne (call $tag (local.get $a)) (i32.const 10))
    (then (call $list-type-error (local.get $a) (i32.const ,(runtime-symbol-id 'string)))))
  #@(if (i32.ne (call $tag (local.get $b)) (i32.const 10))
    (then (call $list-type-error (local.get $b) (i32.const ,(runtime-symbol-id 'string)))))
  #@(call $string-fold-equal-cl (local.get $a) (local.get $b)))

(defun $string-designator-cl ()
  (runtime-function-definition '$string-designator-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$string-designator-cl 'miricl-runtime:text-designator))))

(define-runtime-function $string-designator (($x :nullable-object)) (:nullable-object)
  ;; Package creation precedes installation of ordinary Lisp function bindings.
  #@(if (call $stringp-native (local.get $x)) (then (return (local.get $x))))
  #@(call $string-designator-cl (local.get $x)))

(define-runtime-function $string-element
    (($string :nullable-object) ($index :nullable-object) ($value :nullable-object) ($mutate i32)) (:nullable-object)
  (:locals ($n i32) ($length i32))
  #@(if (i32.ne (call $tag (local.get $string)) (i32.const 10))
    (then (call $type-fail (local.get $string) (call $symbol (i32.const ,(runtime-symbol-id 'string)))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(local.set $length (array.len (struct.get $obj $digits (ref.cast (ref $obj) (local.get $string)))))
  #@(local.set $n (call $natural-count (local.get $index)))
  #@(if (i32.or (i32.ge_u (local.get $n) (local.get $length))
                 (i32.gt_u (call $bitlength (ref.as_non_null (local.get $index))) (i32.const 31)))
    (then (call $type-fail (local.get $index)
      (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'integer)))
        (call $cons (call $int (i32.const 0))
          (call $cons (call $cons (call $int (local.get $length)) (ref.null eq)) (ref.null eq))))
      (i32.const ,(runtime-error-code :expected-designator)))))
  #@(if (local.get $mutate)
    (then
      (if (i32.ne (call $tag (local.get $value)) (i32.const 16))
        (then (call $type-fail (local.get $value) (call $symbol (i32.const ,(runtime-symbol-id 'character)))
          (i32.const ,(runtime-error-code :expected-designator)))))
      (array.set $digits (struct.get $obj $digits (ref.cast (ref $obj) (local.get $string))) (local.get $n)
        (call $value-n (local.get $value)))
      (return (local.get $value))))
  #@(call $node (i32.const 16)
    (array.get $digits (struct.get $obj $digits (ref.cast (ref $obj) (local.get $string))) (local.get $n))
    (ref.null eq) (ref.null eq) (ref.null eq)))

(define-runtime-function $string-comparison (($args :nullable-object) ($op i32)) (:nullable-object)
  (:locals ($a :nullable-object) ($b :nullable-object) ($scan :nullable-object)
           ($key :nullable-object) ($value :nullable-object)
           ($start1 :nullable-object) ($end1 :nullable-object)
           ($start2 :nullable-object) ($end2 :nullable-object)
           ($seen i32) ($bit i32) ($key-index i32) ($unknown i32) ($allow i32)
           ($i i32) ($j i32) ($stop1 i32) ($stop2 i32) ($length1 i32) ($length2 i32)
           ($c1 i32) ($c2 i32) ($fold i32) ($cmp i32) ($match i32)
           ($saved-extra :nullable-object) ($saved-count i32) ($result :nullable-object))
  ;; Bind keywords directly: no rest closure, environment, or per-character
  ;; Lisp calls. SEEN preserves the leftmost occurrence, including NIL bounds
  ;; and :ALLOW-OTHER-KEYS. Validate the whole plist before comparing strings.
  (wat-fail-when #@(ref.is_null (local.get $args)) :wrong-arity)
  #@(local.set $a (call $car (local.get $args)))
  #@(local.set $scan (call $cdr (local.get $args)))
  (wat-fail-when #@(ref.is_null (local.get $scan)) :wrong-arity)
  #@(local.set $b (call $car (local.get $scan)))
  #@(local.set $scan (call $cdr (local.get $scan)))
  #@(block $bound (loop $keys (br_if $bound (ref.is_null (local.get $scan)))
    (local.set $key (call $car (local.get $scan)))
    (local.set $scan (call $cdr (local.get $scan)))
    ,(wat-fail-when #@(ref.is_null (local.get $scan)) :wrong-arity)
    (local.set $value (call $car (local.get $scan)))
    (local.set $scan (call $cdr (local.get $scan)))
    (local.set $key-index (i32.sub (call $id (local.get $key))
      (i32.const ,(runtime-symbol-id :start1))))
    (if (i32.le_u (local.get $key-index) (i32.const 3))
      (then
        (local.set $bit (i32.shl (i32.const 1) (local.get $key-index)))
        (if (i32.eqz (i32.and (local.get $seen) (local.get $bit)))
          (then
            ,(runtime-wat-forms
              (loop for variable in '($start1 $end1 $start2 $end2) for index from 0 collect
                `(if (i32.eq (local.get $key-index) (i32.const ,index))
                  (then (local.set ,variable (local.get $value))))))
            (local.set $seen (i32.or (local.get $seen) (local.get $bit))))))
      (else
        (if (i32.eq (call $id (local.get $key)) (i32.const ,(runtime-symbol-id :allow-other-keys)))
          (then (if (i32.eqz (i32.and (local.get $seen) (i32.const 16)))
            (then (local.set $allow (i32.eqz (ref.is_null (local.get $value))))
              (local.set $seen (i32.or (local.get $seen) (i32.const 16))))))
          (else (local.set $unknown (i32.const 1))))))
    (br $keys)))
  (wat-fail-when #@(i32.and (local.get $unknown) (i32.eqz (local.get $allow))) :wrong-arity)
  #@(local.set $a (call $string-contents (call $string-designator (local.get $a))))
  #@(local.set $b (call $string-contents (call $string-designator (local.get $b))))
  #@(local.set $length1 (array.len (struct.get $obj $digits (ref.cast (ref $obj) (local.get $a)))))
  #@(local.set $length2 (array.len (struct.get $obj $digits (ref.cast (ref $obj) (local.get $b)))))
  #@(local.set $stop1 (if (result i32) (ref.is_null (local.get $end1))
    (then (local.get $length1)) (else (call $bounded-index (local.get $end1) (local.get $length1)))))
  #@(if (i32.and (local.get $seen) (i32.const 1))
    (then (local.set $i (call $bounded-index (local.get $start1) (local.get $stop1)))))
  #@(local.set $stop2 (if (result i32) (ref.is_null (local.get $end2))
    (then (local.get $length2)) (else (call $bounded-index (local.get $end2) (local.get $length2)))))
  #@(if (i32.and (local.get $seen) (i32.const 4))
    (then (local.set $j (call $bounded-index (local.get $start2) (local.get $stop2)))))
  #@(local.set $fold (i32.ge_u (local.get $op) (i32.const 6)))
  #@(if (local.get $fold) (then (local.set $op (i32.sub (local.get $op) (i32.const 6)))))
  #@(if (i32.and (i32.eqz (local.get $op))
      (i32.ne (i32.sub (local.get $stop1) (local.get $i)) (i32.sub (local.get $stop2) (local.get $j))))
    (then (return (ref.null eq))))
  ;; Preserve the tight internal equality loop for whole, case-sensitive
  ;; strings, including its object-identity shortcut.
  #@(if (i32.and (i32.eqz (i32.or (local.get $op) (local.get $fold)))
      (i32.and (i32.eqz (i32.or (local.get $i) (local.get $j)))
        (i32.and (i32.eq (local.get $stop1) (local.get $length1))
                 (i32.eq (local.get $stop2) (local.get $length2)))))
    (then (return (call $bool (call $string-equal (local.get $a) (local.get $b))))))
  #@(local.set $saved-extra (global.get $extra-values))
  #@(local.set $saved-count (global.get $value-count))
  #@(local.set $result (call $compare-character-regions-cl (local.get $a) (local.get $b)
    (call $vector-new (array.new_fixed $objects 6
      (call $int (local.get $i)) (call $int (local.get $stop1))
      (call $int (local.get $j)) (call $int (local.get $stop2)) (call $int (local.get $op))
      (call $sequence-character-fold-function (local.get $fold))))))
  #@(global.set $extra-values (local.get $saved-extra))
  #@(global.set $value-count (local.get $saved-count))
  #@(local.get $result))

(defun $compare-character-regions-cl ()
  (runtime-function-definition '$compare-character-regions-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$compare-character-regions-cl 'miricl-runtime:compare-character-regions))))

(defun $character-bag-contains-cl ()
  (runtime-function-definition '$character-bag-contains-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$character-bag-contains-cl 'miricl-runtime:character-bag-member-p :result :truth))))

(define-runtime-function $character-bag-contains (($bag :digits) ($code i32)) (i32)
  #@(call $character-bag-contains-cl (call $text (local.get $bag))
    (call $node (i32.const 16) (local.get $code) (ref.null eq) (ref.null eq) (ref.null eq))))

(defun $string-trim-native-cl ()
  (runtime-function-definition '$string-trim-native-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$string-trim-native-cl 'miricl-runtime:trim-text-value))))

(define-runtime-function $string-trim-native (($bag :nullable-object) ($string :nullable-object) ($mode i32)) (:nullable-object)
  (:locals ($result :nullable-object))
  #@(local.set $string (call $string-designator (local.get $string)))
  #@(drop (call $proper-sequence-length (local.get $bag)))
  #@(local.set $result (call $string-trim-native-cl (local.get $bag) (local.get $string) (call $int (local.get $mode))))
  ;; The target accepts empty NIL-element vectors as string designators.
  ;; TRIM may return a fresh string: supply an ordinary character-string view
  ;; rather than leaking the impossible element type through an unchanged tail.
  #@(if (i32.eq (call $array-kind (local.get $result)) (i32.const 3))
    (then (return (call $text (array.new_default $digits (i32.const 0))))))
  #@(local.get $result))

(define-runtime-section string-runtime-wat ()
  #@(global $cl-kernel-functions-ready (mut i32) (i32.const 0))
  ($bootstrap-string-equal)
  #@(global $string-case-ranges (ref $digits)
    (array.new_fixed $digits ,(length *string-case-ranges*)
      ,(runtime-wat-forms (loop for value in (string-case-table-values *string-case-ranges*) collect `(i32.const ,value)))))
  #@(global $string-downcase-ranges (ref $digits)
    (array.new_fixed $digits ,(length *string-downcase-ranges*)
      ,(runtime-wat-forms (loop for value in (string-case-table-values *string-downcase-ranges*) collect `(i32.const ,value)))))
  #@(global $string-case-table (mut (ref null eq)) (ref.null eq))
  #@(global $string-downcase-table (mut (ref null eq)) (ref.null eq))
  ($character-case-code-cl) ($initialize-string-case-tables)
  ($string-case-code) ($unicode-case-code) ($string-upcase-code) ($string-fold-equal-cl) ($string-fold-equal)
  ($text)
  ($string-equal-cl) ($string-equal)
  ($string-designator-cl) ($string-designator) ($string-element) ($compare-character-regions-cl) ($string-comparison)
  ($character-bag-contains-cl) ($character-bag-contains) ($string-trim-native-cl) ($string-trim-native))
