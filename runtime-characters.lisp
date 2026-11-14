;;;; Native character codes and comparisons; Unicode folding is shared with strings.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun character-property-ranges (predicate &optional (start 0))
  (let ((ranges nil) (first nil) (last nil))
    (loop for code from start below (min char-code-limit #x110000)
          for char = (code-char code)
          for present = (and char (funcall predicate char)) do
      (cond (present (unless first (setf first code)) (setf last code))
            (first (push first ranges) (push last ranges) (setf first nil))))
    (when first (push first ranges) (push last ranges))
    (nreverse ranges)))

(defparameter *character-alpha-ranges* (character-property-ranges #'alpha-char-p 128))
(defparameter *character-graphic-ranges* (character-property-ranges #'graphic-char-p))

(defparameter *character-names*
  (loop for code below (min char-code-limit #x110000) for char = (code-char code)
        when (and char (or (= code 32) (not (graphic-char-p char))))
          collect (cons code
            (or (cdr (assoc code '((0 . "Null") (8 . "Backspace") (9 . "Tab") (10 . "Newline")
                                   (12 . "Page") (13 . "Return") (32 . "Space") (127 . "Rubout"))))
                (char-name char) (format nil "U~4,'0X" code)))))

(defun character-property-bytes ()
  ;; A packed string stores range endpoints directly in one WasmGC array.
  (runtime-library-bytes
    (list (map 'string #'code-char *character-alpha-ranges*) *character-names*) nil 25))

(defun character-property-data ()
  (runtime-library-data (character-property-bytes)))

;; Non-ASCII digit weights are part of the bootstrapped character repertoire.
;; Each record is START, END, CODE-MINUS-WEIGHT.
(defparameter *character-digit-ranges*
  (let ((pairs (loop for code from 128 below (min char-code-limit #x110000)
                     for char = (code-char code)
                     for weight = (and char (digit-char-p char 36))
                     when weight collect (cons code (- code weight)))))
    (loop while pairs for (start . delta) = (pop pairs) for end = start
          do (loop while (and pairs (= (caar pairs) (1+ end)) (= (cdar pairs) delta))
                   do (setf end (car (pop pairs))))
          append (list start end delta))))

(defun $character-code-cl ()
  (runtime-function-definition '$character-code-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$character-code-cl 'miricl-runtime:checked-character-value))))

(define-runtime-function $character-code (($x :nullable-object)) (i32)
  #@(call $value-n (call $character-code-cl (local.get $x))))

(define-runtime-function $character-compare (($args :nullable-object) ($op i32)) (:nullable-object)
  (:locals ($scan :nullable-object) ($pairs :nullable-object)
           ($a i32) ($b i32) ($fold i32) ($match i32))
  (wat-fail-when #@(ref.is_null (local.get $args)) :wrong-arity)
  ;; Validate all arguments before an early mismatch can return. In particular,
  ;; a bad later argument must not be hidden by an earlier unequal pair.
  #@(local.set $scan (local.get $args))
  #@(block $validated (loop $validate (br_if $validated (ref.is_null (local.get $scan)))
    (drop (call $character-code (call $car (local.get $scan))))
    (local.set $scan (call $cdr (local.get $scan))) (br $validate)))
  #@(local.set $fold (i32.ge_u (local.get $op) (i32.const 6)))
  #@(if (local.get $fold) (then (local.set $op (i32.sub (local.get $op) (i32.const 6)))))
  #@(block $done (loop $next
    (local.set $pairs (call $cdr (local.get $args)))
    (br_if $done (ref.is_null (local.get $pairs)))
    (local.set $a (call $character-code (call $car (local.get $args))))
    (if (local.get $fold) (then (local.set $a (call $string-upcase-code (local.get $a)))))
    (loop $pair
      (local.set $b (call $character-code (call $car (local.get $pairs))))
      (if (local.get $fold) (then (local.set $b (call $string-upcase-code (local.get $b)))))
      ,(runtime-wat-forms
        (loop for comparison in '(i32.eq i32.ne i32.lt_u i32.gt_u i32.le_u i32.ge_u)
              for op from 0 collect
          `(if (i32.eq (local.get $op) (i32.const ,op))
            (then (local.set $match (,comparison (local.get $a) (local.get $b)))))))
      (if (i32.eqz (local.get $match)) (then (return (ref.null eq))))
      (local.set $pairs (call $cdr (local.get $pairs)))
      ;; Distinctness compares every pair, not just adjacent arguments.
      (br_if $pair (i32.and (i32.eq (local.get $op) (i32.const 1))
        (i32.eqz (ref.is_null (local.get $pairs))))))
    (local.set $args (call $cdr (local.get $args))) (br $next)))
  #@(call $bool (i32.const 1)))

(define-runtime-function $character-apply (($id i32) ($args :nullable-object)) (:nullable-object)
  (:locals ($x :nullable-object) ($code i32))
  #@(if (i32.ge_u (local.get $id) (i32.const ,(runtime-symbol-id 'char=)))
    (then (return (call $character-compare (local.get $args)
      (i32.sub (local.get $id) (i32.const ,(runtime-symbol-id 'char=)))))))
  (wat-arity 1)
  #@(local.set $x (call $car (local.get $args)))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'characterp)))
    (then (return (call $bool (i32.eq (call $tag (local.get $x)) (i32.const 16))))))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'code-char)))
    (then
      (local.set $code (call $bounded-index (local.get $x) (i32.const 1114111)))
      ;; Guest text uses Unicode scalar values; surrogate codes are holes.
      (if (i32.and (i32.ge_u (local.get $code) (i32.const 55296))
                   (i32.le_u (local.get $code) (i32.const 57343)))
        (then (return (ref.null eq))))
      (return (call $node (i32.const 16) (local.get $code)
        (ref.null eq) (ref.null eq) (ref.null eq)))))
  #@(call $int (call $character-code (local.get $x))))

(define-runtime-function $character-case-native (($x :nullable-object) ($down i32)) (:nullable-object)
  (:locals ($code i32) ($upper i32))
  #@(local.set $code (call $character-code (local.get $x)))
  #@(local.set $upper (if (result i32) (i32.and (local.get $down) (i32.const 2))
    (then (call $unicode-case-code (local.get $code) (i32.and (local.get $down) (i32.const 1))))
    (else (call $string-case-code (local.get $code) (local.get $down)))))
  #@(if (i32.eq (local.get $code) (local.get $upper)) (then (return (local.get $x))))
  #@(call $node (i32.const 16) (local.get $upper) (ref.null eq) (ref.null eq) (ref.null eq)))

(defun $character-digit-code-cl ()
  (runtime-function-definition '$character-digit-code-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$character-digit-code-cl 'miricl-runtime:character-digit-code-value))))

(define-runtime-function $character-digit-weight (($code i32)) (i32)
  #@(i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null
    (call $character-digit-code-cl (call $int (local.get $code))
      (global.get $character-digit-table))))))

(define-runtime-function $character-digit-apply (($id i32) ($args :nullable-object)) (:nullable-object)
  (:locals ($x :nullable-object) ($radix :nullable-object) ($base i32) ($weight i32))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'standard-char-p)))
    (then ,(wat-arity 1)
      (drop (call $character-code (call $car (local.get $args))))
      (return (call $bool (call $typep (call $car (local.get $args))
        (call $symbol (i32.const ,(runtime-symbol-id 'standard-char))))))))
  (wat-arity 1 2)
  #@(local.set $x (call $car (local.get $args)))
  #@(local.set $base (i32.const 10))
  #@(if (i32.eqz (ref.is_null (call $cdr (local.get $args))))
    (then (local.set $radix (call $car (call $cdr (local.get $args))))
      (local.set $base (call $natural-count (local.get $radix)))
      (if (i32.or (i32.lt_u (local.get $base) (i32.const 2)) (i32.gt_u (local.get $base) (i32.const 36)))
        (then (call $type-fail (local.get $radix)
          (call $cons (call $symbol (i32.const ,(runtime-symbol-id 'integer)))
            (call $cons (call $int (i32.const 2)) (call $cons (call $int (i32.const 36)) (ref.null eq))))
          (i32.const ,(runtime-error-code :expected-designator)))))))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'digit-char)))
    (then
      (local.set $weight (call $natural-count (local.get $x)))
      (if (i32.ge_u (local.get $weight) (local.get $base)) (then (return (ref.null eq))))
      (return (call $node (i32.const 16)
        (i32.add (local.get $weight) (select (i32.const 48) (i32.const 55)
          (i32.lt_u (local.get $weight) (i32.const 10)))) (ref.null eq) (ref.null eq) (ref.null eq)))))
  #@(local.set $weight (call $character-digit-weight (call $character-code (local.get $x))))
  #@(if (i32.ge_u (local.get $weight) (local.get $base)) (then (return (ref.null eq))))
  #@(call $int (local.get $weight)))

(define-runtime-function $initialize-character-properties () ()
  (:locals ($names :nullable-object) ($entry :nullable-object))
  #@(global.set $character-digit-table (call $text (global.get $character-digit-ranges)))
  #@(call $reset-library-strings)
  #@(global.set $cursor (i32.const 0))
  #@(global.set $limit (i32.const ,(length (character-property-bytes))))
  #@(call $reserve (global.get $limit))
  #@(memory.init $character-property-data (i32.const 0) (i32.const 0) (global.get $limit))
  #@(data.drop $character-property-data)
  #@(global.set $character-alpha-ranges (ref.as_non_null
    (struct.get $obj $digits (ref.cast (ref $obj) (call $read-library-literal)))))
  #@(global.set $character-alpha-table (call $text (global.get $character-alpha-ranges)))
  #@(global.set $character-names (call $read-library-literal))
  #@(call $reset-library-strings)
  #@(global.set $character-name-index (array.new_default $objects
    (i32.const ,(1+ (reduce #'max *character-names* :key #'car)))))
  #@(local.set $names (global.get $character-names))
  (wat-list-loop ($names)
    #@(local.set $entry (call $car (local.get $names)))
    #@(array.set $objects (global.get $character-name-index)
      (call $natural-count (call $car (local.get $entry))) (call $cdr (local.get $entry)))))

(defun $character-alpha-code-cl ()
  (runtime-function-definition '$character-alpha-code-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$character-alpha-code-cl 'miricl-runtime:character-alpha-code-p :result :truth))))

(define-runtime-function $character-alpha-code (($code i32)) (i32)
  #@(call $character-alpha-code-cl (call $int (local.get $code))
    (global.get $character-alpha-table)))

(define-runtime-function $character-alphanumeric-code (($code i32)) (i32)
  #@(if (call $character-alpha-code (local.get $code)) (then (return (i32.const 1))))
  #@(i32.lt_u (call $character-digit-weight (local.get $code)) (i32.const 10)))

(define-runtime-function $character-property-apply (($id i32) ($x :nullable-object)) (:nullable-object)
  (:locals ($code i32) ($up i32) ($down i32))
  #@(local.set $code (call $character-code (local.get $x)))
  (wat-symbol-case #@(local.get $id)
    (alpha-char-p #@(return (call $bool (call $character-alpha-code (local.get $code)))))
    (alphanumericp #@(return (call $bool (call $character-alphanumeric-code (local.get $code)))))
    (graphic-char-p
      (runtime-wat-forms
        (loop for (start end) on *character-graphic-ranges* by #'cddr collect
          `(if (i32.and (i32.ge_u (local.get $code) (i32.const ,start))
                         (i32.le_u (local.get $code) (i32.const ,end)))
            (then (return (call $bool (i32.const 1)))))))
      #@(return (ref.null eq))))
  #@(local.set $up (call $string-case-code (local.get $code) (i32.const 0)))
  #@(local.set $down (call $string-case-code (local.get $code) (i32.const 1)))
  (wat-symbol-case #@(local.get $id)
    (upper-case-p #@(return (call $bool (i32.and (i32.eq (local.get $code) (local.get $up))
      (i32.ne (local.get $code) (local.get $down))))))
    (lower-case-p #@(return (call $bool (i32.and (i32.eq (local.get $code) (local.get $down))
      (i32.ne (local.get $code) (local.get $up)))))))
  #@(call $bool (i32.ne (local.get $up) (local.get $down))))

(define-runtime-function $character-name (($code i32)) (:nullable-object)
  #@(if (result (ref null eq)) (i32.lt_u (local.get $code) (array.len (global.get $character-name-index)))
    (then (array.get $objects (global.get $character-name-index) (local.get $code)))
    (else (ref.null eq))))

(define-runtime-function $character-name-code (($name :nullable-object)) (i32)
  (:locals ($names :nullable-object) ($entry :nullable-object))
  #@(local.set $names (global.get $character-names))
  (wat-list-loop ($names)
    #@(local.set $entry (call $car (local.get $names)))
    #@(if (call $string-fold-equal (local.get $name) (call $cdr (local.get $entry)))
      (then (return (call $natural-count (call $car (local.get $entry)))))))
  ;; Linefeed is the same guest character as Newline; Nul is a common alias.
  #@(if (call $string-fold-equal (local.get $name) ,(wat-text-literal "Linefeed"))
    (then (return (i32.const 10))))
  #@(if (call $string-fold-equal (local.get $name) ,(wat-text-literal "Nul"))
    (then (return (i32.const 0))))
  ;; The CL parser handles the target's Unicode-name extension after table
  ;; aliases have had priority. NAME has already been made a simple string.
  #@(i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null
    (call $unicode-character-name-code-cl (local.get $name))))))

(defun $unicode-character-name-code-cl ()
  (runtime-function-definition '$unicode-character-name-code-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$unicode-character-name-code-cl 'miricl-runtime:unicode-character-name-code))))

(define-runtime-function $character-name-apply (($id i32) ($x :nullable-object)) (:nullable-object)
  (:locals ($code i32) ($name :nullable-object))
  #@(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id 'char-name)))
    (then (local.set $name (call $character-name (call $character-code (local.get $x))))
      (if (ref.is_null (local.get $name)) (then (return (ref.null eq))))
      ;; Keep the internal reader/printer table independent of mutable results.
      (return (call $slice (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $name))))
        (i32.const 0) (array.len (struct.get $obj $digits (ref.cast (ref $obj) (local.get $name))))))))
  #@(local.set $code (call $character-name-code (call $string-contents (call $string-designator (local.get $x)))))
  #@(if (i32.lt_s (local.get $code) (i32.const 0)) (then (return (ref.null eq))))
  #@(call $node (i32.const 16) (local.get $code) (ref.null eq) (ref.null eq) (ref.null eq)))

(define-runtime-section character-runtime-wat ()
  #@(data $character-property-data ,(character-property-data))
  #@(global $character-alpha-ranges (mut (ref $digits)) (array.new_default $digits (i32.const 0)))
  #@(global $character-alpha-table (mut (ref null eq)) (ref.null eq))
  #@(global $character-digit-table (mut (ref null eq)) (ref.null eq))
  #@(global $character-names (mut (ref null eq)) (ref.null eq))
  #@(global $character-name-index (mut (ref $objects)) (array.new_default $objects (i32.const 0)))
  #@(global $character-digit-ranges (ref $digits)
    (array.new_fixed $digits ,(length *character-digit-ranges*)
      ,(runtime-wat-forms (loop for value in *character-digit-ranges* collect `(i32.const ,value)))))
  ($character-code-cl) ($character-code) ($character-compare) ($character-apply) ($character-case-native)
  ($character-digit-code-cl) ($character-digit-weight) ($character-digit-apply) ($initialize-character-properties)
  ($character-alpha-code-cl) ($character-alpha-code) ($character-alphanumeric-code) ($character-property-apply)
  ($character-name) ($unicode-character-name-code-cl) ($character-name-code) ($character-name-apply))
