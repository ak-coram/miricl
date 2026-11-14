;;;; Native list/sequence loops; guest Lisp only binds keyword arguments.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $check-apply-list (($args :nullable-object)) ()
  ;; Match APPLY's proper-list validation before checking the lambda's arity.
  #@(if (ref.is_null (call $list-length-native (local.get $args)))
    (then (call $type-fail (local.get $args) (call $symbol-value ,(clos-library-reference '%proper-list-type))
      (i32.const ,(runtime-error-code :expected-designator))))))

(define-runtime-function $check-apply-arguments (($args :nullable-object) ($count i32)) (:nullable-object)
  #@(call $check-apply-list (local.get $args))
  #@(call $arity (local.get $args) (local.get $count) (local.get $count))
  #@(ref.null eq))

(defun $pop-argument-cl ()
  (runtime-function-definition '$pop-argument-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$pop-argument-cl 'miricl-runtime:pop-argument-box))))

(define-runtime-function $pop-argument (($box :nullable-object) ($required :nullable-object)) (:nullable-object)
  #@(if (i32.ne (call $tag (local.get $box)) (i32.const 7))
    (then (call $list-type-error (local.get $box) (i32.const ,(runtime-symbol-id 'cons)))))
  #@(call $pop-argument-cl (local.get $box) (local.get $required)))

(defun $list-eq ()
  (runtime-function-definition '$list-eq
    (wat-fragment (miricl-cross:runtime-kernel-wat '$list-eq 'miricl-runtime:list-eq :interruptible t))))

(define-runtime-function $find-method-list
    (($methods :nullable-object) ($qualifiers :nullable-object) ($specializers :nullable-object)) (:nullable-object)
  (:locals ($method :nullable-object))
  #@(block $done (loop $next (br_if $done (ref.is_null (local.get $methods)))
    (local.set $method (call $car (local.get $methods)))
    (if (call $object-equal (local.get $qualifiers)
          (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $method))) ,(clos-library-reference :qualifiers))))
      (then (if (i32.eqz (ref.is_null (call $list-eq (local.get $specializers)
        (call $car (call $property-cell (struct.get $obj $b (ref.cast (ref $obj) (local.get $method))) ,(clos-library-reference :specializers))))))
        (then (return (local.get $method))))))
    (local.set $methods (call $cdr (local.get $methods))) (call $thread-checkpoint) (br $next)))
  #@(ref.null eq))

(defun $check-generic-arguments ()
  (runtime-function-definition '$check-generic-arguments
    (wat-fragment (miricl-cross:runtime-kernel-wat '$check-generic-arguments 'miricl-runtime:check-generic-arguments))))

(define-runtime-function $check-generic-arity (($params :nullable-object) ($args :nullable-object)) (:nullable-object)
  #@(call $check-apply-list (local.get $params))
  #@(call $check-generic-arguments (local.get $params) (local.get $args)))

(define-runtime-function $list-type-error (($x :nullable-object) ($type i32)) ()
  #@(call $type-fail (local.get $x) (call $symbol (local.get $type))
    (i32.const ,(runtime-error-code :expected-designator))))

(defun $require-list-cl ()
  (runtime-function-definition '$require-list-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$require-list-cl 'miricl-runtime:require-list-value))))

(define-runtime-function $require-list (($x :nullable-object)) ()
  #@(drop (call $require-list-cl (local.get $x))))

;; Saturation is useful for finite list lengths. NTHCDR separately retains
;; large counters because its input can also be circular.
(define-runtime-function $natural-count (($x :nullable-object)) (i32)
  (:locals ($value i32))
  ;; Immediate nonnegative integers need neither magnitude limbs nor a
  ;; saturation check. Keep the full checked path for all other values.
  #@(if (ref.test (ref i31) (local.get $x))
    (then (local.set $value (i31.get_s (ref.cast (ref i31) (local.get $x))))
      (if (i32.ge_s (local.get $value) (i32.const 0)) (then (return (local.get $value))))))
  #@(i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null
    (call $natural-count-cl (local.get $x) (call $int (i32.const 2147483647)))))))

(defun $natural-count-cl ()
  (runtime-function-definition '$natural-count-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$natural-count-cl 'miricl-runtime:natural-count-value))))

(defun $copy-list-spine ()
  (runtime-function-definition '$copy-list-spine
    (wat-fragment (miricl-cross:runtime-kernel-wat '$copy-list-spine 'miricl-runtime:copy-list-spine))))

(defun $copy-alist-spine ()
  (runtime-function-definition '$copy-alist-spine
    (wat-fragment (miricl-cross:runtime-kernel-wat '$copy-alist-spine 'miricl-runtime:copy-alist-spine))))

(defun $copy-tree-value-cl ()
  (runtime-function-definition '$copy-tree-value-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$copy-tree-value-cl 'miricl-runtime:copy-tree-value))))

(define-runtime-function $copy-spine (($x :nullable-object) ($mode i32)) (:nullable-object)
  ;; Modes COPY-LIST=0, COPY-ALIST=1, COPY-TREE=2 share sealed CL workers.
  #@(if (i32.eq (local.get $mode) (i32.const 2))
    (then (return_call $copy-tree-value-cl (local.get $x))))
  #@(if (result (ref null eq)) (i32.eq (local.get $mode) (i32.const 1))
    (then (call $copy-alist-spine (local.get $x)))
    (else (call $copy-list-spine (local.get $x)))))

(defun $reverse-copy-onto ()
  (runtime-function-definition '$reverse-copy-onto
    (wat-fragment (miricl-cross:runtime-kernel-wat '$reverse-copy-onto 'miricl-runtime:reverse-copy-onto))))

(defun $reverse-mutate-onto ()
  (runtime-function-definition '$reverse-mutate-onto
    (wat-fragment (miricl-cross:runtime-kernel-wat '$reverse-mutate-onto 'miricl-runtime:reverse-mutate-onto))))

(define-runtime-function $reverse-onto (($x :nullable-object) ($tail :nullable-object) ($mutate i32)) (:nullable-object)
  #@(if (result (ref null eq)) (local.get $mutate)
    (then (call $reverse-mutate-onto (local.get $x) (local.get $tail)))
    (else (call $reverse-copy-onto (local.get $x) (local.get $tail)))))

(define-runtime-function $reverse-sequence (($x :nullable-object) ($mutate i32)) (:nullable-object)
  #@(if (i32.or (i32.eq (call $tag (local.get $x)) (i32.const 35))
                 (i32.eq (call $tag (local.get $x)) (i32.const 36)))
    (then (return (call $reverse-array-sequence (local.get $x) (local.get $mutate)))))
  #@(if (i32.eq (call $tag (local.get $x)) (i32.const 34))
    (then (return (call $vector-reverse (local.get $x) (local.get $mutate)))))
  #@(if (i32.eq (call $tag (local.get $x)) (i32.const 10))
    (then (return (call $reverse-array-sequence (local.get $x) (local.get $mutate)))))
  #@(call $require-list (local.get $x))
  #@(call $reverse-onto (local.get $x) (ref.null eq) (local.get $mutate)))

(defun $make-list-bounded ()
  (runtime-function-definition '$make-list-bounded
    (wat-fragment (miricl-cross:runtime-kernel-wat '$make-list-bounded 'miricl-runtime:make-list-bounded))))

(define-runtime-function $make-list-native (($size :nullable-object) ($value :nullable-object)) (:nullable-object)
  #@(drop (call $natural-count (local.get $size)))
  (wat-fail-when #@(i32.gt_u (call $bitlength (ref.as_non_null (local.get $size))) (i32.const 31)) :allocation)
  #@(call $make-list-bounded (local.get $size) (local.get $value)))

(defun $nthcdr-bounded ()
  (runtime-function-definition '$nthcdr-bounded
    (wat-fragment (miricl-cross:runtime-kernel-wat '$nthcdr-bounded 'miricl-runtime:nthcdr-bounded))))

(defun $nthcdr-general ()
  (runtime-function-definition '$nthcdr-general
    (wat-fragment (miricl-cross:runtime-kernel-wat '$nthcdr-general 'miricl-runtime:nthcdr-general))))

(define-runtime-function $nthcdr-native (($count :nullable-object) ($x :nullable-object)) (:nullable-object)
  (:locals ($n i32))
  #@(local.set $n (call $natural-count (local.get $count)))
  #@(if (i32.or (i32.eqz (local.get $n)) (ref.is_null (local.get $x)))
    (then (return (local.get $x))))
  #@(if (i32.le_u (call $bitlength (ref.as_non_null (local.get $count))) (i32.const 31))
    (then (return (call $nthcdr-bounded (local.get $count) (local.get $x)))))
  #@(call $nthcdr-general (local.get $count) (local.get $x)))

(defun $last-bounded ()
  (runtime-function-definition '$last-bounded
    (wat-fragment (miricl-cross:runtime-kernel-wat '$last-bounded 'miricl-runtime:last-bounded))))

(define-runtime-function $last-native (($x :nullable-object) ($count :nullable-object)) (:nullable-object)
  #@(call $require-list (local.get $x))
  ;; Preserve the existing saturation of large nonnegative counts. Keep
  ;; validation ahead of the SAFETY 0 worker.
  #@(call $last-bounded (local.get $x) (call $int (call $natural-count (local.get $count)))))

(defun $copy-butlast-bounded ()
  (runtime-function-definition '$copy-butlast-bounded
    (wat-fragment (miricl-cross:runtime-kernel-wat '$copy-butlast-bounded 'miricl-runtime:copy-butlast-bounded))))

(defun $truncate-butlast-bounded ()
  (runtime-function-definition '$truncate-butlast-bounded
    (wat-fragment (miricl-cross:runtime-kernel-wat '$truncate-butlast-bounded 'miricl-runtime:truncate-butlast-bounded))))

(define-runtime-function $butlast-native (($x :nullable-object) ($count :nullable-object) ($mutate i32)) (:nullable-object)
  (:locals ($bounded :nullable-object))
  #@(call $require-list (local.get $x))
  #@(local.set $bounded (call $int (call $natural-count (local.get $count))))
  #@(if (result (ref null eq)) (local.get $mutate)
    (then (call $truncate-butlast-bounded (local.get $x) (local.get $bounded)))
    (else (call $copy-butlast-bounded (local.get $x) (local.get $bounded)))))

(defun $copy-before-tail ()
  (runtime-function-definition '$copy-before-tail
    (wat-fragment (miricl-cross:runtime-kernel-wat '$copy-before-tail 'miricl-runtime:copy-before-tail))))

(defun $tail-member-p ()
  (runtime-function-definition '$tail-member-p
    (wat-fragment (miricl-cross:runtime-kernel-wat '$tail-member-p 'miricl-runtime:tail-member-p))))

(define-runtime-function $tail-difference (($x :nullable-object) ($object :nullable-object) ($copy i32)) (:nullable-object)
  #@(call $require-list (local.get $x))
  #@(if (local.get $copy)
    (then (return (call $copy-before-tail (local.get $x) (local.get $object)))))
  #@(call $tail-member-p (local.get $x) (local.get $object)))

(defun $list-length-native ()
  (runtime-function-definition '$list-length-native
    (wat-fragment (miricl-cross:runtime-kernel-wat '$list-length-native 'miricl-runtime:list-length-or-cycle))))

(defun $pair-lists-onto ()
  (runtime-function-definition '$pair-lists-onto
    (wat-fragment (miricl-cross:runtime-kernel-wat '$pair-lists-onto 'miricl-runtime:pair-lists-onto))))

(define-runtime-function $pairlis-native (($keys :nullable-object) ($data :nullable-object) ($alist :nullable-object)) (:nullable-object)
  #@(call $require-list (local.get $alist))
  #@(call $pair-lists-onto (local.get $keys) (local.get $data) (local.get $alist)))

(defun $callable ()
  (runtime-function-definition '$callable
    (wat-fragment (miricl-cross:runtime-kernel-wat '$callable 'miricl-runtime:callable-value))))

;; Modes: MAPCAR=0, MAPC=1, MAPLIST=2, MAPL=3, EVERY=4, SOME=5.
;; Keep cursors separately from callback argument lists: an &REST callback
;; may retain its arguments after this iteration has finished.
(defun $mapping-argument-cl ()
  (runtime-function-definition '$mapping-argument-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$mapping-argument-cl 'miricl-runtime:mapping-argument-value))))

(define-runtime-function $mapping-argument
    (($sequence :nullable-object) ($index i32) ($mode i32)) (:nullable-object)
  #@(call $mapping-argument-cl (local.get $sequence) (call $int (local.get $index)) (call $int (local.get $mode))))

(defun $map-lists-cl ()
  (runtime-function-definition '$map-lists-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$map-lists-cl 'miricl-runtime:map-list-values))))

(define-runtime-function $map-lists-native
    (($function :nullable-object) ($inputs :nullable-object) ($mode i32) ($output :nullable-object)) (:nullable-object)
  #@(call $map-lists-cl (local.get $function) (local.get $inputs) (call $int (local.get $mode)) (local.get $output)))

(defun $map-sequence-result-cl ()
  (runtime-function-definition '$map-sequence-result-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$map-sequence-result-cl 'miricl-runtime:map-sequence-result-value))))

(define-runtime-function $map-sequences-native
    (($type :nullable-object) ($function :nullable-object) ($inputs :nullable-object)) (:nullable-object)
  (:locals ($output :nullable-object))
  #@(local.set $function (call $callable (local.get $function)))
  #@(local.set $output (call $map-sequence-result-cl (local.get $type) (local.get $inputs)))
  #@(call $map-lists-native (local.get $function) (local.get $inputs)
    (select (i32.const 6) (i32.const 7) (i32.eqz (ref.is_null (local.get $type)))) (local.get $output)))

(define-runtime-function $map-into-apply (($args :nullable-object)) (:nullable-object)
  (:locals ($output :nullable-object) ($inputs :nullable-object) ($scan :nullable-object))
  (wat-arity 2 2147483647)
  #@(local.set $output (call $car (local.get $args)))
  #@(if (ref.is_null (local.get $output)) (then (call $clear-values) (return (ref.null eq))))
  #@(drop (call $proper-sequence-length (local.get $output)))
  #@(local.set $inputs (call $cdr (call $cdr (local.get $args))))
  #@(local.set $scan (local.get $inputs))
  #@(block $done (loop $validate (br_if $done (ref.is_null (local.get $scan)))
    (drop (call $proper-sequence-length (call $car (local.get $scan))))
    (local.set $scan (call $cdr (local.get $scan))) (br $validate)))
  #@(call $map-lists-native (call $car (call $cdr (local.get $args)))
    (local.get $inputs) (i32.const 8) (local.get $output)))

;; KIND: MEMBER=0, ASSOC=1, RASSOC=2. MODE: item=0, IF=1, IF-NOT=2.
;; Resolve designators once, then stop at the first match without copying.
(defun $search-list-cl ()
  (runtime-function-definition '$search-list-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$search-list-cl 'miricl-runtime:search-list-values))))

(define-runtime-function $search-list-native
    (($kind i32) ($mode i32) ($item :nullable-object) ($list :nullable-object)
     ($key :nullable-object) ($test :nullable-object) ($test-not :nullable-object)) (:nullable-object)
  (:locals ($function :nullable-object) ($result :nullable-object))
  #@(call $require-list (local.get $list))
  #@(if (i32.eqz (ref.is_null (local.get $key)))
    (then (local.set $key (call $callable (local.get $key)))))
  #@(local.set $function (call $callable
    (if (result (ref null eq)) (local.get $mode) (then (local.get $item))
      (else (if (result (ref null eq)) (ref.is_null (local.get $test-not))
        (then (local.get $test)) (else (local.get $test-not)))))))
  #@(local.set $result (call $search-list-cl (local.get $list) (local.get $item) (local.get $key)
    (call $cons (call $int (local.get $kind))
      (call $cons (call $int (local.get $mode))
        (call $cons (local.get $function) (call $cons (call $bool (i32.eqz (ref.is_null (local.get $test-not)))) (ref.null eq)))))))
  #@(call $clear-values)
  #@(local.get $result))

;; ADJOIN=0, SUBSETP=1, INTERSECTION=2, SET-DIFFERENCE=3, UNION=4.
;; Keys apply to both inputs; the comparison keeps list-1 first even for
;; asymmetric predicates. UNION can share list-2 without modifying it.
(defun $list-exclusive-cl ()
  (runtime-function-definition '$list-exclusive-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$list-exclusive-cl 'miricl-runtime:list-exclusive-values))))

(define-runtime-function $list-exclusive-native
    (($a :nullable-object) ($b :nullable-object) ($key :nullable-object)
     ($test :nullable-object) ($test-not :nullable-object)) (:nullable-object)
  (:locals ($result :nullable-object) ($other :nullable-object))
  #@(call $clear-values)
  #@(if (i32.and (ref.is_null (local.get $test-not)) (call $hash-test-function-id (local.get $test)))
    (then
      (drop (call $proper-sequence-length (local.get $a)))
      (drop (call $proper-sequence-length (local.get $b)))
      (if (ref.is_null (local.get $a)) (then (return (local.get $b))))
      (if (ref.is_null (local.get $b)) (then (return (local.get $a))))
      (local.set $result (call $list-set-native (i32.const 3) (local.get $a) (local.get $b)
        (local.get $key) (local.get $test) (ref.null eq)))
      (local.set $other (call $list-set-native (i32.const 3) (local.get $b) (local.get $a)
        (local.get $key) (local.get $test) (ref.null eq)))
      (return (call $reverse-onto (local.get $result) (local.get $other) (i32.const 1)))))
  ;; General predicates retain the original left/right ordered pair semantics.
  #@(local.set $result (call $list-exclusive-cl (local.get $a) (local.get $b) (local.get $key)
    (call $cons (local.get $test) (local.get $test-not))))
  #@(call $clear-values)
  #@(local.get $result))

;; Shared keyword binding keeps short list operations out of the interpreter.
(define-runtime-function $list-set-apply (($args :nullable-object) ($mode i32)) (:nullable-object)
  (:locals ($a :nullable-object) ($b :nullable-object) ($key :nullable-object)
           ($test :nullable-object) ($test-not :nullable-object) ($keyword :nullable-object)
           ($value :nullable-object) ($seen i32) ($id i32) ($unknown i32) ($allow i32))
  (wat-arity 2 2147483647)
  #@(local.set $a (call $car (local.get $args)))
  #@(local.set $b (call $car (call $cdr (local.get $args))))
  #@(local.set $args (call $cdr (call $cdr (local.get $args))))
  #@(block $bound (loop $keys (br_if $bound (ref.is_null (local.get $args)))
    (local.set $keyword (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
    ,(wat-fail-when #@(ref.is_null (local.get $args)) :wrong-arity)
    (local.set $value (call $car (local.get $args))) (local.set $args (call $cdr (local.get $args)))
    (local.set $id (call $id (local.get $keyword)))
    ,(runtime-wat-forms
      (loop for (keyword variable) in '((:key $key) (:test $test) (:test-not $test-not) (:allow-other-keys $allow))
        for bit = 1 then (* bit 2) collect
        `(if (i32.eq (local.get $id) (i32.const ,(runtime-symbol-id keyword)))
          (then
            (if (i32.eqz (i32.and (local.get $seen) (i32.const ,bit)))
              (then (local.set ,variable ,(if (eq variable '$allow)
                '(i32.eqz (ref.is_null (local.get $value))) '(local.get $value)))
                (local.set $seen (i32.or (local.get $seen) (i32.const ,bit))))) (br $keys)))))
    (local.set $unknown (i32.const 1)) (br $keys)))
  (wat-fail-when #@(i32.and (local.get $unknown) (i32.eqz (local.get $allow))) :wrong-arity)
  (wat-fail-when #@(i32.eq (i32.and (local.get $seen) (i32.const 6)) (i32.const 6)) :wrong-arity)
  #@(if (i32.eqz (i32.and (local.get $seen) (i32.const 2)))
    (then (local.set $test (call $fdefinition (call $symbol (i32.const ,(runtime-symbol-id 'eql)))))))
  #@(if (i32.and (local.get $seen) (i32.const 4))
    (then (local.set $test-not (call $callable (local.get $test-not)))))
  #@(call $list-set-native (local.get $mode) (local.get $a) (local.get $b)
    (local.get $key) (local.get $test) (local.get $test-not)))

(defun $list-set-values-cl ()
  (runtime-function-definition '$list-set-values-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$list-set-values-cl 'miricl-runtime:list-set-values))))

(define-runtime-function $list-set-native
    (($mode i32) ($a :nullable-object) ($b :nullable-object) ($key :nullable-object)
     ($test :nullable-object) ($test-not :nullable-object)) (:nullable-object)
  (:locals ($result :nullable-object) ($hash-test i32) ($key-id i32))
  #@(call $require-list (local.get $b))
  #@(if (local.get $mode) (then (call $require-list (local.get $a))))
  #@(if (i32.eqz (ref.is_null (local.get $key))) (then (local.set $key (call $callable (local.get $key)))))
  #@(if (ref.is_null (local.get $test-not))
    (then (local.set $test (call $callable (local.get $test))))
    (else (local.set $test-not (call $callable (local.get $test-not)))))
  #@(if (i32.eq (local.get $mode) (i32.const 5))
    (then (return (call $list-exclusive-native (local.get $a) (local.get $b) (local.get $key)
      (local.get $test) (local.get $test-not)))))
  ;; Function-object identity, rather than a source name, grants canonical
  ;; hash indexing. Callback-free key selection retains the existing contract.
  #@(block $selected
    (br_if $selected (i32.eqz (ref.is_null (local.get $test-not))))
    (local.set $hash-test (call $hash-test-function-id (local.get $test)))
    (if (i32.eqz (ref.is_null (local.get $key)))
      (then
        (local.set $key-id (i32.sub (call $value-n (local.get $key)) (i32.const 1)))
        ,(wat-symbol-case #@(local.get $key-id)
          ((identity car cdr char-code char-int 1+ 1-) #@(local.set $key-id (i32.const 0))))
        (if (local.get $key-id) (then (local.set $hash-test (i32.const 0)))))))
  #@(local.set $result (call $list-set-values-cl (local.get $a) (local.get $b) (local.get $key)
    (call $vector-new (array.new_fixed $objects 4 (call $int (local.get $mode))
      (local.get $test) (local.get $test-not)
      (if (result (ref null eq)) (local.get $hash-test)
        (then (call $symbol (local.get $hash-test))) (else (ref.null eq)))))))
  #@(call $clear-values)
  #@(local.get $result))

(defun $bounded-index-cl ()
  (runtime-function-definition '$bounded-index-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$bounded-index-cl 'miricl-runtime:bounded-index-value))))

(define-runtime-function $bounded-index (($x :nullable-object) ($limit i32)) (i32)
  #@(i32.wrap_i64 (call $scalar-integer-value (ref.as_non_null
    (call $bounded-index-cl (local.get $x) (call $int (local.get $limit)))))))

(define-runtime-function $sequence-character-kind (($function :nullable-object)) (i32)
  (runtime-wat-forms
    (loop for name in '(eql char= char-equal) for kind from 1 collect
      `(if (i32.eq (call $value-n (local.get $function)) (i32.const ,(1+ (runtime-symbol-id name))))
        (then (return (i32.const ,kind))))))
  #@(i32.const 0))

(define-runtime-function $sequence-character-needle (($kind i32) ($item :nullable-object)) (i32)
  (:locals ($code i32))
  #@(if (i32.and (i32.eq (local.get $kind) (i32.const 1))
                 (i32.ne (call $tag (local.get $item)) (i32.const 16)))
    (then (return (i32.const -1))))
  #@(local.set $code (call $character-code (local.get $item)))
  #@(if (i32.eq (local.get $kind) (i32.const 3))
    (then (return (call $string-upcase-code (local.get $code)))))
  #@(local.get $code))

(define-runtime-function $simple-predicate-value (($id i32) ($value :nullable-object)) (i32)
  (wat-symbol-case #@(local.get $id)
    (null #@(return (ref.is_null (local.get $value))))
    (consp #@(return (i32.eq (call $tag (local.get $value)) (i32.const 7))))
    (atom #@(return (i32.ne (call $tag (local.get $value)) (i32.const 7))))
    (listp #@(return (i32.or (ref.is_null (local.get $value)) (i32.eq (call $tag (local.get $value)) (i32.const 7)))))
    (numberp #@(return (i32.le_u (i32.sub (call $tag (local.get $value)) (i32.const 1)) (i32.const 4))))
    (integerp #@(return (i32.eq (call $tag (local.get $value)) (i32.const 1))))
    (stringp #@(return (call $stringp-native (local.get $value)))))
  #@(i32.const -1))

(defun $sequence-key-value ()
  (runtime-function-definition '$sequence-key-value
    (wat-fragment (miricl-cross:runtime-kernel-wat '$sequence-key-value 'miricl-runtime:sequence-key))))

(define-runtime-function $sequence-item-match
    (($mode i32) ($item :nullable-object) ($value :nullable-object) ($key :nullable-object)
     ($function :nullable-object) ($negate i32)) (i32)
  (:locals ($match i32))
  #@(local.set $value (call $sequence-key-value (local.get $key) (local.get $value)))
  #@(if (local.get $mode)
    (then
      (local.set $match (call $simple-predicate-value
        (i32.sub (call $value-n (local.get $function)) (i32.const 1)) (local.get $value)))
      (if (i32.ge_s (local.get $match) (i32.const 0))
        (then (return (i32.xor (local.get $match) (i32.eq (local.get $mode) (i32.const 2))))))
      (if (i32.or
            (i32.eq (call $value-n (local.get $function)) (i32.const ,(1+ (runtime-symbol-id 'oddp))))
            (i32.eq (call $value-n (local.get $function)) (i32.const ,(1+ (runtime-symbol-id 'evenp)))))
        (then (local.set $match (i32.eqz (ref.is_null (call $oddp-native (local.get $value)))))
          (if (i32.eq (call $value-n (local.get $function)) (i32.const ,(1+ (runtime-symbol-id 'evenp))))
            (then (local.set $match (i32.eqz (local.get $match))))))
        (else (local.set $match (i32.eqz (ref.is_null
          (call $apply (local.get $function) (call $cons (local.get $value) (ref.null eq))))))))
      (return (i32.xor (local.get $match) (i32.eq (local.get $mode) (i32.const 2))))))
  #@(if (i32.eq (call $value-n (local.get $function)) (i32.const ,(1+ (runtime-symbol-id 'eql))))
    (then (local.set $match (call $eql (local.get $item) (local.get $value))))
    (else (if (i32.eq (call $value-n (local.get $function)) (i32.const ,(1+ (runtime-symbol-id 'eq))))
      (then (local.set $match (ref.eq (local.get $item) (local.get $value))))
      (else (local.set $match (i32.eqz (ref.is_null (call $apply (local.get $function)
        (call $cons (local.get $item) (call $cons (local.get $value) (ref.null eq)))))))))))
  #@(i32.xor (local.get $match) (local.get $negate)))

;; COUNT=0, COUNT-IF=1, COUNT-IF-NOT=2. Forward lists use a single
;; cursor. Reverse traversal buffers only the selected range, in O(n).
(defun $count-sequence-value-cl ()
  (runtime-function-definition '$count-sequence-value-cl
    (wat-fragment (miricl-cross:runtime-kernel-wat '$count-sequence-value-cl 'miricl-runtime:count-sequence-value))))

(define-runtime-function $count-sequence-native
    (($mode i32) ($item :nullable-object) ($sequence :nullable-object) ($key :nullable-object)
     ($test :nullable-object) ($test-not :nullable-object) ($start :nullable-object)
     ($end :nullable-object) ($from-end :nullable-object) ($negate i32)) (:nullable-object)
  (:locals ($string i32) ($length i32) ($i i32) ($stop i32) ($left i32) ($count i32)
           ($character-kind i32) ($needle i32)
           ($value :nullable-object) ($function :nullable-object))
  #@(local.set $string (i32.eq (call $tag (local.get $sequence)) (i32.const 10)))
  #@(local.set $length (call $proper-sequence-length (local.get $sequence)))
  #@(local.set $stop (if (result i32) (ref.is_null (local.get $end))
    (then (local.get $length)) (else (call $bounded-index (local.get $end) (local.get $length)))))
  #@(local.set $i (call $bounded-index (local.get $start) (local.get $stop)))
  #@(local.set $left (i32.sub (local.get $stop) (local.get $i)))
  #@(if (i32.eqz (ref.is_null (local.get $key))) (then (local.set $key (call $callable (local.get $key)))))
  #@(if (local.get $mode)
    (then (local.set $function (call $callable (local.get $item))))
    (else (local.set $function (call $callable (if (result (ref null eq)) (local.get $negate)
      (then (local.get $test-not)) (else (local.get $test)))))))
  #@(if (i32.and (i32.ne (local.get $left) (i32.const 0)) (i32.and (local.get $string)
        (i32.and (i32.eqz (local.get $mode)) (ref.is_null (local.get $key)))))
    (then
      (local.set $character-kind (call $sequence-character-kind (local.get $function)))
      (if (local.get $character-kind)
        (then
          (local.set $needle (call $sequence-character-needle (local.get $character-kind) (local.get $item)))
          (local.set $count (call $sequence-character-scan
            (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $sequence))))
            (local.get $i) (local.get $stop) (local.get $needle)
            (i32.eq (local.get $character-kind) (i32.const 3)) (local.get $negate)
            (i32.eqz (ref.is_null (local.get $from-end))) (i32.const 1)))
          (call $clear-values) (return (call $int (local.get $count)))))))
  #@(local.set $value (call $count-sequence-value-cl (local.get $item) (local.get $sequence) (local.get $function)
    (call $vector-new (array.new_fixed $objects 4 (local.get $key) (call $int (local.get $i))
      (call $int (local.get $stop))
      (call $int (i32.or (local.get $mode) (i32.or (i32.shl (local.get $negate) (i32.const 2))
        (i32.shl (i32.eqz (ref.is_null (local.get $from-end))) (i32.const 3)))))))))
  #@(call $clear-values)
  #@(local.get $value))

;; Decode the fixed-size native calling convention once per public call.
(defun list-operation-argument (n)
  `(call $car ,(loop with form = '(local.get $args) repeat n
                    do (setf form `(call $cdr ,form)) finally (return form))))

(defun $check-keyword-pairs ()
  (runtime-function-definition '$check-keyword-pairs
    (wat-fragment (miricl-cross:runtime-kernel-wat '$check-keyword-pairs 'miricl-runtime:check-keyword-pairs))))

(define-runtime-function $check-keywords-native
    (($args :nullable-object) ($allowed :nullable-object) ($allow i32)) (:nullable-object)
  #@(call $check-apply-list (local.get $args))
  #@(call $check-apply-list (local.get $allowed))
  #@(call $check-keyword-pairs (local.get $args) (local.get $allowed) (call $bool (local.get $allow))))

(define-runtime-function $list-primitive (($op i32) ($args :nullable-object)) (:nullable-object)
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%proclaim-variable)))
    (then ,(wat-arity 1)
      (call $check-variable (call $car (local.get $args)))
      (call $proclaim-special (call $car (local.get $args)))
      (return (ref.null eq))))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%reader-dispatch-sub)))
    (then ,(wat-arity 4)
      (return (call $read-input-action ,(list-operation-argument 0) (i32.const 1) (i32.const 1)
        (call $symbol (i32.const 1)) (ref.null eq)
        (i32.add (i32.const 256) (call $sequence-index ,(list-operation-argument 1)))
        (call $character-code ,(list-operation-argument 2)) ,(list-operation-argument 3)))))
  (runtime-wat-forms
    (loop for (code name) in '((34 %%reader-string) (35 %%reader-dispatch) (39 %%reader-quote)
                              (40 %%reader-list) (41 %%reader-close) (44 %%reader-comma)
                              (59 %%reader-comment) (96 %%reader-backquote)) collect
      `(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op name)))
        (then ,(wat-arity 2)
          (return (call $read-input-action ,(list-operation-argument 0) (i32.const 1) (i32.const 1)
            (call $symbol (i32.const 1)) (ref.null eq) (i32.const ,code)
            (call $character-code ,(list-operation-argument 1)) (ref.null eq)))))))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%sequence-construction)))
    (then (return (call $sequence-construction-primitive (local.get $args)))))
  (runtime-wat-forms
    (loop for (name arity function arguments) in
      '((%check-keys 3 $check-keywords-native (0 1 (:truth 2)))
        (%%list-copy 2 $copy-spine (0 (:index 1)))
        (%%make-list 2 $make-list-native (0 1))
        (%%pairlis 3 $pairlis-native (0 1 2))
        (%%nthcdr 2 $nthcdr-native (0 1))
        (%%list-last 2 $last-native (0 1))
        (%%list-butlast 3 $butlast-native (0 1 (:truth 2)))
        (%%tail-difference 3 $tail-difference (0 1 (:truth 2)))
        (%%list-length 1 $list-length-native (0))
        (%%reverse-onto 3 $reverse-onto (0 1 (:truth 2)))
        (%%reverse-sequence 2 $reverse-sequence (0 (:truth 1)))
        (%%map-lists 3 $map-lists-native (0 1 (:index 2) (:null)))
        (%%map-sequences 3 $map-sequences-native (0 1 2))
        (%%count-sequence 9 $count-sequence-native ((:index 0) 1 2 3 4 5 6 7 8 (:truth 5)))
        (%%search-list 7 $search-list-native ((:index 0) (:index 1) 2 3 4 5 6))
        (%%oddp 1 $oddp-native (0))
        (%%string-element 4 $string-element (0 1 2 (:truth 3)))
        (%%list-set 6 $list-set-native ((:index 0) 1 2 3 4 5))
        (%%directory 2 $directory-native (0 (:truth 1)))
        (%%make-logical-path 2 $make-logical-path (0 1))
        (%%host-service 2 $host-service-object (0 1))
        (%%javascript 4 $javascript ((:index 0) 1 2 3))
        (%%thread 3 $thread-operation ((:index 0) 1 2))
        (%%calendar 3 $calendar-native ((:index 0) 1 2))
        (%%pathname-metadata 2 $pathname-metadata (0 1))
        (%%pathname-match 2 $pathname-match-native (0 1))
        (%%filesystem 3 $filesystem-native ((:index 0) 1 2))
        (%%open-file 8 $open-file-native (0 1 (:index 2) (:index 3) (:index 4) 5 (:index 6) (:truth 7)))
        (%%structure-access 5 $structure-access (0 1 2 3 (:truth 4)))
        (%%structure-copy 1 $structure-copy (0))
        (%%structure-make 3 $structure-make (0 1 2))
        (%%typed-structure-p 2 $typed-structure-p (0 1))
        (%%structure-function 4 $direct-function (0 1 2 (:index 3)))
        (%%hash-cell 2 $hash-cell-native (0 1))
        (%%incf-expander 2 $modifier-expansion (0 1 (:constant 0)))
        (%%decf-expander 2 $modifier-expansion (0 1 (:constant 1)))
        (%%when-expander 2 $conditional-expansion (0 (:constant 0)))
        (%%unless-expander 2 $conditional-expansion (0 (:constant 1)))
        (%%cond-expander 2 $conditional-expansion (0 (:constant 2)))
        (%%case-expander 2 $case-expansion (0 (:constant 0)))
        (%%typecase-expander 2 $case-expansion (0 (:constant 1)))
        (%%ecase-expander 2 $case-expansion (0 (:constant 2)))
        (%%etypecase-expander 2 $case-expansion (0 (:constant 3)))
        (%%ccase-expander 2 $case-expansion (0 (:constant 4)))
        (%%ctypecase-expander 2 $case-expansion (0 (:constant 5)))
        (%%psetf-expander 2 $parallel-place-expansion (0 1 (:constant 0)))
        (%%rotatef-expander 2 $parallel-place-expansion (0 1 (:constant 1)))
        (%%shiftf-expander 2 $parallel-place-expansion (0 1 (:constant 2)))
        (%%multiple-value-setq-expander 2 $parallel-place-expansion (0 1 (:constant 3)))
        (%%remf-expander 2 $parallel-place-expansion (0 1 (:constant 4)))
        (%%pushnew-expander 2 $parallel-place-expansion (0 1 (:constant 5)))
        (%%hash-iterator-state 1 $hash-iterator-state (0))
        (%%hash-iterator-next 1 $hash-iterator-next (0))
        (%%package-iterator-state 2 $package-iterator-state (0 (:index 1)))
        (%%package-iterator-next 1 $package-iterator-next (0))
        (%%package-symbol-list 2 $package-symbol-list (0 (:index 1)))
        (%%constant-macro-function 1 $constant-macro-function (0))
        (%%formatter-function 1 $formatter-function (0))
        (%%formatter-expander 2 $formatter-expansion (0))
        (%%pretty-start 4 $pretty-start (0 1 2 (:truth 3)))
        (%%pretty-event 4 $pretty-event (0 (:index 1) 2 3))
        (%%pretty-finish 1 $pretty-finish (0))
        (%%pretty-call 2 $pretty-call (0 1))
        (%%pretty-reference 3 $pretty-reference (0 1 (:index 2)))
        (%%print-structure 2 $print-structure-default (0 1))
        (%%pretty-next 2 $pretty-next (0 1))
        (%%pretty-list-body 4 $pretty-list-body (0 1 (:index 2) 3))
        (%%object-identity 1 $object-identity (0))
        (%%format-logical-body 3 $format-logical-body (0 1 2))
        (%%hash-test-designator-p 1 $hash-test-designator-p (0))
        (%%function-name-symbol 1 $function-name-symbol (0))
        (%%function-documentation 3 $function-documentation (0 1 (:truth 2)))
        (%%psetq-expander 2 $psetq-expansion (0 1))
        (%%define-symbol-macro 2 $define-symbol-macro (0 1))
        (%%setf-expander 4 $setf-expander-access (1 2 3 (:truth 0)))
        (%%loop-expander 2 $loop-expansion (0))
        (%%logical-expander 2 $logical-expansion (0))
        (%%property-list-edit 4 $property-list-edit-values (0 1 2 (:truth 3)))
        (%%remove-sequence 12 $remove-sequence-native ((:index 0) 1 2 3 4 5 (:truth 6) 7 8 (:truth 9) 10 (:truth 11) (:null) (:constant 0)))
        (%%find-sequence 11 $find-sequence-native ((:index 0) 1 2 3 4 5 (:truth 6) 7 8 (:truth 9) (:truth 10)))
        (%%fresh-line 1 $fresh-line-native (0))
        (%%check-generic-arity 2 $check-generic-arity (0 1))
        (%%generic-discriminator 2 $generic-discriminator (0 1))
        (%%define-condition 1 $define-condition (0 (:null)))
        (%%dispatch-eql-key 2 $dispatch-eql-key (0 1))
        (%pop-arg 2 $pop-argument (0 1))
        (%list-eq 2 $list-eq (0 1))
        (%nth 2 $method-context-element (0 1))
        (%%find-method 3 $find-method-list (0 1 2))
        (%invoke-method 5 $invoke-method (0 1 2 3 4 (:constant 0)))
        (%invoke-method-without-next 3 $invoke-method (0 1 (:null) 2 (:null) (:constant 1)))
        (%ensure-layout 1 $ensure-instance-layout (0))
        (%%property-method 2 $direct-function (0 (:null) 1 (:constant 3)))
        (%%property-effective 4 $property-effective (0 1 2 3))
        (%%values-expander 2 $values-expansion (0))
        (%%condition-method 4 $direct-function (0 1 2 (:index 3)))
        (%find-slot 2 $find-instance-slot (0 1))
        (%%slot-operation 4 $instance-slot-operation ((:index 0) 1 2 3))
        (%%restore-saved-slots 4 $restore-saved-slots (0 1 2 3))
        (%%builtin-macro 2 $direct-function (1 0 (:null) (:constant 7)))
        (%%define-constant 2 $define-constant (0 1))
        (%%define-package 2 $defpackage (0 1)))
      collect `(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op name)))
        (then ,(wat-arity arity)
          (return (call ,function ,@(loop for argument in arguments collect
            (if (integerp argument) (list-operation-argument argument)
              (ecase (car argument)
                (:null '(ref.null eq))
                (:constant `(i32.const ,(second argument)))
                (:index `(call $sequence-index ,(list-operation-argument (second argument))))
                (:truth `(i32.eqz (ref.is_null ,(list-operation-argument (second argument))))))))))))))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%storage-state)))
    (then ,(wat-arity 0)
      (return (call $three-values
        (call $uint64 (i64.shl (i64.extend_i32_u (memory.size)) (i64.const 16)))
        (call $int (global.get $next-symbol))
        (call $int (call $length (global.get $packages)))))))
  #@(if (i32.eq (local.get $op) (i32.const ,(clos-primitive-op '%%package-documentation)))
    (then ,(wat-arity 3)
      (if (i32.ne (call $tag ,(list-operation-argument 0)) (i32.const ,(runtime-object-tag :package)))
        (then (call $list-type-error ,(list-operation-argument 0) (i32.const ,(runtime-symbol-id 'package)))))
      (if (i32.eqz (ref.is_null ,(list-operation-argument 2)))
        (then
          (struct.set $obj $c (ref.cast (ref $obj) (struct.get $obj $c (ref.cast (ref $obj) ,(list-operation-argument 0)))) ,(list-operation-argument 1))
          (return ,(list-operation-argument 1))))
      (return (struct.get $obj $c (ref.cast (ref $obj) (struct.get $obj $c (ref.cast (ref $obj) ,(list-operation-argument 0))))))))
  (wat-fail :unsupported)
  #@(ref.null eq))

(define-runtime-section list-operations-runtime-wat ()
  ($check-apply-list) ($check-apply-arguments) ($pop-argument-cl) ($pop-argument) ($list-eq) ($find-method-list)
  ($check-generic-arguments) ($check-generic-arity)
  ($check-keyword-pairs) ($check-keywords-native)
  ($list-type-error) ($require-list-cl) ($require-list) ($natural-count-cl) ($natural-count) ($copy-list-spine) ($copy-alist-spine) ($copy-tree-value-cl) ($copy-spine)
  ($reverse-copy-onto) ($reverse-mutate-onto) ($reverse-onto) ($reverse-sequence)
  ($make-list-bounded) ($make-list-native) ($nthcdr-bounded) ($nthcdr-general) ($nthcdr-native) ($last-bounded) ($last-native)
  ($copy-butlast-bounded) ($truncate-butlast-bounded) ($butlast-native)
  ($copy-before-tail) ($tail-member-p) ($tail-difference)
  ($list-length-native) ($pair-lists-onto) ($pairlis-native) ($callable) ($mapping-argument-cl) ($mapping-argument) ($map-lists-cl) ($map-lists-native) ($map-sequence-result-cl) ($map-sequences-native) ($map-into-apply) ($bounded-index-cl) ($bounded-index)
  ($sequence-character-kind) ($sequence-character-needle) ($simple-predicate-value) ($sequence-key-value) ($sequence-item-match)
  ($search-list-cl) ($search-list-native) ($list-exclusive-cl) ($list-exclusive-native) ($list-set-apply) ($list-set-values-cl) ($list-set-native) ($count-sequence-value-cl) ($count-sequence-native) ($list-primitive))
