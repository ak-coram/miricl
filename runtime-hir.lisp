;;;; Register execution for compact HIR. Code is validated and privately copied
;;;; before it becomes callable; representation-sensitive values are checked.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun hir-field (object index)
  `(array.get $objects (call $vector-data ,object) (i32.const ,index)))
(defun hir-input (index &optional raw)
  ;; Read the operand's register directly. Gathering all operands into a
  ;; second object array adds a copy to every instruction and retains values
  ;; unnecessarily across calls. The shared decoder supplies checked IDs.
  (let ((form `(array.get $objects (local.get $registers)
                 (array.get $digits (local.get $decoded)
                   ,(if (integerp index) `(i32.const ,index) index)))))
    (if raw form `(call $hir-lisp-value ,form))))
(defun hir-literal () '(array.get $objects (local.get $literals) (local.get $a)))
(defun hir-native (function &rest args)
  (native-object `(call ,function ,@(mapcar #'native-external args))))
(defun hir-context-input () `(extern.convert_any (call $hir-context ,(hir-input 0 t))))
(defun hir-list-inputs (start &optional groups)
  `(call $hir-input-list (local.get $registers) (local.get $decoded) (i32.const ,start) (local.get $count)
     (i32.const ,(if groups 1 0))))
(defun hir-primary-call-wat (&optional arity)
  ;; Reuse the native backend's checked, redefinition-aware small-call entries.
  ;; Only CALL-PRIMARY may discard additional values. Debugger evaluation and
  ;; larger calls retain the general application path.
  (let ((fallback `(call $eval-apply ,(hir-input 1) ,(hir-list-inputs 2))))
    (if arity
      `(if (result (ref null eq)) (global.get $debug-evaluating)
         (then ,fallback)
         (else ,(apply #'hir-native (intern (format nil "$NATIVE-CALL~D" arity) :miricl)
           (hir-input 1) (loop for i below arity collect (hir-input (+ 2 i))))))
      fallback)))
(defun hir-wat-sequence (&rest forms)
  (apply #'runtime-wat-sequence
    (mapcar (lambda (form) (if (consp form) (wat-string form) form)) (remove nil forms))))

(define-runtime-function $hir-invalid (($value :nullable-object) ($reason :nullable-object)) ()
  (:locals ($datum :nullable-object))
  ;; Private contexts are not Lisp objects and must never reach a Lisp printer.
  #@(if (i32.or (ref.test (ref null $obj) (local.get $value)) (ref.test (ref i31) (local.get $value)))
    (then (local.set $datum (local.get $value))))
  #@(drop (call $apply ,(clos-library-reference 'miricl-compiler::hir-code-error)
    (call $cons (local.get $datum) (call $cons (local.get $reason) (ref.null eq)))))
  #@(unreachable))

(define-runtime-function $hir-lisp-value (($value :nullable-object)) (:nullable-object)
  #@(if (i32.eqz (i32.or (ref.test (ref null $obj) (local.get $value))
    (ref.test (ref i31) (local.get $value))))
    (then (call $hir-invalid (local.get $value) ,(wat-text-literal "private context used as a Lisp value"))))
  #@(local.get $value))

(define-runtime-function $hir-context (($value :nullable-object)) ((ref $native-context))
  #@(if (i32.eqz (ref.test (ref $native-context) (local.get $value)))
    (then (call $hir-invalid (local.get $value) ,(wat-text-literal "expected a dynamic environment"))))
  #@(ref.cast (ref $native-context) (local.get $value)))

(define-runtime-function $hir-values (($value :nullable-object)) (:nullable-object)
  #@(drop (call $hir-lisp-value (local.get $value)))
  #@(call $check-apply-list (local.get $value))
  #@(local.get $value))

(define-runtime-function $hir-input-list
    (($registers (ref $objects)) ($decoded (ref $digits)) ($start i32) ($end i32) ($groups i32)) (:nullable-object)
  (:locals ($list :nullable-object) ($value :nullable-object))
  #@(block $done (loop $next (br_if $done (i32.le_u (local.get $end) (local.get $start)))
    (local.set $end (i32.sub (local.get $end) (i32.const 1)))
    (local.set $value ,(hir-input '(local.get $end)))
    (if (local.get $groups) (then (drop (call $hir-values (local.get $value)))))
    (local.set $list (call $cons (local.get $value) (local.get $list))) (br $next)))
  #@(local.get $list))

(define-runtime-function $hir-capture-vector
    (($registers (ref $objects)) ($decoded (ref $digits)) ($count i32)) (:nullable-object)
  (:locals ($data (ref $objects)) ($i i32))
  ;; Captures are already indexed operands. Construct their private vector
  ;; directly instead of allocating and then traversing a temporary list.
  #@(local.set $data (array.new_default $objects (local.get $count)))
  #@(block $done (loop $next (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
    (array.set $objects (local.get $data) (local.get $i) ,(hir-input '(local.get $i)))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(call $vector-new (local.get $data)))

(define-runtime-function $hir-word (($bytes (ref $digits)) ($pc i32)) (i32 i32)
  (:locals ($value i32) ($shift i32) ($byte i32))
  ;; Most compact register IDs and immediate operands occupy one byte. Keep
  ;; that checked path outside the shift/continuation loop; public readers
  ;; retain exactly the same bounds, byte and 31-bit overflow checks.
  #@(if (i32.ge_u (local.get $pc) (array.len (local.get $bytes)))
    (then (call $hir-invalid (call $int (local.get $pc)) ,(wat-text-literal "truncated HIR operand"))))
  #@(local.set $byte (array.get $digits (local.get $bytes) (local.get $pc)))
  #@(local.set $pc (i32.add (local.get $pc) (i32.const 1)))
  #@(if (i32.lt_u (local.get $byte) (i32.const 128))
    (then (local.get $byte) (local.get $pc) (return)))
  #@(if (i32.gt_u (local.get $byte) (i32.const 255))
    (then (call $hir-invalid (call $int (local.get $byte)) ,(wat-text-literal "HIR operand exceeds 31 bits"))))
  #@(local.set $value (i32.and (local.get $byte) (i32.const 127)))
  #@(local.set $shift (i32.const 7))
  #@(loop $next
    (if (i32.ge_u (local.get $pc) (array.len (local.get $bytes)))
      (then (call $hir-invalid (call $int (local.get $pc)) ,(wat-text-literal "truncated HIR operand"))))
    (local.set $byte (array.get $digits (local.get $bytes) (local.get $pc)))
    (if (i32.or (i32.gt_u (local.get $byte) (i32.const 255))
          (i32.and (i32.eq (local.get $shift) (i32.const 28)) (i32.ge_u (local.get $byte) (i32.const 8))))
      (then (call $hir-invalid (call $int (local.get $byte)) ,(wat-text-literal "HIR operand exceeds 31 bits"))))
    (local.set $pc (i32.add (local.get $pc) (i32.const 1)))
    (local.set $value (i32.or (local.get $value)
      (i32.shl (i32.and (local.get $byte) (i32.const 127)) (local.get $shift))))
    (local.set $shift (i32.add (local.get $shift) (i32.const 7)))
    (br_if $next (i32.ge_u (local.get $byte) (i32.const 128))))
  #@(local.get $value) #@(local.get $pc))

(define-runtime-function $hir-read-instruction
    (($bytes (ref $digits)) ($pc i32) ($words (ref $digits))) (i32 i32 i32 i32 i32 i32)
  (:locals ($header i32) ($opcode i32) ($shape i32) ($output i32) ($count i32) ($i i32) ($word i32)
           ($a i32) ($b i32))
  #@(if (i32.ge_u (local.get $pc) (array.len (local.get $bytes)))
    (then (call $hir-invalid (call $int (local.get $pc)) ,(wat-text-literal "invalid HIR decoder bounds"))))
  #@(local.set $header (array.get $digits (local.get $bytes) (local.get $pc)))
  #@(local.set $pc (i32.add (local.get $pc) (i32.const 1)))
  #@(local.set $opcode (i32.and (local.get $header) (i32.const 63)))
  #@(if (i32.or (i32.gt_u (local.get $header) (i32.const 255))
                (i32.ge_u (local.get $opcode) (array.len (global.get $hir-operation-shapes))))
    (then (call $hir-invalid (call $int (local.get $header)) ,(wat-text-literal "unknown HIR opcode"))))
  #@(local.set $shape (array.get $digits (global.get $hir-operation-shapes) (local.get $opcode)))
  #@(if (i32.and (local.get $shape) (i32.const 256))
    (then (call $hir-word (local.get $bytes) (local.get $pc)) (local.set $pc) (local.set $output)
      (local.set $output (i32.add (local.get $output) (i32.const 1)))))
  #@(local.set $count (i32.and (local.get $shape) (i32.const 255)))
  ;; Fixed arities come from the operation table itself. Only encoded counts
  ;; need checking against the operation's input-count limits.
  #@(if (i32.eq (local.get $count) (i32.const 255))
    (then (call $hir-word (local.get $bytes) (local.get $pc)) (local.set $pc) (local.set $count)
      (if (i32.or
          (i32.lt_u (local.get $count) (i32.and (i32.shr_u (local.get $shape) (i32.const 11)) (i32.const 15)))
          (i32.and (i32.ne (i32.shr_u (local.get $shape) (i32.const 15)) (i32.const 0))
            (i32.ge_u (local.get $count) (i32.shr_u (local.get $shape) (i32.const 15)))))
        (then (call $hir-invalid (call $int (local.get $count)) ,(wat-text-literal "invalid HIR operand count"))))))
  #@(if (i32.or (i32.gt_u (local.get $count) (array.len (local.get $words)))
                (i32.gt_u (local.get $count) (i32.sub (array.len (local.get $bytes)) (local.get $pc))))
    (then (call $hir-invalid (call $int (local.get $count)) ,(wat-text-literal "truncated HIR inputs or insufficient decoder storage"))))
  #@(block $done (loop $inputs
    (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
    ;; The initial bounds check reserves one byte for every remaining input.
    ;; One-byte words preserve that invariant; after a longer word, check the
    ;; remaining reservation before the next direct byte read.
    (local.set $word (array.get $digits (local.get $bytes) (local.get $pc)))
    (if (i32.lt_u (local.get $word) (i32.const 128))
      (then (local.set $pc (i32.add (local.get $pc) (i32.const 1))))
      (else
        (call $hir-word (local.get $bytes) (local.get $pc)) (local.set $pc) (local.set $word)
        (if (i32.gt_u (i32.sub (local.get $count) (i32.add (local.get $i) (i32.const 1)))
                     (i32.sub (array.len (local.get $bytes)) (local.get $pc)))
          (then (call $hir-invalid (call $int (local.get $pc)) ,(wat-text-literal "truncated HIR operand"))))))
    (array.set $digits (local.get $words) (local.get $i) (local.get $word))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $inputs)))
  #@(if (i32.ge_u (i32.and (local.get $shape) (i32.const 1536)) (i32.const 512))
    (then (call $hir-word (local.get $bytes) (local.get $pc)) (local.set $pc) (local.set $a)))
  #@(if (i32.ge_u (i32.and (local.get $shape) (i32.const 1536)) (i32.const 1024))
    (then (call $hir-word (local.get $bytes) (local.get $pc)) (local.set $pc) (local.set $b)))
  ;; The execution loop consumes metadata directly. Only indexed operands
  ;; need scratch storage; the public reader still exposes its original API.
  #@(local.get $pc) #@(local.get $header) #@(local.get $output)
  #@(local.get $count) #@(local.get $a) #@(local.get $b))

(define-runtime-function $hir-cached-block
    (($code :nullable-object) ($words (ref $digits))) ((ref null $objects))
  (:locals ($bytes (ref $digits)) ($map (ref $objects)) ($inputs (ref $digits))
           ($cached :nullable-object) ($slot i32) ($hash i32) ($pc i32) ($start i32)
           ($header i32) ($output i32) ($count i32) ($a i32) ($b i32))
  #@(local.set $bytes (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $code)))))
  ;; Only small, revisited blocks enter the cache. Cold or larger blocks use
  ;; the ordinary checked reader; these are memory limits, not program limits.
  #@(if (i32.gt_u (array.len (local.get $bytes)) (i32.const 1024))
    (then (return (ref.null $objects))))
  #@(local.set $hash (i32.shr_u (struct.get $obj $tag (ref.cast (ref $obj) (local.get $code))) (i32.const 6)))
  #@(local.set $slot (i32.and (i32.xor (local.get $hash) (i32.shr_u (local.get $hash) (i32.const 6))) (i32.const 63)))
  #@(if (i32.eqz (ref.eq (array.get $objects (global.get $hir-cache-keys) (local.get $slot)) (local.get $code)))
    (then
      (array.set $objects (global.get $hir-cache-keys) (local.get $slot) (local.get $code))
      (array.set $objects (global.get $hir-cache-maps) (local.get $slot) (ref.null eq))
      (return (ref.null $objects))))
  #@(local.set $cached (array.get $objects (global.get $hir-cache-maps) (local.get $slot)))
  #@(if (i32.eqz (ref.is_null (local.get $cached)))
    (then (return (ref.cast (ref $objects) (local.get $cached)))))
  #@(local.set $map (array.new_default $objects (array.len (local.get $bytes))))
  #@(block $done (loop $decode (br_if $done (i32.ge_u (local.get $pc) (array.len (local.get $bytes))))
    (local.set $start (local.get $pc))
    (call $hir-read-instruction (local.get $bytes) (local.get $pc) (local.get $words))
    (local.set $b) (local.set $a) (local.set $count)
    (local.set $output) (local.set $header) (local.set $pc)
    (local.set $inputs (if (result (ref $digits)) (local.get $count)
      (then (array.new_default $digits (local.get $count))) (else (global.get $hir-empty-inputs))))
    (array.copy $digits $digits (local.get $inputs) (i32.const 0) (local.get $words) (i32.const 0) (local.get $count))
    ;; The size limit and checked decoder bound PC and COUNT to 1024. Pack
    ;; their 11-bit fields with the 8-bit header and unsigned 32-bit output.
    (array.set $objects (local.get $map) (local.get $start)
      (struct.new $hir-decoded-instruction
        (i64.or (i64.extend_i32_u (local.get $header))
          (i64.or (i64.shl (i64.extend_i32_u (local.get $count)) (i64.const 8))
            (i64.or (i64.shl (i64.extend_i32_u (local.get $pc)) (i64.const 19))
              (i64.shl (i64.extend_i32_u (local.get $output)) (i64.const 30)))))
        (i64.or (i64.extend_i32_u (local.get $a))
          (i64.shl (i64.extend_i32_u (local.get $b)) (i64.const 32)))
        (local.get $inputs)))
    (br $decode)))
  ;; Construction neither calls Lisp nor yields. Publish only complete maps.
  #@(array.set $objects (global.get $hir-cache-maps) (local.get $slot) (local.get $map))
  #@(local.get $map))

(define-runtime-function $hir-read-instruction-primitive (($args :nullable-object)) (:nullable-object)
  (:locals ($code :nullable-object) ($scratch :nullable-object) ($data (ref $objects))
           ($words (ref $digits)) ($pc i32) ($i i32) ($count i32)
           ($header i32) ($output i32) ($a i32) ($b i32))
  (wat-arity 3)
  #@(local.set $code (call $car (local.get $args)))
  #@(local.set $scratch (call $car (call $cdr (call $cdr (local.get $args)))))
  #@(if (i32.or (i32.ne (call $tag (local.get $code)) (i32.const ,(runtime-object-tag :string)))
                (i32.eqz (ref.test (ref $vector) (local.get $scratch))))
    (then (call $hir-invalid (local.get $code) ,(wat-text-literal "HIR decoder requires a byte string and scratch vector"))))
  #@(local.set $pc (call $natural-count (call $car (call $cdr (local.get $args)))))
  #@(local.set $data (call $vector-data (local.get $scratch)))
  #@(if (i32.lt_u (array.len (local.get $data)) (i32.const 6))
    (then (call $hir-invalid (local.get $scratch) ,(wat-text-literal "invalid HIR decoder bounds"))))
  ;; Successful decoding cannot call Lisp or yield. A failed decode never
  ;; resumes, so nested debugger/compiler use cannot expose partial scratch.
  #@(local.set $words (ref.as_non_null (global.get $hir-decoder-words)))
  #@(if (i32.lt_u (array.len (local.get $words)) (i32.sub (array.len (local.get $data)) (i32.const 6)))
    (then (local.set $words (array.new_default $digits (i32.sub (array.len (local.get $data)) (i32.const 6))))
      (global.set $hir-decoder-words (local.get $words))))
  #@(call $hir-read-instruction
    (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $code)))) (local.get $pc) (local.get $words))
  #@(local.set $b) #@(local.set $a) #@(local.set $count)
  #@(local.set $output) #@(local.set $header) #@(local.set $pc)
  ;; The reusable native buffer can be larger than this caller's vector.
  ;; Check that vector before publishing any decoded fields into it.
  #@(if (i32.gt_u (local.get $count) (i32.sub (array.len (local.get $data)) (i32.const 6)))
    (then (call $hir-invalid (local.get $scratch)
      ,(wat-text-literal "truncated HIR inputs or insufficient decoder storage"))))
  (runtime-wat-forms
    (loop for value in '((i32.and (local.get $header) (i32.const 63))
                        (i32.shr_u (local.get $header) (i32.const 6))
                        (local.get $output) (local.get $count) (local.get $a) (local.get $b))
          for index from 0 collect
      `(array.set $objects (local.get $data) (i32.const ,index)
        (call $int64 (i64.extend_i32_u ,value)))))
  #@(block $done (loop $copy (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
    (array.set $objects (local.get $data) (i32.add (local.get $i) (i32.const 6))
      (call $int64 (i64.extend_i32_u (array.get $digits (local.get $words) (local.get $i)))))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $copy)))
  #@(call $int (local.get $pc)))

(define-runtime-function $hir-unpack-blocks (($code :nullable-object)) (:nullable-object)
  (:locals ($bytes (ref $digits)) ($blocks (ref $objects)) ($data (ref $digits))
           ($pc i32) ($count i32) ($i i32) ($parameter i32) ($environment i32) ($length i32))
  #@(if (i32.ne (call $tag (local.get $code)) (i32.const ,(runtime-object-tag :string)))
    (then (call $hir-invalid (local.get $code) ,(wat-text-literal "HIR block table must be a byte string"))))
  #@(local.set $bytes (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj) (local.get $code)))))
  #@(call $hir-word (local.get $bytes) (i32.const 0)) #@(local.set $pc) #@(local.set $count)
  #@(if (i32.gt_u (local.get $count) (i32.div_u
      (i32.sub (array.len (local.get $bytes)) (local.get $pc)) (i32.const 3)))
    (then (call $hir-invalid (call $int (local.get $count)) ,(wat-text-literal "block count exceeds encoded HIR table"))))
  #@(local.set $blocks (array.new_default $objects (local.get $count)))
  #@(block $done (loop $next (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
    (call $hir-word (local.get $bytes) (local.get $pc)) (local.set $pc) (local.set $parameter)
    (call $hir-word (local.get $bytes) (local.get $pc)) (local.set $pc) (local.set $environment)
    (call $hir-word (local.get $bytes) (local.get $pc)) (local.set $pc) (local.set $length)
    (if (i32.gt_u (local.get $length) (i32.sub (array.len (local.get $bytes)) (local.get $pc)))
      (then (call $hir-invalid (call $int (local.get $length)) ,(wat-text-literal "truncated HIR block code"))))
    (local.set $data (array.new_default $digits (local.get $length)))
    (array.copy $digits $digits (local.get $data) (i32.const 0) (local.get $bytes) (local.get $pc) (local.get $length))
    (array.set $objects (local.get $blocks) (local.get $i)
      (call $vector-new (array.new_fixed $objects 3 (call $int (local.get $parameter))
        (call $int (local.get $environment)) (call $text (local.get $data)))))
    (local.set $pc (i32.add (local.get $pc) (local.get $length)))
    (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $next)))
  #@(if (i32.ne (local.get $pc) (array.len (local.get $bytes)))
    (then (call $hir-invalid (call $int (local.get $pc)) ,(wat-text-literal "trailing HIR block table data"))))
  #@(call $vector-new (local.get $blocks)))

(define-runtime-function $hir-control-data
    (($registers (ref $objects)) ($decoded (ref $digits)) ($count i32) ($mode i32)) (:nullable-object)
  (:locals ($i i32) ($data :nullable-object))
  #@(if (i32.or (i32.eqz (local.get $mode)) (i32.eq (local.get $mode) (i32.const 3)))
    (then (return ,(hir-input 3))))
  #@(if (i32.and (i32.ge_u (local.get $mode) (i32.const 4)) (i32.le_u (local.get $mode) (i32.const 5)))
    (then
      (local.set $i (i32.const 6))
      (block $done (loop $options (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
        (drop (call $hir-values ,(hir-input '(local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 3))) (br $options)))))
  #@(if (i32.eq (local.get $mode) (i32.const 6))
    (then (drop (call $hir-values ,(hir-input 5)))))
  #@(call $hir-input-list (local.get $registers) (local.get $decoded)
    (i32.const 4) (local.get $count) (i32.const 0)))

(define-runtime-function $hir-function
    (($payload :nullable-object) ($index i32) ($captures :nullable-object)) (:nullable-object)
  ;; Only internal calls use this constructor; the public primitive below
  ;; calls VALIDATE-HIR-CODE and supplies the root with no captures.
  (wat-new-object #@(i32.const 8) :n #@(i32.const 2147483641)
    :f #@(f64.convert_i32_u (local.get $index)) :a #@(local.get $payload)
    :b #@(local.get $captures)
    :c (wat-fragment (wat-string (hir-field
         `(array.get $objects (call $vector-data ,(hir-field '(local.get $payload) 1)) (local.get $index)) 0)))))

(define-runtime-function $hir-load (($args :nullable-object)) (:nullable-object)
  (:locals ($payload :nullable-object))
  (wat-arity 1)
  #@(local.set $payload (call $apply ,(clos-library-reference 'miricl-compiler::validate-hir-code)
    (local.get $args)))
  #@(call $hir-function (local.get $payload) (i32.const 0)
    (call $vector-new (array.new_default $objects (i32.const 0)))))

(defun hir-read-word (local)
  (hir-wat-sequence
    #@(call $hir-word (ref.as_non_null (local.get $bytes)) (local.get $pc))
    #@(local.set $pc) `(local.set ,local)))

(defun hir-operation-wat (opcode &optional call-arity)
  (let ((result nil) (effects nil))
    (case opcode
      (:constant (setf result (hir-literal)))
      ((:argument :rest-argument)
       (setf result (native-object `(call ,(if (eq opcode :argument) '$native-argument '$native-rest-argument)
         (extern.convert_any (local.get $args)) (local.get $a)))))
      (:check-arguments (push #@(call $native-check-arguments (extern.convert_any (local.get $args))
        (local.get $a) (i32.sub (local.get $b) (i32.const 1))) effects))
      (:truth (setf result `(call $bool (i32.eqz (ref.is_null ,(hir-input 0))))))
      ((:function-reference :function-reference-inline :function-reference-notinline)
       (setf result (hir-native '$native-function-reference (hir-literal))))
      (:global-read (setf result `(call $symbol-value ,(hir-literal))))
      (:global-write (setf result `(call $set-symbol-value ,(hir-literal) ,(hir-input 1))))
      (:dynamic-bind (setf result (native-object `(call $native-bind ,(hir-context-input)
        (extern.convert_any ,(hir-literal)) (extern.convert_any ,(hir-input 1))))))
      (:environment-restore (push `(call $native-restore-environment ,(hir-context-input)) effects))
      ((:values :list) (setf result (hir-list-inputs 0)))
      (:values-list (setf result (hir-input 0)))
      ((:value-ref :checked-argument) (setf result (native-object `(call $native-argument
        (extern.convert_any (call $hir-values ,(hir-input 0))) (local.get $a)))))
      (:cell-new (setf result `(call $cons ,(hir-input 0) (ref.null eq))))
      ((:cell-read :primary) (setf result `(call $car ,(hir-input 0))))
      (:cell-write
       (push `(local.set $value ,(hir-input 0)) effects)
       (push (wat-fail-when #@(i32.ne (call $tag (local.get $value)) (i32.const 7)) :malformed-form) effects)
       (push `(struct.set $obj $a (ref.cast (ref $obj) (local.get $value)) ,(hir-input 1)) effects)
       (setf result (hir-input 1)))
      (:call (setf result `(call $value-list (call $eval-apply ,(hir-input 1) ,(hir-list-inputs 2)))))
      (:call-primary (setf result (hir-primary-call-wat call-arity)))
      (:capture (setf result '(array.get $objects (local.get $captures) (local.get $a))))
      (:closure (setf result `(call $hir-function (local.get $payload) (local.get $a)
        (call $hir-capture-vector (local.get $registers) (local.get $decoded) (local.get $count)))))
      (:check-apply-list (push `(call $check-apply-list ,(hir-input 0)) effects))
      (:check-apply-arguments (push `(drop (call $check-apply-arguments ,(hir-input 0) (local.get $a))) effects))
      (:block-enter (setf result (native-object `(call $native-block-enter ,(hir-context-input)))))
      (:block-environment
       (push `(local.set $value ,(hir-input 0)) effects)
       (push (wat-fail-when #@(i32.ne (call $tag (local.get $value)) (i32.const 21)) :malformed-form) effects)
       (setf result (hir-native '$native-block-environment '(local.get $value))))
      (:control-call
       (setf result (native-object `(call $native-control ,(hir-context-input) (local.get $a)
         (extern.convert_any ,(hir-input 1))
         (extern.convert_any (call $hir-control-data (local.get $registers) (local.get $decoded)
           (local.get $count) (local.get $a)))
         (extern.convert_any ,(hir-input 2))))))
      (:restart-signal (setf result (native-object `(call $native-restart-signal ,(hir-context-input)
        (local.get $a) (extern.convert_any ,(hir-list-inputs 2)) (extern.convert_any ,(hir-input 1))))))
      (:multiple-value-call (setf result (native-object `(call $native-multiple-value-call ,(hir-context-input)
        (extern.convert_any ,(hir-input 1)) (extern.convert_any ,(hir-list-inputs 2 t))))))
      (:branch
       (push `(local.set $block (select (local.get $a) (local.get $b)
         (i32.eqz (ref.is_null ,(hir-input 0))))) effects)
       (push '(local.set $enter (i32.const 1)) effects))
      ((:jump :checked-jump)
       (push '(local.set $block (local.get $a)) effects)
       (push `(local.set $incoming (if (result (ref null eq)) (local.get $count)
         (then ,(if (eq opcode :checked-jump) `(call $hir-values ,(hir-input 0)) (hir-input 0)))
         (else (ref.null eq)))) effects)
       (push '(local.set $enter (i32.const 1)) effects))
      (:return (push `(return (call $return-values (call $hir-values ,(hir-input 0)))) effects))
      (:block-return
       (push `(local.set $incoming (if (result (ref null eq)) (i32.eq (local.get $count) (i32.const 3))
         (then (call $hir-values ,(hir-input 2))) (else (ref.null eq)))) effects)
       (push `(local.set $value ,(hir-input 1)) effects)
       (push (wat-fail-when #@(i32.ne (call $tag (local.get $value)) (i32.const 21)) :malformed-form) effects)
       (push `(if (i32.eqz (local.get $a))
         (then (call $native-block-return (extern.convert_any ,(hir-input 1)) (extern.convert_any (local.get $incoming))))
         (else (local.set $block (i32.sub (local.get $a) (i32.const 1)))
           (local.set $enter (i32.const 1)))) effects))
      (:throw (push `(call $native-throw ,(hir-context-input) (extern.convert_any ,(hir-input 1))
        (extern.convert_any (call $hir-values ,(hir-input 2)))) effects))
      (:unreachable (push (wat-fail :control-error) effects)))
    (hir-wat-sequence (apply #'hir-wat-sequence (nreverse effects))
      (when result `(array.set $objects (local.get $registers)
        (local.get $output) ,result))
      ;; The dispatcher and nonlocal landing path establish one value. These
      ;; operations only manipulate explicit registers and cannot change the
      ;; ambient value packet. Calls still capture their results and clear it.
      (unless (member opcode '(:constant :argument :rest-argument :check-arguments :truth
                               :values :values-list :list :value-ref :checked-argument
                               :cell-new :cell-read :cell-write :primary :capture :closure
                               :check-apply-list :check-apply-arguments :branch :jump :checked-jump))
        #@(call $clear-values))
      #@(br $next))))

(defun hir-dispatch-wat ()
  ;; Named labels make dispatch independent of the nesting depth of each
  ;; operation. BR_TABLE avoids a linear opcode comparison chain.
  (let* ((names (loop for i below (length miricl-compiler::*hir-code-operations*)
                     collect (intern (format nil "$HIR-OP-~D" i) :miricl)))
         (body `(br_table ,@names $invalid (local.get $opcode))))
    (loop for spec across miricl-compiler::*hir-code-operations* for name in names do
      (setq body `(block (block ,name ,body)
        ,(hir-operation-wat (first spec)
          (when (and (eq (first spec) :call-primary) (eql (second spec) (third spec)))
            (- (second spec) 2))))))
    `(block $invalid ,body)))

(define-runtime-function $hir-apply (($function :nullable-object) ($args :nullable-object)) (:nullable-object)
  (:locals ($payload :nullable-object) ($descriptor (ref $objects)) ($blocks (ref $objects))
           ($literals (ref $objects)) ($captures (ref $objects)) ($registers (ref $objects))
           ($decoded (ref $digits)) ($targets (ref $objects)) ($block-data (ref $objects))
           ($bytes (ref null $digits)) ($pc i32) ($block i32) ($enter i32) ($opcode i32) ($header i32)
           ($cache (ref null $objects)) ($instruction (ref $hir-decoded-instruction)) ($info i64) ($attributes i64)
           ($output i32) ($count i32) ($a i32) ($b i32) ($i i32) ($word i32)
           ($value :nullable-object) ($incoming :nullable-object) ($ticks i32))
  #@(local.set $payload (struct.get $obj $a (ref.cast (ref $obj) (local.get $function))))
  #@(local.set $descriptor (call $vector-data (array.get $objects
    (call $vector-data ,(hir-field '(local.get $payload) 1))
    (i32.trunc_f64_u (struct.get $obj $f (ref.cast (ref $obj) (local.get $function)))))))
  #@(local.set $blocks (call $vector-data (array.get $objects (local.get $descriptor) (i32.const 5))))
  #@(local.set $literals (call $vector-data ,(hir-field '(local.get $payload) 0)))
  #@(local.set $captures (call $vector-data (struct.get $obj $b (ref.cast (ref $obj) (local.get $function)))))
  #@(local.set $registers (array.new_default $objects
    (call $natural-count (array.get $objects (local.get $descriptor) (i32.const 1)))))
  ;; Gathering operands cannot yield or call Lisp. Every input is consumed
  ;; before invoking a callee or safepoint, and decoded metadata lives in
  ;; locals. Nested calls and other threads may therefore reuse this buffer;
  ;; the next instruction decodes it again before reading any operands.
  #@(local.set $decoded (ref.as_non_null (global.get $hir-decoder-words)))
  #@(local.set $word (call $natural-count (array.get $objects (local.get $descriptor) (i32.const 7))))
  #@(if (i32.lt_u (array.len (local.get $decoded)) (local.get $word))
    (then (local.set $decoded (array.new_default $digits (local.get $word)))
      (global.set $hir-decoder-words (local.get $decoded))))
  #@(local.set $targets (call $vector-data (array.get $objects (local.get $descriptor) (i32.const 8))))
  #@(array.set $objects (local.get $registers)
    (call $natural-count (array.get $objects (local.get $descriptor) (i32.const 2)))
    ,(native-object '(call $native-environment)))
  #@(local.set $block (call $natural-count (array.get $objects (local.get $descriptor) (i32.const 3))))
  #@(local.set $enter (i32.const 1))
  #@(loop $resume
    (block $failed
      (try_table (catch $transfer $failed)
        (loop $next
          (local.set $ticks (i32.add (local.get $ticks) (i32.const 1)))
          (if (i32.eqz (i32.and (local.get $ticks) (i32.const 255)))
            (then (if (global.get $thread-active) (then (call $thread-checkpoint)))))
          (if (local.get $enter)
            (then
              (local.set $block-data (call $vector-data (array.get $objects (local.get $blocks) (local.get $block))))
              (local.set $word (call $natural-count (array.get $objects (local.get $block-data) (i32.const 0))))
              (if (local.get $word) (then (array.set $objects (local.get $registers)
                (i32.sub (local.get $word) (i32.const 1)) (local.get $incoming))))
              (local.set $word (call $natural-count (array.get $objects (local.get $block-data) (i32.const 1))))
              (if (local.get $word) (then (call $native-restore-environment (extern.convert_any
                (call $hir-context (array.get $objects (local.get $registers) (i32.sub (local.get $word) (i32.const 1))))))))
              (local.set $bytes (ref.as_non_null (struct.get $obj $digits (ref.cast (ref $obj)
                (array.get $objects (local.get $block-data) (i32.const 2))))))
              (local.set $decoded (ref.as_non_null (global.get $hir-decoder-words)))
              (local.set $cache (call $hir-cached-block
                (array.get $objects (local.get $block-data) (i32.const 2)) (local.get $decoded)))
              (local.set $pc (i32.const 0)) (local.set $enter (i32.const 0))))
          (if (ref.is_null (local.get $cache))
            (then
              (call $hir-read-instruction (ref.as_non_null (local.get $bytes)) (local.get $pc) (local.get $decoded))
              (local.set $b) (local.set $a) (local.set $count)
              (local.set $output) (local.set $header) (local.set $pc))
            (else
              (if (i32.ge_u (local.get $pc) (array.len (local.get $cache)))
                (then (call $hir-invalid (call $int (local.get $pc)) ,(wat-text-literal "invalid HIR decoder bounds"))))
              (local.set $instruction (ref.cast (ref $hir-decoded-instruction)
                (array.get $objects (local.get $cache) (local.get $pc))))
              (local.set $info (struct.get $hir-decoded-instruction $info (local.get $instruction)))
              (local.set $attributes (struct.get $hir-decoded-instruction $attributes (local.get $instruction)))
              (local.set $decoded (struct.get $hir-decoded-instruction $inputs (local.get $instruction)))
              (local.set $header (i32.and (i32.wrap_i64 (local.get $info)) (i32.const 255)))
              (local.set $count (i32.and (i32.wrap_i64 (i64.shr_u (local.get $info) (i64.const 8))) (i32.const 2047)))
              (local.set $pc (i32.and (i32.wrap_i64 (i64.shr_u (local.get $info) (i64.const 19))) (i32.const 2047)))
              (local.set $output (i32.wrap_i64 (i64.shr_u (local.get $info) (i64.const 30))))
              (local.set $a (i32.wrap_i64 (local.get $attributes)))
              (local.set $b (i32.wrap_i64 (i64.shr_u (local.get $attributes) (i64.const 32))))))
          (local.set $opcode (i32.and (local.get $header) (i32.const 63)))
          (local.set $output (i32.sub (local.get $output) (i32.const 1)))
          ,(hir-dispatch-wat)
          ,(wat-fail :malformed-form))))
    ;; Resume a lexical RETURN-FROM or GO invoked by a captured closure.
    (local.set $i (i32.const 0))
    (block $foreign (loop $landing (br_if $foreign (i32.ge_u (local.get $i) (array.len (local.get $targets))))
      (local.set $word (call $natural-count (array.get $objects (local.get $targets) (local.get $i))))
      (if (call $native-block-match (extern.convert_any (array.get $objects (local.get $registers) (local.get $word))))
        (then
          (local.set $incoming ,(native-object '(call $native-block-values)))
          (local.set $block (call $natural-count (array.get $objects (local.get $targets)
            (i32.add (local.get $i) (i32.const 1)))))
          (local.set $enter (i32.const 1)) (br $resume)))
      (local.set $i (i32.add (local.get $i) (i32.const 2))) (br $landing)))
    (throw $transfer))
  #@(unreachable))

(define-runtime-section hir-runtime-wat ()
  #@(type $hir-decoded-instruction (struct (field $info i64) (field $attributes i64) (field $inputs (ref $digits))))
  #@(global $hir-empty-inputs (ref $digits) (array.new_default $digits (i32.const 0)))
  #@(global $hir-cache-keys (ref $objects) (array.new_default $objects (i32.const 64)))
  #@(global $hir-cache-maps (ref $objects) (array.new_default $objects (i32.const 64)))
  #@(global $hir-decoder-words (mut (ref null $digits)) (array.new_default $digits (i32.const 16)))
  #@(global $hir-operation-shapes (ref $digits)
    (array.new_fixed $digits ,(length miricl-compiler::*hir-code-operations*)
      ,@(loop for spec across miricl-compiler::*hir-code-operations* collect
        `(i32.const ,(+ (if (eql (second spec) (third spec)) (second spec) 255)
                       (if (fifth spec) 256 0) (* 512 (fourth spec))
                       (* 2048 (second spec)) (* 32768 (if (third spec) (1+ (third spec)) 0)))))))
  ($hir-invalid) ($hir-lisp-value) ($hir-context) ($hir-values) ($hir-input-list) ($hir-capture-vector) ($hir-word)
  ($hir-read-instruction) ($hir-cached-block) ($hir-read-instruction-primitive)
  ($hir-unpack-blocks)
  ($hir-control-data)
  ($hir-function) ($hir-load) ($hir-apply))
