;;;; Initial boxed Wasm backend. Control flow and calls execute in native Wasm;
;;;; existing runtime primitives implement Common Lisp object operations.
(in-package :miricl-compiler)

(defvar *compile-evaluations* nil
  "When true, EVAL, source loading and local REPL input use the native compiler.
Bind to NIL to interpret forms, including forms without native lowering.")
(defvar *compiling-evaluation* nil)

(defvar *wasm-bytes*)
(defvar *wasm-literals*)
(defvar *wasm-literal-index*)
(defvar *wasm-literal-locals* nil)
(defvar *wasm-literal-groups* nil)
(defvar *native-functions*)
(defvar *native-function-indices*)
(defvar *wasm-function-relocations* nil)
(defvar *wasm-current-function*)
(defvar *wasm-static-closures*)
(defvar *wasm-body-keys*)
(defvar *wasm-function-plans* nil)
(defvar *wasm-inline-constants* nil)
(defvar *wasm-last-local* nil)
(defvar *wasm-optimize* t)
(defvar *compiler-bootstrapping* nil)
(defvar *wasm-used-imports*)
(defvar *wasm-import-counts* nil)
(defvar *wasm-referenced-functions*)
(defvar *wasm-local-cells*)
(defvar *wasm-live-values*)
(defvar *wasm-locals*)
(defvar *wasm-single-calls*)
(defvar *wasm-scalar-groups*)
(defvar *wasm-object-types*)
(defvar *wasm-builtin-codes*)
(defvar *wasm-dispatch-depth*)
(defvar *wasm-dynamic-bindings*)
(defvar *wasm-entry-environment*)
(defvar *wasm-exceptions*)

;; IDs are dense within one function. Slot zero represents the NIL key used
;; by instructions without outputs; value zero starts at slot one. Keeping
;; this as a generalized place also supports SETF, PUSH and INCF directly.
(defmacro native-value-slot (table value)
  (let ((entry (gensym "HIR-VALUE")))
    (list 'svref table
      (list 'let (list (list entry value))
        (list 'if entry (list '1+ (list 'hir-value-id entry)) 0)))))

(defmacro native-value-table (function &optional initial)
  (list 'make-array (list '1+ (list 'hir-function-value-count function))
        :initial-element initial))

(setf (get 'native-value-slot 'miricl::runtime-inline-macro) t
      (get 'native-value-table 'miricl::runtime-inline-macro) t)
(defparameter *native-imports*
  '(("native-literal" (:object :i32) :object)
    ("native-argument" (:object :i32) :object)
    ("native-rest-argument" (:object :i32) :object)
    ("native-check-arguments" (:object :i32 :i32) nil)
    ("native-check-apply-arguments" (:object :i32) nil)
    ("native-check-apply-list" (:object) nil)
    ("native-environment" nil :object)
    ("native-global-read" (:object :object) :object)
    ("native-global-write" (:object :object :object) :object)
    ("native-function-reference" (:object) :object)
    ("native-cons" (:object :object) :object)
    ("native-cell" (:object) :object)
    ("native-primary" (:object) :object)
    ("native-cell-read" (:object) :object)
    ("native-cell-write" (:object :object) :object)
    ("native-call" (:object :object :object) :object)
    ("native-return" (:object) :object)
    ("native-close" (:function :object :object) :object)
    ("native-close-fixed" (:function :object :object :i32) :object)
    ("native-close-cached" (:function :object :object :i32 :i32) :object)
    ("native-capture" (:object :i32) :object)
    ("native-multiple-value-call" (:object :object :object) :object)
    ("native-call0" (:object) :object)
    ("native-call1" (:object :object) :object)
    ("native-call2" (:object :object :object) :object)
    ("native-call3" (:object :object :object :object) :object)
    ("native-call4" (:object :object :object :object :object) :object)
    ("native-poll" nil nil)
    ("native-return1" (:object) :object)
    ("native-bind" (:object :object :object) :object)
    ("native-restore-environment" (:object) nil)
    ("native-control" (:object :i32 :object :object :object) :object)
    ("native-throw" (:object :object :object) nil)
    ("native-block-enter" (:object) :object)
    ("native-block-environment" (:object) :object)
    ("native-block-return" (:object :object) nil)
    ("native-block-match" (:object) :i32)
    ("native-block-values" nil :object)
    ("native-restart-signal" (:object :i32 :object :object) :object)))

(defun wasm-byte (byte)
  ;; VECTOR-PUSH-EXTEND returns exactly one index. Keep the byte writer on
  ;; the scalar call path even when its caller could observe multiple values.
  (values (vector-push-extend byte *wasm-bytes*)))
(defun wasm-append (bytes &optional (start 0) (end (length bytes)))
  ;; Append whole byte ranges using the runtime's sequence copy loop. Emission
  ;; and relocation otherwise dispatch WASM-BYTE once per already encoded byte.
  (let* ((position (length *wasm-bytes*)) (size (+ position (- end start))))
    (when (> size (array-total-size *wasm-bytes*))
      (setq *wasm-bytes* (adjust-array *wasm-bytes*
                          (max size (* 2 (array-total-size *wasm-bytes*))))))
    (setf (fill-pointer *wasm-bytes*) size)
    (replace *wasm-bytes* bytes :start1 position :start2 start :end2 end)))
(defun wasm-u32 (number)
  (loop (wasm-byte (logior (logand number 127) (if (> number 127) 128 0)))
    (setq number (ash number -7)) (when (zerop number) (return))))
(defun wasm-s32 (number)
  (loop
    (let ((byte (logand number 127)))
      (setq number (ash number -7))
      (let ((done (or (and (= number 0) (< byte 64)) (and (= number -1) (>= byte 64)))))
        (wasm-byte (logior byte (if done 0 128))) (when done (return))))))
(defun wasm-name (name)
  (let ((bytes (utf8-bytes name))) (wasm-u32 (length bytes))
    (wasm-append bytes)))
(defun wasm-type (type)
  (ecase type (:object (wasm-byte #x6f)) (:i32 (wasm-byte #x7f)) (:function (wasm-byte #x70))))
(defun wasm-section (kind bytes)
  (wasm-byte kind) (wasm-u32 (length bytes)) (wasm-append bytes))
(defmacro with-wasm-bytes (&body body)
  (list* 'let (list (list '*wasm-bytes*
    '(make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (append body (list '*wasm-bytes*))))
(defun wasm-call (name)
  (pushnew name *wasm-used-imports* :test #'equal)
  (when *wasm-import-counts* (incf (gethash name *wasm-import-counts* 0)))
  (wasm-byte #x10) (wasm-u32 (or (position name *native-imports* :key #'first :test #'equal)
                               (error "Unknown native ABI operation ~S." name))))
(defun wasm-get (value)
  (cond
    ((and *wasm-inline-constants* (native-value-slot *wasm-inline-constants* value))
     (wasm-literal (first (hir-instruction-attributes (hir-value-producer value)))))
    ((and *wasm-last-local* (eq (first *wasm-last-local*) *wasm-bytes*)
          (= (second *wasm-last-local*) (length *wasm-bytes*))
          (= (fifth *wasm-last-local*) #x21)
          (eql (third *wasm-last-local*) (native-value-slot *wasm-locals* value)))
     ;; LOCAL.SET followed immediately by LOCAL.GET is one LOCAL.TEE.
     (setf (aref *wasm-bytes* (fourth *wasm-last-local*)) #x22)
     (setq *wasm-last-local* nil))
    (t (wasm-local (or (native-value-slot *wasm-locals* value) (error "Missing native operand local."))))))
(defun wasm-set (value)
  (let ((local (native-value-slot *wasm-locals* value)))
    (if local
        (if (and *wasm-last-local* (eq (first *wasm-last-local*) *wasm-bytes*)
                 (= (second *wasm-last-local*) (length *wasm-bytes*))
                 (= (fifth *wasm-last-local*) #x20) (= (third *wasm-last-local*) local))
            ;; A value's last use can share the result's local. Copying that
            ;; local to itself has no effect on either the stack or the local.
            (progn (setf (fill-pointer *wasm-bytes*) (fourth *wasm-last-local*))
                   (setq *wasm-last-local* nil))
            (let ((start (length *wasm-bytes*)))
              (wasm-byte #x21) (wasm-u32 local)
              (setq *wasm-last-local* (list *wasm-bytes* (length *wasm-bytes*) local start #x21))))
        (wasm-byte #x1a))))
(defun wasm-integer (value) (wasm-byte #x41) (wasm-s32 value))
(defun wasm-local (index)
  (let ((start (length *wasm-bytes*)))
    (wasm-byte #x20) (wasm-u32 index)
    (setq *wasm-last-local* (list *wasm-bytes* (length *wasm-bytes*) index start #x20))))
(defun wasm-null () (wasm-byte #xd0) (wasm-byte #x6f))
(defun wasm-gc (opcode &rest immediates)
  (wasm-byte #xfb) (wasm-u32 opcode) (dolist (immediate immediates) (wasm-u32 immediate)))
(defun wasm-as-object (value)
  (wasm-get value) (wasm-gc #x1a) (wasm-gc #x17 1))

(defun native-builtin-code (name)
  (multiple-value-bind (code found) (gethash name *wasm-builtin-codes*)
    (if found code
        (setf (gethash name *wasm-builtin-codes*)
          (multiple-value-bind (code native) (compiler-primitive 19 name)
            (if native code
                ;; Host bootstrap uses the very same target symbol ABI.
                (if (fboundp 'miricl::runtime-symbol-id)
                    (1+ (miricl::runtime-symbol-id name)) 0)))))))

(defun emit-native-object-types ()
  ;; Match runtime-objects.lisp's two minimal canonical type groups exactly.
  ;; DIGITS is an independent i32 array. OBJ is a nonfinal recursive struct.
  (dolist (byte '(#x5e #x7f 1
                 #x4e 1 #x50 0 #x5f 7
                 #x7f 0 #x7f 1 #x7c 0 #x63 0 0
                 #x63 1 1 #x63 1 1 #x63 1 1))
    (wasm-byte byte)))
(defun wasm-literal (value)
  (cond ((null value) (wasm-null))
    ((and *wasm-literal-locals* (gethash value *wasm-literal-locals*))
     (wasm-local (gethash value *wasm-literal-locals*)))
    (t
      (let ((index (multiple-value-bind (index found) (gethash value *wasm-literal-index*)
                     (if found index (setf (gethash value *wasm-literal-index*)
                                      (vector-push-extend value *wasm-literals*))))))
        (wasm-local 0) (wasm-integer index) (wasm-call "native-literal")))))
(defun wasm-list (values)
  (dolist (value values) (wasm-get value)) (wasm-null)
  (dotimes (i (length values)) (wasm-call "native-cons")))

(defun native-removable-p (instruction)
  ;; These operations cannot signal or change observable state in valid HIR.
  ;; Global lookups and calls stay live even when their result is discarded.
  (member (hir-instruction-opcode instruction)
    '(:constant :primary :value-ref :values :list :capture :cell-new :cell-read :block-enter)))

(defun native-direct-cells (function)
  ;; Uncaptured bindings use mutable Wasm locals. Unassigned captured
  ;; bindings carry their value directly through closure environments, since
  ;; the private binding cell has no observable identity. Mutation of the
  ;; value itself remains visible through the same object reference.
  (let ((cells (native-value-table function)))
    (dolist (binding (hir-function-bindings function))
      (when (and (hir-binding-cell binding)
                 (or (not (hir-binding-captured-p binding))
                     (not (hir-binding-assigned-p binding))))
        (setf (native-value-slot cells (hir-binding-cell binding)) t)))
    (dolist (value (hir-function-values function))
      (let ((producer (hir-value-producer value)))
        (when (and (%hir-instruction-p producer)
                   (eq (hir-instruction-opcode producer) :capture))
          (let ((binding (second (hir-instruction-attributes producer))))
            (when (and (%hir-binding-p binding) (not (hir-binding-assigned-p binding)))
              (setf (native-value-slot cells value) t))))))
    cells))

(defun native-forward-blocks (function)
  ;; Topologically order only reachable blocks. Acyclic functions need no
  ;; program counter or dispatcher: Wasm's nested labels encode their edges.
  ;; Count edges (including duplicate branch targets), so joins are emitted
  ;; once, after all predecessors, without duplicating either arm's code.
  (let ((seen (make-hash-table :test 'eq)) (incoming (make-hash-table :test 'eq))
        (work (list (hir-function-entry function))) (count 0) (order nil))
    (loop while work do
      (let ((block (pop work)))
        (unless (gethash block seen)
          (setf (gethash block seen) t) (incf count)
          (dolist (successor (hir-instruction-attributes (hir-block-terminator block)))
            (incf (gethash successor incoming 0)) (push successor work)))))
    (setq work (unless (gethash (hir-function-entry function) incoming)
                 (list (hir-function-entry function))))
    (loop while work do
      (let ((block (pop work)))
        (push block order)
        (dolist (successor (reverse (hir-instruction-attributes (hir-block-terminator block))))
          (when (zerop (decf (gethash successor incoming))) (push successor work)))))
    (when (= (length order) count) (nreverse order))))

(defun native-live-values (function &optional dynamic-bindings blocks)
  (let ((live (native-value-table function)) (work nil))
    (dolist (block (or blocks (hir-function-blocks function)))
      (when dynamic-bindings (push (hir-block-environment block) work))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (unless (native-removable-p instruction)
          (dolist (input (hir-instruction-inputs instruction)) (push input work)))))
    (loop while work do
      (let ((value (pop work)))
        (unless (native-value-slot live value)
          (setf (native-value-slot live value) t)
          (let ((producer (hir-value-producer value)))
            (when (%hir-instruction-p producer)
              (dolist (input (hir-instruction-inputs producer)) (push input work)))))))
    live))

(defun native-local-layout (function blocks offset)
  ;; Reuse temporary locals whose entire lifetime is inside one basic block.
  ;; Bindings, dynamic scopes, block arguments and cross-block values keep
  ;; dedicated locals, so loops and nonlocal exits need no interval heuristics.
  (let ((owners (native-value-table function)) (last-use (native-value-table function))
        (locals (native-value-table function)) (aliases (native-value-table function)) (count 0))
    (dolist (block blocks)
      (let ((position 0))
        (dolist (instruction (append (hir-block-instructions block) (list (hir-block-terminator block))))
          (dolist (value (cons (hir-instruction-output instruction) (hir-instruction-inputs instruction)))
            (when value
              (let ((owner (native-value-slot owners value)))
                (setf (native-value-slot owners value) (if (or (null owner) (eq owner block)) block :shared)))))
          (dolist (input (hir-instruction-inputs instruction)) (setf (native-value-slot last-use input) position))
          (incf position))))
    (dolist (value (hir-function-values function))
      (when (and (native-value-slot *wasm-live-values* value) (not (native-value-slot *wasm-inline-constants* value)))
        (let* ((producer (hir-value-producer value))
               (alias (and (%hir-instruction-p producer)
                        (member (hir-instruction-opcode producer) '(:block-enter :block-environment))
                        (not (hir-target-captured-p (first (hir-instruction-attributes producer))))
                        (first (hir-instruction-inputs producer)))))
          (cond (alias (setf (native-value-slot aliases value) alias))
                ((or (member (hir-value-kind value) '(:dynamic :cell)) (native-value-slot *wasm-local-cells* value) (%hir-block-p producer)
                     (eq value (hir-function-environment function))
                     (eq (native-value-slot owners value) :shared))
                 (setf (native-value-slot locals value) (+ offset count)) (incf count))))))
    (let ((pinned count))
      (dolist (block blocks)
        (let* ((instructions (append (hir-block-instructions block) (list (hir-block-terminator block))))
               (even (make-array (1+ (length instructions)) :initial-element nil))
               (odd (make-array (1+ (length instructions)) :initial-element nil))
               (free (loop for i from pinned below count collect (+ offset i))) (position 0))
          (dolist (instruction instructions)
            ;; The old active-list scan reversed survivors each instruction.
            ;; Birth-parity buckets reproduce its exact free-list order while
            ;; touching only locals that expire here, not all survivors.
            (setq free (if (evenp position)
                           (nconc (aref even position) (nreconc (aref odd position) free))
                           (nconc (aref odd position) (nreconc (aref even position) free))))
            (setf (aref even position) nil (aref odd position) nil)
            (let ((output (hir-instruction-output instruction)))
              (when (and output (native-value-slot *wasm-live-values* output)
                         (not (native-value-slot *wasm-inline-constants* output))
                         (not (native-value-slot locals output)) (not (native-value-slot aliases output)))
                (let ((index (if free (pop free) (prog1 (+ offset count) (incf count)))))
                  (setf (native-value-slot locals output) index)
                  ;; Inputs expire before this instruction stores its result.
                  ;; An unused result cannot expire before its own creation.
                  (push index (aref (if (evenp position) even odd)
                               (max (1+ position) (or (native-value-slot last-use output) position)))))))
            (incf position)))))
    (dolist (value (hir-function-values function))
      (let ((alias (native-value-slot aliases value)))
        (when alias (setf (native-value-slot locals value) (native-value-slot locals alias)))))
    (values locals count)))

(defun native-inline-constants (function blocks)
  ;; Literal lookup is pure and always returns the same object. A single use
  ;; needs no temporary local; NIL is cheaper to recreate at any number of uses.
  (let ((uses (native-value-table function 0)) (constants (native-value-table function)))
    (dolist (block blocks)
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (dolist (input (hir-instruction-inputs instruction))
          ;; Direct cell writes can read their value twice in the emitter.
          (incf (native-value-slot uses input) (if (eq (hir-instruction-opcode instruction) :cell-write) 2 1)))))
    (dolist (block blocks)
      (dolist (instruction (hir-block-instructions block))
        (when (and (eq (hir-instruction-opcode instruction) :constant)
                   (or (null (first (hir-instruction-attributes instruction)))
                       (= (native-value-slot uses (hir-instruction-output instruction)) 1)))
          (setf (native-value-slot constants (hir-instruction-output instruction)) t))))
    constants))

(defun native-single-calls (function scalar)
  ;; Multiple-value packets are only needed when a consumer observes the
  ;; group. Do not allocate a packet merely to extract its primary value.
  ;; Keep this representation choice local to emission: HIR remains reusable.
  (let ((calls (native-value-table function)))
    (dolist (value (hir-function-values function))
      (let ((producer (hir-value-producer value)))
        (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :call)
                   (<= (length (hir-instruction-inputs producer)) 6) (native-value-slot scalar value))
          (setf (native-value-slot calls value) t))))
    calls))

(defun native-full-groups (function)
  ;; Propagate multiple-value demand backwards through joins. An IF used in
  ;; a single-value context must not force each arm through the full protocol.
  (let ((full (native-value-table function)) (sources (native-value-table function)) (work nil))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (let* ((opcode (hir-instruction-opcode instruction))
               (target (and (member opcode '(:jump :block-return))
                            (first (hir-instruction-attributes instruction))))
               (parameter (and target (first (hir-block-parameters target)))))
          (cond
            (parameter
             (push (if (eq opcode :jump) (first (hir-instruction-inputs instruction))
                       (third (hir-instruction-inputs instruction))) (native-value-slot sources parameter)))
            ((not (eq opcode :primary))
             (dolist (input (hir-instruction-inputs instruction))
               (when (eq (hir-value-kind input) :multiple) (push input work))))))))
    (loop while work do
      (let ((value (pop work)))
        (unless (native-value-slot full value)
          (setf (native-value-slot full value) t)
          (dolist (source (native-value-slot sources value)) (push source work)))))
    full))

(defun native-scalar-groups (function)
  ;; Value cardinality is independent of object type. Carry groups known to
  ;; contain exactly one value as the value itself, including through joins.
  (let ((scalar (native-value-table function)) (incoming (make-hash-table :test 'eq)) (changed t))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (hir-block-instructions block))
        (when (and (eq (hir-instruction-opcode instruction) :values)
                   (= (length (hir-instruction-inputs instruction)) 1))
          (setf (native-value-slot scalar (hir-instruction-output instruction)) t)))
      (let* ((terminator (hir-block-terminator block)) (opcode (hir-instruction-opcode terminator))
             (target (first (hir-instruction-attributes terminator))))
        (when (and (member opcode '(:jump :block-return)) target (hir-block-parameters target))
          (push (if (eq opcode :jump) (first (hir-instruction-inputs terminator))
                    (third (hir-instruction-inputs terminator)))
                (gethash target incoming)))))
    ;; A captured block also has incoming value groups from other functions.
    (dolist (target (native-block-targets function))
      (push nil (gethash (hir-target-block target) incoming)))
    (loop while changed do
      (setq changed nil)
      (dolist (block (hir-function-blocks function))
        (let ((parameter (first (hir-block-parameters block))) (values (gethash block incoming)))
          (when (and parameter values (not (native-value-slot scalar parameter))
                     (every (lambda (value) (native-value-slot scalar value)) values))
            (setf (native-value-slot scalar parameter) t) (setq changed t)))))
    (let ((full (native-full-groups function)))
      (dolist (value (hir-function-values function))
        (when (and (eq (hir-value-kind value) :multiple) (not (native-value-slot full value)))
          (setf (native-value-slot scalar value) t))))
    scalar))

(defun emit-native-fixed-call (inputs)
  (dolist (input (cdr inputs)) (wasm-get input))
  (wasm-call (nth (length (cddr inputs))
               '("native-call0" "native-call1" "native-call2" "native-call3" "native-call4"))))

(defun emit-native-cons-access (inputs)
  (let* ((function (second inputs)) (producer (hir-value-producer function))
         (name (when (and (%hir-instruction-p producer)
                         (eq (hir-instruction-opcode producer) :function-reference))
                 (first (hir-instruction-attributes producer))))
         (field (cond ((member name '(car first)) 4) ((member name '(cdr rest)) 5)))
         (code (when (and field (= (length inputs) 3)) (native-builtin-code name))))
    (unless (and code (plusp code)) (return-from emit-native-cons-access nil))
    (setq *wasm-object-types* t)
    ;; Guard the resolved function as well as its argument. Redefinition and
    ;; exceptional operands use the ordinary call path; successful access is
    ;; one typed WasmGC field load, with no runtime call or value packet.
    (wasm-as-object function) (wasm-gc 2 1 1) (wasm-integer code) (wasm-byte #x46)
    (wasm-byte #x04) (wasm-type :object)
      (wasm-get (third inputs)) (wasm-byte #xd1)
      (wasm-byte #x04) (wasm-type :object) (wasm-null)
      (wasm-byte #x05)
        (wasm-as-object (third inputs)) (wasm-gc 2 1 0)
        (wasm-integer 63) (wasm-byte #x71) (wasm-integer 7) (wasm-byte #x46)
        (wasm-byte #x04) (wasm-type :object)
          (wasm-as-object (third inputs)) (wasm-gc 2 1 field) (wasm-gc #x1b)
        (wasm-byte #x05) (emit-native-fixed-call inputs) (wasm-byte #x0b)
      (wasm-byte #x0b)
    (wasm-byte #x05) (emit-native-fixed-call inputs) (wasm-byte #x0b)
    t))

(defun native-fixed-arity (function)
  (dolist (instruction (hir-block-instructions (hir-function-entry function)))
    (when (eq (hir-instruction-opcode instruction) :check-arguments)
      (let ((limits (hir-instruction-attributes instruction)))
        (return-from native-fixed-arity
          (and (= (first limits) (second limits)) (<= 0 (first limits) 4) (first limits)))))))

(defun emit-native-instruction (instruction)
  (let ((inputs (hir-instruction-inputs instruction)) (attributes (hir-instruction-attributes instruction))
        (output (hir-instruction-output instruction)))
    (when (native-value-slot *wasm-inline-constants* output)
      (return-from emit-native-instruction nil))
    (when (and (native-removable-p instruction) (not (native-value-slot *wasm-live-values* output)))
      (return-from emit-native-instruction nil))
    (when (and (member (hir-instruction-opcode instruction) '(:block-enter :block-environment))
               (not (hir-target-captured-p (first attributes))))
      ;; Uncaptured blocks introduce no runtime scope or value copy.
      (return-from emit-native-instruction nil))
    (case (hir-instruction-opcode instruction)
      (:constant (wasm-literal (first attributes)))
      ((:argument :rest-argument)
       (wasm-local 1) (wasm-integer (first attributes))
       (wasm-call (if (eq (hir-instruction-opcode instruction) :argument) "native-argument" "native-rest-argument")))
      (:check-arguments
       (wasm-local 1) (wasm-integer (first attributes)) (wasm-integer (second attributes))
       (wasm-call "native-check-arguments"))
      (:check-apply-arguments
       (wasm-get (first inputs)) (wasm-integer (first attributes))
       (wasm-call "native-check-apply-arguments"))
      (:check-apply-list
       (wasm-get (first inputs)) (wasm-call "native-check-apply-list"))
      (:checked-argument
       (wasm-get (first inputs)) (wasm-integer (first attributes)) (wasm-call "native-argument"))
      (:truth
       (wasm-get (first inputs)) (wasm-byte #xd1)
       (wasm-byte #x04) (wasm-type :object) (wasm-null)
       (wasm-byte #x05) (wasm-literal t) (wasm-byte #x0b))
      (:block-enter
       (wasm-get (first inputs))
       (when (hir-target-captured-p (first attributes)) (wasm-call "native-block-enter")))
      (:block-environment
       (wasm-get (first inputs))
       (when (hir-target-captured-p (first attributes)) (wasm-call "native-block-environment")))
      (:function-reference (wasm-literal (first attributes)) (wasm-call "native-function-reference"))
      (:global-read (wasm-get (first inputs)) (wasm-literal (first attributes)) (wasm-call "native-global-read"))
      (:global-write
       (wasm-get (first inputs)) (wasm-literal (first attributes)) (wasm-get (second inputs))
       (wasm-call "native-global-write"))
      (:dynamic-bind
       (wasm-get (first inputs)) (wasm-literal (first attributes)) (wasm-get (second inputs))
       (wasm-call "native-bind"))
      (:environment-restore
       (wasm-get (first inputs)) (wasm-call "native-restore-environment"))
      (:values (if (native-value-slot *wasm-scalar-groups* output)
                   (if inputs (wasm-get (first inputs)) (wasm-null)) (wasm-list inputs)))
      (:values-list
       (wasm-get (first inputs))
       (when (native-value-slot *wasm-scalar-groups* (first inputs)) (wasm-null) (wasm-call "native-cons")))
      (:list (wasm-list inputs))
      (:value-ref
       (if (native-value-slot *wasm-scalar-groups* (first inputs)) (wasm-null)
           (progn (wasm-get (first inputs)) (wasm-integer (first attributes)) (wasm-call "native-argument"))))
      ((:cell-new :cell-read :cell-write :primary)
       (cond
         ((and (eq (hir-instruction-opcode instruction) :primary)
               (or (native-value-slot *wasm-single-calls* (first inputs))
                   (native-value-slot *wasm-scalar-groups* (first inputs))))
          (wasm-get (first inputs)))
         ((and (eq (hir-instruction-opcode instruction) :cell-new) (native-value-slot *wasm-local-cells* output))
          (wasm-get (first inputs)))
         ((and (eq (hir-instruction-opcode instruction) :cell-read) (native-value-slot *wasm-local-cells* (first inputs)))
          (wasm-get (first inputs)))
         ((and (eq (hir-instruction-opcode instruction) :cell-write) (native-value-slot *wasm-local-cells* (first inputs)))
          (wasm-get (second inputs)) (wasm-set (first inputs)) (wasm-get (second inputs)))
         (t
          (dolist (input inputs) (wasm-get input))
          (wasm-call (ecase (hir-instruction-opcode instruction)
                    (:cell-new "native-cell") (:cell-read "native-cell-read")
                    (:cell-write "native-cell-write") (:primary "native-primary"))))))
      (:call
       (if (native-value-slot *wasm-single-calls* output)
           ;; Keep guards in the shared native primitives. Repeating CAR/CDR
           ;; type and redefinition checks at every call bloats Lisp-heavy code.
           (emit-native-fixed-call inputs)
           (progn (wasm-get (first inputs)) (wasm-get (second inputs))
                  (wasm-list (cddr inputs)) (wasm-call "native-call")
                  (when (native-value-slot *wasm-scalar-groups* output) (wasm-call "native-primary")))))
      (:control-call
       (wasm-get (first inputs)) (wasm-integer (first attributes)) (wasm-get (second inputs))
       (if (member (first attributes) '(1 2 4 5 6 7)) (wasm-list (nthcdr 4 inputs)) (wasm-get (fourth inputs)))
       (wasm-get (third inputs)) (wasm-call "native-control")
       (when (native-value-slot *wasm-scalar-groups* output) (wasm-call "native-primary")))
      (:restart-signal
       (wasm-get (first inputs)) (wasm-integer (first attributes))
       (wasm-list (cddr inputs)) (wasm-get (second inputs)) (wasm-call "native-restart-signal")
       (when (native-value-slot *wasm-scalar-groups* output) (wasm-call "native-primary")))
      (:multiple-value-call
       (wasm-get (first inputs)) (wasm-get (second inputs))
       (dolist (group (cddr inputs))
         (wasm-get group)
         (when (native-value-slot *wasm-scalar-groups* group) (wasm-null) (wasm-call "native-cons")))
       (wasm-null) (dotimes (i (length (cddr inputs))) (wasm-call "native-cons"))
       (wasm-call "native-multiple-value-call")
       (when (native-value-slot *wasm-scalar-groups* output) (wasm-call "native-primary")))
      (:capture
       (wasm-local 0) (wasm-integer (first attributes)) (wasm-call "native-capture"))
      (:closure
       (pushnew (first attributes) *wasm-referenced-functions*)
       (wasm-byte #xd2)
       (when *wasm-function-relocations*
         (push (list (length *wasm-bytes*) (first attributes))
               (gethash *wasm-current-function* *wasm-function-relocations*)))
       (wasm-u32 (+ (length *native-imports*) (gethash (first attributes) *native-function-indices*)))
       (wasm-local 0) (wasm-list inputs)
       (let ((arity (native-fixed-arity (first attributes))))
         (if (and *wasm-static-closures*
                  (every (lambda (input)
                           (let ((producer (hir-value-producer input)))
                             (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :capture)))) inputs))
             (progn
               ;; Existing captures retain the same lexical binding instances
               ;; on every invocation of this parent function object.
               (wasm-integer (or arity -1))
               (wasm-integer (or (gethash (first attributes) *wasm-static-closures*)
                 (setf (gethash (first attributes) *wasm-static-closures*) (hash-table-count *wasm-static-closures*))))
               (wasm-call "native-close-cached"))
             (if arity (progn (wasm-integer arity) (wasm-call "native-close-fixed"))
                 (wasm-call "native-close")))))
      (otherwise (error "Native lowering is not implemented for ~S." (hir-instruction-opcode instruction))))
    (when output (wasm-set output))))

(defun emit-native-jump-value (block value)
  (when value
    (wasm-get value)
    (when (and (native-value-slot *wasm-scalar-groups* value)
               (not (native-value-slot *wasm-scalar-groups* (first (hir-block-parameters block)))))
      (wasm-null) (wasm-call "native-cons"))
    (wasm-set (first (hir-block-parameters block)))))

(defun emit-native-jump (block value from)
  (emit-native-jump-value block value)
  ;; Every cycle has a backward/self edge under this total block ordering.
  ;; Inlining all calls in a loop must not remove its scheduling safepoint.
  (when *wasm-dynamic-bindings*
    (wasm-get (hir-block-environment block)) (wasm-call "native-restore-environment"))
  (when (<= (hir-block-id block) (hir-block-id from)) (wasm-call "native-poll"))
  (wasm-integer (hir-block-id block)) (wasm-byte #x21) (wasm-u32 2)
  (wasm-byte #x0c) (wasm-u32 *wasm-dispatch-depth*))

(defun emit-native-return (value)
  (when *wasm-dynamic-bindings*
    (wasm-get *wasm-entry-environment*) (wasm-call "native-restore-environment"))
  (wasm-get value)
  (wasm-call (if (native-value-slot *wasm-scalar-groups* value) "native-return1" "native-return")))

(defun emit-native-throw (inputs)
  (dolist (input inputs) (wasm-get input))
  (when (native-value-slot *wasm-scalar-groups* (third inputs)) (wasm-null) (wasm-call "native-cons"))
  (wasm-call "native-throw") (wasm-byte #x00))

(defun native-block-targets (function)
  (let ((targets nil))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (hir-block-instructions block))
        (when (and (eq (hir-instruction-opcode instruction) :block-enter)
                   (hir-target-captured-p (first (hir-instruction-attributes instruction))))
          (push (first (hir-instruction-attributes instruction)) targets))))
    (nreverse targets)))

(defun emit-native-block-return (inputs)
  (wasm-get (second inputs)) (wasm-get (third inputs))
  (when (native-value-slot *wasm-scalar-groups* (third inputs)) (wasm-null) (wasm-call "native-cons"))
  (wasm-call "native-block-return") (wasm-byte #x00))

(defun emit-native-forward (blocks)
  (let ((positions (make-hash-table :test 'eq)) (position 0))
    (dolist (block blocks) (setf (gethash block positions) position) (incf position))
    ;; The innermost label names the next block, the next label the one after
    ;; it, and so on. Close a label just before emitting its block.
    (dotimes (i (1- (length blocks))) (wasm-byte #x02) (wasm-byte #x40))
    (setq position 0)
    (loop for tail on blocks for block = (car tail) for next = (cadr tail) do
      (unless (zerop position) (wasm-byte #x0b))
      (when (and *wasm-dynamic-bindings* (not (zerop position)))
        (wasm-get (hir-block-environment block)) (wasm-call "native-restore-environment"))
      (dolist (instruction (hir-block-instructions block)) (emit-native-instruction instruction))
      (let* ((terminator (hir-block-terminator block)) (inputs (hir-instruction-inputs terminator))
             (successors (hir-instruction-attributes terminator)) (target (first successors)))
        (ecase (hir-instruction-opcode terminator)
          (:unreachable (wasm-byte #x00))
          (:throw (emit-native-throw inputs))
          (:return
           (emit-native-return (first inputs))
           (when next (wasm-byte #x0f)))
          ((:jump :block-return)
           (if (and (eq (hir-instruction-opcode terminator) :block-return) (not target))
               (emit-native-block-return inputs)
               (progn
                 (emit-native-jump-value target (if (eq (hir-instruction-opcode terminator) :jump)
                                                  (first inputs) (third inputs)))
                 (unless (eq target next)
                   (wasm-byte #x0c) (wasm-u32 (- (gethash target positions) position 1))))))
          (:branch
           (let ((no (second successors)))
             (wasm-get (first inputs)) (wasm-byte #xd1)
             (when (eq no next) (wasm-byte #x45))
             (wasm-byte #x0d)
             (wasm-u32 (- (gethash (if (eq no next) target no) positions) position 1))
             (unless (or (eq target next) (eq no next))
               (wasm-byte #x0c) (wasm-u32 (- (gethash target positions) position 1)))))))
      (incf position))
    (wasm-byte #x0b)))

(defun emit-native-function (function)
  (let ((*wasm-current-function* function))
    (let ((body (emit-native-function-body function)))
      (if *wasm-optimize* (native-optimize-body body) body))))

(defun native-loop-literals (function blocks)
  ;; Literal identity is stable for the lifetime of this function object.
  ;; Load loop operands and global *names* once per invocation; the function
  ;; definitions themselves are still resolved at each original call site.
  (when (null (native-forward-blocks function))
    (let ((seen (make-hash-table :test 'eq)) (literals nil))
      (dolist (block blocks)
        (dolist (instruction (hir-block-instructions block))
          (when (member (hir-instruction-opcode instruction)
                        '(:constant :function-reference :global-read :global-write :dynamic-bind))
            (let ((value (first (hir-instruction-attributes instruction))))
              (when value
                (unless (gethash value seen) (push value literals))
                (incf (gethash value seen 0)))))))
      ;; A one-site cache adds a prologue load, store and local to save a
      ;; single static lookup. Share repeated literals so code size pays for
      ;; the cache as well as repeated execution benefiting from it.
      (remove-if (lambda (literal) (< (gethash literal seen) 2)) (nreverse literals)))))

(defun native-function-plan (function)
  ;; HIR is unchanged between import discovery and final emission. Literal
  ;; indices, closure objects and relocations remain per-emission state.
  (let* ((targets (native-block-targets function))
        (forward (unless targets (native-forward-blocks function)))
        (blocks (or forward (hir-function-blocks function)))
        (*wasm-inline-constants* (native-inline-constants function blocks))
        (*wasm-local-cells* (native-direct-cells function))
        (*wasm-dynamic-bindings* (or targets (some (lambda (block)
          (some (lambda (instruction) (eq (hir-instruction-opcode instruction) :dynamic-bind))
            (hir-block-instructions block))) blocks)))
        (*wasm-entry-environment* (hir-function-environment function))
        (*wasm-live-values* (native-live-values function *wasm-dynamic-bindings* blocks))
        (*wasm-scalar-groups* (native-scalar-groups function))
        (*wasm-single-calls* (native-single-calls function *wasm-scalar-groups*))
        (locals nil) (count 0)
        (literals (native-loop-literals function blocks))
        (literal-locals (make-hash-table :test 'eq)))
    (multiple-value-setq (locals count)
      (native-local-layout function blocks (if forward 2 3)))
    (dolist (literal literals)
      (setf (gethash literal literal-locals) (+ (if forward 2 3) count)) (incf count))
    (vector targets forward *wasm-inline-constants* *wasm-local-cells*
            *wasm-dynamic-bindings* *wasm-entry-environment* *wasm-live-values*
            *wasm-scalar-groups* *wasm-single-calls* locals count literal-locals literals)))

(defun emit-native-function-body (function)
  ;; Boxed SSA values are externref locals. A dispatcher is a first correct
  ;; lowering of arbitrary CFG edges; structured regions replace it later.
  (let* ((*wasm-current-function* function)
         (*wasm-last-local* nil)
         ;; Unit initializers run once. Other stable closures may share
         ;; their function object, as permitted by CLHS FUNCTION.
         (*wasm-static-closures* (unless (and *wasm-literal-groups* (gethash function *wasm-literal-groups*))
                                  (make-hash-table :test 'eq)))
         (root (loop for root = function then (hir-function-parent root)
                    while (hir-function-parent root) finally (return root)))
        (group (and *wasm-literal-groups* (gethash root *wasm-literal-groups*)))
        (*wasm-literals* (if group (first group) *wasm-literals*))
        (*wasm-literal-index* (if group (second group) *wasm-literal-index*))
        (plan (or (and *wasm-function-plans* (gethash function *wasm-function-plans*))
                  (let ((plan (native-function-plan function)))
                    (when *wasm-function-plans* (setf (gethash function *wasm-function-plans*) plan))
                    plan)))
        (targets (svref plan 0)) (forward (svref plan 1))
        (blocks (or forward (hir-function-blocks function)))
        (*wasm-inline-constants* (svref plan 2)) (*wasm-local-cells* (svref plan 3))
        (*wasm-dynamic-bindings* (svref plan 4)) (*wasm-entry-environment* (svref plan 5))
        (*wasm-live-values* (svref plan 6)) (*wasm-scalar-groups* (svref plan 7))
        (*wasm-single-calls* (svref plan 8)) (*wasm-locals* (svref plan 9)) (count (svref plan 10))
        (*wasm-literal-locals* (svref plan 11)))
   (when *wasm-function-relocations*
     (setf (gethash function *wasm-function-relocations*) nil))
   (with-wasm-bytes
    (wasm-u32 (if forward 1 2))
    (unless forward (wasm-u32 1) (wasm-type :i32))
    (wasm-u32 count) (wasm-type :object)
    (dolist (literal (svref plan 12))
      (let ((*wasm-literal-locals* nil)) (wasm-literal literal))
      (wasm-byte #x21) (wasm-u32 (gethash literal *wasm-literal-locals*)))
    (when (native-value-slot *wasm-live-values* (hir-function-environment function))
      (wasm-call "native-environment") (wasm-set (hir-function-environment function)))
    (when forward
      (emit-native-forward blocks)
      (return-from emit-native-function-body *wasm-bytes*))
    (wasm-integer (hir-block-id (hir-function-entry function))) (wasm-byte #x21) (wasm-u32 2)
    (when targets
      (setq *wasm-exceptions* t)
      (wasm-byte #x03) (wasm-byte #x40) ; resume after a caught lexical exit
      (wasm-byte #x02) (wasm-byte #x40) ; exception landing pad
      ;; TRY_TABLE catches the existing payload-free runtime transfer tag.
      (dolist (byte '(#x1f #x40 1 0 0 0)) (wasm-byte byte)))
    (wasm-byte #x03) (wasm-byte #x40) ; dispatch loop
    (wasm-byte #x02) (wasm-byte #x40) ; invalid program counter
    (dotimes (i (length (hir-function-blocks function))) (wasm-byte #x02) (wasm-byte #x40))
    (wasm-local 2) (wasm-byte #x0e) (wasm-u32 (length (hir-function-blocks function)))
    (dotimes (i (length (hir-function-blocks function))) (wasm-u32 i))
    (wasm-u32 (length (hir-function-blocks function)))
    ;; BR_TABLE selects a block in constant time. The earlier chain of IFs
    ;; rescanned every preceding block on each edge of a compiler's inner loop.
    (let ((*wasm-dispatch-depth* (length (hir-function-blocks function))))
     (dolist (block (hir-function-blocks function))
      (wasm-byte #x0b)
      (when *wasm-dynamic-bindings*
        (wasm-get (hir-block-environment block)) (wasm-call "native-restore-environment"))
      (dolist (instruction (hir-block-instructions block)) (emit-native-instruction instruction))
      (let* ((terminator (hir-block-terminator block)) (inputs (hir-instruction-inputs terminator))
             (successors (hir-instruction-attributes terminator)))
        (ecase (hir-instruction-opcode terminator)
          (:unreachable (wasm-byte #x00))
          (:throw (emit-native-throw inputs))
          (:return (emit-native-return (first inputs)) (wasm-byte #x0f))
          (:jump (emit-native-jump (first successors) (first inputs) block))
          (:block-return
           (if successors (emit-native-jump (first successors) (third inputs) block)
               (emit-native-block-return inputs)))
          (:branch
           (when (some (lambda (successor) (<= (hir-block-id successor) (hir-block-id block))) successors)
             (wasm-call "native-poll"))
           (wasm-get (first inputs)) (wasm-byte #xd1)
           (wasm-byte #x04) (wasm-byte #x40)
           (wasm-integer (hir-block-id (second successors)))
           (wasm-byte #x21) (wasm-u32 2)
           (wasm-byte #x05)
           (wasm-integer (hir-block-id (first successors)))
           (wasm-byte #x21) (wasm-u32 2) (wasm-byte #x0b)
           (wasm-byte #x0c) (wasm-u32 *wasm-dispatch-depth*))))
      (decf *wasm-dispatch-depth*)))
    (wasm-byte #x0b) (wasm-byte #x00) ; invalid program counter traps
    (wasm-byte #x0b) (wasm-byte #x00)
    (when targets
      (wasm-byte #x0b) (wasm-byte #x0b) ; try and landing pad
      (dolist (target targets)
        (wasm-get (hir-target-token target)) (wasm-call "native-block-match")
        (wasm-byte #x04) (wasm-byte #x40)
        (wasm-call "native-block-values")
        (let ((parameter (first (hir-block-parameters (hir-target-block target)))))
          (if parameter
              (progn (when (native-value-slot *wasm-scalar-groups* parameter) (wasm-call "native-primary"))
                     (wasm-set parameter))
              (wasm-byte #x1a)))
        (wasm-integer (hir-block-id (hir-target-block target)))
        (wasm-byte #x21) (wasm-u32 2)
        (wasm-byte #x0c) (wasm-u32 1) (wasm-byte #x0b))
      (wasm-byte #x08) (wasm-u32 0) ; propagate foreign transfers unchanged
      (wasm-byte #x0b) (wasm-byte #x00))
    (wasm-byte #x0b))))

(defun native-literal-groups (entries)
  (let ((groups (make-hash-table :test 'eq)))
    (dolist (entry entries)
      (setf (gethash entry groups) (list (make-array 16 :adjustable t :fill-pointer 0)
                                       (make-hash-table :test 'eq))))
    groups))

(defun native-function-indices (functions)
  (let ((indices (make-hash-table :test 'eq)) (index 0))
    (dolist (function functions) (setf (gethash function indices) index) (incf index))
    indices))

(defun native-coalesce-bodies (bodies)
  ;; Equal bodies may still have different literal values and captures: both
  ;; live in the function object passed through the boxed entry convention.
  (let ((seen (make-hash-table :test 'equal)) (indices (make-hash-table :test 'eq))
        (unique nil) (count 0) (changed nil))
    (loop for function in *native-functions* for body in bodies do
      (let ((key (or (gethash body *wasm-body-keys*)
                     (setf (gethash body *wasm-body-keys*) (map 'string #'code-char body)))))
       (multiple-value-bind (index found) (gethash key seen)
        (unless found
          (setq index count) (incf count) (push body unique)
          (setf (gethash key seen) index))
        (unless (= index (gethash function *native-function-indices*)) (setq changed t))
        (setf (gethash function indices) index))))
    (values (nreverse unique) indices changed)))

(defun native-relocate-body (function body original-indices)
  (let ((relocations (gethash function *wasm-function-relocations*)) (cursor 0))
    (unless (some (lambda (entry)
              (/= (gethash (second entry) original-indices)
                  (gethash (second entry) *native-function-indices*))) relocations)
      (return-from native-relocate-body body))
    (with-wasm-bytes
      (dolist (entry (reverse relocations))
        (wasm-append body cursor (first entry))
        (setq cursor (first entry))
        ;; Skip the original unsigned LEB; its replacement may be shorter.
        (loop for byte = (aref body cursor) do (incf cursor) until (< byte 128))
        (wasm-u32 (+ (length *native-imports*) (gethash (second entry) *native-function-indices*))))
      (wasm-append body cursor))))

(defun emit-native-bodies (entries)
  (let ((original (mapcar #'emit-native-function *native-functions*))
        (original-indices *native-function-indices*)
        (*wasm-body-keys* (make-hash-table :test 'eq)))
    (unless entries (return-from emit-native-bodies original))
    (loop
      (let ((bodies (mapcar (lambda (function body)
                             (native-relocate-body function body original-indices)) *native-functions* original)))
        (multiple-value-bind (unique indices changed) (native-coalesce-bodies bodies)
          (setq *native-function-indices* indices)
          ;; Patch REF.FUNC operands when their targets merge. Literal layouts
          ;; stay unchanged; HIR and full bodies need to be emitted only once.
          (unless changed
            (return (native-order-bodies entries original original-indices unique))))))))

(defun native-order-bodies (entries original original-indices unique)
  ;; Compact frequently referenced function indices after body sharing reaches
  ;; a fixed point. Relocate from the original bodies, whose offsets are known.
  (when (<= (+ (length *native-imports*) (length unique)) 128)
    (return-from native-order-bodies unique))
  (let* ((size (length unique)) (representatives (make-array size :initial-element nil))
         (counts (make-array size :initial-element 0))
         (order (loop for i below size collect i)) (mapping (make-array size)))
    (loop for function in *native-functions* for body in original do
      (let ((index (gethash function *native-function-indices*)))
        (unless (aref representatives index)
          (setf (aref representatives index) (cons function body)))))
    (loop for entry across representatives do
      (dolist (relocation (gethash (car entry) *wasm-function-relocations*))
        (incf (aref counts (gethash (second relocation) *native-function-indices*)))))
    (dolist (entry entries) (incf (aref counts (gethash entry *native-function-indices*))))
    (setq order (stable-sort order #'> :key (lambda (i) (aref counts i))))
    (loop for old in order for new from 0 do (setf (aref mapping old) new))
    (dolist (function *native-functions*)
      (setf (gethash function *native-function-indices*)
        (aref mapping (gethash function *native-function-indices*))))
    (mapcar (lambda (index)
              (let ((entry (aref representatives index)))
                (native-relocate-body (car entry) (cdr entry) original-indices))) order)))

(defun native-prune-module (bodies entries)
  ;; RemoveUnusedModuleElements after branch/DCE cleanup. No calls or closure
  ;; entries are renumbered until body sharing and function layout are final.
  (let* ((old-count (length *native-imports*)) (used (make-array old-count :initial-element nil))
         (mapping (make-array old-count))
         (decoded (coerce (mapcar (lambda (body) (native-read-ops body t)) bodies) 'vector))
         (live (make-array (length bodies) :initial-element nil))
         (functions (make-array (length bodies))) (function-count 0)
         (work (if entries (mapcar (lambda (entry) (gethash entry *native-function-indices*)) entries) '(0)))
         (imports nil) (next 0))
    ;; Only exports and function references in reachable code are roots. The
    ;; declarative element segment grants REF.FUNC permission, not reachability.
    (loop while work for index = (pop work) do
      (unless (aref live index)
        (setf (aref live index) t)
        (loop for op across (aref decoded index) do
          (when (member (native-op-code op) '(#x10 #xd2))
            (let ((target (native-op-argument op)))
              (if (< target old-count) (setf (aref used target) t)
                  (push (- target old-count) work)))))))
    (loop for import in *native-imports* for i from 0 do
      (when (aref used i)
        (setf (aref mapping i) next) (incf next) (push import imports)))
    (dotimes (i (length bodies))
      (when (aref live i) (setf (aref functions i) function-count) (incf function-count)))
    (when (and (= next old-count) (= function-count (length bodies)))
      (return-from native-prune-module bodies))
    (setq *native-imports* (nreverse imports))
    (dolist (function *native-functions*)
      (let ((old (gethash function *native-function-indices*)))
        (setf (gethash function *native-function-indices*) (and (aref live old) (aref functions old)))))
    (setq *wasm-referenced-functions*
      (remove-duplicates
        (remove-if-not (lambda (function) (gethash function *native-function-indices*)) *wasm-referenced-functions*)
        :key (lambda (function) (gethash function *native-function-indices*))))
    (loop for body in bodies for old from 0 when (aref live old) collect
      (let ((ops (aref decoded old)))
              (with-wasm-bytes
                (let ((cursor 0))
                 (loop for op across ops for code = (native-op-code op) do
                      (wasm-append body cursor (native-op-start op))
                      (let ((index (native-op-argument op)))
                        (wasm-byte code)
                        (wasm-u32 (if (< index old-count) (aref mapping index)
                                      (+ next (aref functions (- index old-count))))))
                      (setq cursor (native-op-end op)))
                 (wasm-append body cursor)))))))

(defun emit-native-module (unit &key entries)
  "Return Wasm bytes and literal objects; do not instantiate or run application code."
  (ensure-compiler)
  (verify-hir unit)
  (let* ((*wasm-literals* (make-array 16 :adjustable t :fill-pointer 0))
         (*wasm-literal-index* (make-hash-table :test 'eq))
         (*wasm-literal-groups* (and entries (native-literal-groups entries)))
         (*native-functions* (hir-unit-functions unit))
         (*wasm-function-plans* (make-hash-table :test 'eq))
         (*native-function-indices* (native-function-indices *native-functions*))
         (*wasm-function-relocations* (and entries (make-hash-table :test 'eq)))
         (*wasm-used-imports* nil) (*wasm-import-counts* (make-hash-table :test 'equal))
         (*wasm-referenced-functions* nil)
         (*wasm-object-types* nil) (*wasm-exceptions* nil) (*wasm-builtin-codes* (make-hash-table :test 'eq)))
    ;; Collect an import superset using raw emission; only final bodies need
    ;; size analysis. Regenerate with compact indices, then prune functions and
    ;; imports made unreachable by constant/control cleanup.
    (mapc #'emit-native-function-body *native-functions*)
    (let* ((type-prefix (if *wasm-object-types* 2 0))
         (*native-imports* (stable-sort
                           (remove-if-not (lambda (import) (member (first import) *wasm-used-imports* :test #'equal))
                                          *native-imports*)
                           #'> :key (lambda (import) (gethash (first import) *wasm-import-counts* 0))))
         (*wasm-literals* (make-array 16 :adjustable t :fill-pointer 0))
         (*wasm-literal-index* (make-hash-table :test 'eq))
         (*wasm-literal-groups* (and entries (native-literal-groups entries)))
         (bodies (emit-native-bodies entries))
         (bodies (if *wasm-optimize* (native-prune-module bodies entries) bodies))
         (signatures (remove-duplicates
                       (append (mapcar #'cdr *native-imports*) (list '((:object :object) :object))
                         (when *wasm-exceptions* (list '(nil nil)))) :test #'equal))
         (bytes (with-wasm-bytes
           (dolist (byte '(0 97 115 109 1 0 0 0)) (wasm-byte byte))
           (wasm-section 1 (with-wasm-bytes
             (wasm-u32 (+ (length signatures) type-prefix))
             (when *wasm-object-types* (emit-native-object-types))
             (dolist (signature signatures)
               (wasm-byte #x60) (wasm-u32 (length (first signature)))
               (dolist (type (first signature)) (wasm-type type))
               (wasm-u32 (if (second signature) 1 0)) (when (second signature) (wasm-type (second signature))))))
           (wasm-section 2 (with-wasm-bytes
             (wasm-u32 (+ (length *native-imports*) (if *wasm-exceptions* 1 0)))
             (dolist (import *native-imports*)
               (wasm-name "miricl.runtime") (wasm-name (first import)) (wasm-byte 0)
               (wasm-u32 (+ type-prefix (position (cdr import) signatures :test #'equal))))
             (when *wasm-exceptions*
               (wasm-name "miricl.runtime") (wasm-name "native-transfer") (wasm-byte 4) (wasm-byte 0)
               (wasm-u32 (+ type-prefix (position '(nil nil) signatures :test #'equal))))))
           (wasm-section 3 (with-wasm-bytes
             (wasm-u32 (length bodies))
             (dolist (body bodies) (declare (ignore body))
               (wasm-u32 (+ type-prefix (position '((:object :object) :object) signatures :test #'equal))))))
           (wasm-section 7 (with-wasm-bytes
             (wasm-u32 (if entries (length entries) 1))
             (if entries
                 (loop for entry in entries for index from 0 do
                   (wasm-name (format nil "entry~D" index)) (wasm-byte 0)
                   (wasm-u32 (+ (length *native-imports*) (gethash entry *native-function-indices*))))
                 (progn (wasm-name "entry") (wasm-byte 0) (wasm-u32 (length *native-imports*))))))
           (when *wasm-referenced-functions*
             (wasm-section 9 (with-wasm-bytes
               (wasm-u32 1) (wasm-u32 3) (wasm-byte 0) (wasm-u32 (length *wasm-referenced-functions*))
               (dolist (function *wasm-referenced-functions*)
                 (wasm-u32 (+ (length *native-imports*) (gethash function *native-function-indices*)))))))
           (wasm-section 10 (with-wasm-bytes
             (wasm-u32 (length bodies))
             (dolist (body bodies) (wasm-u32 (length body)) (wasm-append body)))))))
    (values bytes (if entries
                     (mapcar (lambda (entry) (coerce (first (gethash entry *wasm-literal-groups*)) 'simple-vector)) entries)
                     (coerce *wasm-literals* 'simple-vector))))))

(defun compile-native (lambda-expression &key name)
  "Compile a lambda to native Wasm using the current host's module loader."
  (ensure-compiler)
  (let ((unit (compile-to-hir lambda-expression :name name)))
    (multiple-value-bind (bytes literals) (emit-native-module unit)
      (multiple-value-bind (function native) (compiler-primitive 17 bytes literals name)
        (unless native (error "This host cannot instantiate compiled Wasm modules."))
        (let ((arity (native-fixed-arity (first (hir-unit-functions unit)))))
          (if arity (values (compiler-primitive 30 function arity)) function))))))

(defparameter *native-compiler-seeds*
  '(compiler-primitive
                   wasm-byte wasm-append wasm-u32 wasm-s32 wasm-name wasm-type wasm-section wasm-call
                   wasm-get wasm-set wasm-integer wasm-local wasm-null wasm-gc wasm-as-object
                   native-builtin-code emit-native-object-types wasm-literal wasm-list
                   native-removable-p native-direct-cells native-forward-blocks native-live-values native-local-layout native-single-calls native-full-groups native-scalar-groups native-block-targets
                   emit-native-fixed-call emit-native-cons-access native-fixed-arity emit-native-instruction emit-native-jump-value emit-native-jump emit-native-return emit-native-throw emit-native-block-return emit-native-forward
                   hir-new-value hir-new-block hir-emit hir-terminate hir-single hir-multiple hir-nth-value hir-constant
                   hir-capture hir-cell-for-binding hir-special-p hir-declarations hir-shadow-specials
                   hir-bind-value hir-read-variable hir-write-variable hir-function-reference
                   hir-small-membership hir-call-arguments hir-literal-apply-form hir-case-form hir-macro-environment hir-assert-form
                   hir-expand-parallel-places hir-expand-arithmetic-step hir-restart-options hir-correctable-place-form
                   hir-condition-initializer hir-define-condition hir-restart-expression hir-control-form verify-hir
                   native-inline-constants native-loop-literals native-function-plan emit-native-function-body emit-native-function native-literal-groups native-function-indices native-coalesce-bodies native-relocate-body native-order-bodies emit-native-bodies emit-native-module
                   hir-body hir-close hir-choice hir-expand-local-macros hir-lambda hir-form compile-to-hir
                   make-native-op make-native-flow native-read-uleb native-read-sleb native-read-ops native-op-target native-control-matches native-op-flow native-liveness-blocks native-op-liveness native-op-stack native-stackify-ops native-coalesce-ops native-fold-locals native-optimize-body
                   ;; Compile larger size passes after the frontend and emitter.
                   native-i32 native-fold-i32 native-clean-scopes native-simplify-ops native-prune-module
                   symbol-key function-key literal-initializer-p static-type-form-p structure-info class-info method-info
                   definition-info namespace form-references
                   miricl::%compile-place-form miricl::%compile-parallel-places miricl::%compile-loop-collector
                   miricl::%compile-forms miricl::%compile-body miricl::%compile-lambda-parts
                   miricl::%compile-lambda-form miricl::%compile-closure miricl::%compile-bindings
                   miricl::%compile-local-functions miricl::%expand-backquote miricl::%compile-backquote
                   miricl::%compile-option-function miricl::%compile-condition miricl::%compile-form
                   miricl::%file-lexical-name miricl::%file-lexical-specials miricl::%file-lexical-bind
                   miricl::%file-lexical-reference miricl::%file-code-cons miricl::%file-lexical-forms
                   miricl::%file-lexical-function miricl::%file-lexical-call miricl::%file-lexical-form
                   miricl::%fasl-reference miricl::%fasl-dump
                   compile-native record-edges pool-record-less-p layout-records compact-unit compile-linker ensure-compiler compile-compiler))

(defvar *native-compiler-functions* (make-hash-table :test 'eq))
(defvar *compiler-state* :cold)
(defvar *compiler-origin* :source)

(defun ensure-compiler ()
  "Compile the compiler once on first use; startup only loads its packed source."
  (cond (*compiler-bootstrapping* t)
        ((eq *compiler-state* :ready) t)
        ((eq *compiler-state* :interpreted) nil)
        ((eq *compiler-state* :failed) (error "Compiler bootstrap previously failed."))
        ((eq *compiler-state* :compiling)
         (loop while (eq *compiler-state* :compiling) do (miricl-threads:thread-yield))
         (ensure-compiler))
        (t
         (if (compiler-primitive 21 nil)
             (let ((complete nil))
               (setq *compiler-state* :compiling)
               (unwind-protect
                   (multiple-value-bind (installed unsupported) (compile-compiler)
                     (declare (ignore installed))
                     (when unsupported (error "Compiler bootstrap failed: ~S" unsupported))
                     (setq complete t *compiler-state* :ready) t)
                 (unless complete (setq *compiler-state* :failed))))
             (progn (setq *compiler-state* :interpreted) nil)))))

(defun compile-compiler ()
  "Bootstrap supported compiler kernels; return installed names and unsupported sources.
This does not change the policy for compiling application bodies."
  ;; The first kernels make the optimizer itself fast. The image builder then
  ;; recompiles them with optimization enabled, using these compiled kernels.
  (let ((*compiler-bootstrapping* t) (*wasm-optimize* nil) (installed nil) (unsupported nil))
    (dolist (name *native-compiler-seeds*)
      (unless (gethash name *native-compiler-functions*)
        (handler-case
            (let* ((source (function-lambda-expression (symbol-function name)))
                   (function (if source (compile-native source :name name)
                                 (error "Missing compiler bootstrap source for ~S." name))))
              (setf (symbol-function name) function (gethash name *native-compiler-functions*) t))
          (error (condition) (push (list name (princ-to-string condition)) unsupported))))
      (when (gethash name *native-compiler-functions*) (push name installed)))
    (unless unsupported
      (setq *compiler-state* :ready)
      (unless (eq *compiler-origin* :image) (setq *compiler-origin* :first-use)))
    (values (nreverse installed) (nreverse unsupported))))

(defvar *native-linker* nil)
(defun compile-linker ()
  "Bootstrap the compiler's graph traversal kernels using its native backend."
  (ensure-compiler)
  (unless *native-linker*
    (let ((functions nil))
      (dolist (name '(record-edges compact-unit))
        (unless (gethash name *native-compiler-functions*)
          (let ((source (function-lambda-expression (symbol-function name))))
            (unless source (error "Missing bootstrap source for ~S." name))
            (push (cons name (compile-native source :name name)) functions))))
      ;; Install only when all kernels compiled successfully.
      (dolist (entry functions) (setf (symbol-function (car entry)) (cdr entry)))
      (setq *native-linker* t)))
  t)
