;;;; Initial boxed Wasm backend. Control flow and calls execute in native Wasm;
;;;; existing runtime primitives implement Common Lisp object operations.
(in-package :miricl-compiler)

(defvar *wasm-bytes*)
(defvar *wasm-literals*)
(defvar *wasm-literal-index*)
(defvar *native-functions*)
(defvar *compiler-bootstrapping* nil)
(defvar *wasm-used-imports*)
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
(defparameter *native-imports*
  '(("native-literal" (:object :i32) :object)
    ("native-argument" (:object :i32) :object)
    ("native-rest-argument" (:object :i32) :object)
    ("native-check-arguments" (:object :i32 :i32) nil)
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

(defun wasm-byte (byte) (vector-push-extend byte *wasm-bytes*))
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
    (loop for byte across bytes do (wasm-byte byte))))
(defun wasm-type (type)
  (ecase type (:object (wasm-byte #x6f)) (:i32 (wasm-byte #x7f)) (:function (wasm-byte #x70))))
(defun wasm-section (kind bytes)
  (wasm-byte kind) (wasm-u32 (length bytes)) (loop for byte across bytes do (wasm-byte byte)))
(defmacro with-wasm-bytes (&body body)
  (list* 'let (list (list '*wasm-bytes*
    '(make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (append body (list '*wasm-bytes*))))
(defun wasm-call (name)
  (pushnew name *wasm-used-imports* :test #'equal)
  (wasm-byte #x10) (wasm-u32 (or (position name *native-imports* :key #'first :test #'equal)
                               (error "Unknown native ABI operation ~S." name))))
(defun wasm-get (value)
  (wasm-byte #x20) (wasm-u32 (or (gethash value *wasm-locals*) (error "Missing native operand local."))))
(defun wasm-set (value)
  (let ((local (gethash value *wasm-locals*)))
    (if local (progn (wasm-byte #x21) (wasm-u32 local)) (wasm-byte #x1a))))
(defun wasm-integer (value) (wasm-byte #x41) (wasm-s32 value))
(defun wasm-local (index) (wasm-byte #x20) (wasm-u32 index))
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
  (if (null value) (wasm-null)
      (let ((index (multiple-value-bind (index found) (gethash value *wasm-literal-index*)
                     (if found index (setf (gethash value *wasm-literal-index*)
                                      (vector-push-extend value *wasm-literals*))))))
        (wasm-local 0) (wasm-integer index) (wasm-call "native-literal"))))
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
  (let ((cells (make-hash-table :test 'eq)))
    (dolist (binding (hir-function-bindings function))
      (when (and (hir-binding-cell binding)
                 (or (not (hir-binding-captured-p binding))
                     (not (hir-binding-assigned-p binding))))
        (setf (gethash (hir-binding-cell binding) cells) t)))
    (dolist (value (hir-function-values function))
      (let ((producer (hir-value-producer value)))
        (when (and (hir-instruction-p producer)
                   (eq (hir-instruction-opcode producer) :capture))
          (let ((binding (second (hir-instruction-attributes producer))))
            (when (and (hir-binding-p binding) (not (hir-binding-assigned-p binding)))
              (setf (gethash value cells) t))))))
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
  (let ((live (make-hash-table :test 'eq)) (work nil))
    (dolist (block (or blocks (hir-function-blocks function)))
      (when dynamic-bindings (push (hir-block-environment block) work))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (unless (native-removable-p instruction)
          (dolist (input (hir-instruction-inputs instruction)) (push input work)))))
    (loop while work do
      (let ((value (pop work)))
        (unless (gethash value live)
          (setf (gethash value live) t)
          (let ((producer (hir-value-producer value)))
            (when (hir-instruction-p producer)
              (dolist (input (hir-instruction-inputs producer)) (push input work)))))))
    live))

(defun native-single-calls (function scalar)
  ;; Multiple-value packets are only needed when a consumer observes the
  ;; group. Do not allocate a packet merely to extract its primary value.
  ;; Keep this representation choice local to emission: HIR remains reusable.
  (let ((calls (make-hash-table :test 'eq)))
    (dolist (value (hir-function-values function))
      (let ((producer (hir-value-producer value)))
        (when (and (hir-instruction-p producer) (eq (hir-instruction-opcode producer) :call)
                   (<= (length (hir-instruction-inputs producer)) 6) (gethash value scalar))
          (setf (gethash value calls) t))))
    calls))

(defun native-full-groups (function)
  ;; Propagate multiple-value demand backwards through joins. An IF used in
  ;; a single-value context must not force each arm through the full protocol.
  (let ((full (make-hash-table :test 'eq)) (sources (make-hash-table :test 'eq)) (work nil))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (let* ((opcode (hir-instruction-opcode instruction))
               (target (and (member opcode '(:jump :block-return))
                            (first (hir-instruction-attributes instruction))))
               (parameter (and target (first (hir-block-parameters target)))))
          (cond
            (parameter
             (push (if (eq opcode :jump) (first (hir-instruction-inputs instruction))
                       (third (hir-instruction-inputs instruction))) (gethash parameter sources)))
            ((not (eq opcode :primary))
             (dolist (input (hir-instruction-inputs instruction))
               (when (eq (hir-value-kind input) :multiple) (push input work))))))))
    (loop while work do
      (let ((value (pop work)))
        (unless (gethash value full)
          (setf (gethash value full) t)
          (dolist (source (gethash value sources)) (push source work)))))
    full))

(defun native-scalar-groups (function)
  ;; Value cardinality is independent of object type. Carry groups known to
  ;; contain exactly one value as the value itself, including through joins.
  (let ((scalar (make-hash-table :test 'eq)) (incoming (make-hash-table :test 'eq)) (changed t))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (hir-block-instructions block))
        (when (and (eq (hir-instruction-opcode instruction) :values)
                   (= (length (hir-instruction-inputs instruction)) 1))
          (setf (gethash (hir-instruction-output instruction) scalar) t)))
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
          (when (and parameter values (not (gethash parameter scalar))
                     (every (lambda (value) (gethash value scalar)) values))
            (setf (gethash parameter scalar) t) (setq changed t)))))
    (let ((full (native-full-groups function)))
      (dolist (value (hir-function-values function))
        (when (and (eq (hir-value-kind value) :multiple) (not (gethash value full)))
          (setf (gethash value scalar) t))))
    scalar))

(defun emit-native-fixed-call (inputs)
  (dolist (input (cdr inputs)) (wasm-get input))
  (wasm-call (nth (length (cddr inputs))
               '("native-call0" "native-call1" "native-call2" "native-call3" "native-call4"))))

(defun emit-native-cons-access (inputs)
  (let* ((function (second inputs)) (producer (hir-value-producer function))
         (name (when (and (hir-instruction-p producer)
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

(defun emit-native-instruction (instruction)
  (let ((inputs (hir-instruction-inputs instruction)) (attributes (hir-instruction-attributes instruction))
        (output (hir-instruction-output instruction)))
    (when (and (native-removable-p instruction) (not (gethash output *wasm-live-values*)))
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
      (:values (if (gethash output *wasm-scalar-groups*)
                   (if inputs (wasm-get (first inputs)) (wasm-null)) (wasm-list inputs)))
      (:list (wasm-list inputs))
      (:value-ref
       (if (gethash (first inputs) *wasm-scalar-groups*) (wasm-null)
           (progn (wasm-get (first inputs)) (wasm-integer (first attributes)) (wasm-call "native-argument"))))
      ((:cell-new :cell-read :cell-write :primary)
       (cond
         ((and (eq (hir-instruction-opcode instruction) :primary)
               (or (gethash (first inputs) *wasm-single-calls*)
                   (gethash (first inputs) *wasm-scalar-groups*)))
          (wasm-get (first inputs)))
         ((and (eq (hir-instruction-opcode instruction) :cell-new) (gethash output *wasm-local-cells*))
          (wasm-get (first inputs)))
         ((and (eq (hir-instruction-opcode instruction) :cell-read) (gethash (first inputs) *wasm-local-cells*))
          (wasm-get (first inputs)))
         ((and (eq (hir-instruction-opcode instruction) :cell-write) (gethash (first inputs) *wasm-local-cells*))
          (wasm-get (second inputs)) (wasm-set (first inputs)) (wasm-get (second inputs)))
         (t
          (dolist (input inputs) (wasm-get input))
          (wasm-call (ecase (hir-instruction-opcode instruction)
                    (:cell-new "native-cell") (:cell-read "native-cell-read")
                    (:cell-write "native-cell-write") (:primary "native-primary"))))))
      (:call
       (if (gethash output *wasm-single-calls*)
           ;; Keep guards in the shared native primitives. Repeating CAR/CDR
           ;; type and redefinition checks at every call bloats Lisp-heavy code.
           (emit-native-fixed-call inputs)
           (progn (wasm-get (first inputs)) (wasm-get (second inputs))
                  (wasm-list (cddr inputs)) (wasm-call "native-call")
                  (when (gethash output *wasm-scalar-groups*) (wasm-call "native-primary")))))
      (:control-call
       (wasm-get (first inputs)) (wasm-integer (first attributes)) (wasm-get (second inputs))
       (if (member (first attributes) '(1 2 4 5 6 7)) (wasm-list (nthcdr 4 inputs)) (wasm-get (fourth inputs)))
       (wasm-get (third inputs)) (wasm-call "native-control")
       (when (gethash output *wasm-scalar-groups*) (wasm-call "native-primary")))
      (:restart-signal
       (wasm-get (first inputs)) (wasm-integer (first attributes))
       (wasm-list (cddr inputs)) (wasm-get (second inputs)) (wasm-call "native-restart-signal")
       (when (gethash output *wasm-scalar-groups*) (wasm-call "native-primary")))
      (:multiple-value-call
       (wasm-get (first inputs)) (wasm-get (second inputs))
       (dolist (group (cddr inputs))
         (wasm-get group)
         (when (gethash group *wasm-scalar-groups*) (wasm-null) (wasm-call "native-cons")))
       (wasm-null) (dotimes (i (length (cddr inputs))) (wasm-call "native-cons"))
       (wasm-call "native-multiple-value-call")
       (when (gethash output *wasm-scalar-groups*) (wasm-call "native-primary")))
      (:capture
       (wasm-local 0) (wasm-integer (first attributes)) (wasm-call "native-capture"))
      (:closure
       (pushnew (first attributes) *wasm-referenced-functions*)
       (wasm-byte #xd2)
       (wasm-u32 (+ (length *native-imports*) (position (first attributes) *native-functions*)))
       (wasm-local 0) (wasm-list inputs) (wasm-call "native-close"))
      (otherwise (error "Native lowering is not implemented for ~S." (hir-instruction-opcode instruction))))
    (when output (wasm-set output))))

(defun emit-native-jump-value (block value)
  (when value
    (wasm-get value)
    (when (and (gethash value *wasm-scalar-groups*)
               (not (gethash (first (hir-block-parameters block)) *wasm-scalar-groups*)))
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
  (wasm-call (if (gethash value *wasm-scalar-groups*) "native-return1" "native-return")))

(defun emit-native-throw (inputs)
  (dolist (input inputs) (wasm-get input))
  (when (gethash (third inputs) *wasm-scalar-groups*) (wasm-null) (wasm-call "native-cons"))
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
  (when (gethash (third inputs) *wasm-scalar-groups*) (wasm-null) (wasm-call "native-cons"))
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
  ;; Boxed SSA values are externref locals. A dispatcher is a first correct
  ;; lowering of arbitrary CFG edges; structured regions replace it later.
  (let* ((targets (native-block-targets function))
        (forward (unless targets (native-forward-blocks function)))
        (blocks (or forward (hir-function-blocks function)))
        (*wasm-local-cells* (native-direct-cells function))
        (*wasm-dynamic-bindings* (or targets (some (lambda (block)
          (some (lambda (instruction) (eq (hir-instruction-opcode instruction) :dynamic-bind))
            (hir-block-instructions block))) blocks)))
        (*wasm-entry-environment* (hir-function-environment function))
        (*wasm-live-values* (native-live-values function *wasm-dynamic-bindings* blocks))
        (*wasm-scalar-groups* (native-scalar-groups function))
        (*wasm-single-calls* (native-single-calls function *wasm-scalar-groups*))
        (*wasm-locals* (make-hash-table :test 'eq)) (count 0))
   (dolist (value (hir-function-values function))
     (when (gethash value *wasm-live-values*)
       (let* ((producer (hir-value-producer value))
              (alias (and (hir-instruction-p producer)
                       (member (hir-instruction-opcode producer) '(:block-enter :block-environment))
                       (not (hir-target-captured-p (first (hir-instruction-attributes producer))))
                       (first (hir-instruction-inputs producer)))))
         (if alias (setf (gethash value *wasm-locals*) (gethash alias *wasm-locals*))
             (progn (setf (gethash value *wasm-locals*) (+ (if forward 2 3) count)) (incf count))))))
   (with-wasm-bytes
    (wasm-u32 (if forward 1 2))
    (unless forward (wasm-u32 1) (wasm-type :i32))
    (wasm-u32 count) (wasm-type :object)
    (when (gethash (hir-function-environment function) *wasm-live-values*)
      (wasm-call "native-environment") (wasm-set (hir-function-environment function)))
    (when forward
      (emit-native-forward blocks)
      (return-from emit-native-function *wasm-bytes*))
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
              (progn (when (gethash parameter *wasm-scalar-groups*) (wasm-call "native-primary"))
                     (wasm-set parameter))
              (wasm-byte #x1a)))
        (wasm-integer (hir-block-id (hir-target-block target)))
        (wasm-byte #x21) (wasm-u32 2)
        (wasm-byte #x0c) (wasm-u32 1) (wasm-byte #x0b))
      (wasm-byte #x08) (wasm-u32 0) ; propagate foreign transfers unchanged
      (wasm-byte #x0b) (wasm-byte #x00))
    (wasm-byte #x0b))))

(defun emit-native-module (unit)
  "Return Wasm bytes and literal objects; do not instantiate or run application code."
  (ensure-compiler)
  (verify-hir unit)
  (let* ((*wasm-literals* (make-array 16 :adjustable t :fill-pointer 0))
         (*wasm-literal-index* (make-hash-table :test 'eq))
         (*native-functions* (hir-unit-functions unit))
         (*wasm-used-imports* nil) (*wasm-referenced-functions* nil)
         (*wasm-object-types* nil) (*wasm-exceptions* nil) (*wasm-builtin-codes* (make-hash-table :test 'eq)))
    ;; Collect imports using the emitter itself. Regenerate after assigning the
    ;; compact function indices, including REF.FUNC indices in closures.
    (mapc #'emit-native-function *native-functions*)
    (let* ((type-prefix (if *wasm-object-types* 2 0))
         (*native-imports* (remove-if-not (lambda (import) (member (first import) *wasm-used-imports* :test #'equal))
                                          *native-imports*))
         (*wasm-literals* (make-array 16 :adjustable t :fill-pointer 0))
         (*wasm-literal-index* (make-hash-table :test 'eq))
         (bodies (mapcar #'emit-native-function *native-functions*))
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
             (wasm-u32 1) (wasm-name "entry") (wasm-byte 0) (wasm-u32 (length *native-imports*))))
           (when *wasm-referenced-functions*
             (wasm-section 9 (with-wasm-bytes
               (wasm-u32 1) (wasm-u32 3) (wasm-byte 0) (wasm-u32 (length *wasm-referenced-functions*))
               (dolist (function *wasm-referenced-functions*)
                 (wasm-u32 (+ (length *native-imports*) (position function *native-functions*)))))))
           (wasm-section 10 (with-wasm-bytes
             (wasm-u32 (length bodies))
             (dolist (body bodies) (wasm-u32 (length body)) (loop for byte across body do (wasm-byte byte))))))))
    (values bytes (coerce *wasm-literals* 'simple-vector)))))

(defun compile-native (lambda-expression &key name)
  "Compile a lambda to native Wasm using the current host's module loader."
  (when (and (not *compiler-bootstrapping*) (compiler-primitive 23 nil))
    (return-from compile-native (compiler-primitive 24 lambda-expression name)))
  (ensure-compiler)
  (multiple-value-bind (bytes literals) (emit-native-module (compile-to-hir lambda-expression :name name))
    (multiple-value-bind (function native) (compiler-primitive 17 bytes literals name)
      (unless native (error "This host cannot instantiate compiled Wasm modules.")) function)))

(defparameter *native-compiler-seeds*
  '(compiler-primitive
                   wasm-byte wasm-u32 wasm-s32 wasm-name wasm-type wasm-section wasm-call
                   wasm-get wasm-set wasm-integer wasm-local wasm-null wasm-gc wasm-as-object
                   native-builtin-code emit-native-object-types wasm-literal wasm-list
                   native-removable-p native-direct-cells native-forward-blocks native-live-values native-single-calls native-full-groups native-scalar-groups native-block-targets
                   emit-native-fixed-call emit-native-cons-access emit-native-instruction emit-native-jump-value emit-native-jump emit-native-return emit-native-throw emit-native-block-return emit-native-forward
                   hir-new-value hir-new-block hir-emit hir-terminate hir-single hir-multiple hir-nth-value hir-constant
                   hir-capture hir-cell-for-binding hir-special-p hir-declarations hir-shadow-specials
                   hir-bind-value hir-read-variable hir-write-variable hir-function-reference
                   hir-small-membership hir-call-arguments hir-case-form hir-macro-environment hir-assert-form
                   hir-expand-parallel-places hir-expand-arithmetic-step hir-restart-options hir-correctable-place-form
                   hir-condition-initializer hir-define-condition hir-restart-expression hir-control-form verify-hir
                   emit-native-function emit-native-module
                   symbol-key function-key literal-initializer-p static-type-form-p structure-info class-info method-info
                   definition-info namespace form-references
                   miricl::%compile-place-form miricl::%compile-parallel-places miricl::%compile-loop-collector
                   miricl::%compile-forms miricl::%compile-body miricl::%compile-lambda-parts
                   miricl::%compile-lambda-form miricl::%compile-closure miricl::%compile-bindings
                   miricl::%compile-local-functions miricl::%expand-backquote miricl::%compile-backquote
                   miricl::%compile-option-function miricl::%compile-condition miricl::%compile-form
                   miricl::%file-lexical-name miricl::%file-lexical-specials miricl::%file-lexical-bind
                   miricl::%file-lexical-reference miricl::%file-code-cons miricl::%file-lexical-forms
                   miricl::%file-lexical-function miricl::%file-lexical-form
                   miricl::%fasl-reference miricl::%fasl-dump
                   hir-body hir-close hir-choice hir-expand-local-macros hir-lambda hir-form
                   compile-to-hir compile-native record-edges pool-record-less-p layout-records compact-unit compile-linker ensure-compiler compile-compiler))

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
  (let ((*compiler-bootstrapping* t) (installed nil) (unsupported nil))
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
