;;;; Size optimization of the native backend's stable Wasm instruction subset.
;;;; Operate after lowering so mutable cells and hidden ABI reads are explicit.
(in-package :miricl-compiler)

;; The module adapter supplies exact stack effects for arbitrary core-module
;; instructions. Native emission uses its cheaper, fixed ABI description.
(defvar *native-module-stack-effects* nil)

;; These compiler-private records use simple vectors. Macro accessors expand
;; into SVREF in both hosts, including SETF places, so the self-compiled pass
;; never invokes interpreted DEFSTRUCT accessor/constructor closures per op.
(defmacro native-flow-start (block) (list 'svref (list 'the 'simple-vector block) 0))
(defmacro native-flow-end (block) (list 'svref (list 'the 'simple-vector block) 1))
(defmacro native-flow-successors (block) (list 'svref (list 'the 'simple-vector block) 2))
(defmacro native-flow-uses (block) (list 'svref (list 'the 'simple-vector block) 3))
(defmacro native-flow-definitions (block) (list 'svref (list 'the 'simple-vector block) 4))
(defmacro native-flow-in (block) (list 'svref (list 'the 'simple-vector block) 5))
(defmacro native-flow-out (block) (list 'svref (list 'the 'simple-vector block) 6))

(defun make-native-op (code argument start end)
  (declare (type (or null (unsigned-byte 8)) code) (type (unsigned-byte 32) start end))
  (vector code argument start end nil 0 0 0 0))
(defun make-native-flow (start)
  (declare (type (unsigned-byte 32) start))
  (vector start nil nil 0 0 0 0))

(defun native-read-uleb (bytes position)
  (declare (type vector bytes) (type (unsigned-byte 32) position))
  (when (>= position (length bytes))
    (compiler-input-fail "native LEB128" bytes "truncated unsigned operand"))
  ;; Most operands are a single byte, including opcodes, small indices and
  ;; block types. Avoid entering the general shift/accumulate loop for them.
  (let ((byte (aref bytes position)))
    (declare (type (unsigned-byte 8) byte))
    (when (< byte 128) (return-from native-read-uleb (values byte (1+ position)))))
  (let ((value 0) (shift 0))
    (declare (type (integer 0 *) value) (type fixnum shift))
    (loop do
      (when (>= position (length bytes))
        (compiler-input-fail "native LEB128" bytes "truncated unsigned operand"))
      (let ((byte (aref bytes position)))
      (when (and (= shift 63) (> byte 1))
        (compiler-input-fail "native LEB128" bytes "unsigned operand exceeds 64 bits"))
      (incf position) (setq value (logior value (ash (logand byte 127) shift)))
      (when (< byte 128) (return (values value position))) (incf shift 7)))))

(defun native-read-sleb (bytes position)
  (declare (type vector bytes) (type (unsigned-byte 32) position))
  (when (>= position (length bytes))
    (compiler-input-fail "native LEB128" bytes "truncated signed operand"))
  (let ((byte (aref bytes position)))
    (declare (type (unsigned-byte 8) byte))
    (when (< byte 128)
      (return-from native-read-sleb
        (values (if (< byte 64) byte (- byte 128)) (1+ position)))))
  (let ((value 0) (shift 0))
    (declare (type (integer 0 *) value) (type fixnum shift))
    (loop do
      (when (>= position (length bytes))
        (compiler-input-fail "native LEB128" bytes "truncated signed operand"))
      (let ((byte (aref bytes position)))
      (when (and (= shift 63) (not (member byte '(0 127))))
        (compiler-input-fail "native LEB128" bytes "signed operand exceeds 64 bits"))
      (incf position) (setq value (logior value (ash (logand byte 127) shift)))
      (incf shift 7)
      (when (< byte 128)
        (return (values (if (logbitp 6 byte) (- value (ash 1 shift)) value) position)))))))

(defun native-read-heap-type (bytes position)
  ;; An explicit reference type encodes its heap type as signed LEB33.
  (multiple-value-bind (heap end) (native-read-sleb bytes position)
    (unless (and (<= (- end position) 5) (<= (- (ash 1 32)) heap #xffffffff))
      (compiler-input-fail "native heap type" bytes "heap type exceeds signed 33 bits"))
    (values heap end)))

(defun native-local-type-end (bytes position)
  (when (>= position (length bytes))
    (compiler-input-fail "native locals" bytes "missing local type"))
  (case (aref bytes position)
    ((#x7f #x7e #x6c #x6d #x6f) (1+ position))
    ((#x63 #x64) (nth-value 1 (native-read-heap-type bytes (1+ position))))
    (otherwise (compiler-input-fail "native locals" bytes "unsupported local type"))))

(defun native-read-ops (bytes &optional references-only)
  (declare (type vector bytes))
  (let ((decoded (compiler-primitive 38 bytes references-only)))
    (when decoded
      (return-from native-read-ops
        (values (svref decoded 0) (svref decoded 1) (svref decoded 2)))))
  (native-read-ops-portable bytes references-only))

(defun native-read-ops-portable (bytes &optional references-only)
  (declare (type vector bytes))
  (let ((position 0) (offset 2) (locals 0) (ops nil) (preserve nil))
    (declare (type (unsigned-byte 32) position offset locals))
    (multiple-value-bind (groups next) (native-read-uleb bytes position)
      (unless (<= next 5)
        (compiler-input-fail "native locals" bytes "local group count exceeds unsigned 32 bits"))
      (when (> groups (floor (- (length bytes) next) 2))
        (compiler-input-fail "native locals" bytes "truncated local declarations"))
      (setq position next)
      (dotimes (i groups)
        (multiple-value-bind (count next) (native-read-uleb bytes position)
          (unless (and (<= (- next position) 5) (<= (+ offset locals count) #xffffffff))
            (compiler-input-fail "native locals" bytes "too many locals"))
          (setq position (native-local-type-end bytes next))
          (case (aref bytes next)
            ((#x63 #x64 #x6d) (setq preserve t) (incf offset count))
            ((#x7f #x7e #x6c) (incf offset count))
            (#x6f (incf locals count))))))
    (loop while (< position (length bytes)) do
      (let* ((start position) (code (aref bytes position)) (argument nil))
        (declare (type (unsigned-byte 32) start) (type (unsigned-byte 8) code))
        (incf position)
        (cond
          ((member code '(#x02 #x03 #x04 #x08 #x0c #x0d #x10 #x12 #x20 #x21 #x22 #x23 #x41 #x42 #xd0 #xd2))
           ;; Constants and heap types only need their encoded extent here.
           ;; Decode the common one-byte case here, avoiding a helper call
           ;; and multiple-value packet for nearly every instruction operand.
           (when (>= position (length bytes))
             (compiler-input-fail "native instruction" bytes "missing operand"))
           (let ((byte (aref bytes position)))
             (declare (type (unsigned-byte 8) byte))
             (if (< byte 128)
                 (progn (setq argument (if (and (member code '(#x41 #x42)) (>= byte 64)) (- byte 128) byte))
                   (incf position))
                 (multiple-value-setq (argument position)
                   (case code
                     ((#x41 #x42) (native-read-sleb bytes position))
                     (#xd0 (native-read-heap-type bytes position))
                     (otherwise (native-read-uleb bytes position)))))
             (when (and (= code #x23) (or (> argument #xffffffff) (> (- position start) 6)))
               (compiler-input-fail "native global.get" bytes "global index exceeds unsigned 32 bits"))))
          ((= code #x0e)
           (let ((count nil))
             (multiple-value-setq (count position) (native-read-uleb bytes position))
             (unless (< count (- (length bytes) position))
               (compiler-input-fail "native br_table" bytes "truncated branch targets"))
             (dotimes (i (1+ count))
               (let ((depth nil))
                 (multiple-value-setq (depth position) (native-read-uleb bytes position))
                 (unless references-only (push depth argument))))
             (setq argument (nreverse argument))))
          ((= code #x1f)
           (unless (and (< position (length bytes)) (= (aref bytes position) #x40))
             (compiler-input-fail "native try_table" bytes "expected an empty block type"))
           (incf position)
           (let ((count nil))
             (multiple-value-setq (count position) (native-read-uleb bytes position))
             (unless (<= count (floor (- (length bytes) position) 2))
               (compiler-input-fail "native try_table" bytes "truncated catches"))
             (dotimes (i count)
               (unless (< position (length bytes))
                 (compiler-input-fail "native try_table" bytes "missing catch kind"))
               (let ((kind (aref bytes position)))
                 (incf position)
                 (unless (<= 0 kind 3)
                   (compiler-input-fail "native try_table" bytes "unsupported catch kind"))
                 (when (< kind 2)
                   (setq position (nth-value 1 (native-read-uleb bytes position))))
                 (setq position (nth-value 1 (native-read-uleb bytes position)))))))
          ((= code #xfb)
           (multiple-value-setq (argument position) (native-read-uleb bytes position))
           (case argument
             ((2 5)
              (dotimes (i 2) (setq position (nth-value 1 (native-read-uleb bytes position)))))
             ((#x0b #x0e)
              (multiple-value-bind (index end) (native-read-uleb bytes position)
                (unless (and (<= index #xffffffff) (<= (- end position) 5))
                  (compiler-input-fail "native array instruction" bytes "type index exceeds unsigned 32 bits"))
                (setq position end preserve t)))
             (#x0f (setq preserve t))
             ((#x14 #x16 #x17)
              (setq position (nth-value 1 (native-read-heap-type bytes position))))
             ((#x1a #x1b #x1c #x1d))
             (otherwise (compiler-input-fail "native GC instruction" bytes "unsupported subopcode"))))
          ;; These additional scalar opcodes are also outside the optimizer's
          ;; native stack model. Decode their extent and keep the raw body.
          ((member code '(#x6c #x6e)) (setq preserve t))
          ((not (member code '(#x00 #x05 #x0b #x0f #x1a #x45 #x46 #x47 #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5a #x6a #x6b #x71 #x7c #x7d #x7e #x80 #x82 #x83 #x84 #x85 #x86 #x87 #xa7 #xac #xad #xd1 #xd3)))
           (error "Unexpected native instruction ~D." code)))
        (when (or (not references-only) (member code '(#x10 #x12 #xd2)))
          (push (make-native-op code argument start position) ops))))
    (values (coerce (nreverse ops) 'simple-vector) offset locals preserve)))

(defun native-op-target (depth scopes ends ops)
  (declare (type (unsigned-byte 32) depth) (type list scopes) (type simple-vector ends ops))
  (let ((scope (nth depth scopes)))
    (when scope
      (if (= (native-op-code (aref ops scope)) #x03) (1+ scope)
          (1+ (aref ends scope))))))

(defun native-control-matches (ops)
  (declare (type simple-vector ops))
  (let* ((size (length ops)) (ends (make-array size :initial-element nil))
         (elses (make-array size :initial-element nil)) (scopes nil))
    (declare (type (unsigned-byte 32) size) (type simple-vector ends elses) (type list scopes))
    (dotimes (i size)
      (declare (type (unsigned-byte 32) i))
      (case (native-op-code (aref ops i))
        ((#x02 #x03 #x04 #x1f) (push i scopes))
        (#x05 (setf (aref elses (first scopes)) i))
        (#x0b (when scopes (setf (aref ends (pop scopes)) i)))))
    (values ends elses)))

(defun native-op-flow (ops)
  (declare (type simple-vector ops))
  (multiple-value-bind (ends elses) (native-control-matches ops)
   (declare (type simple-vector ends elses))
   (let ((size (length ops)) (scopes nil) (exceptions nil))
    (declare (type (unsigned-byte 32) size) (type list scopes))
    (dotimes (i size)
      (declare (type (unsigned-byte 32) i))
      (let* ((op (aref ops i)) (code (native-op-code op)) (next (and (< (1+ i) size) (1+ i)))
             (successors (and next (list next))))
        (case code
          ((#x02 #x03 #x1f) (push i scopes) (when (= code #x1f) (setq exceptions t)))
          (#x04 (push (if (aref elses i) (1+ (aref elses i)) (aref ends i)) successors)
                (push i scopes))
          (#x05 (setq successors (list (aref ends (first scopes)))))
          (#x0b (pop scopes))
          ((#x00 #x08 #x0f #x12) (setq successors nil))
          ((#x0c #x0d)
           (unless (= code #x0d) (setq successors nil))
           (let ((target (native-op-target (native-op-argument op) scopes ends ops)))
             (when (and target (< target size)) (pushnew target successors))))
          (#x0e
           (setq successors nil)
           (dolist (depth (native-op-argument op))
             (let ((target (native-op-target depth scopes ends ops)))
               (when (and target (< target size)) (pushnew target successors))))))
        (setf (native-op-successors op) successors)))
    exceptions)))

(defun native-liveness-blocks (ops)
  (declare (type simple-vector ops))
  ;; Stackification and coloring change local reads/writes, never CFG edges.
  ;; Build the block topology once and reuse it across all liveness passes.
  (let* ((size (length ops)) (leaders (make-array size :initial-element nil))
         (owners (make-array size)) (blocks nil) (block nil) (count -1))
    (declare (type (unsigned-byte 32) size) (type simple-vector leaders owners)
             (type (or list simple-vector) blocks) (type fixnum count))
    (setf (aref leaders 0) t)
    (dotimes (i size)
      (let ((successors (native-op-successors (aref ops i))))
        (unless (and (= (length successors) 1) (eql (first successors) (1+ i)))
          (dolist (next successors) (setf (aref leaders next) t))
          (when (< (1+ i) size) (setf (aref leaders (1+ i)) t)))))
    (dotimes (i size)
      (when (aref leaders i)
        (when block (setf (native-flow-end block) i))
        (setq block (make-native-flow i)) (push block blocks) (incf count))
      (setf (aref owners i) count))
    (setf (native-flow-end block) size)
    (setq blocks (coerce (nreverse blocks) 'simple-vector))
    (loop for block across blocks do
      (setf (native-flow-successors block)
        (remove-duplicates (mapcar (lambda (next) (aref owners next))
          (native-op-successors (aref ops (1- (native-flow-end block))))))))
    blocks))

(defun native-op-liveness (ops total exceptions &optional blocks)
  (declare (type simple-vector ops) (type (unsigned-byte 32) total)
           (type (or null simple-vector) blocks)
           (notinline native-op-liveness-lisp))
  (setq blocks (or blocks (native-liveness-blocks ops)))
  (when (compiler-primitive 41 ops (vector blocks exceptions) total)
    (return-from native-op-liveness blocks))
  (native-op-liveness-lisp ops total exceptions blocks))

(defun native-op-liveness-lisp (ops total exceptions &optional blocks)
  (declare (type simple-vector ops) (type (unsigned-byte 32) total)
           (type (or null simple-vector) blocks))
  ;; Handler-bearing functions conservatively read every local at each call.
  ;; This protects landing-pad state even though exceptional edges are not
  ;; represented in the normal CFG. Those functions are not coalesced.
  (let ((all (1- (ash 1 total))) (blocks (or blocks (native-liveness-blocks ops)))
        (cyclic nil) (changed t))
    (declare (type (integer 0 *) all) (type simple-vector blocks))
    (loop for block across blocks for index from 0 do
      (when (some (lambda (next) (<= next index)) (native-flow-successors block))
        (setq cyclic t))
      (setf (native-flow-uses block) 0 (native-flow-definitions block) 0
            (native-flow-in block) 0 (native-flow-out block) 0)
      (loop for i from (native-flow-start block) below (native-flow-end block) do
        (let* ((op (aref ops i)) (code (native-op-code op)))
          (setf (native-op-uses op)
                  (cond ((eql code #x20) (ash 1 (native-op-argument op)))
                        ((and exceptions (member code '(#x08 #x10 #x12))) all) (t 0))
                (native-op-definitions op)
                  (if (member code '(#x21 #x22)) (ash 1 (native-op-argument op)) 0))
          ;; Most instructions neither read nor write locals. Reuse the
          ;; existing bitmap instead of copying it through a union with zero.
          (cond
            ((eql code #x20)
             (let ((local (native-op-argument op)))
               (unless (or (logbitp local (native-flow-definitions block))
                           (logbitp local (native-flow-uses block)))
                 (setf (native-flow-uses block)
                   (logior (native-flow-uses block) (native-op-uses op))))))
            ((not (zerop (native-op-uses op)))
             (setf (native-flow-uses block)
               (logior (native-flow-uses block) (logandc2 (native-op-uses op) (native-flow-definitions block))))))
          (unless (or (zerop (native-op-definitions op))
                      (logbitp (native-op-argument op) (native-flow-definitions block)))
            (setf (native-flow-definitions block)
              (logior (native-flow-definitions block) (native-op-definitions op)))))))
    ;; A forward-only CFG needs one reverse sweep. Cycles iterate block
    ;; summaries to a fixed point, without revisiting their instructions.
    (loop while changed do
      (setq changed nil)
      (loop for i downfrom (1- (length blocks)) to 0 do
        (let* ((block (aref blocks i)) (out 0) (in nil))
          (declare (type simple-vector block) (type (integer 0 *) out)
                   (type (or null (integer 0 *)) in))
          (dolist (next (native-flow-successors block))
            (setq out (logior out (native-flow-in (aref blocks next)))))
          (setq in (logior (native-flow-uses block) (logandc2 out (native-flow-definitions block))))
          (unless (and (= in (native-flow-in block)) (= out (native-flow-out block)))
            (when cyclic (setq changed t))
            (setf (native-flow-in block) in (native-flow-out block) out)))))
    (loop for block across blocks do
      (let ((out (native-flow-out block)))
        (declare (type (integer 0 *) out))
        (loop for i downfrom (1- (native-flow-end block)) to (native-flow-start block) do
          (let ((op (aref ops i)))
            (setf (native-op-live-out op) out)
            (unless (or (zerop (native-op-definitions op))
                        (not (logbitp (native-op-argument op) out)))
              (setq out (logandc2 out (native-op-definitions op))))
            (unless (or (zerop (native-op-uses op))
                        (and (eql (native-op-code op) #x20)
                             (logbitp (native-op-argument op) out)))
              (setq out (logior (native-op-uses op) out)))
            (setf (native-op-live-in op) out)))))
    blocks))

(defun native-op-stack (op)
  (declare (type simple-vector op))
  ;; Values consumed/produced by the backend's instruction subset. Scopes are
  ;; barriers; no expression is moved or duplicated across a side effect.
  (let ((code (native-op-code op)))
    (when (and *native-module-stack-effects* (not (member code '(#x20 #x21 #x22 #x1a #x41 #xd0 #xd2))))
      (let ((effect (gethash op *native-module-stack-effects*)))
        (unless effect (error "Missing module instruction stack effect."))
        (return-from native-op-stack (values (car effect) (cdr effect)))))
    (case code
      ((#x20 #x23 #x41 #x42 #xd0 #xd2) (values 0 1))
      ((#x21 #x1a) (values 1 0))
      ((#x22 #x45 #x50 #xa7 #xac #xad #xd1) (values 1 1))
      (#xfb (if (= (native-op-argument op) 5) (values 2 0) (values 1 1)))
      ((#x46 #x47 #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5a #x6a #x6b #x71 #x7c #x7d #x7e #x80 #x82 #x83 #x84 #x85 #x86 #x87 #xd3) (values 2 1))
      ((#x10 #x12)
       (when (and *native-call-stack-effects* (< (native-op-argument op) (length *native-call-stack-effects*)))
         (let ((effect (svref *native-call-stack-effects* (native-op-argument op))))
           (when (consp effect)
             (return-from native-op-stack (values (car effect) (if (= code #x10) (cdr effect) 0))))
           (return-from native-op-stack (values (ash effect -1) (if (= code #x10) (logand effect 1) 0)))))
       (let ((import (nth (native-op-argument op) *native-imports*)))
               (if import
                   (values (length (second import))
                     (if (= code #x10)
                         (let ((results (third import))) (if (listp results) (length results) 1)) 0))
                   (values 2 (if (= code #x10) 1 0)))))
      (otherwise (values 0 0)))))

(defun native-scalar-call-effects (ops effects)
  ;; Preserve the scalar primitive39 ABI. Unused multi-result signatures have
  ;; inert placeholders in a cached adapter; a body actually calling one uses
  ;; the general pass. Never pass a cons effect to the unchecked native kernel.
  (unless (and *native-scalar-stack-effects* (eq (svref *native-scalar-stack-effects* 0) effects))
    (let ((multiple (not (every #'integerp effects))))
      (setq *native-scalar-stack-effects*
        (vector effects multiple
          (if multiple (map 'simple-vector (lambda (effect) (if (integerp effect) effect 0)) effects) effects)))))
  (when (svref *native-scalar-stack-effects* 1)
    (loop for op across ops when (member (native-op-code op) '(#x10 #x12)) do
      (let ((index (native-op-argument op)))
        (unless (and (typep index '(unsigned-byte 32)) (< index (length effects))
                     (integerp (svref effects index)))
          (return-from native-scalar-call-effects nil)))))
  (svref *native-scalar-stack-effects* 2))

(defun native-stackify-ops (ops &optional exceptions types total)
  (declare (type simple-vector ops) (type (or null simple-vector) types)
           (type (or null (unsigned-byte 32)) total))
  ;; The fast pass needs stack counts and defaultable locals, rather than a
  ;; particular numeric representation. Typed i64 locals can use it too.
  ;; Arbitrary module effects and non-defaultable refs retain the Lisp pass.
  (when (and (null *native-module-stack-effects*)
             (or (null types) (every (lambda (type) (not (eql (car type) #x64))) types)))
    (unless total
      (setq total (loop for op across ops
                    when (member (native-op-code op) '(#x20 #x21 #x22))
                      maximize (1+ (native-op-argument op)) into count
                    finally (return (or count 0)))))
    (unless (or *native-call-stack-effects*
                (and *native-import-stack-effects* (eq (car *native-import-stack-effects*) *native-imports*)))
      (setq *native-import-stack-effects*
        (cons *native-imports*
          (map 'simple-vector (lambda (import)
            (native-signature-stack-effect (cdr import))) *native-imports*))))
    (let ((effects (native-scalar-call-effects ops
                     (or *native-call-stack-effects* (cdr *native-import-stack-effects*)))))
      (when (and effects (compiler-primitive 39 ops (vector exceptions effects) total))
        (return-from native-stackify-ops ops))))
  (native-stackify-ops-lisp ops exceptions types total))

(defun native-stackify-ops-lisp (ops &optional exceptions types total)
  (declare (type simple-vector ops) (type (or null simple-vector) types)
           (type (or null (unsigned-byte 32)) total))
  ;; Stack entries are NIL for real operands and an instruction index for a
  ;; store whose value could remain on the stack. Liveness ensures its matching
  ;; load is the last use; any earlier load invalidates the candidate.
  ;; Local numbers form a dense bounded domain. Epochs invalidate the entire
  ;; candidate table at a control boundary without clearing or hashing it.
  (let* ((total (or total (loop for op across ops
                  when (member (native-op-code op) '(#x20 #x21 #x22))
                    maximize (1+ (native-op-argument op)) into count
                  finally (return (or count 0)))))
         (stack nil) (barriers nil)
         (candidates (make-array total :initial-element nil))
         (epochs (make-array total :initial-element 0)) (epoch 1))
    (declare (type list stack barriers) (type simple-vector candidates epochs)
             (type (unsigned-byte 32) total) (type (unsigned-byte 33) epoch))
    (dotimes (i (length ops))
      (declare (type (unsigned-byte 32) i))
      (let* ((op (aref ops i)) (code (native-op-code op)) (local (native-op-argument op))
             (store (and (eql code #x20) (= (aref epochs (the (unsigned-byte 32) local)) epoch)
                         (aref candidates (the (unsigned-byte 32) local))))
             ;; Other candidate stores are not actual operands. Looking past
             ;; them lets sibling temporaries become stack operands together,
             ;; while an actual value (NIL) remains a strict ordering barrier.
             ;; Instruction positions order stack cells. A store above the
             ;; most recent real operand is available without scanning its
             ;; sibling stores. Turning a store into an operand inserts its
             ;; original position as a barrier, leaving younger stores usable.
             (candidate (and store (or (null barriers)
                                      (> (the (unsigned-byte 32) (car store))
                                         (the (unsigned-byte 32) (car barriers)))) store)))
        (declare (type simple-vector op) (type (or null (unsigned-byte 8)) code)
                 (type list store candidate))
        ;; A throwing call may expose any local to a landing pad. Keeping a
        ;; value on the operand stack cannot replace that observable store.
        (when (and exceptions (member code '(#x08 #x10 #x12))) (incf epoch))
        ;; Wasm's definite-initialization validation is structural, rather than
        ;; ordinary CFG liveness. Keep stores to non-defaultable typed refs even
        ;; when all runtime paths overwrite them inside nested blocks.
        (when (and (member code '(#x21 #x22))
                   (not (and types (eql (car (aref types local)) #x64)))
                   (not (logbitp local (native-op-live-out op))))
          (setf (native-op-code op) (if (= code #x21) #x1a nil))
          (setq code (native-op-code op)))
        (cond
          ((null code))
          ((member code '(#x00 #x02 #x03 #x04 #x05 #x08 #x0b #x0c #x0d #x0e #x0f #x1f))
           (setq stack nil barriers nil) (incf epoch))
          ((and (= code #x20) candidate
                (not (and types (eql (car (aref types local)) #x64)))
                (not (logbitp local (native-op-live-out op))))
           (setf (native-op-code (aref ops (car candidate))) nil (native-op-code op) nil)
           (push (car candidate) barriers)
           (setf (car candidate) nil (aref epochs local) 0))
          (t
           (when (= code #x20) (setf (aref epochs local) 0))
           (multiple-value-bind (consumed produced) (native-op-stack op)
             (declare (type (unsigned-byte 32) consumed produced))
             (dotimes (j consumed)
               (loop while (and stack (car stack)) do
                 (setf (aref epochs (native-op-argument (aref ops (pop stack)))) 0))
               (when stack (pop stack) (pop barriers)))
             (when (member code '(#x21 #x22)) (setf (aref epochs local) 0))
             (if (= code #x21)
                 (progn (push i stack) (setf (aref candidates local) stack (aref epochs local) epoch))
                 (dotimes (j produced) (push nil stack) (push i barriers))))))))
  ops))

(defun native-coalesce-ops (ops offset count exceptions &optional types)
  (declare (type simple-vector ops) (type (unsigned-byte 32) offset count)
           (type (or null simple-vector) types))
  (let* ((total (+ offset count)) (conflicts (make-array total :initial-element 0))
         (uses (make-array total :initial-element 0)) (mapping (make-array total :initial-element nil))
         (colors (make-array total :initial-element 0))
         (color-conflicts (make-array total :initial-element 0))
         (color-types (and types (make-array total))) (order nil) (next offset)
         (entry-live (native-op-live-in (aref ops 0))))
    (declare (type (unsigned-byte 32) total next)
             (type simple-vector conflicts uses mapping colors color-conflicts)
             (type (or null simple-vector) color-types) (type list order)
             (type (integer 0 *) entry-live))
    (dotimes (i offset)
      (declare (type (unsigned-byte 32) i))
      (setf (aref mapping i) i))
    (loop for op across ops do
      (when (member (native-op-code op) '(#x20 #x21 #x22))
        (let ((local (native-op-argument op)))
          (incf (aref uses local))
          ;; Exceptional successors conservatively forbid sharing, so their
          ;; interference graph cannot affect the allocation. Avoid building
          ;; its dense call-site cliques just to assign distinct colors below.
          (when (and (not exceptions) (member (native-op-code op) '(#x21 #x22)))
            (let* ((bit (ash 1 local)) (live (logandc2 (native-op-live-out op) bit)))
              (declare (type (integer 0 *) bit live))
              (setf (aref conflicts local) (logior (aref conflicts local) live)))))))
    ;; Precolor parameters (and the native dispatch PC), but permit ordinary
    ;; locals to reuse dead parameter slots of the exact same type. Locals
    ;; live on entry need their implicit zero/NIL, unlike arbitrary parameters.
    (dotimes (i offset)
      (declare (type (unsigned-byte 32) i))
      (setf (aref colors i) (ash 1 i) (aref color-conflicts i) (aref conflicts i))
      (when types (setf (aref color-types i) (aref types i))))
    (loop for i from offset below total do (when (plusp (aref uses i)) (push i order)))
    (setq order (stable-sort (nreverse order) #'> :key (lambda (i) (aref uses i))))
    (dolist (local order)
      (let ((color (cond ((or exceptions (and types (eql (car (aref types local)) #x64))) next)
                         ((logbitp local entry-live) offset) (t 0))))
        (declare (type (unsigned-byte 32) color))
        ;; Keep the directed store/live-out graph. Testing both its outgoing
        ;; neighbors and the union of a color's outgoing neighbors gives the
        ;; same symmetric interference test without expanding every live bit
        ;; into a reverse edge at every store.
        (loop while (and (< color next)
                        (or (and (null types) (<= 2 color) (< color offset))
                            (and types (not (equal (aref types local) (aref color-types color))))
                            ;; Reject the single-bit conflict before scanning
                            ;; both potentially very wide neighbor sets.
                            (logbitp local (aref color-conflicts color))
                            ;; LOGTEST avoids allocating the intersection.
                            (logtest (aref conflicts local) (aref colors color))))
          do (incf color))
        (when (= color next)
          (when types (setf (aref color-types color) (aref types local)))
          (incf next))
        (setf (aref mapping local) color
              (aref colors color) (logior (aref colors color) (ash 1 local))
              (aref color-conflicts color)
                (logior (aref color-conflicts color) (aref conflicts local)))))
    (loop for op across ops do
      (when (member (native-op-code op) '(#x20 #x21 #x22))
        (setf (native-op-argument op) (aref mapping (native-op-argument op)))))
    (when (< (- next offset) count)
      (compiler-statistic :optimization/local-slots-removed nil (- count (- next offset))))
    (values (- next offset) (when types (subseq color-types 0 next))
            mapping)))

(defun native-forward-locals (ops total offset types)
  (declare (type simple-vector ops) (type (unsigned-byte 32) total offset)
           (type (or null simple-vector) types))
  ;; Copies retain their stores until ordinary exception-aware liveness runs.
  ;; Only adjacent active GET/TEE and SET/TEE establish equal local values;
  ;; a source generation invalidates the equality when that local is written.
  ;; Control boundaries discard all equalities, including exceptional/control
  ;; branches normalized by the module adapter. Calls cannot mutate Wasm locals.
  ;; Exact defaultable types preserve reference validation and initialization.
  ;; Without explicit types, only the native ABI's object locals are known;
  ;; the scalar prefix can contain distinct numeric representations.
  ;; Dense tables and epochs bound this pass to O(instructions + locals) work.
  (let ((sources (make-array total :initial-element nil))
        (versions (make-array total :initial-element 0))
        (stamps (make-array total :initial-element 0))
        (epochs (make-array total :initial-element 0))
        (epoch 1) (previous nil) (forwarded 0))
    (declare (type simple-vector sources versions stamps epochs)
             (type (unsigned-byte 33) epoch)
             (type (or null (unsigned-byte 32)) previous)
             (type (unsigned-byte 32) forwarded))
    (loop for op across ops for code = (native-op-code op)
          for local = (native-op-argument op) do
      (cond
        ((null code))
        ((= code #x20)
         (let ((source (aref sources local)))
           (when (and source (= (aref epochs local) epoch)
                      (= (aref stamps local) (aref versions source)))
             (setf (native-op-argument op) source)
             (setq local source) (incf forwarded)))
         (setq previous local))
        ((member code '(#x21 #x22))
         (incf (aref versions local))
         (setf (aref epochs local) 0)
         (when (and previous (/= previous local)
                    (if types
                        (and (not (eql (car (aref types local)) #x64))
                             (equal (aref types local) (aref types previous)))
                        (and (not (and (<= 2 local) (< local offset)))
                             (not (and (<= 2 previous) (< previous offset))))))
           (setf (aref sources local) previous
                 (aref stamps local) (aref versions previous)
                 (aref epochs local) epoch))
         (setq previous (and (= code #x22) local)))
        (t
         (setq previous nil)
         (when (member code '(#x00 #x02 #x03 #x04 #x05 #x08 #x0b #x0c #x0d #x0e #x0f #x12 #x1f))
           (incf epoch)))))
    (when (plusp forwarded)
      (compiler-statistic :optimization/local-forward nil forwarded)))
  ops)

(defun native-result-blocks (ops bytes offset types)
  (declare (type simple-vector ops) (type vector bytes)
           (type (unsigned-byte 32) offset) (type (or null simple-vector) types))
  ;; NATIVE-SIMPLIFY-OPS compacts active records; forwarding changes only
  ;; operands. Original index adjacency therefore describes active neighbors.
  ;; Resolve scopes and incoming branches before mutating any records.
  ;; Each branch belongs to one scope, bounding all scans to O(instructions).
  (when (loop for op across ops thereis
          (or (member (native-op-code op) '(#x1f #x0e))
              (and (eql (native-op-code op) #x0d)
                   (/= (aref bytes (native-op-start op)) #x0d))))
    (return-from native-result-blocks ops))
  (let* ((size (length ops)) (scopes (make-array size)) (top 0)
         (ends (make-array size :initial-element nil))
         (stores (make-array size :initial-element nil))
         (bad (make-array size :initial-element nil)))
    (declare (type (unsigned-byte 32) size top)
             (type simple-vector scopes ends stores bad))
    (dotimes (i size)
      (declare (type (unsigned-byte 32) i))
      (let* ((op (aref ops i)) (code (native-op-code op))
             (prior (and (plusp i) (1- i))))
        (declare (type simple-vector op) (type (unsigned-byte 8) code)
                 (type (or null (unsigned-byte 32)) prior))
        (case code
          ((#x02 #x03 #x04) (setf (aref scopes top) i) (incf top))
          (#x0b (when (plusp top)
                  (decf top) (setf (aref ends (aref scopes top)) i)))
          ((#x0c #x0d)
           (let ((depth (native-op-argument op)))
             (when (< depth top)
               (let ((scope (aref scopes (- top depth 1))))
                 (if (and (= code #x0c)
                          (= (aref bytes (native-op-start op)) #x0c)
                          prior (eql (native-op-code (aref ops prior)) #x21))
                     (push prior (aref stores scope))
                     (setf (aref bad scope) t)))))))))
    (dotimes (i size)
      (declare (type (unsigned-byte 32) i))
      (let* ((op (aref ops i)) (end (aref ends i))
             (load-index (and end (< (1+ end) size) (1+ end)))
             (load (and load-index (aref ops load-index)))
             (local (and load (eql (native-op-code load) #x20)
                         (native-op-argument load)))
             (type (and local
                        (if types
                            (let ((type (aref types local)))
                              (and (null (cdr type)) (car type)))
                            ;; The native ABI's first two parameters and
                            ;; untyped local suffix contain externrefs.
                            (and (or (< local 2) (>= local offset)) #x6f)))))
        (when (and (eql (native-op-code op) #x02)
                   (eql (native-op-argument op) #x40)
                   (= (- (native-op-end op) (native-op-start op)) 2)
                   (= (aref bytes (1+ (native-op-start op))) #x40)
                   (aref stores i) (not (aref bad i))
                   load (eql (native-op-code load) #x20)
                   (member type '(#x7f #x7e #x7d #x7c #x7b #x70 #x6f #x6e #x6d #x6c #x6b #x6a)))
          (let* ((tail (aref ops (1- end)))
                 (fallthrough (and (eql (native-op-code tail) #x21)
                                   (eql (native-op-argument tail) local)))
                 (terminal (and
                             (member (native-op-code tail) '(#x00 #x08 #x0c #x0f #x12))
                             (member (aref bytes (native-op-start tail))
                                     '(#x00 #x08 #x0a #x0c #x0f #x12 #x13 #x15)))))
            ;; Every reaching exit sets this exact defaultable local immediately
            ;; before transfer. Carry that value instead, then TEE at the join
            ;; so later reads still observe the assignment. Exiting branches
            ;; and traps execute neither the join nor any moved assignment.
            (when (and (or fallthrough terminal)
                       (every (lambda (index)
                                (let ((store (aref ops index)))
                                  (and (eql (native-op-code store) #x21)
                                       (eql (native-op-argument store) local))))
                              (aref stores i)))
              (dolist (index (aref stores i))
                (setf (native-op-code (aref ops index)) nil))
              (when fallthrough (setf (native-op-code tail) nil))
              (setf (native-op-argument op) type (native-op-code load) #x22))))))
    ops))

(defun native-fold-locals (ops)
  (declare (type simple-vector ops))
  (let ((previous nil) (prior nil))
    (loop for op across ops for code = (native-op-code op) do
      (when code
        (cond
          ;; Copying A to B and immediately back to A preserves A. Keep the
          ;; assignment to B, including when these locals have different
          ;; compatible reference types; do not move anything across effects.
          ((and prior previous (member code '(#x21 #x22))
                (eql (native-op-code previous) #x22) (eql (native-op-code prior) #x20)
                (eql (native-op-argument prior) (native-op-argument op)))
           (compiler-statistic :optimization/local-copy-back)
           (when (= code #x21) (setf (native-op-code previous) #x21))
           (setf (native-op-code op) nil))
          ((and previous (= code #x22) (eql (native-op-code previous) #x20)
                (eql (native-op-argument op) (native-op-argument previous)))
           (compiler-statistic :optimization/local-self-tee)
           (setf (native-op-code op) nil))
          ((and previous (member code '(#x20 #x21))
                (eql (native-op-argument op) (native-op-argument previous))
                (or (and (= code #x21) (eql (native-op-code previous) #x20))
                    (and (= code #x20) (eql (native-op-code previous) #x21))))
           (compiler-statistic :optimization/local-copy-fold code)
           (setf (native-op-code previous) (if (= code #x20) #x22 nil)
                 (native-op-code op) nil)
           (when (= code #x21) (setq previous nil prior nil)))
          (t (setq prior previous previous op))))))
  ops)

(defun native-i32 (value)
  (declare (type integer value))
  (let ((bits (ldb (byte 32 0) value)))
    (declare (type (unsigned-byte 32) bits))
    (if (logbitp 31 bits) (- bits (ash 1 32)) bits)))

(defun native-fold-i32 (code left right)
  (declare (type (unsigned-byte 8) code) (type (signed-byte 32) left right))
  ;; Deliberately exclude division, conversions and GC casts: their traps are
  ;; observable. Integer operations below have exact modular Wasm semantics.
  (native-i32
    (case code
      (#x46 (if (= left right) 1 0)) (#x47 (if (/= left right) 1 0))
      (#x48 (if (< left right) 1 0))
      (#x49 (if (< (ldb (byte 32 0) left) (ldb (byte 32 0) right)) 1 0))
      (#x4a (if (> left right) 1 0))
      (#x4b (if (> (ldb (byte 32 0) left) (ldb (byte 32 0) right)) 1 0))
      (#x4c (if (<= left right) 1 0))
      (#x4d (if (<= (ldb (byte 32 0) left) (ldb (byte 32 0) right)) 1 0))
      (#x4e (if (>= left right) 1 0))
      (#x4f (if (>= (ldb (byte 32 0) left) (ldb (byte 32 0) right)) 1 0))
      (#x6a (+ left right)) (#x6b (- left right)) (#x6c (* left right))
      (#x71 (logand left right)) (#x72 (logior left right)) (#x73 (logxor left right))
      (#x74 (ash left (logand right 31))) (#x75 (ash left (- (logand right 31))))
      (#x76 (ash (ldb (byte 32 0) left) (- (logand right 31)))))))

(defun native-clean-scopes (ops bytes)
  (declare (type simple-vector ops) (type vector bytes))
  ;; Untargeted void wrappers contribute no execution or values. Adjacent BLOCK
  ;; wrappers with identical literal signatures also have the same exit; alias
  ;; their labels before removing the inner wrapper. Retain IF, exception and
  ;; cast scopes, and rewrite depths against the complete original scope stack.
  (when (loop for op across ops thereis
          (or (eql (native-op-code op) #x1f)
              (and (eql (native-op-code op) #x0d)
                   (/= (aref bytes (native-op-start op)) #x0d))))
    (return-from native-clean-scopes ops))
  (let* ((size (length ops)) (codes (map 'vector (lambda (op) (native-op-code op)) ops))
         (ends (make-array size :initial-element nil))
         (used (make-array size :initial-element nil))
         (removed (make-array size :initial-element nil))
         (previous (make-array size :initial-element nil))
         (scopes (make-array size)) (prefix (make-array (1+ size) :initial-element 0))
         (top 0) (prior nil) (complete nil) (changed nil))
    (declare (type (unsigned-byte 32) size top)
             (type simple-vector codes ends used removed previous scopes prefix))
    (dotimes (i size)
      (declare (type (unsigned-byte 32) i))
      (let ((op (aref ops i)))
        (when (aref codes i)
          (when complete (return-from native-clean-scopes ops))
          (setf (aref previous i) prior) (setq prior i))
        (case (aref codes i)
          ((#x02 #x03 #x04) (setf (aref scopes top) i) (incf top))
          (#x0b (if (plusp top)
                    (progn (decf top) (setf (aref ends (aref scopes top)) i))
                    (setq complete t)))
          ((#x0c #x0d #x0e)
           (dolist (depth (if (eql (aref codes i) #x0e) (native-op-argument op)
                             (list (native-op-argument op))))
             (when (< depth top)
               (setf (aref used (aref scopes (- top depth 1))) t)))))))
    ;; An incomplete match supplies no permission to delete or rewrite scopes.
    (when (or (plusp top) (not complete)) (return-from native-clean-scopes ops))
    (labels ((signature (index)
               (let* ((op (aref ops index)) (argument (native-op-argument op)))
                 (when (= (- (native-op-end op) (native-op-start op)) 2)
                   ;; Match the writer: a newly selected literal result takes
                   ;; precedence over the original bytes. The module decoder
                   ;; otherwise represents every nonvoid signature as T.
                   (if (member argument '(#x7f #x7e #x7d #x7c #x7b #x70 #x6f #x6e #x6d #x6c #x6b #x6a))
                       argument
                       (let ((literal (aref bytes (1+ (native-op-start op)))))
                         (and (member literal '(#x40 #x7f #x7e #x7d #x7c #x7b #x70 #x6f #x6e #x6d #x6c #x6b #x6a))
                              literal)))))))
      ;; Reverse order propagates a targeted inner label through an arbitrary
      ;; chain of aliases before deciding whether the surviving void label is
      ;; unused. Openings and matching ENDs must be consecutive live records.
      (loop for i downfrom (1- size) to 0 for outer = (aref previous i) do
        (when (and (eql (aref codes i) #x02) outer
                   (eql (aref codes outer) #x02)
                   (eql (aref previous (aref ends outer)) (aref ends i))
                   (signature i) (eql (signature i) (signature outer)))
          (setf changed t (aref removed i) t)
          (when (aref used i) (setf (aref used outer) t)))))
    (dotimes (i size)
      (declare (type (unsigned-byte 32) i))
      (when (or (aref removed i)
                (and (member (aref codes i) '(#x02 #x03))
                     (eql (native-op-argument (aref ops i)) #x40) (not (aref used i))))
        (setf changed t (aref removed i) t
              (native-op-code (aref ops i)) nil
              (native-op-code (aref ops (aref ends i))) nil)))
    (unless changed (return-from native-clean-scopes ops))
    (setq top 0)
    (labels ((depth (old)
               (declare (type (unsigned-byte 32) old))
               ;; Prefix counts answer how many inner scopes disappeared in
               ;; constant time, including branches to the implicit function
               ;; label. Walking each branch's nesting depth was quadratic for
               ;; large dispatcher frames and generated numerical functions.
               (- old (- (aref prefix top) (aref prefix (max 0 (- top old)))))))
      (dotimes (i size)
        (declare (type (unsigned-byte 32) i))
        (let ((op (aref ops i)))
          (case (aref codes i)
            ((#x02 #x03 #x04)
             (setf (aref prefix (1+ top)) (+ (aref prefix top) (if (aref removed i) 1 0)))
             (incf top))
            (#x0b (when (plusp top) (decf top)))
            ((#x0c #x0d) (setf (native-op-argument op) (depth (native-op-argument op))))
            (#x0e (setf (native-op-argument op) (mapcar #'depth (native-op-argument op))))))))
    ops))

(defun native-simplify-ops-lisp (ops bytes &optional (clean-scopes t))
  (declare (type simple-vector ops) (type vector bytes))
  ;; Precompute, Vacuum and DCE on the linear instruction stream. Replacements
  ;; preserve operand evaluation, and control frames stay until depths can be
  ;; rewritten together. No computation is speculated across a branch.
  (let* ((size (length ops)) (ends nil) (elses nil)
         (active (make-array size)) (top 0) (dead nil) (void-scopes nil))
    (declare (type (unsigned-byte 32) size top) (type simple-vector active)
             (type (or null simple-vector) ends elses))
    (dotimes (i size)
      (declare (type (unsigned-byte 32) i))
      (let* ((op (aref ops i)) (code (native-op-code op))
             (previous (and (plusp top) (aref ops (aref active (1- top)))))
             (prior (and (> top 1) (aref ops (aref active (- top 2))))))
        (declare (type simple-vector op) (type (or null (unsigned-byte 8)) code)
                 (type (or null simple-vector) previous prior))
        (cond
          ((null code))
          ((= code #x01) (setf (native-op-code op) nil))
          ((and dead (not (member code '(#x02 #x03 #x04 #x05 #x0b #x1f))))
           (setf (native-op-code op) nil))
          (t
           (case code
             (#xd1 (when (and previous (member (native-op-code previous) '(#xd0 #xd2)))
              (setf (native-op-argument previous) (if (= (native-op-code previous) #xd0) 1 0)
                    (native-op-code previous) #x41 (native-op-code op) nil)))
             (#xa7
              (when previous
                (case (native-op-code previous)
                  (#x42
                   (setf (native-op-argument previous) (native-i32 (native-op-argument previous))
                         (native-op-code previous) #x41 (native-op-code op) nil))
                  ((#xac #xad)
                   (setf (native-op-code previous) nil (native-op-code op) nil) (decf top)))))
             (#x45
              (when previous
                (let ((previous-code (native-op-code previous)))
                  (cond
                    ((eql previous-code #x41)
                     (setf (native-op-argument previous) (if (zerop (native-op-argument previous)) 1 0)
                           (native-op-code op) nil))
                    ((and previous-code (<= #x46 previous-code #x4f))
                     (setf (native-op-code previous) (logxor previous-code (if (< previous-code #x48) 1 6))
                           (native-op-code op) nil))
                    ((and previous-code (<= #x51 previous-code #x5a))
                     (setf (native-op-code previous)
                             (- (if (<= previous-code #x52) #xa3 (if (oddp previous-code) #xac #xae))
                                previous-code)
                           (native-op-code op) nil))))))
             ((#x46 #x47 #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f #x6a #x6b #x6c #x71 #x72 #x73 #x74 #x75 #x76)
              (cond
               ((and prior (eql (native-op-code previous) #x41) (eql (native-op-code prior) #x41))
              (setf (native-op-argument prior)
                      (native-fold-i32 code (native-op-argument prior) (native-op-argument previous))
                    (native-op-code previous) nil (native-op-code op) nil)
              (decf top))
               ((and previous (eql (native-op-code previous) #x41)
                   (or (and (member code '(#x6a #x6b #x72 #x73)) (zerop (native-op-argument previous)))
                       (and (= code #x6c) (= (native-op-argument previous) 1))
                       (and (= code #x71) (= (native-op-argument previous) -1))))
              (setf (native-op-code previous) nil (native-op-code op) nil) (decf top))))
             (#x24 (when (and previous (eql (native-op-code previous) #x23)
                              (eql (native-op-argument op) (native-op-argument previous)))
              (setf (native-op-code previous) nil (native-op-code op) nil) (decf top)))
             (#x1a
              (loop while (plusp top) for previous = (aref ops (aref active (1- top)))
                    for previous-code = (native-op-code previous) do
                (cond
                  ((member previous-code '(#x20 #x23 #x41 #x42 #x43 #x44 #xd0 #xd2))
                   (setf (native-op-code previous) nil (native-op-code op) nil) (decf top) (return))
                  ((= previous-code #x22)
                   (setf (native-op-code previous) #x21 (native-op-code op) nil) (return))
                  ((member previous-code '(#x45 #x50 #x67 #x68 #x69 #x79 #x7a #x7b #xa7 #xac #xad
                                          #xc0 #xc1 #xc2 #xc3 #xc4 #xd1))
                   (setf (native-op-code previous) nil) (decf top))
                  (t (return)))))
             ((#x04 #x0d)
              ;; Only i32 truth consumers admit dropping normalization. GC
              ;; conditional branches can share the analysis opcode #x0d.
              (when (and previous prior (eql (native-op-code previous) #x45)
                         (eql (native-op-code prior) #x45)
                         (member (aref bytes (native-op-start op)) '(#x04 #x0d)))
                (setf (native-op-code previous) nil (native-op-code prior) nil) (decf top 2))
              (when (and previous (eql (native-op-code previous) #x41)
                         (or (= code #x04) (= (aref bytes (native-op-start op)) #x0d)))
              (let ((zero (zerop (native-op-argument previous))))
                (setf (native-op-code previous) nil) (decf top)
                (setq clean-scopes t)
                (if (= code #x0d)
                    (setf (native-op-code op) (unless zero #x0c))
                    (progn
                     (unless ends (multiple-value-setq (ends elses) (native-control-matches ops)))
                     (let ((otherwise (aref elses i)) (end (aref ends i)))
                      (setf (native-op-code op) #x02)
                      (loop for j from (if zero (1+ i) (or otherwise end))
                            below (if zero (if otherwise (1+ otherwise) end) end)
                            do (setf (native-op-code (aref ops j)) nil)))))))))
           ;; Keep the original opcode binding immutable: the surrounding
           ;; NULL test can then prove it is a byte throughout the branch.
           ;; Rewrites change the record, so inspect its new opcode separately.
           (let ((live-code (native-op-code op)))
             (declare (type (or null (unsigned-byte 8)) live-code))
             (when live-code
               (setf (aref active top) i) (incf top)
               (when (or (= live-code #x02)
                         (and (= live-code #x03) (eql (native-op-argument op) #x40)))
                 (setq void-scopes t))
               (setq dead (member live-code '(#x00 #x08 #x0c #x0e #x0f #x12)))))))))
    (when (and clean-scopes void-scopes) (native-clean-scopes ops bytes))
    (remove-if-not (lambda (op) (native-op-code op)) ops)))

(defun native-simplify-ops (ops bytes &optional (clean-scopes t))
  ;; Host kernel caches track the portable pass's live definition. Preserve
  ;; that call across same-file compilation as well as cache invalidation.
  (declare (type simple-vector ops) (type vector bytes)
           (notinline native-simplify-ops-lisp))
  (let ((status (compiler-primitive 42 ops bytes)))
    (when status
      (when (and (logbitp 2 status) (or clean-scopes (logbitp 1 status)))
        (native-clean-scopes ops bytes))
      (return-from native-simplify-ops
        (remove-if-not (lambda (op) (native-op-code op)) ops))))
  (native-simplify-ops-lisp ops bytes clean-scopes))

(defun native-optimize-body (bytes &optional decoded-ops parameter-count local-count local-types)
  (declare (type vector bytes) (type (or null simple-vector) decoded-ops local-types)
           (type (or null (unsigned-byte 32)) parameter-count local-count))
  (multiple-value-bind (ops offset count preserve)
      (if decoded-ops (values decoded-ops parameter-count local-count) (native-read-ops bytes))
    (declare (type simple-vector ops) (type (unsigned-byte 32) offset count))
    ;; The untyped allocator cannot re-encode reference declarations. Raw
    ;; regions also keep their explicit reference clears and audit spans.
    (when preserve (return-from native-optimize-body (values bytes nil)))
    (setq ops (native-simplify-ops ops bytes))
    (native-forward-locals ops (+ offset count) offset local-types)
    (native-result-blocks ops bytes offset local-types)
    (let ((exceptions (native-op-flow ops))
          (relocations (and *wasm-function-relocations*
                           (gethash *wasm-current-function* *wasm-function-relocations*)))
          (new-relocations nil) (blocks nil) (mapping nil) (second-mapping nil))
      (setq blocks (native-op-liveness ops (+ offset count) exceptions))
      (native-stackify-ops ops exceptions local-types (+ offset count))
      ;; Recompute liveness after eliminating stores, then color the CFG.
      (native-op-liveness ops (+ offset count) exceptions blocks)
      (multiple-value-setq (count local-types mapping)
        (native-coalesce-ops ops offset count exceptions local-types))
      (native-fold-locals ops)
      (native-op-liveness ops (+ offset count) exceptions blocks)
      (native-stackify-ops ops exceptions local-types (+ offset count))
      (multiple-value-setq (count local-types second-mapping)
        (native-coalesce-ops ops offset count exceptions local-types))
      (when decoded-ops
        (dotimes (i (length mapping))
          (when (aref mapping i) (setf (aref mapping i) (aref second-mapping (aref mapping i))))))
      (native-fold-locals ops)
      (setq ops (native-simplify-ops ops bytes nil))
      (let ((result (with-wasm-bytes
                      (if local-types
                          (let ((groups nil))
                            (loop for i from offset below (+ offset count) for type = (aref local-types i) do
                              (if (and groups (equal type (cdar groups)))
                                  (incf (caar groups)) (push (cons 1 type) groups)))
                            (wasm-u32 (length groups))
                            (dolist (group (nreverse groups))
                              (wasm-u32 (car group)) (dolist (byte (cdr group)) (wasm-byte byte))))
                          (progn
                            (wasm-u32 (+ (if (> offset 2) 1 0) (if (plusp count) 1 0)))
                            (when (> offset 2) (wasm-u32 (- offset 2)) (wasm-type :i32))
                            (when (plusp count) (wasm-u32 count) (wasm-type :object))))
                      (loop for op across ops for code = (native-op-code op) do
                        (when code
                          (cond
                            ((member code '(#x20 #x21 #x22))
                             (wasm-byte code) (wasm-u32 (native-op-argument op)))
                            ((and (= code #x02)
                                  (= (- (native-op-end op) (native-op-start op)) 2)
                                  (member (native-op-argument op) '(#x7f #x7e #x7d #x7c #x7b #x70 #x6f #x6e #x6d #x6c #x6b #x6a)))
                             (wasm-byte code) (wasm-byte (native-op-argument op)))
                            ((= code #x1a) (wasm-byte code))
                            ((= code #x41) (wasm-byte code) (wasm-s32 (native-op-argument op)))
                            ;; Comparison inversion changes a literal numeric
                            ;; opcode only; normalized calls and GC operations
                            ;; must continue to copy their original bytes.
                            ((and (or (<= #x46 code #x4f) (<= #x51 code #x5a))
                                  (= (native-op-end op) (1+ (native-op-start op)))
                                  (let ((original (aref bytes (native-op-start op))))
                                    (or (<= #x46 original #x4f) (<= #x51 original #x5a))))
                             (wasm-byte code))
                            ((and (= code #x44) (integerp (native-op-argument op)))
                             ;; The host module reader proved exact conversion.
                             ;; Keep F64 stack semantics during analysis and
                             ;; expand only while writing function instructions.
                             (wasm-integer (native-op-argument op)) (wasm-byte #xb7))
                            ((and (member code '(#x0c #x0d #x0e))
                                  (member (aref bytes (native-op-start op)) '(#x0c #x0d #x0e)))
                             (wasm-byte code)
                             (if (= code #x0e)
                                 (progn (wasm-u32 (1- (length (native-op-argument op))))
                                   (dolist (depth (native-op-argument op)) (wasm-u32 depth)))
                                 (wasm-u32 (native-op-argument op))))
                            ((and (= code #x02) (= (aref bytes (native-op-start op)) #x04))
                             (wasm-byte code)
                             (loop for i from (1+ (native-op-start op)) below (native-op-end op)
                                   do (wasm-byte (aref bytes i))))
                            (t
                             (when (member code '(#xd2 #x10 #x12))
                               (let ((entry (assoc (1+ (native-op-start op)) relocations)))
                                 (when entry (push (list (1+ (length *wasm-bytes*)) (second entry)) new-relocations))))
                             (loop for i from (native-op-start op) below (native-op-end op)
                               do (wasm-byte (aref bytes i))))))))))
        (when *wasm-function-relocations*
          (setf (gethash *wasm-current-function* *wasm-function-relocations*) new-relocations))
        (compiler-size-statistic
          (:lowering/wasm-input-bytes :lowering/wasm-output-bytes :size-units/wasm-body-optimization)
          (length bytes) (length result))
        (values result mapping)))))
(defvar *native-outlining-budget* 1000000)
(defvar *native-outlining-minimum-bytes* 0)
(defvar *native-outlining-minimum-saving* 512)
(defvar *native-outlining-helper-limit* 64)

;;;; Repeated straight-line Wasm regions. Records and signatures are private,
;;;; validated emitter/decoder data; this is not a public bytecode boundary.

(defun native-outline-result (op bytes types signatures)
  ;; NIL is an unknown stack type, never evidence for a helper signature.
  (let ((code (native-op-code op)) (argument (native-op-argument op)))
    (case code
      ((#x20 #x22) (aref types argument))
      (#x10 (first (second (aref signatures argument))))
      ((#x41 #x45 #x46 #x47 #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5a #x6a #x6b #x71 #xa7 #xd1 #xd3) '(#x7f))
      ((#x42 #x7c #x7d #x7e #x80 #x82 #x83 #x84 #x85 #x86 #x87 #xac #xad) '(#x7e))
      (#xd0 (let ((start (1+ (native-op-start op))))
              (if (and (= (native-op-end op) (1+ start))
                       (member (aref bytes start) '(#x6f #x6e #x6d #x70)))
                  (list (aref bytes start))
                  (cons #x63 (coerce (subseq bytes start (native-op-end op)) 'list)))))
      (#xfb (case (or argument (aref bytes (1+ (native-op-start op))))
              (#x1a '(#x6e)) (#x1b '(#x6f))
              ((#x1d #x1e) '(#x7f))
              ((#x16 #x17)
               (cons (if (= (or argument (aref bytes (1+ (native-op-start op)))) #x16) #x64 #x63)
                 (coerce (subseq bytes (+ 2 (native-op-start op)) (native-op-end op)) 'list))))))))

(defun native-outline-match-region (body ops start end order output local-types parameters results region)
  ;; A bounded fingerprint bucket only chooses which owned helper to try.
  ;; Compare its complete normalized encoding and signature without allocating
  ;; another helper or string key. A collision supplies no equality evidence.
  (unless (and (equal parameters (third region)) (equal results (fourth region)))
    (return-from native-outline-match-region nil))
  (let* ((helper (second region)) (size (length helper)) (cursor 0))
    (unless (and (< cursor size) (= (aref helper cursor) (length local-types)))
      (return-from native-outline-match-region nil))
    (incf cursor)
    (dolist (type local-types)
      (unless (and (< (+ cursor (length type)) size) (= (aref helper cursor) 1))
        (return-from native-outline-match-region nil))
      (incf cursor)
      (dolist (byte type)
        (unless (= byte (aref helper cursor)) (return-from native-outline-match-region nil))
        (incf cursor)))
    (loop for i from start below end for op = (aref ops i) for code = (native-op-code op) do
      (if (member code '(#x20 #x21 #x22))
          (progn
            (unless (and (< (1+ cursor) size) (= (aref helper cursor) code)
                         (= (aref helper (1+ cursor)) (position (native-op-argument op) order)))
              (return-from native-outline-match-region nil))
            (incf cursor 2))
          (let ((first (native-op-start op)) (last (native-op-end op)))
            (unless (<= (+ cursor (- last first)) size)
              (return-from native-outline-match-region nil))
            (loop for offset from first below last do
              (unless (= (aref body offset) (aref helper cursor))
                (return-from native-outline-match-region nil))
              (incf cursor)))))
    (dolist (index output)
      (unless (and (< (1+ cursor) size) (= (aref helper cursor) #x20)
                   (= (aref helper (1+ cursor)) (position index order)))
        (return-from native-outline-match-region nil))
      (incf cursor 2))
    (and (= (1+ cursor) size) (= (aref helper cursor) #x0b))))

(defun native-outline-region (body ops types start end stack &optional discovery bucket)
  ;; Alpha-renaming respects aliases. A local read before its first write is
  ;; an input; other locals begin with a dominating write inside the helper.
  (let ((locals nil) (inputs nil) (written nil) (output nil))
    (if discovery
        ;; Discovery extends these sets as END advances. Copy the lists into
        ;; encounter order; retained sites must not share its evolving state.
        (setq locals (reverse (first discovery)) inputs (reverse (second discovery))
              written (third discovery))
        (progn
          (loop for i from start below end for op = (aref ops i)
                for code = (native-op-code op) for index = (native-op-argument op)
                when (member code '(#x20 #x21 #x22)) do
                  (pushnew index locals)
                  (if (= code #x20)
                      (unless (member index written) (pushnew index inputs))
                      (pushnew index written)))
          (setq locals (nreverse locals) inputs (nreverse inputs))))
    ;; Every write remains visible in the caller, including uses reached through
    ;; a backedge before this region in textual order. Omitting a write would
    ;; require control-flow liveness, not its last textual occurrence.
    (dolist (index locals)
      (when (member index written) (push index output)))
    (setq output (nreverse output))
    (unless (and (<= (length inputs) 6) (<= (length output) 4) (<= (length locals) 12))
      (return-from native-outline-region nil))
    (let* ((temporaries (remove-if (lambda (index) (member index inputs)) locals))
           (order (append inputs temporaries))
           (parameters (mapcar (lambda (index) (aref types index)) inputs))
           (results (append (reverse stack) (mapcar (lambda (index) (aref types index)) output)))
           (local-types (mapcar (lambda (index) (aref types index)) temporaries)))
      (unless (or *native-module-stack-effects*
                  (every (lambda (type) (and (= (length type) 1)
                                             (member (first type) '(#x6f #x7f #x7e #x70 #x6c))))
                         (append parameters results local-types)))
        (return-from native-outline-region nil))
      (dolist (group bucket)
        (let ((representative (first group)))
          (when (native-outline-match-region body ops start end order output local-types parameters results representative)
            (return-from native-outline-region
              (values (list (first representative) (second representative)
                            (third representative) (fourth representative) inputs output)
                      group)))))
      (let ((helper (with-wasm-bytes
                     ;; The local cap proves these counts and renamed indices
                     ;; have single-byte unsigned LEB128 encodings.
                     (wasm-byte (length temporaries))
                     (dolist (type local-types) (wasm-byte 1) (dolist (byte type) (wasm-byte byte)))
                     ;; Normalize directly into the owned helper buffer; an
                     ;; intermediate instruction vector would only be copied.
                     (loop for i from start below end for op = (aref ops i) do
                       (if (member (native-op-code op) '(#x20 #x21 #x22))
                           (progn (wasm-byte (native-op-code op))
                             (wasm-byte (position (native-op-argument op) order)))
                           (let ((first (native-op-start op)) (last (native-op-end op)))
                             ;; Copy adjacent unchanged instructions together;
                             ;; local operands still require alpha-renaming.
                             (loop while (< (1+ i) end) do
                               (let ((next (aref ops (1+ i))))
                                 (unless (and (not (member (native-op-code next) '(#x20 #x21 #x22)))
                                              (= last (native-op-start next)))
                                   (return))
                                 (incf i) (setq last (native-op-end next))))
                             (wasm-append body first last))))
                     (dolist (index output) (wasm-byte #x20) (wasm-byte (position index order))) (wasm-byte #x0b))))
      ;; Key owns the complete normalized bytes and types, not a lossy hash.
      (list (list (map 'string #'code-char helper) parameters results)
            helper parameters results inputs output)))))

(defun native-outline-site (region index)
  (with-wasm-bytes
    (dolist (local (fifth region)) (wasm-byte #x20) (wasm-u32 local))
    (wasm-byte #x10) (wasm-u32 index)
    (dolist (local (reverse (sixth region))) (wasm-byte #x21) (wasm-u32 local))))

(defun native-outline-code (records signatures eligible &optional (budget *native-outlining-budget*))
  "Share repeated regions in checked private body records.
RECORDS contain bytes, decoded operations, local types and exact stack effects.
SIGNATURES contain raw parameter/result types in final function-index order.
ELIGIBLE contains each body's packed SPACE/SPEED/COMPILATION-SPEED policy.
Return replacement bodies and (body parameters results) helper records."
  (let ((groups (make-hash-table :test 'equal)) (group-count 0)
        (fingerprints (make-hash-table :test 'eql)) (occurrences 0)
        (candidates nil) (selected nil) (helpers nil)
        (helper-limit *native-outlining-helper-limit*)
        (frequencies (make-hash-table :test 'eql)))
    ;; Preserve large-region discovery, then count and discover short regions
    ;; from the other end of the module. Each phase has its own work/storage
    ;; bounds, so short candidates neither evict large groups nor spend their
    ;; scan budget. Frequency is only a discovery filter, never a match proof.
    (dolist (phase '(nil :count :short))
      (let ((work budget) (short phase))
        (clrhash groups) (clrhash fingerprints) (setq group-count 0 occurrences 0)
        (dotimes (ordinal (length records))
          (let* ((function (if short (- (length records) ordinal 1) ordinal))
                 (record (aref records function)))
            (when (and record (aref eligible function) (plusp work)
                       (plusp (logand 3 (ash (aref eligible function) -2)))
                       (<= (logand 3 (aref eligible function)) (logand 3 (ash (aref eligible function) -2))))
              (let* ((body (first record)) (ops (second record)) (types (third record))
                     (policy (aref eligible function))
                     (space (logand 3 (ash policy -2)))
                     (minimum (max *native-outlining-minimum-bytes*
                                   (case space (1 256) (2 128) (otherwise (if short 6 64)))))
                     (function-work (case (ash policy -4) (0 200000) (1 100000) (2 25000) (otherwise 4000)))
                     (*native-module-stack-effects* (fourth record)))
                ;; A moved store must remain visible if a handler in this activation
                ;; catches a transfer. Keep these functions intact, including their
                ;; handler-free regions; a smaller proof can relax this later.
                (unless (find #x1f ops :key (lambda (op) (native-op-code op)))
                  (dotimes (start (length ops))
                    (when (and (plusp work) (plusp function-work) (member (native-op-code (aref ops start)) '(#x20 #x41 #x42 #xd0)))
                      (let ((stack nil) (depth 0) (fingerprint 0) (core-fingerprint 0)
                            (discovery (list nil nil nil)))
                        (declare (type (unsigned-byte 24) fingerprint core-fingerprint))
                        (loop for end from start below (min (length ops) (+ start (if short 16 128)))
                              for op = (aref ops end) for code = (native-op-code op) do
                          (decf work) (decf function-work)
                          ;; Control, tails, REF.FUNC and unknown operations are
                          ;; barriers. Regions neither consume an outside operand
                          ;; nor introduce a new branch or retained recursive frame.
                          (unless (and (plusp work) (plusp function-work) (or (/= code #x10) (= (aref body (native-op-start op)) #x10))
                                       (member code '(#x10 #x1a #x20 #x21 #x22 #x41 #x42 #x45 #x46 #x47 #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5a #x6a #x6b #x71 #x7c #x7d #x7e #x80 #x82 #x83 #x84 #x85 #x86 #x87 #xa7 #xac #xad #xd0 #xd1 #xd3 #xfb)))
                            (return))
                          (multiple-value-bind (pops pushes)
                              (if (= code #x10)
                                  (let ((signature (aref signatures (native-op-argument op))))
                                    (values (length (first signature)) (length (second signature))))
                                  (native-op-stack op))
                            (unless (and (<= pops depth) (<= pushes 1)) (return))
                            (dotimes (i pops) (pop stack)) (decf depth pops)
                            (when (plusp pushes)
                              (push (native-outline-result op body types signatures) stack) (incf depth)))
                          (when (member code '(#x20 #x21 #x22))
                            (let ((index (native-op-argument op)))
                              (pushnew index (first discovery))
                              (if (= code #x20)
                                  (unless (member index (third discovery)) (pushnew index (second discovery)))
                                  (pushnew index (third discovery)))))
                          (setq fingerprint (logand #xffffff (+ (* fingerprint 33) code)))
                          (unless (member code '(#x20 #x21 #x22))
                            (loop for offset from (1+ (native-op-start op)) below (native-op-end op) do
                              (setq fingerprint (logand #xffffff (+ (* fingerprint 33) (aref body offset))))))
                          (unless (= code #x20) (setq core-fingerprint fingerprint))
                          ;; Equal helpers have equal opcode sequences after removing
                          ;; trailing GETs, including those appended for outputs. Once
                          ;; groups fill, a missing fingerprint rules out a match;
                          ;; collisions still require the complete normalized key.
                          ;; Nonlocal immediates strengthen this filter; normalized
                          ;; locals contribute their opcode, never their old index.
                          ;; Short regions have 3..16 operations. Count their
                          ;; fingerprints before allocating normalized groups:
                          ;; unique literal combinations would otherwise consume
                          ;; the group cap before common small helpers are found.
                          (when (and (>= (- end start) (if short 2 15)) (<= depth 1)
                                     (every #'identity stack)
                                     (>= (- (native-op-end op) (native-op-start (aref ops start))) minimum)
                                     (or (eq phase :count)
                                         (and (or (null short)
                                                  (>= (* (gethash core-fingerprint frequencies 0)
                                                         (- (native-op-end op) (native-op-start (aref ops start))))
                                                      *native-outlining-minimum-saving*))
                                              (or (< group-count 4096) (gethash core-fingerprint fingerprints)))))
                            (if (eq phase :count)
                                (incf (gethash core-fingerprint frequencies 0))
                                (multiple-value-bind (region matched)
                                    (native-outline-region body ops types start (1+ end) stack
                                                           discovery (gethash core-fingerprint fingerprints))
                                  ;; Even the smallest possible helper index cannot
                                  ;; make a nonshrinking site worth retaining. Final
                                  ;; selection still prices the actual helper index.
                                  (when (and region
                                             (> (- (native-op-end op) (native-op-start (aref ops start)))
                                                (+ 1 (wasm-u32-width (length signatures))
                                                   (loop for local in (append (fifth region) (sixth region))
                                                         sum (+ 1 (wasm-u32-width local))))))
                                    (let* ((key (first region)) (group (or matched (gethash key groups))))
                                      (when (and (null group) (< group-count 4096))
                                        ;; Group: representative region and occurrences.
                                        (setq group (list region nil)) (setf (gethash key groups) group)
                                        (incf group-count) (push group candidates)
                                        ;; Bound collision comparisons. Further exact
                                        ;; groups retain the ordinary full-key lookup.
                                        (when (< (length (gethash core-fingerprint fingerprints)) 8)
                                          (push group (gethash core-fingerprint fingerprints))))
                                      (when (and group (< occurrences 32768))
                                        ;; A site owns its local numbering, even when
                                        ;; all normalized bytes match another function.
                                        (push (list function (native-op-start (aref ops start))
                                                    (native-op-end op) region) (second group))
                                        (incf occurrences))))))))))))))))))
    (let ((occupied (make-array (length records) :initial-element nil)))
      ;; Larger total byte savings first. Price the actual parameter traffic,
      ;; helper, signature, body/function counts and bounded section growth.
      (labels ((saving (group sites index)
                 (let* ((region (first group)) (helper (second region))
                        (parameters (third region)) (results (fourth region)))
                   (- (loop for site in sites sum
                        (- (- (third site) (second site)) (length (native-outline-site (fourth site) index))))
                      (length helper) (wasm-u32-width (length helper))
                      (loop for type in (append parameters results) sum (length type))
                      (wasm-u32-width (length parameters)) (wasm-u32-width (length results)) 16)))
               (available (site)
                 (not (some (lambda (other) (and (< (second site) (third other))
                                                 (< (second other) (third site))))
                            (aref occupied (first site))))))
        (setq candidates (stable-sort candidates #'>
          :key (lambda (group) (saving group (second group) (length signatures)))))
        (dolist (group candidates)
          (when (< (length helpers) helper-limit)
            (let ((sites nil) (index (+ (length signatures) (length helpers))))
              (dolist (site (reverse (second group)))
                (when (and (available site)
                           (not (some (lambda (other) (and (= (first other) (first site))
                                                          (< (second site) (third other))
                                                          (< (second other) (third site)))) sites)))
                  (push site sites)))
              (when (and (cdr sites) (>= (saving group sites index) *native-outlining-minimum-saving*))
                (let ((region (first group)))
                  (push (list (second region) (third region) (fourth region)) helpers))
                (dolist (site sites)
                  (push site (aref occupied (first site)))
                  (push (list site (native-outline-site (fourth site) index)) selected))))))
        (values
          (loop for record across records for i from 0 collect
            (let ((body (and record (first record)))
                  (sites (sort (remove-if-not (lambda (entry) (= (first (first entry)) i)) selected)
                               #'< :key (lambda (entry) (second (first entry))))))
              (if sites
                  (with-wasm-bytes
                    (let ((cursor 0))
                      (dolist (entry sites)
                        (wasm-append body cursor (second (first entry))) (wasm-append (second entry))
                        (setq cursor (third (first entry))))
                      (wasm-append body cursor)))
                  body)))
          (nreverse helpers))))))
