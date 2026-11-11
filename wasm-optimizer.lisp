;;;; Size optimization of the native backend's stable Wasm instruction subset.
;;;; Operate after lowering so mutable cells and hidden ABI reads are explicit.
(in-package :miricl-compiler)

;; The module adapter supplies exact stack effects for arbitrary core-module
;; instructions. Native emission uses its cheaper, fixed ABI description.
(defvar *native-module-stack-effects* nil)

;; These compiler-private records use simple vectors. Macro accessors expand
;; into SVREF in both hosts, including SETF places, so the self-compiled pass
;; never invokes interpreted DEFSTRUCT accessor/constructor closures per op.
(defmacro native-op-code (op) (list 'svref (list 'the 'simple-vector op) 0))
(defmacro native-op-argument (op) (list 'svref (list 'the 'simple-vector op) 1))
(defmacro native-op-start (op) (list 'svref (list 'the 'simple-vector op) 2))
(defmacro native-op-end (op) (list 'svref (list 'the 'simple-vector op) 3))
(defmacro native-op-successors (op) (list 'svref (list 'the 'simple-vector op) 4))
(defmacro native-op-live-in (op) (list 'svref (list 'the 'simple-vector op) 5))
(defmacro native-op-live-out (op) (list 'svref (list 'the 'simple-vector op) 6))
(defmacro native-op-uses (op) (list 'svref (list 'the 'simple-vector op) 7))
(defmacro native-op-definitions (op) (list 'svref (list 'the 'simple-vector op) 8))
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
  ;; Most operands are a single byte, including opcodes, small indices and
  ;; block types. Avoid entering the general shift/accumulate loop for them.
  (let ((byte (aref bytes position)))
    (declare (type (unsigned-byte 8) byte))
    (when (< byte 128) (return-from native-read-uleb (values byte (1+ position)))))
  (let ((value 0) (shift 0))
    (declare (type (integer 0 *) value) (type fixnum shift))
    (loop for byte = (aref bytes position) do
      (incf position) (setq value (logior value (ash (logand byte 127) shift)))
      (when (< byte 128) (return (values value position))) (incf shift 7))))

(defun native-read-sleb (bytes position)
  (declare (type vector bytes) (type (unsigned-byte 32) position))
  (let ((byte (aref bytes position)))
    (declare (type (unsigned-byte 8) byte))
    (when (< byte 128)
      (return-from native-read-sleb
        (values (if (< byte 64) byte (- byte 128)) (1+ position)))))
  (let ((value 0) (shift 0))
    (declare (type (integer 0 *) value) (type fixnum shift))
    (loop for byte = (aref bytes position) do
      (incf position) (setq value (logior value (ash (logand byte 127) shift)))
      (incf shift 7)
      (when (< byte 128)
        (return (values (if (logbitp 6 byte) (- value (ash 1 shift)) value) position))))))

(defun native-read-ops (bytes &optional references-only)
  (declare (type vector bytes))
  (let ((position 0) (offset 2) (locals 0) (ops nil))
    (declare (type (unsigned-byte 32) position offset locals))
    (multiple-value-bind (groups next) (native-read-uleb bytes position)
      (setq position next)
      (dotimes (i groups)
        (multiple-value-bind (count next) (native-read-uleb bytes position)
          (setq position next)
          (case (aref bytes position)
            (#x7f (incf offset count))
            (#x6f (incf locals count))
            (otherwise (error "Unexpected native local type.")))
          (incf position))))
    (loop while (< position (length bytes)) do
      (let* ((start position) (code (aref bytes position)) (argument nil))
        (declare (type (unsigned-byte 32) start) (type (unsigned-byte 8) code))
        (incf position)
        (cond
          ((member code '(#x02 #x03 #x04 #x08 #x0c #x0d #x10 #x12 #x20 #x21 #x22 #x41 #xd0 #xd2))
           ;; Constants and heap types only need their encoded extent here.
           ;; Decode the common one-byte case here, avoiding a helper call
           ;; and multiple-value packet for nearly every instruction operand.
           (let ((byte (aref bytes position)))
             (declare (type (unsigned-byte 8) byte))
             (if (< byte 128)
                 (progn (setq argument (if (and (= code #x41) (>= byte 64)) (- byte 128) byte))
                   (incf position))
                 (multiple-value-setq (argument position)
                   (if (= code #x41) (native-read-sleb bytes position) (native-read-uleb bytes position))))))
          ((= code #x0e)
           (let ((count nil))
             (multiple-value-setq (count position) (native-read-uleb bytes position))
             (dotimes (i (1+ count))
               (let ((depth nil))
                 (multiple-value-setq (depth position) (native-read-uleb bytes position))
                 (push depth argument)))
             (setq argument (nreverse argument))))
          ((= code #x1f)
           (incf position) ; block type: the emitter uses the empty signature
           (let ((count nil))
             (multiple-value-setq (count position) (native-read-uleb bytes position))
             (dotimes (i count)
               (let ((kind (aref bytes position)) (ignored nil))
                 (incf position)
                 (when (< kind 2)
                   (multiple-value-setq (ignored position) (native-read-uleb bytes position)))
                 (multiple-value-setq (ignored position) (native-read-uleb bytes position))))))
          ((= code #xfb)
           (multiple-value-setq (argument position) (native-read-uleb bytes position))
           (dotimes (i (case argument (2 2) ((#x14 #x16 #x17) 1) ((#x1a #x1b #x1c #x1d) 0)
                        (otherwise (error "Unexpected native GC instruction ~D." argument))))
             (multiple-value-bind (ignored next) (native-read-uleb bytes position)
               (declare (ignore ignored)) (setq position next))))
          ((not (member code '(#x00 #x05 #x0b #x0f #x1a #x45 #x46 #x48 #x4a #x4c #x4e #x53 #x55 #x57 #x59 #x71 #x7c #x7d #x7e #xa7 #xac #xd1)))
           (error "Unexpected native instruction ~D." code)))
        (when (or (not references-only) (member code '(#x10 #x12 #xd2)))
          (push (make-native-op code argument start position) ops))))
    (values (coerce (nreverse ops) 'simple-vector) offset locals)))

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
      ((#x20 #x41 #xd0 #xd2) (values 0 1))
      ((#x21 #x1a) (values 1 0))
      ((#x22 #x45 #xa7 #xac #xd1 #xfb) (values 1 1))
      ((#x46 #x48 #x4a #x4c #x4e #x53 #x55 #x57 #x59 #x71 #x7c #x7d #x7e) (values 2 1))
      ((#x10 #x12) (let ((import (nth (native-op-argument op) *native-imports*)))
               (if import (values (length (second import)) (if (and (= code #x10) (third import)) 1 0))
                   (values 2 (if (= code #x10) 1 0)))))
      (otherwise (values 0 0)))))

(defun native-stackify-ops (ops &optional exceptions types)
  (declare (type simple-vector ops) (type (or null simple-vector) types))
  ;; Stack entries are NIL for real operands and an instruction index for a
  ;; store whose value could remain on the stack. Liveness ensures its matching
  ;; load is the last use; any earlier load invalidates the candidate.
  (let ((stack nil) (barriers nil) (candidates (make-hash-table :test 'eql)))
    (declare (type list stack barriers) (type hash-table candidates))
    (dotimes (i (length ops))
      (let* ((op (aref ops i)) (code (native-op-code op)) (local (native-op-argument op))
             (store (and (eql code #x20) (gethash local candidates)))
             ;; Other candidate stores are not actual operands. Looking past
             ;; them lets sibling temporaries become stack operands together,
             ;; while an actual value (NIL) remains a strict ordering barrier.
             ;; Instruction positions order stack cells. A store above the
             ;; most recent real operand is available without scanning its
             ;; sibling stores. Turning a store into an operand inserts its
             ;; original position as a barrier, leaving younger stores usable.
             (candidate (and store (or (null barriers) (> (car store) (car barriers))) store)))
        ;; A throwing call may expose any local to a landing pad. Keeping a
        ;; value on the operand stack cannot replace that observable store.
        (when (and exceptions (member code '(#x08 #x10 #x12))) (clrhash candidates))
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
           (setq stack nil barriers nil) (clrhash candidates))
          ((and (= code #x20) candidate
                (not (and types (eql (car (aref types local)) #x64)))
                (not (logbitp local (native-op-live-out op))))
           (setf (native-op-code (aref ops (car candidate))) nil (native-op-code op) nil)
           (push (car candidate) barriers)
           (setf (car candidate) nil) (remhash local candidates))
          (t
           (when (= code #x20) (remhash local candidates))
           (multiple-value-bind (consumed produced) (native-op-stack op)
             (dotimes (j consumed)
               (loop while (and stack (car stack)) do
                 (remhash (native-op-argument (aref ops (pop stack))) candidates))
               (when stack (pop stack) (pop barriers)))
             (when (member code '(#x21 #x22)) (remhash local candidates))
             (if (= code #x21)
                 (progn (push i stack) (setf (gethash local candidates) stack))
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
    (dotimes (i offset) (setf (aref mapping i) i))
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
    (values (- next offset) (when types (subseq color-types 0 next))
            mapping)))

(defun native-fold-locals (ops)
  (declare (type simple-vector ops))
  (let ((previous nil))
    (loop for op across ops for code = (native-op-code op) do
      (when code
        (cond
          ((and previous (member code '(#x20 #x21))
                (eql (native-op-argument op) (native-op-argument previous))
                (or (and (= code #x21) (eql (native-op-code previous) #x20))
                    (and (= code #x20) (eql (native-op-code previous) #x21))))
           (setf (native-op-code previous) (if (= code #x20) #x22 nil)
                 (native-op-code op) nil)
           (when (= code #x21) (setq previous nil)))
          (t (setq previous op))))))
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
  ;; MergeBlocks/RemoveUnusedBrs: an untargeted void block/loop contributes no
  ;; execution or values. Retain if scopes and all exception/cast branch scopes.
  ;; Rewrite depths against the original scope stack when wrappers disappear.
  (when (loop for op across ops thereis
          (or (eql (native-op-code op) #x1f)
              (and (eql (native-op-code op) #x0d)
                   (/= (aref bytes (native-op-start op)) #x0d))))
    (return-from native-clean-scopes ops))
  (let* ((size (length ops)) (codes (map 'vector (lambda (op) (native-op-code op)) ops))
         (ends (make-array size :initial-element nil))
         (used (make-array size :initial-element nil))
         (removed (make-array size :initial-element nil)) (scopes nil))
    (declare (type (unsigned-byte 32) size) (type simple-vector codes ends used removed)
             (type list scopes))
    (dotimes (i size)
      (let ((op (aref ops i)))
        (case (aref codes i)
          ((#x02 #x03 #x04) (push i scopes))
          (#x0b (when scopes (setf (aref ends (pop scopes)) i)))
          ((#x0c #x0d #x0e)
           (dolist (depth (if (eql (aref codes i) #x0e) (native-op-argument op)
                             (list (native-op-argument op))))
             (let ((target (nth depth scopes)))
               (when target (setf (aref used target) t))))))))
    (dotimes (i size)
      (when (and (member (aref codes i) '(#x02 #x03))
                 (eql (native-op-argument (aref ops i)) #x40) (not (aref used i)))
        (setf (aref removed i) t
              (native-op-code (aref ops i)) nil
              (native-op-code (aref ops (aref ends i))) nil)))
    (setq scopes nil)
    (labels ((depth (old)
               (declare (type (unsigned-byte 32) old))
               (- old (loop for scope in scopes for level below old
                            count (aref removed scope)))))
      (dotimes (i size)
        (let ((op (aref ops i)))
          (case (aref codes i)
            ((#x02 #x03 #x04) (push i scopes))
            (#x0b (pop scopes))
            ((#x0c #x0d) (setf (native-op-argument op) (depth (native-op-argument op))))
            (#x0e (setf (native-op-argument op) (mapcar #'depth (native-op-argument op))))))))
    ops))

(defun native-simplify-ops (ops bytes &optional (clean-scopes t))
  (declare (type simple-vector ops) (type vector bytes))
  ;; Precompute, Vacuum and DCE on the linear instruction stream. Replacements
  ;; preserve operand evaluation, and control frames stay until depths can be
  ;; rewritten together. No computation is speculated across a branch.
  (let* ((size (length ops)) (ends nil) (elses nil)
         (active (make-array size)) (top 0) (dead nil) (void-scopes nil))
    (declare (type (unsigned-byte 32) size top) (type simple-vector active)
             (type (or null simple-vector) ends elses))
    (dotimes (i size)
      (let* ((op (aref ops i)) (code (native-op-code op))
             (previous (and (plusp top) (aref ops (aref active (1- top)))))
             (prior (and (> top 1) (aref ops (aref active (- top 2))))))
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
             (#x45 (when (and previous (eql (native-op-code previous) #x41))
              (setf (native-op-argument previous) (if (zerop (native-op-argument previous)) 1 0)
                    (native-op-code op) nil)))
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
           (setq code (native-op-code op))
           (when code
             (setf (aref active top) i) (incf top)
             (when (and (member code '(#x02 #x03)) (eql (native-op-argument op) #x40))
               (setq void-scopes t))
             (setq dead (member code '(#x00 #x08 #x0c #x0e #x0f #x12))))))))
    (when (and clean-scopes void-scopes) (native-clean-scopes ops bytes))
    (remove-if-not (lambda (op) (native-op-code op)) ops)))

(defun native-optimize-body (bytes &optional decoded-ops parameter-count local-count local-types)
  (declare (type vector bytes) (type (or null simple-vector) decoded-ops local-types)
           (type (or null (unsigned-byte 32)) parameter-count local-count))
  (multiple-value-bind (ops offset count)
      (if decoded-ops (values decoded-ops parameter-count local-count) (native-read-ops bytes))
    (declare (type simple-vector ops) (type (unsigned-byte 32) offset count))
    (setq ops (native-simplify-ops ops bytes))
    (let ((exceptions (native-op-flow ops))
          (relocations (and *wasm-function-relocations*
                           (gethash *wasm-current-function* *wasm-function-relocations*)))
          (new-relocations nil) (blocks nil) (mapping nil) (second-mapping nil))
      (setq blocks (native-op-liveness ops (+ offset count) exceptions))
      (native-stackify-ops ops exceptions local-types)
      ;; Recompute liveness after eliminating stores, then color the CFG.
      (native-op-liveness ops (+ offset count) exceptions blocks)
      (multiple-value-setq (count local-types mapping)
        (native-coalesce-ops ops offset count exceptions local-types))
      (native-fold-locals ops)
      (native-op-liveness ops (+ offset count) exceptions blocks)
      (native-stackify-ops ops exceptions local-types)
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
                            ((= code #x1a) (wasm-byte code))
                            ((= code #x41) (wasm-byte code) (wasm-s32 (native-op-argument op)))
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
        (values result mapping)))))
