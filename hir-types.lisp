;;;; Flow of primary-value types through HIR, independent of Wasm lowering.
(in-package :miricl-compiler)

(defun hir-result-callee (value &optional callees)
  "Executable code provenance, without asserting closure object identity."
  (or (and callees (gethash value callees))
   (loop repeat 32 do
    (let ((origin (hir-value-producer value)))
      (unless (%hir-instruction-p origin) (return nil))
      (case (hir-instruction-opcode origin)
        ((:copy :primary :values) (setq value (first (hir-instruction-inputs origin)))
         (unless value (return nil)))
        (:closure (return (first (hir-instruction-attributes origin))))
        ;; A cell's LOCAL record is descriptive, not a proof that no write,
        ;; alias or capture has changed its current function identity.
        (otherwise (return nil)))))))

(defun hir-primary-flow (unit barriers)
  "Primary forwarding edges in current validated HIR, or NIL on exhaustion.
Charge graph containers, values, captures, parameters and every operand, including
edges unrelated to primary forwarding. Captured landing parameters have unknown
ingress; empty VALUES has no function primary. This is not an escape certificate."
  (let ((sources (make-hash-table :test 'eq)) (fuel 65536))
    (dolist (function (hir-unit-functions unit))
      ;; Validation establishes the value/block counts. Charge values without
      ;; traversing them here; callee resolution visits that list independently.
      (when (minusp (decf fuel (+ 1 (hir-function-value-count function) (hir-function-block-count function))))
        (return-from hir-primary-flow nil))
      (dolist (identity (hir-function-captures function))
        (declare (ignore identity))
        (when (minusp (decf fuel)) (return-from hir-primary-flow nil)))
      (dolist (block (hir-function-blocks function))
        (dolist (parameter (hir-block-parameters block))
          (when (minusp (decf fuel)) (return-from hir-primary-flow nil))
          (when (gethash function barriers)
            (push nil (gethash parameter sources))))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (when (minusp (decf fuel)) (return-from hir-primary-flow nil))
          (let ((inputs (hir-instruction-inputs instruction)))
            (dolist (input inputs)
              (declare (ignore input))
              (when (minusp (decf fuel)) (return-from hir-primary-flow nil)))
            (case (hir-instruction-opcode instruction)
              ((:copy :primary :values)
               (when inputs (setf (gethash (hir-instruction-output instruction) sources) (list (first inputs)))))
              ((:jump :block-return)
               (let* ((target (first (hir-instruction-attributes instruction)))
                      (parameter (and target (first (hir-block-parameters target)))))
                 (when parameter
                   (push (if (eq (hir-instruction-opcode instruction) :jump) (first inputs) (third inputs))
                         (gethash parameter sources))))))))))
    sources))

(defun hir-local-callee-flow (unit barriers sources)
  "Certify captured lexical function cells from executable initialization.
LOCAL metadata only selects candidates. Audit every alias, write and closure
capture; assigned initialization must precede calls/exposure in the owner's entry.
Debugger-visible, escaping or mutated cells retain ordinary lookup."
  (let ((cells (make-hash-table :test 'eq)) (aliases (make-hash-table :test 'eq))
        (writes (make-hash-table :test 'eq)) (bad (make-hash-table :test 'eq))
        (initialized (make-hash-table :test 'eq)) (reads (make-hash-table :test 'eq))
        (fuel 65536))
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
      (unless (or (gethash function barriers) (>= (hir-function-debug-level function) 2))
        (dolist (block (hir-function-blocks function))
          (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
          (dolist (instruction (hir-block-instructions block))
            (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
            (when (eq (hir-instruction-opcode instruction) :cell-new)
              (let ((binding (first (hir-instruction-attributes instruction)))
                    (cell (hir-instruction-output instruction)))
                (when (and (%hir-binding-p binding) (hir-binding-local binding)
                           (eq cell (hir-binding-cell binding)))
                  (setf (gethash cell cells) binding (gethash cell aliases) cell))))))))
    (when (zerop (hash-table-count cells)) (return-from hir-local-callee-flow nil))
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
      ;; A frame can expose a binding without an ordinary executable load.
      (when (>= (hir-function-debug-level function) 2)
        (dolist (identity (hir-function-captures function))
          (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
          (when (%hir-binding-p identity) (setf (gethash (hir-binding-cell identity) bad) t))))
      (dolist (block (hir-function-blocks function))
        (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
        (dolist (instruction (hir-block-instructions block))
          (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
          (when (eq (hir-instruction-opcode instruction) :capture)
            (let* ((binding (second (hir-instruction-attributes instruction)))
                   (cell (and (%hir-binding-p binding) (hir-binding-cell binding))))
              (when (gethash cell cells)
                (setf (gethash (hir-instruction-output instruction) aliases) cell)
                (when (>= (hir-function-debug-level function) 2) (setf (gethash cell bad) t))))))))
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
      (dolist (block (hir-function-blocks function))
        (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
          (let* ((opcode (hir-instruction-opcode instruction)) (inputs (hir-instruction-inputs instruction))
                 (captures (and (eq opcode :closure)
                                (hir-function-captures (first (hir-instruction-attributes instruction))))))
            ;; Capture records do not certify that a client supplied the right
            ;; context operand at every creation of that function's closure.
            (loop for identity in captures for input in inputs do
              (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
              (let ((cell (and (%hir-binding-p identity) (hir-binding-cell identity))))
                (when (and (gethash cell cells) (not (eq cell (gethash input aliases))))
                  (setf (gethash cell bad) t))))
            (loop for input in inputs for index from 0 for identities = captures then (cdr identities)
                  for cell = (gethash input aliases) do
              (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
              (when cell
                (cond ((and (zerop index) (eq opcode :cell-read))
                       (push instruction (gethash cell reads)))
                      ((and (zerop index) (eq opcode :cell-write))
                       (push instruction (gethash cell writes)))
                      ((and (eq opcode :closure) (eq (car identities) (gethash cell cells))))
                      (t (setf (gethash cell bad) t)))))))))
    ;; A write-free allocation initializes the cell itself. Valid SSA use
    ;; dominance also covers cells allocated inside conditional/local scopes.
    (maphash (lambda (cell binding)
      (declare (ignore binding))
      (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
      (unless (gethash cell writes)
        (setf (gethash cell initialized) (first (hir-instruction-inputs (hir-value-producer cell)))))) cells)
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
      (let ((prefix t) (ready (make-hash-table :test 'eq)))
        (dolist (instruction (hir-block-instructions (hir-function-entry function)))
          (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
          (let* ((opcode (hir-instruction-opcode instruction)) (inputs (hir-instruction-inputs instruction))
                 (cell (if (eq opcode :cell-new) (hir-instruction-output instruction) (gethash (first inputs) aliases))))
            (unless (or (member opcode '(:constant :argument :capture :closure :cell-new :cell-read :copy :primary :values :check-arguments))
                        (and (eq opcode :cell-write) (gethash cell cells)))
              (setq prefix nil))
            (when (and prefix (gethash cell cells) (eq (hir-value-owner cell) function))
              (let ((source (case opcode
                              (:cell-new (unless (gethash cell writes) (first inputs)))
                              (:cell-write (when (null (cdr (gethash cell writes))) (second inputs))))))
                (when source
                  (setf (gethash cell initialized) source (gethash cell ready) t))))
            ;; An early load snapshots the pre-initialization value; it must
            ;; not inherit a later write's target even though the cell is local.
            (when (and cell (gethash cell writes) (eq (hir-value-owner cell) function)
                       (eq opcode :cell-read) (not (gethash cell ready)))
              (setf (gethash instruction bad) t))))))
    ;; No partial certificates survive an incomplete alias/use audit.
    (maphash (lambda (cell source)
      (unless (gethash cell bad)
        (dolist (instruction (gethash cell reads))
          (when (minusp (decf fuel)) (return-from hir-local-callee-flow nil))
          (unless (gethash instruction bad)
            (setf (gethash (hir-instruction-output instruction) sources) (list source)))))) initialized)))

(defun hir-result-callees (unit barriers)
  "Bounded executable code facts for this HIR invocation only.
Same-target acyclic forwarding preserves the selected SSA closure as context.
Unknown inputs, cycles and exhausted work provide no certificate."
  (let ((callees (make-hash-table :test 'eq)) (fuel 65536)
        (sources (hir-primary-flow unit barriers)) (active (make-hash-table :test 'eq)))
    (unless sources (return-from hir-result-callees callees))
    (hir-local-callee-flow unit barriers sources)
    (dolist (function (hir-unit-functions unit))
      (when (minusp (decf fuel)) (return-from hir-result-callees callees))
      (unless (gethash function barriers)
        ;; Successful primary preflight bounds all instructions and operands
        ;; scanned by this shared helper, called once per function. Keep that
        ;; prerequisite rather than duplicating its private-cell use rules.
        (let ((cells (hir-private-cells function barriers))
              (writes (make-hash-table :test 'eq)) (targets (make-hash-table :test 'eq)))
          (dolist (block (hir-function-blocks function))
            (when (minusp (decf fuel)) (return-from hir-result-callees callees))
            (dolist (instruction (hir-block-instructions block))
              (when (minusp (decf fuel)) (return-from hir-result-callees callees))
              (when (eq (hir-instruction-opcode instruction) :cell-write)
                (push instruction (gethash (first (hir-instruction-inputs instruction)) writes)))))
          ;; Write-free allocations initialize the cell before every valid
          ;; SSA use, including allocations in nested conditional scopes.
          (maphash (lambda (cell private)
            (declare (ignore private))
            (when (minusp (decf fuel)) (return-from hir-result-callees callees))
            (unless (gethash cell writes)
              (setf (gethash cell targets) (first (hir-instruction-inputs (hir-value-producer cell)))))) cells)
          ;; Assigned cells need initialization in the entry block, before
          ;; successor edges. A single write then dominates later blocks.
          (dolist (instruction (hir-block-instructions (hir-function-entry function)))
            (when (minusp (decf fuel)) (return-from hir-result-callees callees))
            (let* ((inputs (hir-instruction-inputs instruction))
                   (cell (if (eq (hir-instruction-opcode instruction) :cell-new)
                             (hir-instruction-output instruction) (first inputs))))
              (case (hir-instruction-opcode instruction)
                (:cell-new
                 (when (and (gethash cell cells) (null (gethash cell writes)))
                   (setf (gethash cell targets) (first inputs))))
                (:cell-write
                 (when (and (gethash cell cells) (null (cdr (gethash cell writes))))
                   (setf (gethash cell targets) (second inputs))))
                (:cell-read
                 (when (gethash cell targets)
                   (setf (gethash (hir-instruction-output instruction) sources) (list (gethash cell targets))))))))
          (dolist (block (hir-function-blocks function))
            (when (minusp (decf fuel)) (return-from hir-result-callees callees))
            (unless (eq block (hir-function-entry function))
              (dolist (instruction (hir-block-instructions block))
                (when (minusp (decf fuel)) (return-from hir-result-callees callees))
                (when (eq (hir-instruction-opcode instruction) :cell-read)
                  (let ((target (gethash (first (hir-instruction-inputs instruction)) targets)))
                    (when target (setf (gethash (hir-instruction-output instruction) sources) (list target)))))))))))
    ;; Memoization bounds shared DAG traversal;
    ;; an active dependency (including a loop parameter) remains unknown.
    (labels ((target (value depth)
               (when (or (null value) (> depth 64) (gethash value active)) (return-from target nil))
               (when (minusp (decf fuel)) (return-from target nil))
               (multiple-value-bind (known found) (gethash value callees)
                 (when found (return-from target known)))
               (setf (gethash value active) t)
               (let* ((inputs (gethash value sources))
                      (code (if inputs
                                (let ((first (target (first inputs) (1+ depth))))
                                  (and first (every (lambda (input) (eq first (target input (1+ depth)))) (rest inputs)) first))
                                (hir-result-callee value))))
                 (remhash value active)
                 (setf (gethash value callees) code))))
      (dolist (function (hir-unit-functions unit))
        (dolist (value (hir-function-values function))
          (when (minusp fuel) (return-from hir-result-callees callees))
          (target value 0))))
    callees))

(defun hir-result-arities-checked (unit &optional callee-facts)
  "Prove exact value counts from validated, current HIR, independently of types.
NIL means unknown; integer zero is a proof of no values. Cycles and exhausted
work retain unknown facts. No declarations or replaceable global names certify
an arity. Recompute after any HIR mutation."
  (let ((facts (make-hash-table :test 'eq))
        (incoming (make-hash-table :test 'eq))
        (barriers (hir-optimization-barriers unit)) (fuel 65536))
   (let ((callees (or callee-facts (hir-result-callees unit barriers))))
    (labels ((join (sources)
               (when sources
                 (let ((count (gethash (first sources) facts)))
                   (and count (every (lambda (source) (eql count (gethash source facts))) (rest sources)) count)))))
      (dolist (function (hir-unit-functions unit))
        (dolist (block (hir-function-blocks function))
          (let* ((end (hir-block-terminator block)) (opcode (hir-instruction-opcode end))
                 (target (and (member opcode '(:jump :block-return))
                              (first (hir-instruction-attributes end)))))
            (when target
              (push (if (eq opcode :jump) (first (hir-instruction-inputs end))
                        (third (hir-instruction-inputs end)))
                    (gethash (first (hir-block-parameters target)) incoming)))))
        ;; Structural capture edges, rather than client-maintained capture
        ;; flags, prevent certifying local-only incoming groups at landing pads.
        (when (gethash function barriers)
          (dolist (block (hir-function-blocks function))
            (dolist (parameter (hir-block-parameters block))
              (push nil (gethash parameter incoming))))))
      (dotimes (pass 32)
        (let ((changed nil))
          (dolist (function (hir-unit-functions unit))
            (dolist (value (hir-function-values function))
              (when (minusp (decf fuel)) (return-from hir-result-arities-checked (values facts callees)))
              (unless (gethash value facts)
                (let* ((origin (hir-value-producer value))
                       (count (cond
                                ((%hir-instruction-p origin)
                                 (case (hir-instruction-opcode origin)
                                   (:values (length (hir-instruction-inputs origin)))
                                   (:copy (gethash (first (hir-instruction-inputs origin)) facts))
                                   (:call (gethash (hir-result-callee (second (hir-instruction-inputs origin)) callees) facts))
                                   (otherwise (unless (eq (hir-value-kind value) :multiple) 1))))
                                (t (join (gethash value incoming))))))
                  (when count (setf (gethash value facts) count changed t)))))
            (unless (gethash function facts)
              (let ((returns nil))
                (dolist (block (hir-function-blocks function))
                  (let ((end (hir-block-terminator block)))
                    (when (eq (hir-instruction-opcode end) :return)
                      (push (first (hir-instruction-inputs end)) returns))))
                (let ((count (join returns)))
                  (when count (setf (gethash function facts) count changed t))))))
          (unless changed (return))))
      (values facts callees)))))

(defvar *hir-inline-definitions* nil
  "Canonical INLINE definitions selected for one analysis/emission.
Share this snapshot with lowering so a cooperative redefinition cannot turn
a proved type predicate back into an ordinary call with different semantics.")

(defun hir-value-type-at (type index)
  (declare (type (unsigned-byte 32) index))
  ;; THE and FTYPE constrain values without imposing an exact value count.
  (if (and (consp type) (eq (car type) 'values))
      (let ((optional nil))
        (dolist (part (cdr type) t)
          (cond ((eq part '&optional) (setq optional t))
                ((eq part '&rest) (return t))
                ((zerop index) (return (if optional (list 'or 'null part) part)))
                (t (decf index)))))
      (if (zerop index) type t)))

(defun hir-type-equal-p (left right)
  "Structural type equality, respecting literal identity in MEMBER and EQL.
An explicit worklist and memoized pairs handle shared type DAGs. Opaque type
arguments and literal objects are never recursively compared."
  (when (eq left right) (return-from hir-type-equal-p t))
  (unless (and (consp left) (consp right) (eq (car left) (car right)))
    (return-from hir-type-equal-p nil))
  ;; Normalized intervals account for most fixed-point comparisons.
  (when (member (car left) '(integer signed-byte unsigned-byte mod simple-vector))
    (return-from hir-type-equal-p (equal left right)))
  (let ((work (list (cons left right))) (seen (make-hash-table :test 'eq)))
    (loop while work do
      (let* ((pair (pop work)) (a (car pair)) (b (cdr pair)))
        (unless (eq a b)
          (unless (and (consp a) (consp b) (eq (car a) (car b)))
            (return-from hir-type-equal-p nil))
          (case (car a)
            ((integer signed-byte unsigned-byte mod simple-vector)
             (unless (equal a b) (return-from hir-type-equal-p nil)))
            (eql (unless (eql (second a) (second b)) (return-from hir-type-equal-p nil)))
            ((and or not cons values member)
             (unless (member b (gethash a seen) :test miricl::%compiler-eq-function)
               (push b (gethash a seen))
               (let ((x (cdr a)) (y (cdr b)) (literal (eq (car a) 'member)))
                 (loop while (and x y) do
                   (if literal
                       (unless (eql (car x) (car y)) (return-from hir-type-equal-p nil))
                       (push (cons (car x) (car y)) work))
                   (setq x (cdr x) y (cdr y)))
                 (unless (and (null x) (null y)) (return-from hir-type-equal-p nil)))))
            (otherwise (return-from hir-type-equal-p nil))))))
    t))

(defun hir-integer-interval (type &optional (depth 32) fuel)
  "An integer interval containing TYPE, with NIL endpoints for infinity.
The third value proves integer membership. Unknown types prove nothing."
  (declare (type (integer 0 32) depth) (type list fuel))
  (when (zerop depth)
    (compiler-statistic :proof/budget-exhausted :integer-interval)
    (return-from hir-integer-interval (values nil nil nil)))
  (when fuel
    (when (zerop (car fuel))
      (compiler-statistic :proof/budget-exhausted :integer-interval)
      (return-from hir-integer-interval (values nil nil nil)))
    (decf (car fuel)))
  (cond ((eq type 'bit) (values 0 1 t))
        ((eq type 'fixnum) (values -9223372036854775808 9223372036854775807 t))
        ((member type '(integer signed-byte)) (values nil nil t))
        ((eq type 'unsigned-byte) (values 0 nil t))
        ((consp type)
         (case (car type)
           (integer
            (let ((low (second type)) (high (third type)))
              (values (cond ((integerp low) low) ((consp low) (1+ (car low))))
                      (cond ((integerp high) high) ((consp high) (1- (car high)))) t)))
           (signed-byte
            (let ((bits (second type)))
              (if (and (integerp bits) (<= 1 bits 64))
                  (let ((limit (ash 1 (1- bits)))) (values (- limit) (1- limit) t))
                  (values nil nil t))))
           (unsigned-byte
            (let ((bits (second type)))
              (values 0 (and (integerp bits) (<= 0 bits 64) (1- (ash 1 bits))) t)))
           (mod (values 0 (1- (second type)) t))
           (eql (when (integerp (second type)) (values (second type) (second type) t)))
           (member
            (let ((low nil) (high nil))
              (dolist (item (cdr type))
                (unless (integerp item) (return-from hir-integer-interval (values nil nil nil)))
                (setq low (if low (min low item) item) high (if high (max high item) item)))
              (when low (values low high t))))
           ((and or)
            ;; Share a total work budget across siblings as well as limiting
            ;; depth. Small shared type DAGs must not cause exponential work.
            (unless fuel (setq fuel (list 128)))
            (let ((intersection (eq (car type) 'and)) (low nil) (high nil) (known nil))
              (dolist (part (cdr type))
                (multiple-value-bind (a b integer) (hir-integer-interval part (1- depth) fuel)
                  (cond
                    (intersection
                     ;; Every conjunct restricts the same values. Opaque
                     ;; conjuncts cannot weaken an established integer bound.
                     (when integer
                       (setq low (if (and low a) (max low a) (or low a))
                             high (if (and high b) (min high b) (or high b)) known t)))
                    ((not integer) (return-from hir-integer-interval (values nil nil nil)))
                    ((not known) (setq low a high b known t))
                    (t (setq low (and low a (min low a)) high (and high b (max high b)))))))
              (values low high known)))))))

(defun hir-integer-bounds (type)
  ;; Keep the hot, normalized interval case small. More expressive CL types
  ;; use the same rules, independently of the host's SUBTYPEP and DEFTYPE.
  (cond ((eq type 'bit) (values 0 1 t))
        ((eq type 'fixnum) (values -9223372036854775808 9223372036854775807 t))
        ((and (consp type) (eq (car type) 'integer)
              (integerp (second type)) (integerp (third type)))
         (when (<= (second type) (third type)) (values (second type) (third type) t)))
        ((and (consp type) (eq (car type) 'unsigned-byte)
              (integerp (second type)) (<= 0 (second type) 64))
         (values 0 (1- (ash 1 (second type))) t))
        ((and (consp type) (eq (car type) 'signed-byte)
              (integerp (second type)) (<= 1 (second type) 64))
         (let ((limit (ash 1 (1- (second type))))) (values (- limit) (1- limit) t)))
        (t (multiple-value-bind (low high integer) (hir-integer-interval type)
             (when (and integer low high (<= low high)) (values low high t))))))

(defun hir-nominal-type-mask (type)
  "Positive target type facts; never expand an opaque type or inspect a value.
Bits from low to high: number/real/rational/integer/float, array/vector/simple-array/simple-vector,
string/simple-string/sequence. Parameterized array element types are invariant."
  (let ((name (if (consp type) (car type) type)))
    (case name
      ((integer fixnum bignum bit signed-byte unsigned-byte mod) 15)
      ((rational ratio) 7) (real 3)
      ((float short-float single-float double-float long-float) 19)
      ((number complex) 1)
      ((vector bit-vector) 2144)
      (simple-vector 2528)
      ((string base-string) 2656)
      ((simple-string simple-base-string) 3808)
      (simple-bit-vector 2272)
      ((list cons null) 2048)
      ((array simple-array)
       (let* ((simple (eq name 'simple-array)) (mask (if simple 160 32))
              (shape (and (consp type) (third type))))
         ;; A rank specifier of 1 and a one-axis shape both mean VECTOR.
         ;; Read only this bounded shape, even if a private query is cyclic.
         (when (or (eql shape 1)
                   (and (consp shape) (null (cdr shape))
                        (or (eq (car shape) '*)
                            (and (integerp (car shape)) (<= 0 (car shape))))))
           (setq mask (logior mask 2112))
           (let ((element (second type)))
             (cond ((eq element t) (when simple (setq mask (logior mask 256))))
                   ;; These built-in element types upgrade only within the
                   ;; target's character family. No user DEFTYPE is queried.
                   ((member element '(character base-char standard-char))
                    (setq mask (logior mask (if simple 1536 512)))))))
         mask))
      (otherwise 0))))

(defun hir-disjoint-type-mask (type)
  "A conservative cover by disjoint CLHS 4.2.2 families, or zero if unknown."
  ;; Reuse target nominal rules for numeric and parameterized array types.
  ;; LIST includes NIL, which belongs to SYMBOL. Do not cover SEQUENCE by
  ;; LIST and VECTOR: those need not exhaust that class. Opaque classes and
  ;; STANDARD-OBJECT/STRUCTURE-OBJECT supply no disjointness certificate.
  (let ((nominal (hir-nominal-type-mask type)))
    (cond ((not (zerop (logand nominal 1))) 1)
          ((not (zerop (logand nominal 32))) 2)
          (t (case (if (consp type) (car type) type)
               ((symbol keyword null boolean) 4)
               (cons 8) (list 12)
               ((character base-char standard-char extended-char) 16)
               ((function compiled-function) 32)
               (hash-table 64) (package 128) (readtable 256)
               (otherwise 0))))))

(defun hir-type-proves-p (type required &optional (depth 32) fuel)
  "True only when CL type relationships establish TYPE is within REQUIRED.
NIL means unknown; this is deliberately not a complete SUBTYPEP procedure."
  (declare (type (integer 0 32) depth) (type list fuel))
  (when (zerop depth)
    (compiler-statistic :proof/budget-exhausted :subtype)
    (return-from hir-type-proves-p nil))
  (when (or (and (consp type) (member (car type) '(and or member eql)))
            (and (consp required) (member (car required) '(and or not))))
    (unless fuel (setq fuel (list 128))))
  (when fuel
    (when (zerop (car fuel))
      (compiler-statistic :proof/budget-exhausted :subtype)
      (return-from hir-type-proves-p nil))
    (decf (car fuel)))
  (cond ((or (eq required t) (hir-type-equal-p type required)) t)
        ((and (consp type) (eq (car type) 'eql))
         (cond ((eq required 'null) (null (second type)))
               ((equal required '(not null)) (not (null (second type))))
               (t (hir-type-proves-p (hir-constant-type (second type)) required (1- depth) fuel))))
        ((and (consp type) (eq (car type) 'member))
         (every (lambda (item) (hir-type-proves-p (list 'eql item) required (1- depth) fuel)) (cdr type)))
        ((and (consp type) (eq (car type) 'or))
         (every (lambda (part) (hir-type-proves-p part required (1- depth) fuel)) (cdr type)))
        ((and (consp required) (eq (car required) 'and))
         (every (lambda (part) (hir-type-proves-p type part (1- depth) fuel)) (cdr required)))
        ((and (consp type) (eq (car type) 'and))
         (some (lambda (part) (hir-type-proves-p part required (1- depth) fuel)) (cdr type)))
        ((and (consp required) (eq (car required) 'or))
         (some (lambda (part) (hir-type-proves-p type part (1- depth) fuel)) (cdr required)))
        ((eq required 'list) (or (member type '(null cons)) (and (consp type) (eq (car type) 'cons))))
        ((eq required 'cons) (and (consp type) (eq (car type) 'cons)))
        ((eq required 'symbol) (member type '(null keyword)))
        ((equal required '(not null))
         ;; Reuse the target's numeric/array families, including specialized
         ;; vectors and numeric subtypes. The sequence bit alone is insufficient:
         ;; NIL is a list. Opaque types do not supply any nominal facts.
         (or (member type '(cons character function compiled-function hash-table package readtable keyword))
             (and (consp type) (member (car type) '(cons function)))
             (not (zerop (logand 33 (hir-nominal-type-mask type))))))
        ((and (consp required) (eq (car required) 'not))
         (let ((excluded (second required)))
           (cond ((and (consp excluded) (member (car excluded) '(and or)))
                  (let ((queries (cdr excluded)))
                    (if (eq (car excluded) 'or)
                        (every (lambda (part) (hir-type-proves-p type (list 'not part) (1- depth) fuel)) queries)
                        (some (lambda (part) (hir-type-proves-p type (list 'not part) (1- depth) fuel)) queries))))
                 (t (let ((left (hir-disjoint-type-mask type)) (right (hir-disjoint-type-mask excluded)))
                      (and (plusp left) (plusp right) (zerop (logand left right))))))))
        ;; Shared nominal facts prove only unparameterized supertypes.
        ;; Shape/element constraints and integer bounds keep their own paths;
        ;; skip this domain altogether for those frequent compound queries.
        ((and (not (consp required))
              (let ((mask (case required
                            (number 1) (real 2) (rational 4) (integer 8) (float 16)
                            (array 32) (vector 64) (simple-array 128) (simple-vector 256)
                            (string 512) (simple-string 1024) (sequence 2048))))
                (and mask (not (zerop (logand mask (hir-nominal-type-mask type))))))) t)
        (t
         (multiple-value-bind (low high integer) (hir-integer-interval type depth fuel)
           (multiple-value-bind (a b target) (hir-integer-interval required depth fuel)
             (and integer target (or (null a) (and low (>= low a)))
                  (or (null b) (and high (<= high b)))))))))

(defun hir-type-join (left right)
  (cond ((or (eq left t) (eq right t)) t)
        ((null left) right) ((null right) left) ((hir-type-equal-p left right) left)
        (t
         (multiple-value-bind (low-a high-a integer-a) (hir-integer-bounds left)
           (multiple-value-bind (low-b high-b integer-b) (hir-integer-bounds right)
             (if (and integer-a integer-b)
                 ;; Reuse an interval already covering the union. An
                 ;; arbitrary bounded type may have holes (MEMBER, OR, or
                 ;; an opaque AND conjunct), so its hull alone is not enough
                 ;; to justify returning that type as the union.
                 (cond ((and (consp left) (eq (car left) 'integer)
                             (<= low-a low-b high-b high-a)) left)
                       ((and (consp right) (eq (car right) 'integer)
                             (<= low-b low-a high-a high-b)) right)
                       (t (list 'integer (min low-a low-b) (max high-a high-b))))
                 (let ((parts (if (and (consp left) (eq (car left) 'or)) (cdr left) (list left))))
                   (dolist (part (if (and (consp right) (eq (car right) 'or)) (cdr right) (list right)))
                     (unless (member part parts :test #'hir-type-equal-p) (setq parts (append parts (list part)))))
                   (cons 'or parts))))))))

(defun hir-type-restrict (derived asserted)
  (setq asserted (hir-value-type-at asserted 0))
  (cond ((null derived) nil) ((eq asserted t) derived) ((eq derived t) asserted)
        ((hir-type-equal-p derived asserted) asserted)
        (t
         (multiple-value-bind (low-a high-a integer-a) (hir-integer-bounds derived)
           (multiple-value-bind (low-b high-b integer-b) (hir-integer-bounds asserted)
             (if (and integer-a integer-b)
                 (let ((low (max low-a low-b)) (high (min high-a high-b)))
                   ;; Either operand contains the intersection. Retaining
                   ;; one whose bounds already match avoids fresh conses,
                   ;; and can retain useful MEMBER/EQL restrictions too.
                   (cond ((> low high) nil)
                         ((and (= low low-a) (= high high-a)) derived)
                         ((and (= low low-b) (= high high-b)) asserted)
                         (t (list 'integer low high))))
                 ;; Retaining just the assertion is a sound upper bound of
                 ;; the intersection, and avoids growing recursive AND trees.
                 asserted))))))

(defun hir-constant-type (value)
  (cond ((integerp value) (list 'integer value value))
        ((null value) 'null) ((symbolp value) 'symbol)
        ((typep value 'simple-vector) 'simple-vector)
        ((consp value) 'cons) ((characterp value) 'character) ((stringp value) 'string)
        ((floatp value) 'float) (t t)))

(defun hir-dispatch-candidate (type &optional (depth 16) (fuel (list 64)))
  "A class-name or singleton candidate for a guarded generic call.
Nominal declarations include subclasses: this is not an exact-class proof.
Lowering must retain an exact runtime class guard. Opaque compound types
provide no candidate, and no DEFTYPE or SATISFIES code is executed here."
  (when (or (zerop depth) (zerop (car fuel)))
    (return-from hir-dispatch-candidate nil))
  (decf (car fuel))
  (multiple-value-bind (low high bounded) (hir-integer-bounds type)
    (when (and bounded (= low high))
      (return-from hir-dispatch-candidate (list :eql low))))
  (cond ((or (eq type t) (null type)) nil)
        ((eq type 'null) (list :eql nil))
        ((hir-type-proves-p type 'fixnum) (list :class 'fixnum))
        ((symbolp type) (list :class type))
        ((consp type)
         (case (car type)
           (eql (list :eql (second type)))
           (member (when (and (cdr type) (every (lambda (x) (eql x (second type))) (cddr type)))
                     (list :eql (second type))))
           (and (dolist (part (cdr type))
                  (let ((candidate (hir-dispatch-candidate part (1- depth) fuel)))
                    (when candidate (return candidate)))))
           (or (let ((candidate nil))
                 (dolist (part (cdr type) candidate)
                   (let ((next (hir-dispatch-candidate part (1- depth) fuel)))
                     (unless (and next
                                  (or (not candidate)
                                      (and (eq (car next) (car candidate))
                                           (eql (second next) (second candidate)))))
                       (return nil))
                     (setq candidate next)))))
           ((cons simple-vector vector array simple-array string simple-string)
            (list :class (case (car type)
                           ((array simple-array) 'array)
                           ((vector simple-vector) 'vector)
                           ((string simple-string) 'string)
                           (otherwise 'cons))))))))

(defun hir-type-cell (value)
  (declare (type simple-vector value))
  (let ((producer (hir-value-producer value)))
    (when (%hir-instruction-p producer)
      (case (hir-instruction-opcode producer)
        (:cell-new value)
        (:capture (let ((binding (second (hir-instruction-attributes producer))))
                    (when (%hir-binding-p binding) (hir-binding-cell binding))))))))

(defun hir-type-without-null (type &optional (depth 32) fuel)
  "A conservative CL type containing TYPE's non-NIL members."
  (declare (type (integer 0 32) depth) (type list fuel))
  (when (zerop depth) (return-from hir-type-without-null type))
  (when fuel
    (when (zerop (car fuel)) (return-from hir-type-without-null type))
    (decf (car fuel)))
  (cond ((eq type 'null) nil) ((eq type 'list) 'cons)
        ((eq type 'boolean) '(eql t))
        ((consp type)
         (case (car type)
           (or (unless fuel (setq fuel (list 128)))
               (let ((result nil))
                 (dolist (part (cdr type) result)
                   (setq result (hir-type-join result (hir-type-without-null part (1- depth) fuel))))))
           (member (let ((members (remove nil (cdr type)))) (and members (cons 'member members))))
           (eql (and (second type) type))
           (otherwise type)))
        (t type)))

(defun hir-copy-source (value)
  (declare (type simple-vector value))
  (dotimes (depth 32 value)
    (let ((producer (hir-value-producer value)))
      (unless (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :copy))
        (return value))
      (setq value (first (hir-instruction-inputs producer))))))

(defun hir-type-truth-premise (test cache &optional cells)
  "Recover a truth or canonical type predicate through immutable temporaries.
The third value is a type established by a positive predicate, an integer
predicate name, or NIL for ordinary truth. CELLS records actual private uses,
not client binding flags."
  (declare (type simple-vector test) (type hash-table cache))
  (let ((current test) (inverse nil) (predicate nil))
    (dotimes (depth 32)
      (let ((producer (hir-value-producer current)))
        (unless (%hir-instruction-p producer) (return))
        (case (hir-instruction-opcode producer)
          (:primary (setq current (first (hir-instruction-inputs producer))))
          (:copy
           (when predicate (return-from hir-type-truth-premise (values current inverse)))
           (setq current (first (hir-instruction-inputs producer))))
          (:call
           (unless (= (length (hir-instruction-inputs producer)) 3) (return))
           (case (hir-inline-primitive producer cache)
             ((null not)
              (setq predicate t inverse (not inverse)
                    current (third (hir-instruction-inputs producer))))
             ((consp simple-vector-p vectorp stringp symbolp integerp characterp)
              (return-from hir-type-truth-premise
                (values (third (hir-instruction-inputs producer)) inverse
                  (case (hir-inline-primitive producer cache)
                    (consp 'cons) (simple-vector-p 'simple-vector) (vectorp 'vector) (stringp 'string) (symbolp 'symbol) (integerp 'integer) (characterp 'character)))))
             ((zerop minusp plusp)
              ;; Numeric predicates currently select their canonical inline
              ;; implementation only at SAFETY 0. Other calls stay replaceable.
              (when (zerop (hir-instruction-safety-level producer))
                (return-from hir-type-truth-premise
                  (values (third (hir-instruction-inputs producer)) inverse
                    (hir-inline-primitive producer cache))))
              (return))
             (otherwise (return))))
          (:cell-read
           (when predicate (return-from hir-type-truth-premise (values current inverse)))
           (let* ((cell (first (hir-instruction-inputs producer)))
                  (origin (hir-value-producer cell)))
             (unless (and cells (eq (gethash cell cells) t) (%hir-instruction-p origin)
                          (eq (hir-instruction-opcode origin) :cell-new))
               (return))
             (setq current (first (hir-instruction-inputs origin)))))
          (otherwise (return)))))
    ;; If the expression is not a recognized predicate, a direct cell read
    ;; still establishes its own truth. Never transfer a partial inversion.
    (let ((producer (hir-value-producer test)))
      (when (and (%hir-instruction-p producer) (member (hir-instruction-opcode producer) '(:cell-read :copy)))
        (values test nil)))))

(defun hir-type-branch-guards (unit)
  "Type premises established by truth tests of structurally private cells.
Joins are followed only after all predecessors establish the premise.
A mutable cell's premise ends at its next write. Its tested read must be in
the branch block with no intervening write; immutable predicate temporaries
must not reestablish an old observation after mutation.
A budget limits cost; stopping early only loses optimization opportunities."
  (declare (type hir-unit unit))
  (let ((cells (make-hash-table :test 'eq)) (guards (make-hash-table :test 'eq))
        (barriers (make-hash-table :test 'eq))
        (primitives (or *hir-inline-definitions* (make-hash-table :test 'equal)))
        (size 0))
    (dolist (function (hir-unit-functions unit))
      (incf size (hir-function-value-count function))
      (dolist (value (hir-function-values function))
        (let ((producer (hir-value-producer value)))
          (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :cell-new))
            (setf (gethash value cells) t)))))
    ;; Inspect actual uses, not client-visible assigned/captured flags.
    ;; Direct writes kill premises locally. Aliasing, capture or passing the
    ;; cell itself to any other operation forbids premises across reads.
    (dolist (function (hir-unit-functions unit))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (when (eq (hir-instruction-opcode instruction) :capture)
            (let ((target (second (hir-instruction-attributes instruction))))
              (when (%hir-target-p target) (setf (gethash (hir-target-block target) barriers) t))))
          ;; A verified CELL-READ has exactly one operand, its cell. Skip
          ;; those common uses altogether instead of hashing every read.
          (unless (eq (hir-instruction-opcode instruction) :cell-read)
            (loop for input in (hir-instruction-inputs instruction) for index from 0 do
              (when (gethash input cells)
                (if (and (eq (hir-instruction-opcode instruction) :cell-write) (zerop index))
                    (setf (gethash input cells) :mutable)
                    (remhash input cells))))))))
    (let ((budget (max 256 (* size 4))))
      (dolist (function (hir-unit-functions unit))
        (let* ((count (hir-function-block-count function))
               (epochs (make-array count :initial-element 0))
               (remaining (make-array count :initial-element 0))
               (incoming (make-array count :initial-element 0)) (serial 0))
          (declare (type simple-vector epochs remaining incoming) (type (unsigned-byte 32) serial))
          (dolist (block (hir-function-blocks function))
            (let ((id (hir-block-id block)))
              (declare (type (unsigned-byte 32) id))
              (setf (aref incoming id) (length (hir-block-predecessors block)))))
          (dolist (block (hir-function-blocks function))
            (let* ((branch (hir-block-terminator block))
                   (test (and (eq (hir-instruction-opcode branch) :branch)
                              (first (hir-instruction-inputs branch))))
                   (premise (and test (multiple-value-list (hir-type-truth-premise test primitives cells))))
                   (observed (first premise))
                   (producer (and observed (hir-value-producer observed)))
                   (cell (and (%hir-instruction-p producer)
                              (eq (hir-instruction-opcode producer) :cell-read)
                              (first (hir-instruction-inputs producer))))
                   (source (and (%hir-instruction-p producer)
                                (eq (hir-instruction-opcode producer) :copy)
                                (hir-copy-source observed)))
                   (arms (hir-instruction-attributes branch))
                   (private (gethash cell cells)))
              (when (and (plusp budget) (< (hir-function-debug-level function) 2)
                         (eq (hir-instruction-opcode branch) :branch)
                         (or source (eq private t)
                             (and (eq private :mutable)
                               (let ((available nil))
                                 (dolist (instruction (hir-block-instructions block) available)
                                   (when (minusp (decf budget)) (setq available nil) (return))
                                   (when (eq instruction producer) (setq available t))
                                   (when (and (eq (hir-instruction-opcode instruction) :cell-write)
                                              (eq (first (hir-instruction-inputs instruction)) cell))
                                     (setq available nil))))))
                         (not (eq (first arms) (second arms))))
                (loop for arm in arms for positive in '(t nil) do
                  (incf serial)
                  ;; The initial edge must itself establish the premise.
                  ;; Subsequent joins need every predecessor inside this region.
                  (let* ((positive (if (second premise) (not positive) positive))
                         (restriction (third premise))
                         ;; Canonical type predicates establish their type's
                         ;; complement on the false arm as well.
                         (pending (when (= (aref incoming (the (unsigned-byte 32) (hir-block-id arm))) 1)
                                    (list arm))))
                    (loop while (and pending (plusp budget)) do
                      (let* ((current (pop pending)) (id (hir-block-id current)) (killed nil))
                        (declare (type (unsigned-byte 32) id))
                        (decf budget)
                        (when (and (not (gethash current barriers))
                                   (not (eq current (hir-function-entry function)))
                                   (not (and (= (aref epochs id) serial) (= (aref remaining id) -1))))
                          (setf (aref epochs id) serial (aref remaining id) -1)
                          (dolist (instruction (hir-block-instructions current))
                            (when (minusp (decf budget)) (return))
                            (when (and cell (eq (hir-instruction-opcode instruction) :cell-write)
                                       (eq (first (hir-instruction-inputs instruction)) cell))
                              (compiler-statistic :proof/branch-write-barrier)
                              (setq killed t)
                              (return))
                            (when (or (and cell (eq (hir-instruction-opcode instruction) :cell-read)
                                           (eq (first (hir-instruction-inputs instruction)) cell))
                                      (and (eq (hir-instruction-opcode instruction) :copy)
                                           (eq (hir-copy-source (hir-instruction-output instruction))
                                               (or source observed))))
                              (compiler-statistic :proof/branch-premise (or restriction positive))
                              (push (cons observed (if (member restriction '(zerop minusp plusp))
                                                      (cons restriction positive)
                                                      (if restriction
                                                          (if positive restriction (list 'not restriction))
                                                          positive)))
                                    (gethash (hir-instruction-output instruction) guards))))
                          (let ((terminator (hir-block-terminator current)) (previous nil))
                            (when (and (not killed) (member (hir-instruction-opcode terminator) '(:jump :branch)))
                              (dolist (successor (hir-instruction-attributes terminator))
                                ;; Predecessors are unique even if both branch
                                ;; arms name the same successor.
                                (unless (eq successor previous)
                                  (setq previous successor)
                                  (let ((next (hir-block-id successor)))
                                    (declare (type (unsigned-byte 32) next))
                                    (unless (= (aref epochs next) serial)
                                      (setf (aref epochs next) serial
                                            (aref remaining next) (aref incoming next)))
                                    (when (plusp (aref remaining next))
                                      (when (zerop (decf (aref remaining next)))
                                        (push successor pending)))))))))))))))))))
    guards))

(defun hir-inline-primitive (instruction cache)
  (declare (type simple-vector instruction) (type hash-table cache))
  (let* ((reference (second (hir-instruction-inputs instruction)))
         (producer (hir-value-producer reference))
         (attributes (and (%hir-instruction-p producer)
                          (eq (hir-instruction-opcode producer) :function-reference)
                          (hir-instruction-attributes producer)))
         (name (first attributes)))
    ;; INLINE is the contract that permits relying on the definition selected
    ;; at compilation. Ordinary calls retain their unrestricted result types.
    (when (and (eq (second attributes) 'inline) (member name '(char char-code code-char sbit row-major-aref length integer-length mod truncate cons list list* consp simple-vector-p vectorp stringp symbolp integerp characterp null not eq eql = /= < <= > >= zerop minusp plusp evenp oddp 1+ 1- + - * logand logior logxor lognot ash)))
      (multiple-value-bind (canonical found) (gethash name cache)
        (unless found
          (setq canonical
            (and (fboundp name)
                 (multiple-value-bind (actual native) (compiler-primitive 19 name)
                   (or (not native) (= actual (native-canonical-builtin-code name))))))
          (setf (gethash name cache) canonical))
        (and canonical name)))))

(defun hir-integer-identity (name left right)
  "Return :LEFT or :RIGHT when integer types prove a binary result is that operand."
  (declare (type symbol name))
  (unless (member name '(+ - * logand logior logxor ash))
    (return-from hir-integer-identity nil))
  (multiple-value-bind (a b integer-a) (hir-integer-interval left)
    (multiple-value-bind (c d integer-b) (hir-integer-interval right)
      ;; Prove both operands are integers, including unbounded bignums.
      ;; Numeric identities do not generally preserve floating-point or
      ;; complex contagion, and an unknown operand can require a type error.
      (unless (and integer-a integer-b)
        (compiler-statistic :proof/integer-identity-unknown name))
      (when (and integer-a integer-b)
        (let ((zero-a (and (eql a 0) (eql b 0)))
              (zero-b (and (eql c 0) (eql d 0)))
              (one-a (and (eql a 1) (eql b 1)))
              (one-b (and (eql c 1) (eql d 1)))
              (minus-a (and (eql a -1) (eql b -1)))
              (minus-b (and (eql c -1) (eql d -1))))
          (let ((result (case name
            ((+ logxor) (cond (zero-b :left) (zero-a :right)))
            (- (when zero-b :left))
            (* (cond (zero-a :left) (zero-b :right) (one-a :right) (one-b :left)))
            (logand (cond (zero-a :left) (zero-b :right) (minus-a :right) (minus-b :left)))
            (logior (cond (minus-a :left) (minus-b :right) (zero-b :left) (zero-a :right)))
            (ash (when (or zero-a zero-b) :left)))))
            (compiler-statistic (if result :proof/integer-identity-known :proof/integer-identity-unknown) name)
            result))))))

(defun hir-integer-branch-type (type name positive)
  "An upper bound of integer TYPE's members satisfying this predicate arm.
Numeric predicates alone cannot establish INTEGER: floats and other numeric
types retain their existing type. Removing an interior zero would leave a
hole, so keep the original interval in that case."
  (declare (type symbol name))
  (multiple-value-bind (low high integer) (hir-integer-interval type)
    (unless integer (return-from hir-integer-branch-type type))
    (let ((a low) (b high))
      (case name
        (zerop
         (cond (positive (setq a 0 b 0))
               ((and low (>= low 0)) (setq a (max low 1)))
               ((and high (<= high 0)) (setq b (min high -1)))))
        (minusp (if positive (setq b (if high (min high -1) -1))
                            (setq a (if low (max low 0) 0))))
        (plusp (if positive (setq a (if low (max low 1) 1))
                           (setq b (if high (min high 0) 0)))))
      (cond ((and a b (> a b)) nil)
            ((and (eql a low) (eql b high)) type)
            (t (compiler-statistic :proof/integer-branch-refinement name)
               (list 'integer (or a '*) (or b '*)))))))

(defun hir-integer-predicate-type (name type)
  (declare (type symbol name))
  (multiple-value-bind (low high integer) (hir-integer-interval type)
    (let ((answer
            (when integer
              (case name
                (zerop (cond ((and (eql low 0) (eql high 0)) :true)
                             ((or (and low (> low 0)) (and high (< high 0))) :false)))
                (minusp (cond ((and high (< high 0)) :true) ((and low (>= low 0)) :false)))
                (plusp (cond ((and low (> low 0)) :true) ((and high (<= high 0)) :false)))
                ((evenp oddp)
                 (when (and low high (= low high))
                   ;; Query the target integer kernel, never an application's
                   ;; possibly redefined ODDP/EVENP binding during a proof.
                   (let ((odd (multiple-value-bind (value native) (compiler-primitive 47 low)
                                (if native value (oddp low)))))
                     (if (if (eq name 'evenp) (not odd) odd) :true :false))))))))
      (case answer
        (:true (compiler-statistic :proof/integer-predicate-true name) '(eql t))
        (:false (compiler-statistic :proof/integer-predicate-false name) 'null)
        (otherwise (compiler-statistic :proof/integer-predicate-unknown name) 'boolean)))))

(defun hir-integer-comparison-type (name left right)
  "Prove comparison results from target integer intervals, otherwise BOOLEAN.
Equal integer values suffice for EQL. EQ identity is not implied by equal
numbers, including bignums, and is never proved true by this rule."
  (declare (type symbol name))
  (multiple-value-bind (a b integer-a) (hir-integer-interval left)
    (multiple-value-bind (c d integer-b) (hir-integer-interval right)
      (unless (and integer-a integer-b)
        (compiler-statistic :proof/integer-comparison-unknown name)
        (return-from hir-integer-comparison-type 'boolean))
      (let ((answer
        (case name
          ((eq eql = /=)
           (cond ((or (and b c (< b c)) (and d a (< d a)))
                  (if (eq name '/=) :true :false))
                 ((and (not (eq name 'eq)) a b c d (= a b c d))
                  (if (eq name '/=) :false :true))))
          (< (cond ((and b c (< b c)) :true) ((and a d (>= a d)) :false)))
          (<= (cond ((and b c (<= b c)) :true) ((and a d (> a d)) :false)))
          (> (cond ((and a d (> a d)) :true) ((and b c (<= b c)) :false)))
          (>= (cond ((and a d (>= a d)) :true) ((and b c (< b c)) :false))))))
        (case answer
          (:true (compiler-statistic :proof/integer-comparison-true name) '(eql t))
          (:false (compiler-statistic :proof/integer-comparison-false name) 'null)
          (otherwise (compiler-statistic :proof/integer-comparison-unknown name) 'boolean))))))

(defun hir-inline-result-type (name types)
  (declare (type symbol name) (type list types))
  (when (member nil types) (return-from hir-inline-result-type nil))
  ;; These describe successful returns of canonical target operations. Keep
  ;; the target's Unicode repertoire independent of host CHAR-CODE-LIMIT.
  (when (and (eq name 'char) (= (length types) 2))
    (return-from hir-inline-result-type 'character))
  (when (and (eq name 'char-code) (= (length types) 1))
    (return-from hir-inline-result-type '(integer 0 #x10ffff)))
  (when (and (eq name 'code-char) (= (length types) 1))
    (multiple-value-bind (low high known) (hir-integer-bounds (first types))
      (return-from hir-inline-result-type
        (if (and known (<= 0 low high #x10ffff) (or (< high #xd800) (> low #xdfff)))
            'character '(or null character)))))
  (when (and (eq name 'sbit) (= (length types) 2))
    ;; A successful canonical SBIT returns one bit. Bounds, rank and array
    ;; checks remain in lowering; this fact never validates its operands.
    (return-from hir-inline-result-type '(integer 0 1)))
  (when (and (eq name 'integer-length) (= (length types) 1))
    ;; Successful canonical returns are nonnegative; do not assume a host
    ;; word size or finite operand representation for arbitrary integers.
    (return-from hir-inline-result-type 'unsigned-byte))
  (when (and (member name '(eq eql = /= < <= > >=)) (= (length types) 2))
    (return-from hir-inline-result-type
      (hir-integer-comparison-type name (first types) (second types))))
  (when (and (= (length types) 1) (member name '(zerop minusp plusp evenp oddp)))
    (return-from hir-inline-result-type (hir-integer-predicate-type name (first types))))
  (when (and (= (length types) 1) (member name '(consp simple-vector-p vectorp stringp symbolp integerp characterp null not)))
    (let* ((type (first types))
           (positive (case name (consp 'cons) (simple-vector-p 'simple-vector) (vectorp 'vector) (stringp 'string) (symbolp 'symbol) (integerp 'integer) (characterp 'character) (otherwise 'null)))
           ;; Ask the same target rules for the complement. In particular,
           ;; NIL is a symbol, and LIST cannot prove SYMBOLP false.
           (negative (list 'not positive)))
      (return-from hir-inline-result-type
        (cond ((hir-type-proves-p type positive) '(eql t))
              ((hir-type-proves-p type negative) 'null) (t 'boolean)))))
  (when (and (eq name 'cons) (= (length types) 2))
    (return-from hir-inline-result-type 'cons))
  (when (eq name 'list)
    (return-from hir-inline-result-type (if types 'cons 'null)))
  (when (and (eq name 'list*) types)
    (return-from hir-inline-result-type (if (cdr types) 'cons (first types))))
  (when (and (member name '(1+ 1-)) (= (length types) 1))
    (return-from hir-inline-result-type
      (hir-inline-result-type (if (eq name '1+) '+ '-) (list (first types) '(integer 1 1)))))
  (when (and (eq name 'lognot) (= (length types) 1))
    (multiple-value-bind (low high known) (hir-integer-bounds (first types))
      ;; Complement reverses integer order. Bound the work and retain the
      ;; same signed-i64 closure used by native bitwise lowering.
      (return-from hir-inline-result-type
        (if (and known (<= -9223372036854775808 low high 9223372036854775807))
            (progn (compiler-statistic :proof/integer-complement-range)
                   (list 'integer (lognot high) (lognot low))) t))))
  (when (and (eq name 'length) (= (length types) 1))
    (let ((type (first types)))
      (when (or (eq type 'simple-vector) (and (consp type) (eq (first type) 'simple-vector)))
        ;; A simple vector has no fill pointer. Its WasmGC backing array uses
        ;; a nonnegative signed-i32 length; sized vectors can be more precise.
        (let ((size (and (consp type) (second type))))
          (return-from hir-inline-result-type
            (if (typep size '(integer 0 2147483647)) (list 'integer size size) '(integer 0 2147483647)))))
      ;; General target vectors also have bounded storage and fill pointers.
      ;; This proves only the returned integer range, never a stable length:
      ;; callbacks can still change an adjustable vector or its fill pointer.
      (when (hir-type-proves-p type 'vector)
        (return-from hir-inline-result-type '(integer 0 2147483647)))))
  (when (= (length types) 2)
    (let ((identity (hir-integer-identity name (first types) (second types))))
      (when identity
        (return-from hir-inline-result-type (if (eq identity :left) (first types) (second types)))))
    (multiple-value-bind (a b integer-a) (hir-integer-bounds (first types))
      (multiple-value-bind (c d integer-b) (hir-integer-bounds (second types))
        ;; A finite mask bounds the result even when the other integer is a
        ;; bignum or has no declaration. These describe successful returns;
        ;; the original call still checks its operands and signals errors.
        (when (eq name 'logand)
          (let ((left (and integer-a (<= 0 a b 9223372036854775807) b))
                (right (and integer-b (<= 0 c d 9223372036854775807) d)))
            (when (or left right)
              (return-from hir-inline-result-type
                (list 'integer 0 (if (and left right) (min left right) (or left right)))))))
        ;; In two's complement, OR can only set bits: a negative operand
        ;; bounds the result between that operand and -1, at any other width.
        (when (eq name 'logior)
          (let ((left (and integer-a (<= -9223372036854775808 a b -1) a))
                (right (and integer-b (<= -9223372036854775808 c d -1) c)))
            (when (or left right)
              (return-from hir-inline-result-type
                (list 'integer (if (and left right) (max left right) (or left right)) -1)))))
        (when (and (eq name 'ash) integer-a integer-b (<= d 64)
                   (<= -9223372036854775808 a b 9223372036854775807))
          ;; Scalar arithmetic right shifts saturate at the sign bit, even
          ;; when a count bound is a bignum. ASH is monotone in its first
          ;; operand; either endpoint count supplies each extremum, depending
          ;; on that operand's sign. Bound positive shifts before computing.
          (let* ((first-count (max -63 c)) (last-count (max -63 d))
                 (low (min (ash a first-count) (ash a last-count)))
                 (high (max (ash b first-count) (ash b last-count))))
            (return-from hir-inline-result-type
              (if (<= -9223372036854775808 low high 9223372036854775807)
                  (progn (compiler-statistic :proof/integer-shift-result-range
                           (if (minusp d) :right :left))
                         (list 'integer low high)) t))))
        (when (and integer-a integer-b (<= -9223372036854775808 a b 9223372036854775807)
                   (<= -9223372036854775808 c d 9223372036854775807))
          (when (member name '(+ - *))
            (let ((low nil) (high nil))
              (case name
                (+ (setq low (+ a c) high (+ b d)))
                (- (setq low (- a d) high (- b c)))
                (* (let ((ac (* a c)) (ad (* a d)) (bc (* b c)) (bd (* b d)))
                     (setq low (min ac ad bc bd) high (max ac ad bc bd)))))
              ;; Leaving i64 is a loss of scalar information, never wrapping
              ;; arithmetic. The ordinary call still promotes to a bignum.
              (return-from hir-inline-result-type
                (if (<= -9223372036854775808 low high 9223372036854775807)
                    (list 'integer low high) t))))
          (let ((bits (max (integer-length a) (integer-length b) (integer-length c) (integer-length d))))
            (case name
              (logand
               (return-from hir-inline-result-type
                 (cond ((and (>= a 0) (>= c 0)) (list 'integer 0 (min b d)))
                       ((>= a 0) (list 'integer 0 b)) ((>= c 0) (list 'integer 0 d))
                       (t (list 'signed-byte (1+ bits))))))
              ((logior logxor)
               (return-from hir-inline-result-type
                 (if (and (>= a 0) (>= c 0)) (list 'integer 0 (1- (ash 1 bits)))
                     (list 'signed-byte (1+ bits)))))))))))
  ;; Stop expanding interval chains once they leave the scalar domain. This
  ;; keeps inference terminating for loops such as X := (ASH X 1).
  ;; Exact integer primitives still retain their coarse successful-return type;
  ;; this is not a finite-width proof and cannot enable unchecked scalar code.
  (if (and (= (length types) 2) (member name '(ash logand))) 'integer t))

(defun infer-hir-types (unit &key (roots nil rootsp) entries)
  "Return fresh primary-value type facts keyed by HIR values and functions.
NIL is the empty type and T is unknown. Value counts remain a separate analysis.
Captured cells and debug-enabled functions keep conservative inferred types."
  (declare (optimize (safety 3)))
  (verify-hir unit)
  (compiler-list-length roots "type-inference roots")
  (compiler-list-length entries "type-inference entry points")
  (dolist (entry entries)
    (unless (member entry (hir-unit-functions unit) :test miricl::%compiler-eq-function)
      (compiler-input-fail "type-inference entry point" entry "function does not belong to this HIR unit")))
  (dolist (root roots)
    (unless (and (%hir-value-p root) (member (hir-value-owner root) (hir-unit-functions unit) :test miricl::%compiler-eq-function)
                 (member root (hir-function-values (hir-value-owner root)) :test miricl::%compiler-eq-function))
      (compiler-input-fail "type-inference root" root "value does not belong to this HIR unit")))
  (let ((*hir-inline-definitions* nil))
    (values (infer-hir-types-checked unit roots rootsp entries))))

(defun infer-hir-types-checked (unit roots rootsp &optional entries proven-only callee-facts)
  "Internal inference kernel; UNIT and ROOTS have passed the public boundary.
Return primary type facts and executable branch premises as separate values."
  (declare (type hir-unit unit) (type list roots entries))
  ;; Explicitly requesting no facts needs no dependency or escape graph.
  ;; Many initialization units contain no operations using inferred types.
  (when (and rootsp (null roots))
    (return-from infer-hir-types-checked (make-hash-table :test 'eq)))
  (let* ((*hir-inline-definitions* (or *hir-inline-definitions* (make-hash-table :test 'equal)))
        (facts (make-hash-table :test 'eq)) (sources (make-hash-table :test 'eq))
        (users (make-hash-table :test 'eq)) (calls (make-hash-table :test 'eq))
        (references (make-hash-table :test 'eq)) (candidates (make-hash-table :test 'eq))
        (escaped (make-hash-table :test 'eq)) (sites (make-hash-table :test 'eq))
        (opaque (make-hash-table :test 'eq)) (queued (make-hash-table :test 'eq))
        (required (make-hash-table :test 'eq)) (growth (make-hash-table :test 'eq)) (work nil)
        (primitives *hir-inline-definitions*) (intrinsics (make-hash-table :test 'eq))
        (guards (hir-type-branch-guards unit))
        (callees (or callee-facts (hir-result-callees unit (hir-optimization-barriers unit)))))
    (declare (type hash-table facts sources users calls references candidates escaped
                   sites opaque queued required growth guards)
             (type list work))
    (labels ((edge (source destination &optional (flow t))
               (when (and source destination)
                 (when flow (push source (gethash destination sources)))
                 (push destination (gethash source users))))
             (enqueue (value)
               (when (and (or (not rootsp) (gethash value required)) (not (gethash value queued)))
                 (setf (gethash value queued) t) (push value work)))
             (joined (value)
               (let ((type nil))
                 (dolist (input (gethash value sources) type)
                   (setq type (hir-type-join type (gethash input facts)))))))
      ;; Parameter facts require a closed set of callers. A function passed as
      ;; a value to application code, returned, or stored in a cell must keep
      ;; its ordinary unrestricted entry parameters.
      ;; Code provenance alone is not a closed-use certificate: storing a
      ;; closure in a cell remains conservative even with LOCAL metadata.
      (dolist (function (hir-unit-functions unit))
        (dolist (value (hir-function-values function))
          (let ((target (hir-result-callee value callees)))
            (when target (setf (gethash value references) target (gethash target candidates) t)))))
      (dolist (function (hir-unit-functions unit))
        (dolist (block (hir-function-blocks function))
          (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
            (let ((inputs (hir-instruction-inputs instruction)) (opcode (hir-instruction-opcode instruction)))
              (when (eq opcode :call)
                (let ((target (gethash (second inputs) references)))
                  (when target (push inputs (gethash target sites)))))
              (loop for input in inputs for index from 0 do
                (let ((target (gethash input references)))
                  (when (and target
                         (not (and (eq opcode :call) (= index 1)))
                         (not (eq opcode :copy)))
                    (setf (gethash target escaped) t))))))))
      ;; Explicit Wasm exports admit callers outside this unit, even when a
      ;; function's only HIR references appear to be private direct calls.
      (dolist (entry entries) (setf (gethash entry escaped) t))
      (dolist (function (hir-unit-functions unit))
        (let ((parameters (hir-function-parameters function)) (callers (gethash function sites)))
          (unless (and (gethash function candidates) (not (gethash function escaped)) callers
                    (every (lambda (name) (and (symbolp name) (not (member name lambda-list-keywords)))) parameters)
                    (every (lambda (inputs) (= (length (cddr inputs)) (length parameters))) callers))
            (setf (gethash function escaped) t))))
      ;; Build dependencies once. A worklist revisits only users whose inputs
      ;; change; long acyclic expression chains do not need repeated sweeps.
      (dolist (function (hir-unit-functions unit))
        (enqueue function)
        (dolist (value (hir-function-values function))
          (enqueue value)
          (when (>= (hir-function-debug-level function) 2) (setf (gethash value opaque) t))
          (let ((producer (hir-value-producer value)))
            (when (%hir-instruction-p producer)
              (let ((inputs (hir-instruction-inputs producer)))
                (case (hir-instruction-opcode producer)
                  (:argument
                   (unless (gethash function escaped)
                     (dolist (inputs (gethash function sites))
                       (edge (nth (+ 2 (first (hir-instruction-attributes producer))) inputs) value))))
                  ((:primary :values :copy) (edge (first inputs) value))
                  (:value-ref
                   (let* ((group (first inputs)) (origin (hir-value-producer group))
                          (index (first (hir-instruction-attributes producer))))
                     (when (and (%hir-instruction-p origin) (eq (hir-instruction-opcode origin) :values))
                       (edge (nth index (hir-instruction-inputs origin)) value))))
                  (:cell-new
                   (let ((binding (first (hir-instruction-attributes producer))))
                     (if (hir-binding-captured-p binding) (setf (gethash value opaque) t)
                         (edge (first inputs) value))))
                  (:cell-read (edge (hir-type-cell (first inputs)) value))
                  (:cell-write
                   (edge (second inputs) value)
                   (edge (second inputs) (hir-type-cell (first inputs))))
                  (:call
                   (let ((target (hir-result-callee (second inputs) callees)))
                     (if target (progn (setf (gethash value calls) target) (edge target value))
                         (let ((primitive (hir-inline-primitive producer primitives)))
                           (when primitive
                             (setf (gethash value intrinsics) primitive)
                             (dolist (input (cddr inputs)) (edge input value))))))))))))
        (dolist (block (hir-function-blocks function))
          (let* ((terminator (hir-block-terminator block)) (inputs (hir-instruction-inputs terminator))
                 (opcode (hir-instruction-opcode terminator))
                 (target (first (hir-instruction-attributes terminator))))
            (case opcode
              (:return (edge (first inputs) function))
              ((:jump :block-return)
               (when (and (eq opcode :block-return) (null target))
                 (let ((producer (hir-value-producer (second inputs))))
                   (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :capture))
                     (let ((exit (second (hir-instruction-attributes producer))))
                       (when (%hir-target-p exit) (setq target (hir-target-block exit)))))))
               (when (and target (hir-block-parameters target))
                 (edge (if (eq opcode :jump) (first inputs) (third inputs))
                       (first (hir-block-parameters target)))))))))
      (maphash (lambda (value premises)
                (dolist (premise premises) (edge (car premise) value nil))) guards)
      (when rootsp
        (let ((pending roots) (needed nil))
          (loop while pending do
            (let ((value (pop pending)))
              (unless (gethash value required)
                (setf (gethash value required) t) (push value needed)
                (dolist (source (gethash value sources)) (push source pending))
                (dolist (premise (gethash value guards)) (push (car premise) pending)))))
          (dolist (value needed) (enqueue value))))
      (loop while work do
        (let* ((value (pop work)) (previous (gethash value facts))
               (function (%hir-function-p value))
               (producer (unless function (hir-value-producer value)))
               (derived
                 (cond ((gethash value opaque) t)
                       ((or function (%hir-block-p producer)) (joined value))
                       ((not (%hir-instruction-p producer)) t)
                       (t
                        (let ((inputs (hir-instruction-inputs producer)))
                          (case (hir-instruction-opcode producer)
                            (:constant (hir-constant-type (first (hir-instruction-attributes producer))))
                            (:argument (if (gethash (hir-value-owner value) escaped) t (joined value)))
                            (:values (if inputs (joined value) 'null))
                            ((:primary :copy :cell-new :cell-read :cell-write) (joined value))
                            (:value-ref
                             (let* ((group (first inputs)) (origin (hir-value-producer group))
                                    (index (first (hir-instruction-attributes producer))))
                               (if (and (%hir-instruction-p origin) (eq (hir-instruction-opcode origin) :values))
                                   (if (gethash value sources) (joined value) 'null)
                                   (if proven-only t (hir-value-type-at (hir-value-type group) index)))))
                            (:closure 'function)
                            (:call
                             (cond ((gethash value calls) (joined value))
                                   ((gethash value intrinsics)
                                    (hir-inline-result-type (gethash value intrinsics)
                                      (mapcar (lambda (input) (gethash input facts)) (cddr inputs))))
                                   (t (if proven-only t
                                          (hir-value-type-at (hir-function-result-type (hir-value-type (second inputs))) 0)))))
                            (otherwise t))))))
               ;; Guard elimination needs facts established by executable
               ;; producers and closed data flow, not unchecked declarations
               ;; or FTYPE promises about outside callers and callees.
               (refined (if (or function proven-only) derived
                            (hir-type-restrict derived (hir-value-type value))))
               (next
                 (progn
                   (dolist (premise (gethash value guards))
                     (setq refined (hir-type-restrict refined
                       (case (cdr premise)
                         ((t) (hir-type-without-null (gethash (car premise) facts)))
                         ((nil) 'null)
                         (otherwise
                          (if (and (consp (cdr premise))
                                   (member (cadr premise) '(zerop minusp plusp)))
                              (let ((type (gethash (car premise) facts t)))
                                ;; A scalar operand guarantees the backend
                                ;; selects the canonical predicate. A wider
                                ;; fallback can still call a replaced global
                                ;; definition, so it supplies no branch premise.
                                (multiple-value-bind (low high bounded) (hir-integer-bounds type)
                                  (if (and bounded (<= -9223372036854775808 low high 9223372036854775807))
                                      (hir-integer-branch-type type (cadr premise) (cddr premise))
                                      type)))
                              (cdr premise)))))))
                   (hir-type-join previous refined))))
          (remhash value queued)
          (unless (hir-type-equal-p previous next)
            ;; Backedges can grow an interval one integer at a time. At
            ;; repeatedly changing multi-input nodes, widen to the source
            ;; assertion (or unknown) after four strict range expansions.
            ;; Initial facts retain precise intervals; source declarations
            ;; remain usable when repeated joins require widening.
            (when (and previous (cdr (gethash value sources)))
              (multiple-value-bind (old-low old-high old-integer) (hir-integer-bounds previous)
                (multiple-value-bind (low high integer) (hir-integer-bounds next)
                  (when (and old-integer integer (or (< low old-low) (> high old-high)))
                    (let ((count (1+ (gethash value growth 0))))
                      (setf (gethash value growth) count)
                      (when (>= count 4)
                        (compiler-statistic :proof/interval-widening)
                        (setq next
                          (if (or function proven-only) t
                              (hir-type-restrict
                                ;; Repeated arithmetic growth can stabilize at
                                ;; declared operand bounds after its storage
                                ;; join widens. Use those owned assertions and
                                ;; literal facts, independent of this cycle's
                                ;; evolving inferred intervals. Unknown inputs
                                ;; still give the ordinary unknown result.
                                (if (and (%hir-instruction-p producer)
                                         (eq (hir-instruction-opcode producer) :call)
                                         (gethash value intrinsics))
                                    (hir-inline-result-type (gethash value intrinsics)
                                      (mapcar (lambda (input)
                                                (let ((origin (hir-value-producer input)))
                                                  (if (and (%hir-instruction-p origin)
                                                           (eq (hir-instruction-opcode origin) :constant))
                                                      (hir-constant-type (first (hir-instruction-attributes origin)))
                                                      (hir-value-type-at (hir-value-type input) 0))))
                                        (cddr (hir-instruction-inputs producer))))
                                    t)
                                (hir-value-type value))))))))))
            (setf (gethash value facts) next)
            (dolist (user (gethash value users)) (enqueue user)))))
      (values facts guards))))
