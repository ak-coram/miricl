;;;; Flow of primary-value types through HIR, independent of Wasm lowering.
(in-package :miricl-compiler)

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
             (unless (member b (gethash a seen) :test #'eq)
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
  (when (zerop depth) (return-from hir-integer-interval (values nil nil nil)))
  (when fuel
    (when (zerop (car fuel)) (return-from hir-integer-interval (values nil nil nil)))
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

(defun hir-type-proves-p (type required &optional (depth 32) fuel)
  "True only when CL type relationships establish TYPE is within REQUIRED.
NIL means unknown; this is deliberately not a complete SUBTYPEP procedure."
  (declare (type (integer 0 32) depth) (type list fuel))
  (when (zerop depth) (return-from hir-type-proves-p nil))
  (when (or (and (consp type) (member (car type) '(and or member eql)))
            (and (consp required) (member (car required) '(and or))))
    (unless fuel (setq fuel (list 128))))
  (when fuel
    (when (zerop (car fuel)) (return-from hir-type-proves-p nil))
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
        ((eq required 'simple-vector) (and (consp type) (eq (car type) 'simple-vector)))
        ((eq required 'list) (or (member type '(null cons)) (and (consp type) (eq (car type) 'cons))))
        ((eq required 'cons) (and (consp type) (eq (car type) 'cons)))
        ((equal required '(not null))
         (or (member type '(cons simple-vector vector array simple-array string simple-string
                            character number integer rational real float complex function compiled-function
                            hash-table package readtable keyword bit fixnum signed-byte unsigned-byte))
             (and (consp type)
                  (member (car type) '(cons simple-vector vector array simple-array string simple-string
                                      integer signed-byte unsigned-byte mod function)))))
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
  "Recover a canonical NULL/NOT test through immutable predicate temporaries.
CELLS records actual read-only cell uses, rather than client binding flags."
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
           (unless (and (= (length (hir-instruction-inputs producer)) 3)
                        (member (hir-inline-primitive producer cache) '(null not)))
             (return))
           (setq predicate t inverse (not inverse)
                 current (third (hir-instruction-inputs producer))))
          (:cell-read
           (when predicate (return-from hir-type-truth-premise (values current inverse)))
           (let* ((cell (first (hir-instruction-inputs producer)))
                  (origin (hir-value-producer cell)))
             (unless (and cells (gethash cell cells) (%hir-instruction-p origin)
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
  "Type premises established by truth tests of structurally read-only cells.
Joins are followed only after all predecessors establish the premise.
A budget limits cost; stopping early only loses optimization opportunities."
  (declare (type hir-unit unit))
  (let ((cells (make-hash-table :test 'eq)) (guards (make-hash-table :test 'eq))
        (barriers (make-hash-table :test 'eq))
        (primitives (make-hash-table :test 'eq))
        (size 0))
    (dolist (function (hir-unit-functions unit))
      (incf size (hir-function-value-count function))
      (dolist (value (hir-function-values function))
        (let ((producer (hir-value-producer value)))
          (when (and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :cell-new))
            (setf (gethash value cells) t)))))
    ;; Inspect actual uses, not just the client-visible assigned/captured
    ;; flags. Writes, aliases, capture and passing a cell to a call forbid
    ;; extending a test of its contents to subsequent reads.
    (dolist (function (hir-unit-functions unit))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
          (when (eq (hir-instruction-opcode instruction) :capture)
            (let ((target (second (hir-instruction-attributes instruction))))
              (when (%hir-target-p target) (setf (gethash (hir-target-block target) barriers) t))))
          ;; A verified CELL-READ has exactly one operand, its cell. Skip
          ;; those common uses altogether instead of hashing every read.
          (unless (eq (hir-instruction-opcode instruction) :cell-read)
            (dolist (input (hir-instruction-inputs instruction))
              (when (gethash input cells) (remhash input cells)))))))
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
                   (arms (hir-instruction-attributes branch)))
              (when (and (plusp budget) (< (hir-function-debug-level function) 2)
                         (eq (hir-instruction-opcode branch) :branch)
                         (or source (gethash cell cells)) (not (eq (first arms) (second arms))))
                (loop for arm in arms for positive in '(t nil) do
                  (incf serial)
                  ;; The initial edge must itself establish the premise.
                  ;; Subsequent joins need every predecessor inside this region.
                  (let ((pending (when (= (aref incoming (the (unsigned-byte 32) (hir-block-id arm))) 1)
                                   (list arm))))
                    (loop while (and pending (plusp budget)) do
                      (let* ((current (pop pending)) (id (hir-block-id current)))
                        (declare (type (unsigned-byte 32) id))
                        (decf budget)
                        (when (and (not (gethash current barriers))
                                   (not (eq current (hir-function-entry function)))
                                   (not (and (= (aref epochs id) serial) (= (aref remaining id) -1))))
                          (setf (aref epochs id) serial (aref remaining id) -1)
                          (dolist (instruction (hir-block-instructions current))
                            (when (minusp (decf budget)) (return))
                            (when (or (and cell (eq (hir-instruction-opcode instruction) :cell-read)
                                           (eq (first (hir-instruction-inputs instruction)) cell))
                                      (and source (eq (hir-instruction-opcode instruction) :copy)
                                           (eq (hir-copy-source (hir-instruction-output instruction)) source)))
                              (push (cons observed (if (second premise) (not positive) positive))
                                    (gethash (hir-instruction-output instruction) guards))))
                          (let ((terminator (hir-block-terminator current)) (previous nil))
                            (when (member (hir-instruction-opcode terminator) '(:jump :branch))
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
    (when (and (eq (second attributes) 'inline) (member name '(length cons consp simple-vector-p null not zerop minusp plusp evenp oddp 1+ 1- + - * logand logior logxor ash)))
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
      (when (and integer-a integer-b)
        (let ((zero-a (and (eql a 0) (eql b 0)))
              (zero-b (and (eql c 0) (eql d 0)))
              (one-a (and (eql a 1) (eql b 1)))
              (one-b (and (eql c 1) (eql d 1)))
              (minus-a (and (eql a -1) (eql b -1)))
              (minus-b (and (eql c -1) (eql d -1))))
          (case name
            ((+ logxor) (cond (zero-b :left) (zero-a :right)))
            (- (when zero-b :left))
            (* (cond (zero-a :left) (zero-b :right) (one-a :right) (one-b :left)))
            (logand (cond (zero-a :left) (zero-b :right) (minus-a :right) (minus-b :left)))
            (logior (cond (minus-a :left) (minus-b :right) (zero-b :left) (zero-a :right)))
            (ash (when (or zero-a zero-b) :left))))))))

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
      (case answer (:true '(eql t)) (:false 'null) (otherwise 'boolean)))))

(defun hir-inline-result-type (name types)
  (declare (type symbol name) (type list types))
  (when (member nil types) (return-from hir-inline-result-type nil))
  (when (and (= (length types) 1) (member name '(zerop minusp plusp evenp oddp)))
    (return-from hir-inline-result-type (hir-integer-predicate-type name (first types))))
  (when (and (= (length types) 1) (member name '(consp simple-vector-p null not)))
    (let* ((type (first types))
           (positive (case name (consp 'cons) (simple-vector-p 'simple-vector) (otherwise 'null)))
           (negative (if (member name '(null not)) '(not null) 'null)))
      (return-from hir-inline-result-type
        (cond ((hir-type-proves-p type positive) '(eql t))
              ((hir-type-proves-p type negative) 'null) (t 'boolean)))))
  (when (and (eq name 'cons) (= (length types) 2))
    (return-from hir-inline-result-type 'cons))
  (when (and (member name '(1+ 1-)) (= (length types) 1))
    (return-from hir-inline-result-type
      (hir-inline-result-type (if (eq name '1+) '+ '-) (list (first types) '(integer 1 1)))))
  (when (and (eq name 'length) (= (length types) 1))
    (let ((type (first types)))
      (when (or (eq type 'simple-vector) (and (consp type) (eq (first type) 'simple-vector)))
        ;; A simple vector has no fill pointer. Its WasmGC backing array uses
        ;; a nonnegative signed-i32 length; sized vectors can be more precise.
        (let ((size (and (consp type) (second type))))
          (return-from hir-inline-result-type
            (if (typep size '(integer 0 2147483647)) (list 'integer size size) '(integer 0 2147483647)))))))
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
                     (list 'signed-byte (1+ bits)))))
              (ash
               (when (and (= c d) (<= -64 c 64))
                 (let ((low (ash a c)) (high (ash b c)))
                   (when (<= -9223372036854775808 low high 9223372036854775807)
                     (return-from hir-inline-result-type (list 'integer low high))))))))))))
  ;; Stop expanding interval chains once they leave the scalar domain. This
  ;; keeps inference terminating for loops such as X := (ASH X 1).
  t)

(defun infer-hir-types (unit &key (roots nil rootsp) entries)
  "Return fresh primary-value type facts keyed by HIR values and functions.
NIL is the empty type and T is unknown. Value counts remain a separate analysis.
Captured cells and debug-enabled functions keep conservative inferred types."
  (declare (optimize (safety 3)))
  (verify-hir unit)
  (compiler-list-length roots "type-inference roots")
  (compiler-list-length entries "type-inference entry points")
  (dolist (entry entries)
    (unless (member entry (hir-unit-functions unit) :test #'eq)
      (compiler-input-fail "type-inference entry point" entry "function does not belong to this HIR unit")))
  (dolist (root roots)
    (unless (and (%hir-value-p root) (member (hir-value-owner root) (hir-unit-functions unit) :test #'eq)
                 (member root (hir-function-values (hir-value-owner root)) :test #'eq))
      (compiler-input-fail "type-inference root" root "value does not belong to this HIR unit")))
  (infer-hir-types-checked unit roots rootsp entries))

(defun infer-hir-types-checked (unit roots rootsp &optional entries)
  "Internal inference kernel; UNIT and ROOTS have passed the public boundary."
  (declare (type hir-unit unit) (type list roots entries))
  ;; Explicitly requesting no facts needs no dependency or escape graph.
  ;; Many initialization units contain no operations using inferred types.
  (when (and rootsp (null roots))
    (return-from infer-hir-types-checked (make-hash-table :test 'eq)))
  (let ((facts (make-hash-table :test 'eq)) (sources (make-hash-table :test 'eq))
        (users (make-hash-table :test 'eq)) (calls (make-hash-table :test 'eq))
        (references (make-hash-table :test 'eq)) (candidates (make-hash-table :test 'eq))
        (escaped (make-hash-table :test 'eq)) (sites (make-hash-table :test 'eq))
        (opaque (make-hash-table :test 'eq)) (queued (make-hash-table :test 'eq))
        (required (make-hash-table :test 'eq)) (growth (make-hash-table :test 'eq)) (work nil)
        (primitives (make-hash-table :test 'eq)) (intrinsics (make-hash-table :test 'eq))
        (guards (hir-type-branch-guards unit)))
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
      ;; a value to application code, returned, or stored outside its private
      ;; function cell must keep its ordinary unrestricted entry parameters.
      (dolist (function (hir-unit-functions unit))
        (dolist (value (hir-function-values function))
          (let ((target (native-known-callee value)))
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
                         (not (eq opcode :copy))
                         (not (and (eq opcode :cell-write) (= index 1)
                           (let* ((cell (hir-type-cell (first inputs)))
                                  (producer (and cell (hir-value-producer cell)))
                                  (binding (and (%hir-instruction-p producer)
                                             (first (hir-instruction-attributes producer)))))
                             (and (%hir-binding-p binding) (hir-binding-local binding)
                                  (eq target (hir-local-function (hir-binding-local binding))))))))
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
                   (let ((target (native-known-callee (second inputs))))
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
                                   (hir-value-type-at (hir-value-type group) index))))
                            (:closure 'function)
                            (:call
                             (cond ((gethash value calls) (joined value))
                                   ((gethash value intrinsics)
                                    (hir-inline-result-type (gethash value intrinsics)
                                      (mapcar (lambda (input) (gethash input facts)) (cddr inputs))))
                                   (t (hir-value-type-at (hir-function-result-type (hir-value-type (second inputs))) 0))))
                            (otherwise t))))))
               (refined (if function derived (hir-type-restrict derived (hir-value-type value))))
               (next
                 (progn
                   (dolist (premise (gethash value guards))
                     (setq refined (hir-type-restrict refined
                       (if (cdr premise) (hir-type-without-null (gethash (car premise) facts)) 'null))))
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
                        (setq next (if function t (hir-type-restrict t (hir-value-type value))))))))))
            (setf (gethash value facts) next)
            (dolist (user (gethash value users)) (enqueue user)))))
      facts)))
