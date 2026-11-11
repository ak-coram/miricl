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

(defun hir-integer-bounds (type)
  ;; Only concrete bounded intervals participate in interval normalization.
  ;; Opaque types still remain useful declarations, without executing DEFTYPE
  ;; expanders or relying on a host's FIXNUM representation.
  (cond ((eq type 'bit) (values 0 1 t))
        ((eq type 'fixnum) (values -9223372036854775808 9223372036854775807 t))
        ((consp type)
         (case (car type)
           (integer (let ((low (second type)) (high (third type)))
                      (when (and (integerp low) (integerp high) (<= low high)) (values low high t))))
           (signed-byte (let ((bits (second type)))
                          (when (and (integerp bits) (<= 1 bits 64))
                            (let ((limit (ash 1 (1- bits)))) (values (- limit) (1- limit) t)))))
           (unsigned-byte (let ((bits (second type)))
                            (when (and (integerp bits) (<= 0 bits 64)) (values 0 (1- (ash 1 bits)) t))))))))

(defun hir-type-join (left right)
  (cond ((or (eq left t) (eq right t)) t)
        ((null left) right) ((null right) left) ((equal left right) left)
        (t
         (multiple-value-bind (low-a high-a integer-a) (hir-integer-bounds left)
           (multiple-value-bind (low-b high-b integer-b) (hir-integer-bounds right)
             (if (and integer-a integer-b)
                 (list 'integer (min low-a low-b) (max high-a high-b))
                 (let ((parts (if (and (consp left) (eq (car left) 'or)) (cdr left) (list left))))
                   (dolist (part (if (and (consp right) (eq (car right) 'or)) (cdr right) (list right)))
                     (unless (member part parts :test #'equal) (setq parts (append parts (list part)))))
                   (cons 'or parts))))))))

(defun hir-type-restrict (derived asserted)
  (setq asserted (hir-value-type-at asserted 0))
  (cond ((null derived) nil) ((eq asserted t) derived) ((eq derived t) asserted)
        ((equal derived asserted) asserted)
        (t
         (multiple-value-bind (low-a high-a integer-a) (hir-integer-bounds derived)
           (multiple-value-bind (low-b high-b integer-b) (hir-integer-bounds asserted)
             (if (and integer-a integer-b)
                 (let ((low (max low-a low-b)) (high (min high-a high-b)))
                   (and (<= low high) (list 'integer low high)))
                 ;; Retaining just the assertion is a sound upper bound of
                 ;; the intersection, and avoids growing recursive AND trees.
                 asserted))))))

(defun hir-constant-type (value)
  (cond ((integerp value) (list 'integer value value))
        ((null value) 'null) ((symbolp value) 'symbol)
        ((typep value 'simple-vector) 'simple-vector)
        ((characterp value) 'character) ((stringp value) 'string)
        ((floatp value) 'float) (t t)))

(defun hir-type-cell (value)
  (declare (type simple-vector value))
  (let ((producer (hir-value-producer value)))
    (when (%hir-instruction-p producer)
      (case (hir-instruction-opcode producer)
        (:cell-new value)
        (:capture (let ((binding (second (hir-instruction-attributes producer))))
                    (when (%hir-binding-p binding) (hir-binding-cell binding))))))))

(defun infer-hir-types (unit &key (roots nil rootsp))
  "Return fresh primary-value type facts keyed by HIR values and functions.
NIL is the empty type and T is unknown. Value counts remain a separate analysis.
Captured cells and debug-enabled functions keep conservative inferred types."
  (declare (type hir-unit unit) (type list roots))
  ;; Explicitly requesting no facts needs no dependency or escape graph.
  ;; Many initialization units contain no operations using inferred types.
  (when (and rootsp (null roots))
    (return-from infer-hir-types (make-hash-table :test 'eq)))
  (let ((facts (make-hash-table :test 'eq)) (sources (make-hash-table :test 'eq))
        (users (make-hash-table :test 'eq)) (calls (make-hash-table :test 'eq))
        (references (make-hash-table :test 'eq)) (candidates (make-hash-table :test 'eq))
        (escaped (make-hash-table :test 'eq)) (sites (make-hash-table :test 'eq))
        (opaque (make-hash-table :test 'eq)) (queued (make-hash-table :test 'eq))
        (required (make-hash-table :test 'eq)) (work nil))
    (declare (type hash-table facts sources users calls references candidates escaped
                   sites opaque queued required)
             (type list work))
    (labels ((edge (source destination)
               (when (and source destination)
                 (push source (gethash destination sources)) (push destination (gethash source users))))
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
                         (not (and (eq opcode :cell-write) (= index 1)
                           (let* ((cell (hir-type-cell (first inputs)))
                                  (producer (and cell (hir-value-producer cell)))
                                  (binding (and (%hir-instruction-p producer)
                                             (first (hir-instruction-attributes producer)))))
                             (and (%hir-binding-p binding) (hir-binding-local binding)
                                  (eq target (hir-local-function (hir-binding-local binding))))))))
                    (setf (gethash target escaped) t))))))))
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
                  ((:primary :values) (edge (first inputs) value))
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
                     (when target (setf (gethash value calls) target) (edge target value)))))))))
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
      (when rootsp
        (let ((pending roots) (needed nil))
          (loop while pending do
            (let ((value (pop pending)))
              (unless (gethash value required)
                (setf (gethash value required) t) (push value needed)
                (dolist (source (gethash value sources)) (push source pending)))))
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
                            ((:primary :cell-new :cell-read :cell-write) (joined value))
                            (:value-ref
                             (let* ((group (first inputs)) (origin (hir-value-producer group))
                                    (index (first (hir-instruction-attributes producer))))
                               (if (and (%hir-instruction-p origin) (eq (hir-instruction-opcode origin) :values))
                                   (if (gethash value sources) (joined value) 'null)
                                   (hir-value-type-at (hir-value-type group) index))))
                            (:closure 'function)
                            (:call
                             (if (gethash value calls) (joined value)
                                 (hir-value-type-at (hir-function-result-type (hir-value-type (second inputs))) 0)))
                            (otherwise t))))))
               (next (hir-type-join previous
                       (if function derived (hir-type-restrict derived (hir-value-type value))))))
          (remhash value queued)
          (unless (equal previous next)
            (setf (gethash value facts) next)
            (dolist (user (gethash value users)) (enqueue user)))))
      facts)))
