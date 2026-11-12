;;;; Small semantic HIR passes. Facts belong to this invocation, never the IR.
(in-package :miricl-compiler)


(defun hir-forward-blocks (function)
  "A topological order of reachable blocks, or NIL for a cyclic graph."
  (declare (type simple-vector function))
  (let ((seen (make-array (hir-function-block-count function) :initial-element nil))
        (incoming (make-array (hir-function-block-count function) :initial-element 0))
        (work (list (hir-function-entry function))) (count 0) (order nil))
    (declare (type simple-vector seen incoming) (type list work order) (type (unsigned-byte 32) count))
    (loop while work do
      (let* ((block (pop work)) (id (hir-block-id block)))
        (declare (type simple-vector block) (type (unsigned-byte 32) id))
        (unless (svref seen id)
          (setf (svref seen id) t) (incf count)
          (dolist (successor (hir-instruction-attributes (hir-block-terminator block)))
            (let* ((next (hir-block-id successor)) (n (svref incoming next)))
              ;; Verified HIR has at most two outgoing edges per block.
              (declare (type (unsigned-byte 32) next) (type (unsigned-byte 33) n))
              (setf (svref incoming next) (1+ n)))
            (push successor work)))))
    (setq work (when (zerop (the (unsigned-byte 33)
                            (svref incoming (the (unsigned-byte 32) (hir-block-id (hir-function-entry function))))))
                 (list (hir-function-entry function))))
    (loop while work do
      (let ((block (pop work)))
        (declare (type simple-vector block))
        (push block order)
        (dolist (successor (reverse (hir-instruction-attributes (hir-block-terminator block))))
          (let* ((id (hir-block-id successor)) (n (svref incoming id)))
            (declare (type (unsigned-byte 32) id) (type (unsigned-byte 33) n))
            (let ((next (1- n)))
              (declare (type (unsigned-byte 33) next))
              (setf (svref incoming id) next)
              (when (zerop next) (push successor work)))))))
    (when (= (length order) count) (nreverse order))))

(defun hir-optimization-barriers (unit)
  ;; Derive capture/landing facts from the graph, not client-maintained flags.
  (let ((barriers (make-hash-table :test 'eq)))
    (dolist (function (hir-unit-functions unit))
      (dolist (identity (hir-function-captures function))
        (if (%hir-binding-p identity)
            (setf (gethash (hir-binding-cell identity) barriers) t)
            (setf (gethash (hir-target-owner identity) barriers) t)))
      (dolist (block (hir-function-blocks function))
        (dolist (instruction (hir-block-instructions block))
          (when (eq (hir-instruction-opcode instruction) :capture)
            (let ((identity (second (hir-instruction-attributes instruction))))
              (if (%hir-binding-p identity)
                  (setf (gethash (hir-binding-cell identity) barriers) t)
                  (setf (gethash (hir-target-owner identity) barriers) t)))))))
    barriers))

(defun hir-private-cells (function barriers)
  (let ((cells (make-hash-table :test 'eq)))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (hir-block-instructions block))
        (when (and (eq (hir-instruction-opcode instruction) :cell-new)
                   (eq (hir-value-kind (first (hir-instruction-inputs instruction))) :single)
                   (not (gethash (hir-instruction-output instruction) barriers)))
          (setf (gethash (hir-instruction-output instruction) cells) t))))
    (when (zerop (hash-table-count cells)) (return-from hir-private-cells cells))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (cons (hir-block-terminator block) (hir-block-instructions block)))
        (loop for input in (hir-instruction-inputs instruction) for position from 0 do
          (unless (and (zerop position)
                       (member (hir-instruction-opcode instruction) '(:cell-read :cell-write))
                       (hir-instruction-output instruction)
                       (eq (hir-value-kind (hir-instruction-output instruction)) :single)
                       (or (eq (hir-instruction-opcode instruction) :cell-read)
                           (eq (hir-value-kind (second (hir-instruction-inputs instruction))) :single)))
            (remhash input cells)))))
    cells))

(defun hir-copy-instruction (instruction value)
  (declare (type simple-vector instruction value))
  ;; Keep the result identity and its scoped assertion. Moving its declaration
  ;; onto VALUE would incorrectly extend that declaration's lexical scope.
  (setf (hir-instruction-opcode instruction) :copy
        (hir-instruction-inputs instruction) (list value)
        (hir-instruction-attributes instruction) nil
        (hir-instruction-effect instruction) :pure))

(defun hir-forward-private-cells (function cells)
  (declare (type simple-vector function) (type hash-table cells))
  (when (zerop (hash-table-count cells)) (return-from hir-forward-private-cells nil))
  ;; Promote reaching definitions to SSA values. A join needs no phi when
  ;; every predecessor supplies the identical value. Differing definitions
  ;; and unvisited loop backedges retain their cells.
  (let ((out (make-hash-table :test 'eq)) (immutable (make-hash-table :test 'eq))
        (written (make-hash-table :test 'eq)) (reads (make-hash-table :test 'eq))
        (fuel (min 65536 (* 8 (the (unsigned-byte 32) (hir-function-value-count function))))))
    (declare (type (integer 0 65536) fuel))
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (hir-block-instructions block))
        (when (eq (hir-instruction-opcode instruction) :cell-write)
          (setf (gethash (first (hir-instruction-inputs instruction)) written) t))))
    (maphash (lambda (cell private)
               (declare (ignore private))
               (unless (gethash cell written)
                 (setf (gethash cell immutable)
                   (first (hir-instruction-inputs (hir-value-producer cell)))))) cells)
    (dolist (block (hir-function-blocks function))
      (let* ((predecessors (hir-block-predecessors block))
             (state (car (gethash (first predecessors) out))))
        (when (cdr predecessors)
          (let ((incoming state))
            (setq state nil)
            (dolist (entry incoming)
              (when (zerop fuel) (return))
              (let ((same t))
                (dolist (previous (cdr predecessors))
                  (when (zerop fuel) (setq same nil) (return))
                  (decf fuel)
                  (let ((record (gethash previous out)))
                    (unless (and record (eq (cdr entry) (gethash (car entry) (cdr record))))
                      (setq same nil) (return))))
                (when same (push entry state))))))
        (dolist (instruction (hir-block-instructions block))
          (let* ((inputs (hir-instruction-inputs instruction))
                 (cell (first inputs)))
            (case (hir-instruction-opcode instruction)
              (:cell-new
               ;; Read-only initializers are already known in IMMUTABLE.
               ;; Only assigned cells need per-edge reaching definitions.
               (when (and (gethash (hir-instruction-output instruction) cells)
                          (gethash (hir-instruction-output instruction) written))
                 (push (cons (hir-instruction-output instruction) cell) state)))
              (:cell-write
               (when (gethash cell cells)
                 ;; Never mutate an inherited alist entry shared with a sibling.
                 (push (cons cell (second inputs)) state)))
              (:cell-read
               (when (gethash cell cells)
                 (let ((value (or (cdr (assoc cell state)) (gethash cell immutable))))
                   (if value (hir-copy-instruction instruction value)
                       (progn
                         (setf (gethash cell reads) t)
                         ;; An observed load is a reaching definition until a
                         ;; write. Private cells cannot change through aliases.
                         (push (cons cell (hir-instruction-output instruction)) state)))))))))
        ;; Terminal blocks cannot supply a successor's reaching definitions.
        (when (and state (hir-instruction-attributes (hir-block-terminator block)))
          (let ((seen (make-hash-table :test 'eq)) (definitions nil))
            (dolist (entry state)
              (unless (gethash (car entry) seen)
                (setf (gethash (car entry) seen) (cdr entry)) (push entry definitions)))
            ;; Keep only the last definition. Empty and unknown predecessors
            ;; both supply no facts; the table makes join lookups constant-time.
            (setf (gethash block out) (cons definitions seen))))))
    ;; A private cell with no remaining reads has no observable stores.
    ;; Keep the stored expression and SETQ's primary result, in their order.
    (dolist (block (hir-function-blocks function))
      (dolist (instruction (hir-block-instructions block))
        (when (and (eq (hir-instruction-opcode instruction) :cell-write)
                   (gethash (first (hir-instruction-inputs instruction)) cells)
                   (not (gethash (first (hir-instruction-inputs instruction)) reads)))
          (hir-copy-instruction instruction (second (hir-instruction-inputs instruction))))))))

(defun hir-constant-fact (instruction facts)
  (declare (type simple-vector instruction facts))
  ;; NIL is bottom (not evaluated yet), :UNKNOWN is top, and a cons contains
  ;; the known primary value, including NIL. Multiple-value cardinality is
  ;; deliberately not represented by this lattice.
  (let ((inputs (hir-instruction-inputs instruction)))
    (case (hir-instruction-opcode instruction)
      (:constant (hir-instruction-attributes instruction))
      ((:copy :primary) (svref facts (the (unsigned-byte 32) (hir-value-id (first inputs)))))
      (:cell-write (svref facts (the (unsigned-byte 32) (hir-value-id (second inputs)))))
      (:values (if inputs (svref facts (the (unsigned-byte 32) (hir-value-id (first inputs)))) (list nil)))
      (:truth
       (let ((fact (svref facts (the (unsigned-byte 32) (hir-value-id (first inputs))))))
         (if (consp fact) (list (not (null (car fact)))) fact)))
      (:value-ref
       (let* ((producer (hir-value-producer (first inputs)))
              (index (first (hir-instruction-attributes instruction))))
         (cond ((and (%hir-instruction-p producer) (eq (hir-instruction-opcode producer) :values))
                (let ((value (nth index (hir-instruction-inputs producer))))
                  (if value (svref facts (the (unsigned-byte 32) (hir-value-id value))) (list nil))))
               ((zerop index) (svref facts (the (unsigned-byte 32) (hir-value-id (first inputs)))))
               (t :unknown))))
      (otherwise :unknown))))

(defun hir-constant-join (left right)
  ;; EQ preserves object identity. Numbers and characters may be coalesced as
  ;; file constants, but this pass needs no such permission or host arithmetic.
  (cond ((null left) right) ((null right) left)
        ((and (consp left) (consp right) (eq (car left) (car right))) left)
        (t :unknown)))

(defun hir-update-constant (value fact facts)
  (declare (type simple-vector value facts))
  (let* ((id (hir-value-id value)) (old (svref facts id)) (new (hir-constant-join old fact)))
    (declare (type (unsigned-byte 32) id))
    (unless (or (eq old new) (and (consp old) (consp new) (eq (car old) (car new))))
      (setf (svref facts id) new) t)))

(defun hir-constant-edge (block successor edges live)
  (declare (type simple-vector block successor edges live))
  (let ((id (hir-block-id block)))
    (declare (type (unsigned-byte 32) id))
    (unless (member successor (svref edges id) :test #'eq)
      (push successor (svref edges id))
      (setf (svref live (the (unsigned-byte 32) (hir-block-id successor))) t) t)))

(defun hir-propagate-constants (function)
  (declare (type simple-vector function))
  (let* ((blocks (hir-function-blocks function))
         (forward (if (and (null (cdr blocks))
                           (null (hir-instruction-attributes (hir-block-terminator (first blocks)))))
                      blocks (hir-forward-blocks function)))
         (order (or forward blocks))
         (facts (make-array (hir-function-value-count function) :initial-element nil))
        (live (make-array (hir-function-block-count function) :initial-element nil))
        (edges (make-array (hir-function-block-count function) :initial-element nil)) (changed t)
        (fuel (min 65536 (* 8 (the (unsigned-byte 32) (hir-function-value-count function))))))
    (declare (type (integer -1 65536) fuel) (type simple-vector facts live edges))
    (setf (svref live (the (unsigned-byte 32) (hir-block-id (hir-function-entry function)))) t)
    ;; Bounded fixed point. If incomplete, apply none of its provisional facts.
      (loop while changed do
        (setq changed nil)
        (dolist (block order)
          (when (svref live (the (unsigned-byte 32) (hir-block-id block)))
            (when (minusp (decf fuel)) (return-from hir-propagate-constants nil))
            (when (hir-block-parameters block)
              (let ((fact nil))
                (dolist (predecessor (hir-block-predecessors block))
                  (when (member block (svref edges (the (unsigned-byte 32) (hir-block-id predecessor))) :test #'eq)
                    (let* ((end (hir-block-terminator predecessor))
                           (inputs (hir-instruction-inputs end)))
                      (setq fact (hir-constant-join fact
                        (if (member (hir-instruction-opcode end) '(:jump :block-return))
                            (let ((value (car (last inputs))))
                              (if value (svref facts (the (unsigned-byte 32) (hir-value-id value))) :unknown)) :unknown))))))
                (when (hir-update-constant (first (hir-block-parameters block)) fact facts)
                  (setq changed t))))
            (dolist (instruction (hir-block-instructions block))
              (when (minusp (decf fuel)) (return-from hir-propagate-constants nil))
              (when (and (hir-instruction-output instruction)
                         (not (eq (svref facts (the (unsigned-byte 32) (hir-value-id (hir-instruction-output instruction)))) :unknown)))
                (when (hir-update-constant (hir-instruction-output instruction)
                        (hir-constant-fact instruction facts) facts)
                  (setq changed t))))
            (let* ((end (hir-block-terminator block))
                   (successors (hir-instruction-attributes end)))
              (if (eq (hir-instruction-opcode end) :branch)
                  (let ((fact (svref facts (the (unsigned-byte 32) (hir-value-id (first (hir-instruction-inputs end)))))))
                    (dolist (successor (cond ((eq fact :unknown) successors)
                                            ((consp fact) (list (if (car fact) (first successors) (second successors))))))
                      (when (hir-constant-edge block successor edges live) (setq changed t))))
                  (dolist (successor successors)
                    (when (hir-constant-edge block successor edges live) (setq changed t)))))))
        ;; In a DAG, every executable predecessor and dominating definition
        ;; was solved before its consumer. No provisional fact needs a sweep.
        (when forward (setq changed nil)))
      (dolist (block (hir-function-blocks function))
        (when (svref live (the (unsigned-byte 32) (hir-block-id block)))
          (dolist (instruction (hir-block-instructions block))
            (let* ((output (hir-instruction-output instruction))
                   (fact (and output (svref facts (the (unsigned-byte 32) (hir-value-id output))))))
              (when (and (consp fact) (eq (hir-value-kind output) :single)
                         (member (hir-instruction-opcode instruction) '(:copy :primary :truth :value-ref)))
                (setf (hir-instruction-opcode instruction) :constant
                      (hir-instruction-inputs instruction) nil
                      (hir-instruction-attributes instruction) (list (car fact))
                      (hir-instruction-effect instruction) :pure))))
          (let* ((end (hir-block-terminator block))
                 (fact (and (eq (hir-instruction-opcode end) :branch)
                            (svref facts (the (unsigned-byte 32) (hir-value-id (first (hir-instruction-inputs end))))))))
            (when (consp fact)
              (setf (hir-instruction-attributes end)
                      (list (if (car fact) (first (hir-instruction-attributes end))
                                (second (hir-instruction-attributes end))))
                    (hir-instruction-opcode end) :jump
                    (hir-instruction-inputs end) nil)))))
    t))

(defun hir-simplify-cfg (function)
  (declare (type simple-vector function))
  ;; Forward empty blocks without crossing dynamic environments. Retain block
  ;; identities and metadata; native reachability omits unused code later.
  (let ((fuel (min 65536 (* 4 (the (unsigned-byte 32) (hir-function-block-count function))))))
   (declare (type (integer 0 65536) fuel))
  (dolist (block (hir-function-blocks function))
    (let ((end (hir-block-terminator block)))
      (setf (hir-instruction-attributes end)
        (mapcar (lambda (target)
          (let ((next target))
            (loop while (and (plusp fuel) (null (hir-block-parameters next))
                             (null (hir-block-instructions next))
                             (eq (hir-block-environment next) (hir-block-environment target))
                             (eq (hir-instruction-opcode (hir-block-terminator next)) :jump)
                             (null (hir-instruction-inputs (hir-block-terminator next)))) do
              (decf fuel)
              (setq next (first (hir-instruction-attributes (hir-block-terminator next)))))
            (if (plusp fuel) next target))) (hir-instruction-attributes end)))
      ;; Forwarding can make both arms identical. Preserve the test's effects
      ;; but remove the conditional edge; its fallthrough needs no label.
      (when (and (eq (hir-instruction-opcode end) :branch)
                 (eq (first (hir-instruction-attributes end)) (second (hir-instruction-attributes end))))
        (setf (hir-instruction-opcode end) :jump
              (hir-instruction-inputs end) nil
              (hir-instruction-attributes end) (list (first (hir-instruction-attributes end))))))))
  function)

(defun hir-refresh-predecessors (function)
  ;; Only edges changed: identities, allocation counts and ownership still
  ;; have their checked meanings. Rebuild the affected analysis in O(blocks).
  (declare (type simple-vector function))
  (dolist (block (hir-function-blocks function))
    (setf (hir-block-predecessors block) nil))
  (dolist (block (hir-function-blocks function))
    (let ((previous nil))
      (dolist (successor (hir-instruction-attributes (hir-block-terminator block)))
        (unless (eq successor previous)
          (push block (hir-block-predecessors successor)) (setq previous successor)))))
  function)

(defun optimize-hir-checked (unit)
  (declare (type hir-unit unit))
  (let ((barriers (hir-optimization-barriers unit)))
      (dolist (function (hir-unit-functions unit))
        (unless (or (>= (hir-function-debug-level function) 2) (gethash function barriers))
          (hir-forward-private-cells function (hir-private-cells function barriers))
          (hir-propagate-constants function)
          (hir-simplify-cfg function)
          (hir-refresh-predecessors function))))
  unit)

(defun optimize-hir (unit)
  "Destructively simplify validated HIR, preserving value and literal identities."
  (declare (optimize (safety 3)))
  (verify-hir unit)
  (verify-hir-code-operands unit)
  (verify-hir-code-dominance unit)
  (optimize-hir-checked unit)
  (verify-hir unit)
  unit)
