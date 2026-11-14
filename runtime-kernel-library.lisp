;;;; Ordinary CL runtime kernels, executable on the host and cross-compiled
;;;; for the target. Keep this first slice independent of bootstrap metadata.
(defpackage #:miricl-runtime
  (:use #:cl)
  (:export #:property-cell #:member-eq #:list-eq #:reverse-copy-onto #:reverse-mutate-onto
           #:last-bounded #:copy-before-tail #:copy-butlast-bounded #:truncate-butlast-bounded
           #:copy-proper-list #:copy-list-onto #:remove-first-eq #:copy-list-spine #:copy-alist-spine #:nthcdr-bounded
           #:tail-member-p #:ensure-list #:property-set-location #:property-link-cell #:reverse-list-copy
           #:make-list-bounded #:list-length-or-cycle #:join-list-spines
           #:vector-list-copy #:reverse-simple-vector-in-place #:fill-vector-from-list
           #:pair-lists-onto #:sequence-key #:reduce-pair #:ordering-predicate #:merge-list-runs
           #:require-list-value #:callable-value #:put-property-box #:remove-property-box
           #:byte-spec-component #:natural-count-value
           #:read-list-path #:write-list-path #:pop-argument-box #:bounded-index-value
           #:check-keyword-pairs #:list-cursor-buffer
           #:property-value #:nthcdr-general #:check-generic-arguments
           #:integer-gcd #:integer-lcm-list #:integer-square-root #:integer-bit-present-p #:integer-all-ones-p #:integer-power-of-two-p #:deposit-integer-field #:text-equal-p #:text-fold-equal-p #:text-designator #:ascii-white-p #:ascii-upper-code #:ascii-digit-value #:integer-power #:calendar-part-value #:calendar-days-before-year #:calendar-days-before-month
           #:if-expression #:expand-values-form #:expand-logical-form #:expand-conditional-form #:expand-selection-form #:function-block-forms #:split-function-documentation #:condition-operator-name-p
           #:allocate-like-vector #:reverse-vector-value #:fill-array-values #:slice-simple-vector #:vector-elements-equalp #:fill-sequence-values #:copy-sequence-values #:copy-subsequence-values #:concatenate-sequence-values #:character-bag-member-p #:trim-text-value #:generated-symbol-text #:copy-symbol-value #:format-index-value #:format-parameter-value #:format-comparison-value #:format-escape-value-p #:format-minimum-column #:flatten-value-groups #:standard-syntax-code
           #:checked-integer-value #:checked-float-value #:checked-readtable-value #:checked-hash-table-value #:checked-random-state-value #:checked-stream-value #:checked-character-value #:checked-bit-value #:checked-fill-pointer-value #:checked-bit-array-value #:calendar-zone-value #:packed-argument-value #:argument-value #:rest-argument-values #:check-private-arguments #:path-component-value-p #:path-directory-end-value #:path-name-end-value #:path-prefix-value-p #:hash-key-value-p #:loop-word-value-p #:spread-call-arguments #:integer-interval-bound-value
           #:proper-list-count #:check-arity-count #:check-zero-arity #:check-one-arity #:check-two-arity #:check-three-arity #:sequence-index-value #:native-list-tail-value #:arrays-same-shape-p #:array-contents-count #:array-subscript-value #:complex-component-type #:cons-type-components #:printer-limit-value #:pretty-tab-distance #:compiled-byte-header-p #:text-position-column #:read-ordering-buffer #:merge-ordering-buffers #:loop-syntax-equal-p #:copy-loop-syntax #:tree-value-equal-p #:path-directory-value-p #:format-small-integer))
(in-package #:miricl-runtime)

(export '(find-sequence-value))

(defun find-sequence-value (item sequence function state)
  "Find one element in a previously validated sequence region.
The native checked boundary supplies STATE = #(key first stop flags), with
mode in bits 0..1, TEST-NOT in bit 2, reverse traversal in bit 3 and position
result in bit 4. Bounds and callable definitions are validated before entry."
  (declare (type simple-vector state) (type function function)
           (optimize (safety 0) (debug 0))
           (inline svref (setf svref) car cdr checked-path-car checked-path-cdr
                   vectorp row-major-aref = < 1+ 1- - logand zerop not))
  (values
   (block result
     (let* ((key (svref state 0)) (first (svref state 1)) (stop (svref state 2))
            (flags (svref state 3)) (mode (logand flags 3))
            (reverse (not (zerop (logand flags 8))))
            (indexed (vectorp sequence)) (remaining (- stop first))
            (index first) (cursor sequence) (buffer nil) (offset 0))
       (declare (type (integer 0 #x7fffffff) first stop remaining index offset)
                (type (integer 0 31) flags) (type (integer 0 2) mode))
       (if (= remaining 0) (return-from result nil))
       (if indexed nil
           (progn
             (tagbody skip
                (if (= offset first) (go skipped))
                (setq cursor (checked-path-cdr cursor)
                      offset (the (integer 0 #x7fffffff) (1+ offset)))
                (go skip)
               skipped)
             (if reverse
                 (progn
                   (setq buffer (make-array remaining) offset 0)
                   (let ((scan cursor))
                     (tagbody save
                        (if (= offset remaining) (go saved))
                        (setf (svref (the simple-vector buffer) offset) scan)
                        (setq scan (checked-path-cdr scan)
                              offset (the (integer 0 #x7fffffff) (1+ offset)))
                        (go save)
                       saved))))))
       (if reverse (setq index (the (integer 0 #x7fffffff) (1- stop))))
       (tagbody next
          (if (= remaining 0) (return-from result nil))
          (let* ((value (if indexed (row-major-aref sequence index)
                            (checked-path-car
                             (if reverse (svref (the simple-vector buffer) (- index first)) cursor))))
                 (keyed (if key (funcall key value) value))
                 (match (if (= mode 0) (funcall function item keyed) (funcall function keyed))))
            (if (= mode 0)
                (if (zerop (logand flags 4)) nil (setq match (not match)))
                (if (= mode 2) (setq match (not match))))
            (if match
                (return-from result (if (zerop (logand flags 16)) value index))))
          ;; Keep each visited cons check live after arbitrary callback effects.
          (if indexed nil (if reverse nil (setq cursor (checked-path-cdr cursor))))
          (setq remaining (the (integer 0 #x7fffffff) (1- remaining)))
          (if (= remaining 0) (return-from result nil))
          (setq index (the (integer 0 #x7fffffff) (if reverse (1- index) (1+ index))))
          (go next))))))

;; Keep checked error paths separate so the host does not diagnose a known
;; non-list CAR/CDR at compile time. Closed-module compilation can inline them.
(defun checked-car (object)
  (declare (optimize (safety 3)) (inline car))
  (car object))

(defun checked-cdr (object)
  (declare (optimize (safety 3)) (inline cdr))
  (cdr object))

;; Save source for SBCL's local INLINE declarations in the migrated access
;; kernels, without changing the existing error-only spine helpers above.
(declaim (inline checked-path-car checked-path-cdr))
(defun checked-path-car (object)
  (declare (optimize (safety 3)) (inline car))
  (car object))
(defun checked-path-cdr (object)
  (declare (optimize (safety 3)) (inline cdr))
  (cdr object))

;; Path codes are private, positive bit strings supplied by the accessor
;; dispatcher. The low bit selects CAR/CDR; the high bit terminates the path.
(defun read-list-path (object code)
  (declare (type (integer 2 #x7fffffff) code)
           (optimize (safety 0) (debug 0)) (inline checked-path-car checked-path-cdr <= ash logand zerop))
  (values (block done
    (tagbody next
      (if (<= code 3)
          (return-from done (if (zerop (logand code 1)) (checked-path-car object) (checked-path-cdr object))))
      (setq object (if (zerop (logand code 1)) (checked-path-car object) (checked-path-cdr object))
            code (ash code -1))
      (go next)))))

(defun write-list-path (object code value)
  (declare (type (integer 2 #x7fffffff) code)
           (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr consp rplaca rplacd <= ash logand zerop))
  (values (block done
    (tagbody next
      (if (<= code 3) (go store))
      (setq object (if (zerop (logand code 1)) (checked-path-car object) (checked-path-cdr object))
            code (ash code -1))
      (go next)
     store
      (if (consp object) nil (error 'type-error :datum object :expected-type 'cons))
      (if (zerop (logand code 1)) (rplaca (the cons object) value) (rplacd (the cons object) value))
      (return-from done value)))))

(defun pop-argument-box (box required)
  "Pop the caller-owned argument head only after validating the next spine."
  (declare (type cons box) (optimize (safety 0) (debug 0))
           (inline car rplaca checked-path-car checked-path-cdr))
  (values (let ((items (car box)))
    (if items
        (let ((tail (checked-path-cdr items)))
          (rplaca box tail)
          (checked-path-car items))
        (if required (error 'program-error) nil)))))

(defun bounded-index-value (object limit)
  (declare (type (integer 0 #x7fffffff) limit)
           (optimize (safety 0) (debug 0)) (inline integerp < > list))
  (values (if (integerp object)
      (if (< object 0) (error 'type-error :datum object :expected-type (list 'integer 0 limit))
          (if (> object limit) (error 'type-error :datum object :expected-type (list 'integer 0 limit))
              object))
      (error 'type-error :datum object :expected-type (list 'integer 0 limit)))))

(defun check-keyword-pairs (arguments allowed allow)
  "Validate a proper argument spine, honoring the first allowance pair."
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr symbolp eq))
  (values (block done
    (let ((scan arguments) (seen nil) (unknown nil))
      (tagbody next
        (if scan nil (go finish))
        (let* ((key (checked-path-car scan)) (tail (checked-path-cdr scan)))
          (if tail nil (error 'program-error))
          (if (symbolp key) nil (error 'program-error))
          (if (eq key :allow-other-keys)
              (if seen nil
                  (progn (setq seen t) (if allow nil (setq allow (checked-path-car tail)))))
              (if allow nil
                  (let ((names allowed))
                    (tagbody search
                      (if names nil (progn (setq unknown t) (go found)))
                      (if (eq key (checked-path-car names)) (go found))
                      (setq names (checked-path-cdr names))
                      (go search)
                     found))))
          (setq scan (checked-path-cdr tail)))
        (go next)
       finish
        (if allow nil (if unknown (error 'program-error)))
        (return-from done nil))))))

(defun list-cursor-buffer (cursor count)
  "Record exactly COUNT successive cursors, sharing their original cons cells."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline checked-path-cdr = 1+ (setf svref)))
  (values (let ((buffer (make-array count)) (index 0))
    (declare (type simple-vector buffer) (type (integer 0 #x7fffffff) index))
    (tagbody next
      (if (= index count) (return-from list-cursor-buffer buffer))
      (setf (svref buffer index) cursor)
      (setq cursor (checked-path-cdr cursor) index (the (integer 0 #x7fffffff) (1+ index)))
      (go next)))))

(defun property-value (items indicator default)
  "Read the first property by identity; validate every visited pair."
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr consp car cdr eq))
  (values (block done
    (tagbody next
      (if items nil (return-from done default))
      (let ((cell (checked-path-cdr items)))
        (if (consp cell) nil (error 'type-error :datum cell :expected-type 'cons))
        (if (eq (checked-path-car items) indicator) (return-from done (car (the cons cell))))
        (setq items (cdr (the cons cell))))
      (go next)))))

(defun nthcdr-general (count items)
  "Apply checked CDR with a validated arbitrary-precision nonnegative count."
  (declare (type unsigned-byte count) (optimize (safety 0) (debug 0))
           (inline checked-path-cdr zerop 1-))
  (values (block done
    (tagbody next
      (if items nil (return-from done nil))
      (if (zerop count) (return-from done items))
      (setq items (checked-path-cdr items) count (1- count))
      (go next)))))

(defun check-generic-arguments (parameters arguments)
  "Check required/optional counts; REST or KEY permits additional arguments."
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr eq length 1+ < >))
  (values (block done
    (let ((minimum 0) (maximum 0) (optional nil) (unbounded nil) (count (length arguments)))
      (declare (type (integer 0 #x7fffffff) minimum maximum count))
      (tagbody next
        (if parameters nil (go check))
        (let ((name (checked-path-car parameters)))
          (if (eq name '&rest) (progn (setq unbounded t) (go check)))
          (if (eq name '&key) (progn (setq unbounded t) (go check)))
          (if (eq name '&aux) (go check))
          (if (eq name '&optional) (setq optional t)
              (progn
                (if optional nil (setq minimum (the (integer 0 #x7fffffff) (1+ minimum))))
                (setq maximum (the (integer 0 #x7fffffff) (1+ maximum))))))
        (setq parameters (checked-path-cdr parameters))
        (go next)
       check
        (if (< count minimum) (error 'program-error))
        (if unbounded nil (if (> count maximum) (error 'program-error)))
        (return-from done nil))))))

(defun copy-proper-list (items)
  "Copy a private proper spine, sharing its elements and checking dotted tails."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr consp cons rplacd))
  (values (block done
    (if items nil (return-from done nil))
    (if (consp items) nil (return-from done (checked-car items)))
    (let* ((result (cons (car items) nil)) (end result))
      (setq items (cdr items))
      (tagbody next
        (if items nil (return-from done result))
        (if (consp items) nil (return-from done (checked-car items)))
        (let ((cell (cons (car items) nil)))
          (rplacd (the cons end) cell)
          (setq end cell items (cdr items)))
        (go next))))))

(defun copy-list-onto (items tail)
  "Copy a private proper spine onto an arbitrary shared tail, without recursion."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr consp cons rplacd))
  (values (block done
    (if items nil (return-from done tail))
    (if (consp items) nil (return-from done (checked-car items)))
    (let* ((result (cons (car items) tail)) (end result))
      (setq items (cdr items))
      (tagbody next
        (if items nil (return-from done result))
        (if (consp items) nil (return-from done (checked-car items)))
        (let ((cell (cons (car items) tail)))
          (rplacd (the cons end) cell)
          (setq end cell items (cdr items)))
        (go next))))))

(defun require-list-value (object)
  (declare (optimize (safety 0) (debug 0)) (inline consp))
  (values (if object
              (if (consp object) object (error 'type-error :datum object :expected-type 'list))
              object)))

(defun callable-value (object)
  (declare (optimize (safety 0) (debug 0)) (inline symbolp functionp fdefinition))
  (values (let ((function (if (symbolp object) (fdefinition object) object)))
            (if (functionp function) function
                (error 'type-error :datum function :expected-type 'function)))))

(defun natural-count-value (object limit)
  "Validate a nonnegative integer and saturate at the caller's bounded limit."
  (declare (type (integer 0 #x7fffffff) limit) (optimize (safety 0) (debug 0))
           (inline integerp minusp >))
  (values (if (integerp object)
              (if (minusp object) (error 'type-error :datum object :expected-type 'unsigned-byte)
                  (if (> object limit) limit object))
              (error 'type-error :datum object :expected-type 'unsigned-byte))))

(defun byte-spec-component (spec position-p)
  "Validate both components in order and return the original size or position."
  (declare (optimize (safety 0) (debug 0)) (inline checked-car checked-cdr))
  (values (let* ((size (checked-car spec)) (position (checked-cdr spec)))
            (if (integerp size)
                (if (minusp size) (error 'type-error :datum size :expected-type 'unsigned-byte))
                (error 'type-error :datum size :expected-type 'unsigned-byte))
            (if (integerp position)
                (if (minusp position) (error 'type-error :datum position :expected-type 'unsigned-byte))
                (error 'type-error :datum position :expected-type 'unsigned-byte))
            (if position-p position size))))

(defun pair-lists-onto (keys data alist)
  "Pair equal-length proper lists in their input order, sharing ALIST."
  (declare (optimize (safety 0) (debug 0)) (inline consp car cdr cons rplacd))
  (values (block done
    (let ((head nil) (tail nil))
      (tagbody next
        (if keys nil (if data nil (return-from done (if tail head alist))))
        (if (consp keys) nil (error 'type-error :datum keys :expected-type 'cons))
        (if (consp data) nil (error 'type-error :datum data :expected-type 'cons))
        (let ((cell (cons (cons (car (the cons keys)) (car (the cons data))) alist)))
          (if tail (rplacd (the cons tail) cell) (setq head cell))
          (setq tail cell keys (cdr (the cons keys)) data (cdr (the cons data))))
        (go next))))))

(defun sequence-key (key value)
  (declare (optimize (safety 0) (debug 0)))
  (values (if key (funcall key value) value)))

(defun put-property-box (box key value)
  "Update the first property or prepend it through the caller-owned head cell."
  (declare (type cons box) (optimize (safety 0) (debug 0))
           (inline car cdr consp eq cons rplaca))
  (values (block done
    (let ((items (car box)))
      (tagbody next
        (if items nil (go add))
        (if (consp items) nil (return-from done (checked-car items)))
        (let ((cell (cdr (the cons items))))
          (if (consp cell) nil (error 'type-error :datum cell :expected-type 'cons))
          (if (eq (car (the cons items)) key)
              (progn (rplaca (the cons cell) value) (return-from done value)))
          (setq items (cdr (the cons cell))))
        (go next)
       add
        (rplaca box (cons key (cons value (car box))))
        (return-from done value))))))

(defun remove-property-box (box key)
  "Unlink the first matching pair, retaining all other cons identities."
  (declare (type cons box) (optimize (safety 0) (debug 0))
           (inline car cdr consp eq rplaca rplacd))
  (values (block done
    (let ((items (car box)) (previous nil))
      (tagbody next
        (if items nil (return-from done nil))
        (if (consp items) nil (return-from done (checked-car items)))
        (let ((cell (cdr (the cons items))))
          (if (consp cell) nil (error 'type-error :datum cell :expected-type 'cons))
          (if (eq (car (the cons items)) key)
              (progn
                (if previous (rplacd (the cons previous) (cdr (the cons cell)))
                    (rplaca box (cdr (the cons cell))))
                (return-from done t)))
          (setq previous cell items (cdr (the cons cell))))
        (go next))))))

(defun reduce-pair (function first second)
  (declare (optimize (safety 0) (debug 0)))
  (values (funcall function first second)))

(defun ordering-predicate (first second key predicate)
  (declare (optimize (safety 0) (debug 0)))
  (values (let* ((left (if key (funcall key first) first))
                 (right (if key (funcall key second) second)))
            (if (funcall predicate left right) t nil))))

(defun merge-list-runs (left right key predicate)
  "Stably splice sorted list runs; equal elements take the left run first."
  (declare (optimize (safety 0) (debug 0))
           (inline checked-car checked-cdr rplacd))
  (values (block done
    (let ((head nil) (tail nil))
      (tagbody next
        (if left
            (if right nil
                (if tail (progn (rplacd (the cons tail) left) (return-from done head))
                    (return-from done left)))
            (if tail (progn (rplacd (the cons tail) right) (return-from done head))
                (return-from done right)))
        (let* ((right-value (checked-car right)) (left-value (checked-car left))
               (first (if key (funcall key right-value) right-value))
               (second (if key (funcall key left-value) left-value))
               (cell (if (funcall predicate first second)
                         (let ((cell right)) (setq right (checked-cdr right)) cell)
                         (let ((cell left)) (setq left (checked-cdr left)) cell))))
          (if tail (rplacd (the cons tail) cell) (setq head cell))
          (setq tail cell))
        (go next))))))

(defun remove-first-eq (item items)
  "Remove one identity match, copying its prefix and sharing its remaining tail.
This is the package tables' internal operation, rather than public REMOVE."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr consp eq cons rplacd))
  (values (block done
    (let ((result nil) (end nil))
      (tagbody next
        (if items nil (return-from done result))
        (if (consp items) nil (return-from done (checked-car items)))
        (if (eq item (car items))
            (let ((tail (cdr items)))
              (if end
                  (progn (rplacd (the cons end) tail) (return-from done result))
                  (return-from done tail))))
        (let ((cell (cons (car items) nil)))
          (if end (rplacd (the cons end) cell) (setq result cell))
          (setq end cell items (cdr items)))
        (go next))))))

(defun copy-list-spine (items)
  "Copy a list's spine, sharing elements and any dotted terminator."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr consp cons rplacd))
  (values (block done
    (if items nil (return-from done nil))
    (if (consp items) nil (return-from done (checked-car items)))
    (let* ((result (cons (car items) nil)) (end result))
      (setq items (cdr items))
      (tagbody next
        (if (consp items) nil
            (progn (rplacd (the cons end) items) (return-from done result)))
        (let ((cell (cons (car items) nil)))
          (rplacd (the cons end) cell)
          (setq end cell items (cdr items)))
        (go next))))))

(declaim (inline copy-alist-entry))
(defun copy-alist-entry (entry)
  (declare (optimize (safety 0) (debug 0)) (inline car cdr consp cons))
  (values (if entry
              (if (consp entry) (cons (car entry) (cdr entry)) (checked-car entry))
              nil)))

(defun copy-alist-spine (items)
  "Copy the outer list and each non-NIL pair, sharing its key and value."
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr consp cons rplacd copy-alist-entry))
  (values (block done
    (let ((result nil) (end nil))
      (tagbody next
        (if items nil (return-from done result))
        (if (consp items) nil (return-from done (checked-car items)))
        (let ((cell (cons (copy-alist-entry (car items)) nil)))
          (if end (rplacd (the cons end) cell) (setq result cell))
          (setq end cell items (cdr items)))
        (go next))))))

(defun nthcdr-bounded (count items)
  "Return a shared tail after COUNT checked CDRs; the boundary validates COUNT."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline zerop 1- consp cdr))
  (values (block done
    (let ((remaining count))
      (declare (type (integer 0 #x7fffffff) remaining))
      (tagbody next
        (if items nil (return-from done nil))
        (if (zerop remaining) (return-from done items))
        (if (consp items) nil (return-from done (checked-cdr items)))
        (setq items (cdr (the cons items))
              remaining (the (integer 0 #x7fffffff) (1- remaining)))
        (go next))))))

(defun tail-member-p (items object)
  "Find a shared tail; the caller validates the initial list argument."
  (declare (optimize (safety 0) (debug 0)) (inline eq eql consp cdr))
  (values (block done
    (tagbody next
      (if (eq items object) (return-from done t))
      ;; Cons cells compare by identity. Only a dotted terminator can need
      ;; numeric/character EQL, so keep that operation outside the spine loop.
      (if (consp items) nil (return-from done (eql items object)))
      (setq items (cdr (the cons items)))
      (go next)))))

(defun ensure-list (object)
  "Keep existing spines and wrap other non-NIL objects once."
  (declare (optimize (safety 0) (debug 0)) (inline consp cons))
  (values (if object (if (consp object) object (cons object nil)) object)))

(defun join-list-spines (arguments copy-p)
  "APPEND/NCONC traversal over a private argument spine, sharing the last input."
  (declare (optimize (safety 0) (debug 0)) (inline consp car cdr cons rplacd))
  (values (block done
    (let ((result nil) (tail nil))
      (tagbody next
        (if arguments nil (return-from done result))
        (if (consp arguments) nil (return-from done (checked-car arguments)))
        (let ((items (car (the cons arguments))) (more (cdr (the cons arguments))))
          (if more nil
              (if tail
                  (progn (rplacd (the cons tail) items) (return-from done result))
                  (return-from done items)))
          (tagbody item
            (if items nil (go item-done))
            (if (consp items) nil (return-from done (checked-car items)))
            (let ((cell (if copy-p (cons (car (the cons items)) nil) items)))
              (if tail (rplacd (the cons tail) cell) (setq result cell))
              (setq tail cell items (cdr (the cons items))))
            ;; NCONC replaces an intermediate dotted terminator. APPEND must
            ;; instead diagnose that terminator on the next checked access.
            (if copy-p nil (if (consp items) nil (go item-done)))
            (go item)
           item-done)
          (setq arguments more))
        (go next))))))

(defun make-list-bounded (count initial-element)
  "Allocate a fresh proper spine; the boundary validates the bounded count."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline zerop 1- cons))
  (values (block done
    (let ((remaining count) (result nil))
      (declare (type (integer 0 #x7fffffff) remaining))
      (tagbody next
        (if (zerop remaining) (return-from done result))
        (setq result (cons initial-element result)
              remaining (the (integer 0 #x7fffffff) (1- remaining)))
        (go next))))))

(defun vector-list-copy (vector)
  "Copy a validated simple vector into a fresh list, sharing its elements."
  (declare (type simple-vector vector) (optimize (safety 0) (debug 0))
           (inline length zerop 1- svref cons))
  (values (block done
    (let ((index (length vector)) (result nil))
      (declare (type (integer 0 #x7fffffff) index))
      (tagbody next
        (if (zerop index) (return-from done result))
        (setq index (the (integer 0 #x7fffffff) (1- index))
              result (cons (svref vector index) result))
        (go next))))))

(defun fill-vector-from-list (vector items)
  "Fill a private vector from a list, repeating its last element when needed.
The reader's size-prefix checks and allocation boundary remain with the caller."
  (declare (type simple-vector vector) (optimize (safety 0) (debug 0))
           (inline length = 1+ consp car cdr (setf svref)))
  (values (block done
    (let ((index 0) (size (length vector)) (last nil))
      (declare (type (integer 0 #x7fffffff) index size))
      (tagbody next
        (if (= index size) (return-from done vector))
        (if items
            (if (consp items)
                (setq last (car (the cons items)) items (cdr (the cons items)))
                (return-from done (checked-car items))))
        (setf (svref vector index) last)
        (setq index (the (integer 0 #x7fffffff) (1+ index)))
        (go next))))))

(defun reverse-simple-vector-in-place (vector)
  "Swap a validated simple vector's elements in place and return that vector."
  (declare (type simple-vector vector) (optimize (safety 0) (debug 0))
           (inline length < 1+ 1- svref (setf svref)))
  (values (block done
    (let ((left 0) (right (length vector)))
      (declare (type (integer 0 #x7fffffff) left right))
      (tagbody next
        (if (< (1+ left) right) nil (return-from done vector))
        (setq right (the (integer 0 #x7fffffff) (1- right)))
        (let ((value (svref vector left)))
          (setf (svref vector left) (svref vector right))
          (setf (svref vector right) value))
        (setq left (the (integer 0 #x7fffffff) (1+ left)))
        (go next))))))

(defun list-length-or-cycle (items)
  "Count a proper spine or return NIL for a cycle, checking every CDR."
  (declare (optimize (safety 0) (debug 0)) (inline consp cdr eq 1+))
  (values (block done
    (let ((fast items) (slow items) (count 0))
      ;; Count beyond 32 bits too. Even a 64-bit addressable heap cannot hold
      ;; 2^62 distinct cons cells; the arithmetic still fits a signed i64.
      (declare (type (integer 0 #x3fffffffffffffff) count))
      (tagbody next
        (if fast nil (return-from done count))
        (if (consp fast) nil (return-from done (checked-cdr fast)))
        (setq fast (cdr (the cons fast))
              count (the (integer 0 #x3fffffffffffffff) (1+ count)))
        (if fast nil (return-from done count))
        (if (consp fast) nil (return-from done (checked-cdr fast)))
        (setq fast (cdr (the cons fast))
              slow (cdr (the cons slow))
              count (the (integer 0 #x3fffffffffffffff) (1+ count)))
        (if (eq fast slow) (return-from done nil))
        (go next))))))

(defun property-cell (plist key)
  "Find KEY's value cons without copying; an odd final key may return NIL.
This internal lookup deliberately leaves keyword-list validation to its caller."
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr eq consp))
  (values (block found
   (tagbody
   next
     (if plist
         (if (consp plist)
             (if (eq (car (the cons plist)) key)
                 (return-from found (cdr (the cons plist))))
             ;; Preserve ordinary Lisp errors for malformed storage. SAFETY 0
             ;; applies only after the structural checks above and below.
             (return-from found (checked-car plist)))
         (return-from found nil))
     (let ((tail (cdr (the cons plist))))
       (if tail
           (if (consp tail)
               (setq plist (cdr (the cons tail)))
               (return-from found (checked-cdr tail)))
           (return-from found nil)))
     (go next)))))

(defun member-eq (item items)
  "Internal identity membership predicate; return one Lisp boolean."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr eq consp))
  (values (block found
    (tagbody
     next
       (if items
           (if (consp items)
               (if (eq item (car (the cons items))) (return-from found t))
               (return-from found (checked-car items)))
           (return-from found nil))
       (setq items (cdr (the cons items)))
       (go next)))))

(defun property-link-cell (previous indicator)
  "Find the value cons before a matching pair, malformed spine, or list end.
The checked caller supplies a value cons. All assignments preserve that type;
its CDR identifies the next pair for the caller to validate or relink."
  (declare (type cons previous) (optimize (safety 0) (debug 0)) (inline eq consp car cdr))
  (values (block done
    (tagbody next
      (let ((items (cdr previous)))
        (if (consp items) nil (return-from done previous))
        (let ((value (cdr (the cons items))))
          (if (consp value) nil (return-from done previous))
          (if (eq (car (the cons items)) indicator) (return-from done previous))
          (setq previous (the cons value))))
      (go next)))))

(defun property-set-location (items indicators)
  "Find a matching key cons, NIL, or the first malformed property-list spine.
The caller validates the returned pair before reading its value. An odd final
key cons stays distinct from NIL so it retains the ordinary type diagnosis."
  (declare (optimize (safety 0) (debug 0)) (inline eq consp car cdr member-eq))
  (values (block done
    (tagbody next
      (if items nil (return-from done nil))
      (if (consp items) nil (return-from done items))
      (let ((value (cdr (the cons items))))
        (if (consp value) nil (return-from done items))
        (if (member-eq (car (the cons items)) indicators) (return-from done items))
        (setq items (cdr (the cons value))))
      (go next)))))

(defun list-eq (left right)
  "Internal pairwise identity comparison, retaining checked list access."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr eq consp))
  (values (block done
    (tagbody
     next
       (if left
           (if (consp left) nil (return-from done (checked-car left)))
           (return-from done (if right nil t)))
       ;; The old primitive reads LEFT even when RIGHT is NIL. Keep that
       ;; validation and error order before taking the unequal-length exit.
       (let ((item (car (the cons left))))
         (if right
             (if (consp right) nil (return-from done (checked-car right)))
             (return-from done nil))
         (if (eq item (car (the cons right))) nil (return-from done nil)))
       (setq left (cdr (the cons left)) right (cdr (the cons right)))
       (go next)))))

(defun reverse-copy-onto (items tail)
  "Copy and reverse a proper list onto an arbitrary, shared tail."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr consp cons))
  (values (block done
    (tagbody
     next
       (if items
           (if (consp items) nil (return-from done (checked-cdr items)))
           (return-from done tail))
       (setq tail (cons (car (the cons items)) tail)
             items (cdr (the cons items)))
       (go next)))))

(defun reverse-list-copy (items)
  "Reverse a private proper spine, retaining checked errors and shared elements."
  (declare (optimize (safety 0) (debug 0)) (inline reverse-copy-onto))
  (values (if items (reverse-copy-onto items nil) nil)))

(defun reverse-mutate-onto (items tail)
  "Reverse a proper list by relinking its conses onto a shared tail."
  (declare (optimize (safety 0) (debug 0)) (inline cdr consp rplacd))
  (values (block done
    (tagbody
     next
       (if items
           (if (consp items) nil (return-from done (checked-cdr items)))
           (return-from done tail))
       (let ((rest (cdr (the cons items))))
         (rplacd (the cons items) tail)
         (setq tail items items rest))
       (go next)))))

(defun last-bounded (items count)
  "Find the last COUNT conses; the native boundary checks LIST and bounds COUNT."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline cdr consp zerop 1-))
  (values (block done
    (let ((ahead items))
      (tagbody
       advance
         (if (zerop count) (go scan))
         (if (consp ahead) nil (go scan))
         (setq ahead (cdr (the cons ahead))
               count (the (integer 0 #x7fffffff) (1- count)))
         (go advance)
       scan
         (if (consp ahead) nil (return-from done items))
         ;; AHEAD is COUNT conses ahead of ITEMS, so ITEMS is also a cons.
         (setq ahead (cdr (the cons ahead)) items (cdr (the cons items)))
         (go scan))))))

(defun copy-before-tail (items object)
  "Copy up to OBJECT, retaining a dotted terminator when OBJECT is absent.
The native boundary checks LIST before entering this sealed worker."
  ;; ITEMS can become an arbitrary dotted terminator after CDR. The entry
  ;; boundary proves LIST only on entry, not throughout this mutable binding.
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr consp eql cons rplacd))
  (values (block done
    (if (eql items object) (return-from done nil))
    (if items nil (return-from done nil))
    (let* ((result (cons (car (the cons items)) nil)) (end result))
      (setq items (cdr (the cons items)))
      (tagbody
       next
         (if (eql items object) (return-from done result))
         (if (consp items) nil
             (progn (rplacd (the cons end) items) (return-from done result)))
         (let ((cell (cons (car (the cons items)) nil)))
           (rplacd (the cons end) cell)
           (setq end cell items (cdr (the cons items))))
         (go next))))))

(declaim (inline advance-list-tail))
(defun advance-list-tail (items count)
  "Advance at most COUNT conses. The boundary checks COUNT before this worker."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline cdr consp zerop 1-))
  (values (block done
    (tagbody next
      (if (zerop count) (return-from done items))
      (if (consp items) nil (return-from done items))
      (setq items (cdr (the cons items)) count (the (integer 0 #x7fffffff) (1- count)))
      (go next)))))

(defun copy-butlast-bounded (items count)
  "Copy all but COUNT trailing conses, dropping any dotted terminator."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline advance-list-tail car cdr consp cons rplacd))
  (values (block done
    (let ((ahead (advance-list-tail items count)) (result nil) (end nil))
      (tagbody next
        (if (consp ahead) nil (return-from done result))
        ;; AHEAD is a tail of ITEMS: a cons ahead proves the cursor is a cons.
        (let ((cell (cons (car (the cons items)) nil)))
          (if end (rplacd (the cons end) cell) (setq result cell))
          (setq end cell items (cdr (the cons items)) ahead (cdr (the cons ahead))))
        (go next))))))

(defun truncate-butlast-bounded (items count)
  "Drop COUNT trailing conses by changing the last retained cons's CDR."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline advance-list-tail cdr consp rplacd))
  (values (block done
    (let ((ahead (advance-list-tail items count)) (scan items) (end nil))
      (tagbody next
        (if (consp ahead) nil (go finish))
        (setq end scan scan (cdr (the cons scan)) ahead (cdr (the cons ahead)))
        (go next)
       finish
        (if end
            (progn (rplacd (the cons end) nil) (return-from done items))
            (return-from done nil)))))))

(defun integer-gcd (left right)
  (declare (type integer left right) (optimize (safety 0) (debug 0)))
  (values (block done
    (setq left (abs left) right (abs right))
    (tagbody next
      (if (zerop right) (return-from done left))
      (let ((remainder (rem left right))) (setq left right right remainder))
      (go next)))))

(defun integer-lcm-list (arguments)
  (declare (optimize (safety 0) (debug 0)) (inline consp car cdr integerp))
  (values (block done
    (let ((result 1))
      (tagbody next
        (if arguments nil (return-from done result))
        (let ((value (checked-path-car arguments)))
          (if (integerp value) nil (error 'type-error :datum value :expected-type 'integer))
          (setq value (abs value))
          (if (zerop result) nil
              (setq result (if (zerop value) 0 (* (truncate result (gcd result value)) value)))))
        (setq arguments (checked-path-cdr arguments))
        (go next))))))

(defun integer-square-root (value)
  (declare (type unsigned-byte value) (optimize (safety 0) (debug 0)))
  (values (block done
    (if (< value 2) (return-from done value))
    (let ((root (ash 1 (ceiling (integer-length value) 2))))
      (tagbody next
        (let ((next (ash (+ root (truncate value root)) -1)))
          (if (>= next root) (return-from done root))
          (setq root next))
        (go next))))))

(defun integer-bit-present-p (index value)
  (declare (type unsigned-byte index) (type integer value)
           (optimize (safety 0) (debug 0)) (inline zerop logand ash -))
  (values (if (zerop (logand 1 (ash value (- index)))) nil t)))

(defun integer-all-ones-p (value)
  (declare (type integer value) (optimize (safety 0) (debug 0)))
  (values (let ((magnitude (abs value)))
    (zerop (logand magnitude (1+ magnitude))))))

(defun integer-power-of-two-p (value)
  (declare (type integer value) (optimize (safety 0) (debug 0)))
  (values (let ((magnitude (abs value)))
    (if (zerop magnitude) nil (zerop (logand magnitude (1- magnitude)))))))

(defun deposit-integer-field (new spec value aligned)
  (declare (type integer new value) (type cons spec) (optimize (safety 0) (debug 0)) (inline car cdr))
  (values (let ((size (car spec)) (position (cdr spec)))
    (if (zerop size) value
        (let* ((shifted (ash value (- position)))
               (replacement (if aligned (ash new (- position)) new))
               (delta (ldb (byte size 0) (logxor shifted replacement))))
          (if (zerop delta) value (logxor value (ash delta position))))))))

(defun text-equal-p (left right)
  (declare (type string left right) (optimize (safety 0) (debug 0))
           (inline eq length = 1+ char char-code))
  (values (block done
    (if (eq left right) (return-from done t))
    (let ((size (length left)) (index 0))
      (declare (type (integer 0 #x7fffffff) size index))
      (if (= size (length right)) nil (return-from done nil))
      (tagbody next
        (if (= index size) (return-from done t))
        (if (= (char-code (char left index)) (char-code (char right index))) nil (return-from done nil))
        (setq index (the (integer 0 #x7fffffff) (1+ index)))
        (go next))))))

(defun text-fold-equal-p (left right)
  (declare (type string left right) (optimize (safety 0) (debug 0))
           (inline eq length = 1+ char char-code))
  (values (block done
    (if (eq left right) (return-from done t))
    (let ((size (length left)) (index 0))
      (declare (type (integer 0 #x7fffffff) size index))
      (if (= size (length right)) nil (return-from done nil))
      (tagbody next
        (if (= index size) (return-from done t))
        (let ((a (char left index)) (b (char right index)))
          (if (= (char-code a) (char-code b)) nil
              (if (char-equal a b) nil
                  (return-from done nil))))
        (setq index (the (integer 0 #x7fffffff) (1+ index)))
        (go next))))))

(defun text-designator (object)
  (declare (optimize (safety 0) (debug 0)))
  (values (if (stringp object) object
      (if (characterp object) (make-array 1 :element-type 'character :initial-element object)
          (if (symbolp object) (symbol-name object)
              (error 'type-error :datum object :expected-type (list 'or 'string 'symbol 'character)))))))

(defun ascii-white-p (code)
  (declare (type (signed-byte 32) code) (optimize (safety 0) (debug 0)) (inline = >= <=))
  (values (if (= code 32) t (if (>= code 9) (<= code 13) nil))))

(defun ascii-upper-code (code)
  (declare (type (signed-byte 32) code) (optimize (safety 0) (debug 0)) (inline >= <= -))
  (values (if (>= code 97) (if (<= code 122) (- code 32) code) code)))

(defun ascii-digit-value (code)
  (declare (type (signed-byte 32) code) (optimize (safety 0) (debug 0)) (inline >= <= - ascii-upper-code))
  (values (block done
    (if (>= code 48) (if (<= code 57) (return-from done (- code 48))))
    (setq code (ascii-upper-code code))
    (if (>= code 65) (if (<= code 90) (return-from done (- code 55))))
    99)))

(defun integer-power (base exponent)
  (declare (type (integer 0 #x7fffffff) base exponent)
           (optimize (safety 0) (debug 0)) (inline zerop logand ash))
  (values (block done
    (let ((result 1) (power base))
      (tagbody next
        (if (zerop exponent) (return-from done result))
        (if (zerop (logand exponent 1)) nil (setq result (* result power)))
        (setq exponent (the (integer 0 #x7fffffff) (ash exponent -1)))
        (if (zerop exponent) nil (setq power (* power power)))
        (go next))))))

(defun calendar-part-value (value minimum maximum)
  (declare (type (integer 0 #x7fffffff) minimum maximum)
           (optimize (safety 0) (debug 0)))
  (values (if (integerp value)
      (if (< value minimum) (error 'type-error :datum value :expected-type (list 'integer minimum maximum))
          (if (> value maximum) (error 'type-error :datum value :expected-type (list 'integer minimum maximum)) value))
      (error 'type-error :datum value :expected-type (list 'integer minimum maximum)))))

(defun calendar-days-before-year (year)
  (declare (type (integer 1 #x7fffffff) year) (optimize (safety 0) (debug 0)) (inline 1- + - *))
  (values (let ((previous (1- year)))
    (+ (- (+ (* previous 365) (truncate previous 4)) (truncate previous 100)) (truncate previous 400)))))

(defun calendar-days-before-month (year month)
  (declare (type (integer 1 #x7fffffff) year) (type (integer 1 13) month)
           (optimize (safety 0) (debug 0)) (inline > = + - *))
  (values (+ (truncate (- (* 367 month) 362) 12)
             (if (> month 2)
                 (if (= 0 (mod year 4)) (if (= 0 (mod year 100)) (if (= 0 (mod year 400)) -1 -2) -1) -2)
                 0))))



(defun if-expression (test yes no)
  (declare (optimize (safety 0) (debug 0)) (inline list))
  (values (list 'if test yes no)))

(defun expand-values-form (form)
  (declare (optimize (safety 0) (debug 0)) (inline checked-path-car checked-path-cdr eq list))
  (values (let* ((arguments (checked-path-cdr form))
                 (nth-p (eq (checked-path-car form) 'nth-value))
                 (count (length arguments)))
    (if (= count (if nth-p 2 1)) nil (error 'program-error))
    (let ((expression (list 'multiple-value-call (list 'function 'list)
                       (if nth-p (checked-path-car (checked-path-cdr arguments)) (checked-path-car arguments)))))
      (if nth-p (list 'nth (checked-path-car arguments) expression) expression)))))

(defun expand-logical-form (form)
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr eq list if-expression))
  (values (block done
    (let* ((and-p (eq (checked-path-car form) 'and))
           (arguments (reverse (checked-path-cdr form))) (result (checked-path-car arguments)))
      (if arguments nil (return-from done and-p))
      (setq arguments (checked-path-cdr arguments))
      (tagbody next
        (if arguments nil (return-from done result))
        (let ((test (checked-path-car arguments)))
          (setq result (if and-p (if-expression test result nil)
                           (let ((temporary (gensym)))
                             (list 'let (list (list temporary test)) (if-expression temporary temporary result))))))
        (setq arguments (checked-path-cdr arguments))
        (go next))))))

(defun expand-conditional-form (form kind)
  (declare (type (integer 0 2) kind) (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr < zerop list if-expression))
  (values (block done
    (let ((arguments (checked-path-cdr form)))
      (if (< kind 2)
          (progn
            (if arguments nil (error 'program-error))
            (let ((body (cons 'progn (checked-path-cdr arguments))) (test (checked-path-car arguments)))
              (return-from done (if (zerop kind) (list 'if test body nil) (list 'if test nil body))))))
      (if arguments nil (return-from done nil))
      (let* ((clause (checked-path-car arguments)) (count (length clause))
             (test (checked-path-car clause)) (alternative (cons 'cond (checked-path-cdr arguments))))
        (if (zerop count) (error 'program-error))
        (if (checked-path-cdr clause)
            (return-from done (list 'if test (cons 'progn (checked-path-cdr clause)) alternative)))
        (let ((temporary (gensym)))
          (list 'let (list (list temporary test)) (list 'if temporary temporary alternative))))))))

(defun expand-selection-form (keyform clauses mode markers)
  (declare (type (integer 0 5) mode) (type cons markers)
           (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr consp car cdr eq zerop logand cons))
  (values (block done
    (let ((scan clauses))
      (tagbody next
        (if scan nil (return-from done (cons (car markers) (cons mode (cons keyform clauses)))))
        (let* ((clause (checked-path-car scan)) (count (length clause)) (keys (checked-path-car clause)))
          (if (zerop count) (error 'program-error))
          (if (zerop mode)
              (if (if (eq keys t) t (eq keys (cdr markers)))
                  (if (checked-path-cdr scan) (error 'program-error))))
          (if (zerop (logand mode 1)) (if (consp keys) (length keys))))
        (setq scan (checked-path-cdr scan))
        (go next))))))



(defun function-block-forms (name body)
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr consp car cdr cons eq list rplacd))
  (values (block done
    (let ((head nil) (tail nil))
      (tagbody next
        (let ((form (checked-path-car body)))
          (if (if (consp form) (eq (car form) 'declare) nil) nil (go finish)))
        (let ((cell (cons (checked-path-car body) nil)))
          (if tail (rplacd (the cons tail) cell) (setq head cell))
          (setq tail cell body (checked-path-cdr body)))
        (go next)
       finish
        (let ((result (list (cons 'block (cons name body)))))
          (if tail (progn (rplacd (the cons tail) result) (return-from done head))
              (return-from done result))))))))

(defun split-function-documentation (body)
  "Return (remaining-forms . documentation), preserving declaration identities."
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr consp car cdr cons eq rplacd))
  (values (block done
    (let ((scan body) (head nil) (tail nil))
      (tagbody declarations
        (let ((form (checked-path-car scan)))
          (if (if (consp form) (eq (car form) 'declare) nil) nil (go found)))
        (setq scan (checked-path-cdr scan))
        (go declarations)
       found
        (if (checked-path-cdr scan) nil (return-from done (cons body nil)))
        (if (stringp (checked-path-car scan)) nil (return-from done (cons body nil)))
       copy
        (if (eq body scan) (go finish))
        (let ((cell (cons (checked-path-car body) nil)))
          (if tail (rplacd (the cons tail) cell) (setq head cell))
          (setq tail cell body (checked-path-cdr body)))
        (go copy)
       finish
        (if tail (rplacd (the cons tail) (checked-path-cdr scan)) (setq head (checked-path-cdr scan)))
        (return-from done (cons head (checked-path-car scan))))))))

(defun condition-operator-name-p (name)
  (declare (optimize (safety 0) (debug 0)) (inline eq))
  (values (if (eq name 'define-condition) t
    (if (eq name 'handler-bind) t (if (eq name 'handler-case) t
    (if (eq name 'ignore-errors) t (if (eq name 'restart-bind) t
    (if (eq name 'restart-case) t (if (eq name 'with-simple-restart) t
    (if (eq name 'with-condition-restarts) t (if (eq name 'unwind-protect) t
    (if (eq name 'block) t (if (eq name 'return-from) t (if (eq name 'return) t
    (if (eq name 'catch) t (if (eq name 'throw) t (if (eq name 'assert) t
    (if (eq name 'check-type) t (if (eq name 'tagbody) t (eq name 'go))))))))))))))))))))

(defun allocate-like-vector (source size)
  (declare (type vector source) (type (integer 0 #x1ffffffe) size) (optimize (safety 0) (debug 0)))
  (values (make-array size :element-type (array-element-type source))))

(defun reverse-vector-value (source write)
  (declare (type vector source) (optimize (safety 0) (debug 0)) (inline length < ash 1+ 1-))
  (values (block done
    (let* ((size (length source)) (result (if write source (make-array size :element-type (array-element-type source))))
           (left 0) (right size))
      (declare (type (integer 0 #x7fffffff) size left right))
      (tagbody next
        (if (< left (if write (ash size -1) size)) nil (return-from done result))
        (setq right (the (integer 0 #x7fffffff) (1- right)))
        (let ((value (elt source left)))
          (if write (setf (elt result left) (elt source right)))
          (setf (elt result right) value))
        (setq left (the (integer 0 #x7fffffff) (1+ left)))
        (go next))))))

(defun fill-array-values (array first count value)
  (declare (type array array) (type (integer 0 #x7fffffff) first count)
           (optimize (safety 0) (debug 0)) (inline zerop 1+ 1- (setf row-major-aref)))
  (values (block done
    (tagbody next
      (if (zerop count) (return-from done nil))
      (setf (row-major-aref array first) value)
      (setq first (the (integer 0 #x7fffffff) (1+ first)) count (the (integer 0 #x7fffffff) (1- count)))
      (go next)))))

(defun slice-simple-vector (vector first count)
  (declare (type simple-vector vector) (type (integer 0 #x7fffffff) first count)
           (optimize (safety 0) (debug 0)) (inline svref (setf svref) = 1+))
  (values (block done
    (let ((result (make-array count)) (index 0))
      (declare (type simple-vector result) (type (integer 0 #x7fffffff) index))
      (tagbody next
        (if (= index count) (return-from done result))
        (setf (svref result index) (svref vector first))
        (setq index (the (integer 0 #x7fffffff) (1+ index)) first (the (integer 0 #x7fffffff) (1+ first)))
        (go next))))))

(defun vector-elements-equalp (left right)
  (declare (type vector left right) (optimize (safety 0) (debug 0)) (inline length = 1+))
  (values (block done
    (let ((size (length left)) (index 0))
      (declare (type (integer 0 #x7fffffff) size index))
      (if (= size (length right)) nil (return-from done nil))
      (tagbody next
        (if (= index size) (return-from done t))
        (if (equalp (elt left index) (elt right index)) nil (return-from done nil))
        (setq index (the (integer 0 #x7fffffff) (1+ index)))
        (go next))))))

(defun fill-sequence-values (sequence value start end)
  (declare (optimize (safety 0) (debug 0))
           (inline length consp car cdr rplaca = 1+))
  (values (block done
    (let* ((size (length sequence)) (stop end) (index start))
      (declare (type (integer 0 #x7fffffff) size stop index))
      (if (listp sequence)
          (let ((scan (nthcdr index sequence)))
            (tagbody next-list
              (if (= index stop) (return-from done sequence))
              (if (consp scan) nil (return-from done (checked-path-car scan)))
              (rplaca (the cons scan) value)
              (setq scan (cdr (the cons scan)) index (the (integer 0 #x7fffffff) (1+ index)))
              (go next-list)))
          (tagbody next-vector
            (if (= index stop) (return-from done sequence))
            (setf (elt sequence index) value)
            (setq index (the (integer 0 #x7fffffff) (1+ index)))
            (go next-vector)))))))

(defun copy-sequence-values (target source bounds)
  "Copy a validated storage region through a temporary, retaining overlap.
Public sequence callers validate logical lengths first. Array resizing also
uses this worker for physical storage beyond a vector's fill pointer."
  (declare (type cons bounds) (optimize (safety 0) (debug 0))
           (inline car cdr listp rplaca svref (setf svref) row-major-aref (setf row-major-aref) = 1+))
  (values (block done
    (let* ((first (car bounds)) (from (car (cdr bounds)))
           (count (car (cdr (cdr bounds))))
           (buffer (make-array count)) (index 0)
           (source-list-p (listp source)) (target-list-p (listp target))
           (source-cursor (if source-list-p (nthcdr from source) nil))
           (target-cursor (if target-list-p (nthcdr first target) nil)))
      (declare (type (integer 0 #x7fffffff) first from count index) (type simple-vector buffer))
      (tagbody read
        (if (= index count) (go write-start))
        (setf (svref buffer index) (if source-list-p (car (the cons source-cursor)) (row-major-aref source from)))
        (if source-list-p (setq source-cursor (cdr (the cons source-cursor))))
        (setq index (the (integer 0 #x7fffffff) (1+ index)) from (the (integer 0 #x7fffffff) (1+ from)))
        (go read)
       write-start
        (setq index 0)
       write
        (if (= index count) (return-from done nil))
        (if target-list-p
            (progn
              (rplaca (the cons target-cursor) (svref buffer index))
              (setq target-cursor (cdr (the cons target-cursor))))
            (setf (row-major-aref target first) (svref buffer index)))
        (setq index (the (integer 0 #x7fffffff) (1+ index)) first (the (integer 0 #x7fffffff) (1+ first)))
        (go write))))))



(defun copy-subsequence-values (sequence start end)
  (declare (optimize (safety 0) (debug 0))
           (inline length - bounded-index-value consp car cdr cons rplacd = 1+))
  (values (block done
    (let* ((size (length sequence)) (stop end) (first start) (count (- stop first)) (index 0))
      (declare (type (integer 0 #x7fffffff) size stop first count index))
      (if (listp sequence)
          (let ((scan (nthcdr first sequence)) (head nil) (tail nil))
            (tagbody next-list
              (if (= index count) (return-from done head))
              (if (consp scan) nil (return-from done (checked-path-car scan)))
              (let ((cell (cons (car (the cons scan)) nil)))
                (if tail (rplacd (the cons tail) cell) (setq head cell))
                (setq tail cell scan (cdr (the cons scan))))
              (setq index (the (integer 0 #x7fffffff) (1+ index)))
              (go next-list)))
          (let ((result (make-array count :element-type (array-element-type sequence))))
            (tagbody next-vector
              (if (= index count) (return-from done result))
              (setf (elt result index) (elt sequence first))
              (setq index (the (integer 0 #x7fffffff) (1+ index)) first (the (integer 0 #x7fffffff) (1+ first)))
              (go next-vector))))))))

(defun concatenate-sequence-values (sequences kind)
  (declare (type (integer 0 4) kind) (optimize (safety 0) (debug 0))
           (inline car cdr listp length = 1+ + > cons rplacd))
  (values (block done
    (let ((scan sequences) (size 0) (output 0) (head nil) (tail nil) (result nil))
      (declare (type (integer 0 #x1ffffffe) size output))
      (tagbody size
        (if scan nil (go allocate))
        (let ((next (+ size (length (car (the cons scan))))))
          (if (> next #x1ffffffe) (error 'storage-condition))
          (setq size (the (integer 0 #x1ffffffe) next) scan (cdr (the cons scan))))
        (go size)
       allocate
        (setq result (if (= kind 0) nil
                         (make-array size :element-type (if (= kind 1) 'character (if (= kind 3) 'bit (if (= kind 4) nil t)))))
              scan sequences)
       next
        (if scan nil (return-from done (if (= kind 0) head result)))
        (let* ((input (car (the cons scan))) (count (length input)) (index 0)
               (list-p (listp input)))
          (declare (type (integer 0 #x7fffffff) count index))
          (tagbody copy
            (if (= index count) (go copied))
            (let ((value (if list-p (car (the cons input)) (elt input index))))
              (if (= kind 0)
                  (let ((cell (cons value nil)))
                    (if tail (rplacd (the cons tail) cell) (setq head cell))
                    (setq tail cell))
                  (setf (elt result output) value)))
            (if list-p (setq input (cdr (the cons input))))
            (setq index (the (integer 0 #x7fffffff) (1+ index)) output (the (integer 0 #x1ffffffe) (1+ output)))
            (go copy)
           copied))
        (setq scan (cdr (the cons scan)))
        (go next))))))

(defun character-bag-member-p (bag character)
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr eql length = 1+))
  (values (block done
    (if (listp bag)
        (tagbody next-list
          (if bag nil (return-from done nil))
          (if (eql (checked-path-car bag) character) (return-from done t))
          (setq bag (checked-path-cdr bag))
          (go next-list))
        (let ((size (length bag)) (index 0))
          (declare (type (integer 0 #x7fffffff) size index))
          (tagbody next-vector
            (if (= index size) (return-from done nil))
            (if (eql (elt bag index) character) (return-from done t))
            (setq index (the (integer 0 #x7fffffff) (1+ index)))
            (go next-vector)))))))

(defun trim-text-value (bag string mode)
  (declare (type (integer 0 2) mode) (type string string) (optimize (safety 0) (debug 0))
           (inline length = /= 1+ 1-))
  (values (block done
    ;; Validate the complete bag, including characters beyond an early match.
    (let ((count (length bag)) (index 0) (start 0) (end (length string)) (right (= mode 2)))
      (declare (type (integer 0 #x7fffffff) count index start end))
      (tagbody validate
        (if (= index count) (go scan))
        (let ((item (elt bag index)))
          (if (characterp item) nil (error 'type-error :datum item :expected-type 'character)))
        (setq index (the (integer 0 #x7fffffff) (1+ index)))
        (go validate)
       scan
        (if (= start end) (go finish))
        (if (find (char string (if right (1- end) start)) bag)
            (if right (setq end (the (integer 0 #x7fffffff) (1- end)))
                (setq start (the (integer 0 #x7fffffff) (1+ start))))
            (if right (go finish) (if (= mode 1) (go finish) (setq right t))))
        (go scan)
       finish
        (return-from done (if (= start 0) (if (= end (length string)) string (subseq string start end))
                              (subseq string start end))))))))

(defun generated-symbol-text (prefix suffix)
  (declare (type string prefix) (type unsigned-byte suffix) (optimize (safety 0) (debug 0))
           (inline length cons car cdr zerop = < + 1+ mod truncate char (setf char) code-char make-array))
  (values (block done
    (let ((digits nil) (count 0) (index 0) (size (length prefix)))
      (declare (type (integer 0 #x7fffffff) count index size))
      ;; Keep ordinary counters in a finite integer range while preserving
      ;; arbitrary-size suffixes through the same decimal algorithm below.
      (if (< suffix #x80000000)
          (let ((small (the (integer 0 #x7fffffff) suffix)))
            (declare (type (integer 0 #x7fffffff) small))
            (tagbody digit
              (setq digits (cons (mod small 10) digits)
                    small (the (integer 0 #x7fffffff) (truncate small 10))
                    count (the (integer 0 #x7fffffff) (1+ count)))
              (if (zerop small) nil (go digit))))
          (tagbody digit
            (setq digits (cons (mod suffix 10) digits) suffix (truncate suffix 10)
                  count (the (integer 0 #x7fffffff) (1+ count)))
            (if (zerop suffix) nil (go digit))))
      (tagbody
        (let ((result (the simple-string (make-array (+ size count) :element-type 'character))))
          (tagbody prefix
            (if (= index size) (go suffix))
            (setf (char result index) (char prefix index))
            (setq index (the (integer 0 #x7fffffff) (1+ index)))
            (go prefix)
           suffix
            (if digits nil (return-from done result))
            (setf (char result index) (code-char (+ 48 (the (integer 0 9) (car (the cons digits))))))
            (setq digits (cdr (the cons digits)) index (the (integer 0 #x7fffffff) (1+ index)))
            (go suffix))))))))

(defun copy-symbol-value (original properties function)
  (declare (type symbol original) (optimize (safety 0) (debug 0)))
  (values (let ((copy (make-symbol (symbol-name original))))
    (if properties
        (progn
          (if (boundp original) (setf (symbol-value copy) (symbol-value original)))
          (if function (setf (symbol-function copy) function))
          (setf (symbol-plist copy) (copy-list (symbol-plist original)))))
    copy)))

(defun format-index-value (value)
  (declare (type integer value) (optimize (safety 0) (debug 0)))
  (values (if (> (integer-length (abs value)) 30) -1 value)))

(defun format-parameter-value (value default)
  (declare (type (signed-byte 32) default) (optimize (safety 0) (debug 0)))
  (values (if value (if (characterp value) (char-code value)
                        (if (integerp value) (if (> (integer-length (abs value)) 30) (error 'program-error) value)
                            (error 'type-error :datum value :expected-type 'integer)))
              default)))

(defun format-comparison-value (value)
  (declare (optimize (safety 0) (debug 0)))
  (values (if value (if (characterp value) (char-code value)
                        (if (integerp value) value (error 'type-error :datum value :expected-type 'integer)))
              nil)))

(defun format-escape-value-p (a b c remaining)
  (declare (optimize (safety 0) (debug 0)))
  (values (if a (if b (if c (if (<= a b) (<= b c) nil) (= a b)) (zerop a)) (null remaining))))

(defun format-minimum-column (value)
  (declare (optimize (safety 0) (debug 0)) (inline format-parameter-value))
  (values (if value (if (integerp value) (if (<= value 0) 0 (format-parameter-value value 0))
                        (format-parameter-value value 0))
              0)))

(defun flatten-value-groups (groups)
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr cons rplacd))
  (values (block done
    (let ((head nil) (tail nil))
      (tagbody group
        (if groups nil (return-from done head))
        (let ((items (checked-path-car groups)))
          (length items)
          (tagbody item
            (if items nil (go group-done))
            (let ((cell (cons (checked-path-car items) nil)))
              (if tail (rplacd (the cons tail) cell) (setq head cell))
              (setq tail cell items (checked-path-cdr items)))
            (go item)
           group-done))
        (setq groups (checked-path-cdr groups))
        (go group))))))

(defun standard-syntax-code (code)
  (declare (type (signed-byte 32) code) (optimize (safety 0) (debug 0))
           (inline = ascii-white-p))
  (values (if (ascii-white-p code) 1
              (if (= code 92) 2 (if (= code 124) 3
                  (if (= code 34) code (if (= code 35) code (if (= code 39) code
                  (if (= code 40) code (if (= code 41) code (if (= code 44) code
                  (if (= code 59) code (if (= code 96) code 0)))))))))))))

(defun checked-integer-value (object)
  (declare (optimize (safety 0) (debug 0)))
  (values (if (integerp object) object (error 'type-error :datum object :expected-type 'integer))))
(defun checked-float-value (object)
  (declare (optimize (safety 0) (debug 0)))
  (values (if (floatp object) object (error 'type-error :datum object :expected-type 'float))))
(defun checked-readtable-value (object)
  (declare (optimize (safety 0) (debug 0)))
  (values (if (readtablep object) object (error 'type-error :datum object :expected-type 'readtable))))
(defun checked-hash-table-value (object)
  (declare (optimize (safety 0) (debug 0)))
  (values (if (hash-table-p object) object (error 'type-error :datum object :expected-type 'hash-table))))
(defun checked-random-state-value (object)
  (declare (optimize (safety 0) (debug 0)))
  (values (if (random-state-p object) object (error 'type-error :datum object :expected-type 'random-state))))
(defun checked-stream-value (object)
  (declare (optimize (safety 0) (debug 0)))
  (values (if (streamp object) object (error 'type-error :datum object :expected-type 'stream))))
(defun checked-character-value (object)
  (declare (optimize (safety 0) (debug 0)) (inline characterp))
  (values (if (characterp object) object (error 'type-error :datum object :expected-type 'character))))
(defun checked-bit-value (object)
  (declare (optimize (safety 0) (debug 0)) (inline eql))
  (values (if (eql object 0) 0 (if (eql object 1) 1 (error 'type-error :datum object :expected-type 'bit)))))
(defun checked-fill-pointer-value (object)
  (declare (optimize (safety 0) (debug 0)))
  (values (if (vectorp object)
      (if (array-has-fill-pointer-p object) object
          (error 'type-error :datum object :expected-type (list 'and 'vector (list 'satisfies 'array-has-fill-pointer-p))))
      (error 'type-error :datum object :expected-type (list 'and 'vector (list 'satisfies 'array-has-fill-pointer-p))))))
(defun checked-bit-array-value (object simple)
  (declare (optimize (safety 0) (debug 0)) (inline eq))
  (values (block done
    (if (arrayp object)
        (if (eq (array-element-type object) 'bit)
            (if simple (if (typep object (list 'simple-array 'bit)) (return-from done object))
                (return-from done object))))
    (error 'type-error :datum object :expected-type (list (if simple 'simple-array 'array) 'bit)))))
(defun calendar-zone-value (zone)
  (declare (optimize (safety 0) (debug 0)))
  (values (block done
    (if (rationalp zone)
        (if (<= -24 zone) (if (<= zone 24)
            (let ((seconds (* zone 3600))) (if (integerp seconds) (return-from done seconds))))))
    (error 'type-error :datum zone :expected-type (list 'rational -24 24)))))

(defun packed-argument-value (arguments index)
  (declare (type simple-vector arguments) (type (signed-byte 32) index)
           (optimize (safety 0) (debug 0)) (inline length < >= svref))
  (values (if (< index 0) nil (if (>= index (length arguments)) nil (svref arguments index)))))
(defun argument-value (arguments index)
  (declare (type (integer 0 #x7fffffff) index) (optimize (safety 0) (debug 0))
           (inline simple-vector-p length >= svref nthcdr-bounded checked-path-car))
  (values (if (simple-vector-p arguments) (if (>= index (length arguments)) nil (svref (the simple-vector arguments) index))
              (checked-path-car (nthcdr-bounded index arguments)))))
(defun rest-argument-values (arguments index)
  (declare (type (integer 0 #x7fffffff) index) (optimize (safety 0) (debug 0))
           (inline simple-vector-p length <= 1- svref cons nthcdr-bounded))
  (values (block done
    (if (simple-vector-p arguments)
        (let ((count (length arguments)) (result nil))
          (declare (type (integer 0 #x7fffffff) count))
          (tagbody next
            (if (<= count index) (return-from done result))
            (setq count (the (integer 0 #x7fffffff) (1- count)) result (cons (svref arguments count) result))
            (go next)))
        (nthcdr-bounded index arguments)))))
(defun check-private-arguments (arguments minimum maximum)
  "Check the private call packet, retaining PROGRAM-ERROR on the host."
  (declare (type (signed-byte 32) minimum maximum) (optimize (safety 0) (debug 0))
           (inline private-arguments-valid-p))
  (values (if (private-arguments-valid-p arguments minimum maximum) nil
              (error 'program-error))))

(export '(private-arguments-valid-p))
(defun private-arguments-valid-p (arguments minimum maximum)
  "Validate the existing private packet convention without choosing a reporter.
The native boundary translates a false result into its checked arity condition."
  (declare (type (signed-byte 32) minimum maximum) (optimize (safety 0) (debug 0))
           (inline simple-vector-p length < > = 1+ consp cdr))
  (values (block done
    (if (< minimum 0) (return-from done nil))
    (if (if (< maximum 0) (simple-vector-p arguments) t)
        (let ((size (length arguments)))
          (if (< size minimum) (return-from done nil))
          (if (< maximum 0) nil (if (> size maximum) (return-from done nil)))
          (return-from done t)))
    (let ((index 0) (scan arguments))
      (declare (type (integer 0 #x7fffffff) index))
      (tagbody next
        (if (= index minimum) (return-from done t))
        (if (consp scan) nil (return-from done nil))
        (setq scan (cdr (the cons scan)) index (the (integer 0 #x7fffffff) (1+ index)))
        (go next))))))

(defun path-component-value-p (text pattern bounds)
  (declare (type string text pattern) (type cons bounds) (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr = < >= 1+))
  (values (block done
    (let* ((index (checked-path-car bounds)) (tail (checked-path-cdr bounds))
           (end (checked-path-car tail)) (tail2 (checked-path-cdr tail))
           (position (checked-path-car tail2)) (stop (checked-path-car (checked-path-cdr tail2)))
           (star -1) (retry 0))
      (declare (type (integer 0 #x7fffffff) index end position stop retry)
               (type (integer -1 #x7fffffff) star))
      (tagbody match
        (if (>= index end)
            (tagbody stars
              (if (>= position stop) (return-from done (= position stop)))
              (if (= (char-code (char pattern position)) 42) nil (return-from done nil))
              (setq position (the (integer 0 #x7fffffff) (1+ position)))
              (go stars)))
        (if (< position stop)
            (let ((code (char-code (char pattern position))))
              (if (= code 42)
                  (progn (setq star position retry index position (the (integer 0 #x7fffffff) (1+ position))) (go match)))
              (if (= code (char-code (char text index)))
                  (progn (setq position (the (integer 0 #x7fffffff) (1+ position)) index (the (integer 0 #x7fffffff) (1+ index))) (go match)))))
        (if (< star 0) (return-from done nil))
        (setq position (the (integer 0 #x7fffffff) (1+ star)) retry (the (integer 0 #x7fffffff) (1+ retry)) index retry)
        (go match))))))
(defun path-directory-end-value (text)
  (declare (type string text) (optimize (safety 0) (debug 0)) (inline length = 1+))
  (values (block done
    (let ((index 0) (end 0) (size (length text)))
      (declare (type (integer 0 #x7fffffff) index end size))
      (tagbody next
        (if (= index size) (return-from done end))
        (if (= (char-code (char text index)) 47) (setq end (the (integer 0 #x7fffffff) (1+ index))))
        (setq index (the (integer 0 #x7fffffff) (1+ index))) (go next))))))
(defun path-name-end-value (text index)
  (declare (type string text) (type (integer 0 #x7fffffff) index)
           (optimize (safety 0) (debug 0)) (inline length = >= 1+))
  (values (block done
    (let ((size (length text)) (end (length text)))
      (declare (type (integer 0 #x7fffffff) size end))
      (tagbody next
        (if (>= index size) (return-from done end))
        (if (= (char-code (char text index)) 46) (setq end index))
        (setq index (the (integer 0 #x7fffffff) (1+ index))) (go next))))))
(defun path-prefix-value-p (path candidate)
  (declare (type string path candidate) (optimize (safety 0) (debug 0)) (inline length <= = 1+))
  (values (block done
    (let ((size (length path)) (index 0))
      (declare (type (integer 0 #x7fffffff) size index))
      (if (<= (length candidate) size) (return-from done nil))
      (if (= (char-code (char candidate size)) 47) nil (return-from done nil))
      (tagbody next
        (if (= index size) (return-from done t))
        (if (= (char-code (char path index)) (char-code (char candidate index))) nil (return-from done nil))
        (setq index (the (integer 0 #x7fffffff) (1+ index))) (go next))))))
(defun hash-key-value-p (test key candidate)
  (declare (type symbol test) (optimize (safety 0) (debug 0)) (inline eq eql))
  (values (if (eq key candidate) t
              (if (eq test 'eq) nil (if (eq test 'eql) (eql key candidate)
                  (if (eq test 'equal) (equal key candidate) (equalp key candidate)))))))
(defun loop-word-value-p (word text)
  (declare (type string text) (optimize (safety 0) (debug 0)))
  (values (if (symbolp word) (string= (symbol-name word) text) nil)))
(defun spread-call-arguments (arguments)
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr consp car cdr cons rplacd eq length <))
  (values (block done
    (if (< (length arguments) 2) (error 'program-error))
    (let ((scan (checked-path-cdr arguments)) (head nil) (tail nil))
      (tagbody next
        (if (checked-path-cdr scan) nil (go last))
        (let ((cell (cons (checked-path-car scan) nil)))
          (if tail (rplacd (the cons tail) cell) (setq head cell))
          (setq tail cell scan (checked-path-cdr scan)))
        (go next)
       last
        (let ((remaining (checked-path-car scan)))
          (let ((fast remaining) (slow remaining) (phase nil))
            (tagbody validate
              (if fast nil (go valid))
              (if (consp fast) nil (return-from done (checked-cdr fast)))
              (setq fast (cdr (the cons fast)))
              (if phase
                  (progn (setq slow (checked-path-cdr slow))
                    (if (if fast (eq fast slow) nil)
                        (error 'type-error :datum remaining :expected-type (list 'satisfies 'list-length)))))
              (setq phase (not phase))
              (go validate)
             valid))
          (if tail (progn (rplacd (the cons tail) remaining) (return-from done head))
              (return-from done remaining))))))))
(defun integer-interval-bound-value (bound lower)
  (declare (optimize (safety 0) (debug 0))
           (inline consp car cdr eq list))
  (values (block done
    (if (eq bound '*) (return-from done nil))
    (let ((exclusive nil))
      (if (consp bound)
          (progn (if (= (length bound) 1) nil (error 'program-error))
                 (setq bound (car bound) exclusive t)))
      (let ((number (rational bound)))
        (if (integerp number)
            (if exclusive (if lower (1+ number) (1- number)) number)
            (if lower (ceiling number) (floor number))))))))

(defun proper-list-count (items)
  (declare (optimize (safety 0) (debug 0)) (inline consp cdr eq 1+ list))
  (values (let ((count (block done
    (let ((fast items) (slow items) (count 0))
      ;; Count beyond 32 bits too. Even a 64-bit addressable heap cannot hold
      ;; 2^62 distinct cons cells; the arithmetic still fits a signed i64.
      (declare (type (integer 0 #x3fffffffffffffff) count))
      (tagbody next
        (if fast nil (return-from done count))
        (if (consp fast) nil (return-from done (checked-cdr fast)))
        (setq fast (cdr (the cons fast))
              count (the (integer 0 #x3fffffffffffffff) (1+ count)))
        (if fast nil (return-from done count))
        (if (consp fast) nil (return-from done (checked-cdr fast)))
        (setq fast (cdr (the cons fast))
              slow (cdr (the cons slow))
              count (the (integer 0 #x3fffffffffffffff) (1+ count)))
        (if (eq fast slow) (return-from done nil))
        (go next))))))
    (if count count (error 'type-error :datum items :expected-type (list 'satisfies 'list-length))))))
(defun check-arity-count (count minimum maximum)
  (declare (type (integer 0 #x7fffffff) count minimum maximum)
           (optimize (safety 0) (debug 0)) (inline < >))
  (values (if (< count minimum) (error 'program-error)
              (if (> count maximum) (error 'program-error) nil))))
(defun check-zero-arity (count)
  (declare (type (integer 0 #x7fffffff) count) (optimize (safety 0) (debug 0)) (inline zerop))
  (values (if (zerop count) nil (error 'program-error))))
(defun check-one-arity (count)
  (declare (type (integer 0 #x7fffffff) count) (optimize (safety 0) (debug 0)) (inline =))
  (values (if (= count 1) nil (error 'program-error))))
(defun check-two-arity (count)
  (declare (type (integer 0 #x7fffffff) count) (optimize (safety 0) (debug 0)) (inline =))
  (values (if (= count 2) nil (error 'program-error))))
(defun check-three-arity (count)
  (declare (type (integer 0 #x7fffffff) count) (optimize (safety 0) (debug 0)) (inline =))
  (values (if (= count 3) nil (error 'program-error))))
(defun sequence-index-value (value)
  (declare (optimize (safety 0) (debug 0)) (inline integerp < > list))
  (values (if (integerp value)
      (if (< value 0) (error 'type-error :datum value :expected-type 'unsigned-byte)
          (if (> value #x3fffffff) (error 'type-error :datum value :expected-type (list 'integer 0 #x3fffffff)) value))
      (error 'type-error :datum value :expected-type 'integer))))
(defun native-list-tail-value (arguments index)
  (declare (type (integer 0 #x7fffffff) index) (optimize (safety 0) (debug 0)) (inline nthcdr-bounded))
  (values (nthcdr-bounded index arguments)))
(defun arrays-same-shape-p (left right)
  (declare (type array left right) (optimize (safety 0) (debug 0)) (inline = 1+))
  (values (block done
    (let ((rank (array-rank left)) (index 0))
      (declare (type (integer 0 255) rank index))
      (if (= rank (array-rank right)) nil (return-from done nil))
      (tagbody next
        (if (= index rank) (return-from done t))
        (if (= (array-dimension left index) (array-dimension right index)) nil (return-from done nil))
        (setq index (the (integer 0 255) (1+ index))) (go next))))))
(defun array-contents-count (contents reader)
  (declare (optimize (safety 0) (debug 0))
           (inline consp cdr 1+))
  (values (block done
    (if reader nil (return-from done (length contents)))
    (if (vectorp contents) (return-from done (length contents)))
    (let ((count 0) (fast contents) (slow contents) (phase nil))
      (declare (type (integer 0 #x7fffffff) count))
      (tagbody next
        (if fast nil (return-from done count))
        (if (consp fast) nil (error 'program-error))
        (setq fast (cdr (the cons fast)) count (the (integer 0 #x7fffffff) (1+ count)))
        (if phase (progn (setq slow (checked-path-cdr slow))
                         (if (eq slow fast) (if fast (error 'program-error)))))
        (setq phase (not phase))
        (go next))))))
(defun array-subscript-value (array subscripts query)
  (declare (type array array) (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr = < >= + * 1+ list))
  (values (block done
    (let ((rank (array-rank array)) (axis 0) (index 0) (outside nil))
      (declare (type (integer 0 255) rank axis) (type (integer 0 #x7fffffff) index))
      (if (= rank (length subscripts)) nil (error 'program-error))
      (tagbody next
        (if (= axis rank) (return-from done (if outside -1 index)))
        (let ((dimension (array-dimension array axis)) (value (checked-path-car subscripts)))
          (if (integerp value) nil (error 'type-error :datum value :expected-type 'integer))
          (if (if (< value 0) t (>= value dimension))
              (progn (setq outside t)
                (if query nil (error 'type-error :datum value :expected-type (list 'integer 0 (list dimension)))))
              (if outside nil (setq index (the (integer 0 #x7fffffff) (+ (* index dimension) value))))))
        (setq subscripts (checked-path-cdr subscripts) axis (the (integer 0 255) (1+ axis)))
        (go next))))))
(defun complex-component-type (type)
  (declare (optimize (safety 0) (debug 0)) (inline consp car cdr eq))
  (values (if (consp type)
      (let ((arguments (cdr type)))
        (if arguments (if (eq (checked-path-car arguments) '*) 'real (checked-path-car arguments)) 'real))
      'real)))
(defun cons-type-components (type)
  "Return NIL for an unrecognized type, otherwise a fresh (CAR-type . CDR-type)."
  (declare (optimize (safety 0) (debug 0)) (inline consp car cdr eq cons checked-path-car checked-path-cdr))
  (values (block done
    (let ((arguments nil) (left t) (right t))
      (if (eq type 'cons) nil
          (if (consp type)
              (if (eq (car type) 'cons)
                  (progn (setq arguments (cdr type)) (if (> (length arguments) 2) (error 'program-error)))
                  (return-from done nil))
              (return-from done nil)))
      (if arguments
          (progn
            (if (eq (checked-path-car arguments) '*) nil (setq left (checked-path-car arguments)))
            (setq arguments (checked-path-cdr arguments))
            (if arguments (if (eq (checked-path-car arguments) '*) nil (setq right (checked-path-car arguments))))))
      (cons left right)))))
(defun printer-limit-value (readably value limit)
  (declare (type (integer 0 #x7fffffff) limit) (optimize (safety 0) (debug 0)) (inline integerp minusp >))
  (values (if readably limit
              (if value
                  (if (integerp value)
                      (if (minusp value) (error 'type-error :datum value :expected-type 'unsigned-byte)
                          (if (> value limit) limit value))
                      (error 'type-error :datum value :expected-type 'unsigned-byte))
                  limit))))
(defun pretty-tab-distance (column section kind parameters)
  (declare (type (signed-byte 32) column section) (type (integer 0 #x7fffffff) kind) (type cons parameters)
           (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr = >= < + - zerop logand 1- ash))
  (values (block done
    (let ((number (checked-path-car parameters)) (increment (checked-path-car (checked-path-cdr parameters))))
      (declare (type (integer 0 #x7fffffff) number increment))
      (if (if (= kind 8) t (= kind 10)) (setq column (- column section)))
      (if (>= kind 9)
          (return-from done (if (zerop increment) number
                                (+ number (mod (- increment (mod (logand (+ column number) (1- (ash 1 32))) increment)) increment)))))
      (if (< column number) (return-from done (- number column)))
      (if (zerop increment) 0 (- increment (mod (- column number) increment)))))))
(defun compiled-byte-header-p (bytes size)
  (declare (type string bytes) (type (integer 0 #x7fffffff) size)
           (optimize (safety 0) (debug 0)) (inline < =))
  (values (if (< size 8) nil
      (if (= (char-code (char bytes 0)) 0)
          (if (= (char-code (char bytes 1)) 97)
              (if (= (char-code (char bytes 2)) 115) (= (char-code (char bytes 3)) 109) nil) nil)
          nil))))
(defun text-position-column (bytes size position)
  (declare (type string bytes) (type (integer 0 #x7fffffff) size position)
           (optimize (safety 0) (debug 0)) (inline > - zerop logand 1- 1+ = char char-code))
  (values (block done
    (let ((index position) (count 0))
      (declare (type (integer 0 #x7fffffff) index count))
      (if (> index size) (setq count (- index size) index size))
      (tagbody previous
        (if (zerop index) (return-from done count))
        (setq index (the (integer 0 #x7fffffff) (1- index)))
        (let ((code (char-code (char bytes index))))
          (if (= code 10) (return-from done count))
          ;; Only UTF-8 leading bytes contribute a character column.
          (if (= (logand code 192) 128) nil (setq count (the (integer 0 #x7fffffff) (1+ count)))))
        (go previous))))))
(defun read-ordering-buffer (sequence buffer options)
  (declare (type simple-vector buffer) (type cons options) (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr consp car cdr (setf svref) = 1+))
  (values (block done
    (let ((offset (checked-path-car options)) (count (checked-path-car (checked-path-cdr options)))
          (cells (checked-path-car (checked-path-cdr (checked-path-cdr options)))) (index 0)
          (list-p (consp sequence)))
      (declare (type (integer 0 #x7fffffff) offset count index))
      (tagbody next
        (if (= index count) (return-from done nil))
        (setf (svref buffer offset) (if cells sequence (if list-p (checked-path-car sequence) (elt sequence index))))
        (if list-p (setq sequence (checked-path-cdr sequence)))
        (setq offset (the (integer 0 #x7fffffff) (1+ offset)) index (the (integer 0 #x7fffffff) (1+ index)))
        (go next))))))
(defun merge-ordering-buffers (input output options)
  (declare (type simple-vector input output) (type cons options) (optimize (safety 0) (debug 0))
           (inline car cdr svref (setf svref) = < 1+))
  (values (block done
    (let* ((first (car options)) (options (cdr options))
           (middle (car options)) (options (cdr options))
           (stop (car options)) (options (cdr options))
           (cells (car options)) (options (cdr options))
           (key (car options)) (predicate (car (cdr options)))
           (left first) (right middle) (out first))
      (declare (type (integer 0 #x7fffffff) first middle stop left right out))
      (tagbody next
        (if (= out stop) (return-from done nil))
        (let ((take-right (= left middle)))
          (if (< left middle)
              (if (< right stop)
                  (let* ((a (svref input right)) (b (svref input left))
                         (a (if cells (car a) a)) (b (if cells (car b) b))
                         (ka (if key (funcall key a) a)) (kb (if key (funcall key b) b)))
                    (setq take-right (funcall predicate ka kb)))))
          (setf (svref output out) (svref input (if take-right right left)))
          (if take-right (setq right (the (integer 0 #x7fffffff) (1+ right)))
              (setq left (the (integer 0 #x7fffffff) (1+ left)))))
        (setq out (the (integer 0 #x7fffffff) (1+ out)))
        (go next))))))
(defun loop-syntax-equal-p (left right)
  (declare (optimize (safety 0) (debug 0)) (inline consp car cdr cons eq))
  (values (block done
    (let ((pending (list (cons left right))))
      (tagbody next
        (if pending nil (return-from done t))
        (let* ((pair (car (the cons pending))) (a (car (the cons pair))) (b (cdr (the cons pair))))
          (setq pending (cdr (the cons pending)))
          (if (eq a b) nil
              (if (consp a) (if (consp b)
                  (setq pending (cons (cons (car a) (car b)) (cons (cons (cdr a) (cdr b)) pending)))
                  (return-from done nil))
                  (return-from done nil))))
        (go next))))))
(defun copy-loop-syntax (source budget)
  (declare (type (integer 0 #x7fffffff) budget) (optimize (safety 0) (debug 0))
           (inline consp car cdr cons list rplaca rplacd zerop 1-))
  (values (block done
    (let* ((box (cons nil nil)) (pending (list (list source box nil))))
      (tagbody next
        (if pending nil (return-from done (cons (car box) budget)))
        (let* ((task (car (the cons pending))) (node (car (the cons task)))
               (place (car (the cons (cdr (the cons task))))) (side (car (the cons (cdr (the cons (cdr (the cons task)))))))
               (value node))
          (setq pending (cdr (the cons pending)))
          (if (consp node)
              (progn
                (if (zerop budget) (return-from done (cons nil -1)))
                (setq budget (the (integer 0 #x7fffffff) (1- budget)) value (cons nil nil)
                      pending (cons (list (car node) value nil) (cons (list (cdr node) value t) pending)))))
          (if side (rplacd (the cons place) value) (rplaca (the cons place) value)))
        (go next))))))
(defun tree-value-equal-p (left right predicate negate)
  "Compare leaves in depth-first order; delay CDR reads until CAR callbacks finish."
  (declare (optimize (safety 0) (debug 0))
           (inline consp car cdr cons list eq =))
  (values (block done
    (let ((pending (list (list left right 0))) (active (make-hash-table :test 'eq)))
      (tagbody next
        (if pending nil (return-from done t))
        (let* ((task (car (the cons pending))) (a (car (the cons task)))
               (rest (cdr (the cons task))) (b (car (the cons rest))) (phase (car (the cons (cdr (the cons rest))))))
          (setq pending (cdr (the cons pending)))
          (if (= phase 2)
              (remhash b (gethash a active))
              (if (= phase 1)
                  (setq pending (cons (list (cdr (the cons a)) (cdr (the cons b)) 0) pending))
                  (if (consp a)
                      (if (consp b)
                          (let ((table (gethash a active)))
                            (if table nil (progn (setq table (make-hash-table :test 'eq)) (setf (gethash a active) table)))
                            (if (gethash b table)
                                (error 'type-error :datum a :expected-type (list 'not (list 'eql a))))
                            (setf (gethash b table) t)
                            (setq pending (cons (list (car a) (car b) 0)
                                            (cons (list a b 1) (cons (list a b 2) pending)))))
                          (return-from done nil))
                      (if (consp b) (return-from done nil)
                          (if (if (funcall predicate a b) negate (not negate)) (return-from done nil)))))))
        (go next))))))
(defun path-directory-value-p (text pattern bounds)
  (declare (type string text pattern) (type cons bounds) (optimize (safety 0) (debug 0))
           (inline car cdr path-component-value-p cons list = < >= - 1+))
  (values (block done
    (let* ((first (car bounds)) (bounds (cdr bounds))
           (end (car bounds)) (bounds (cdr bounds))
           (pfirst (car bounds)) (stop (car (cdr bounds)))
           (pending (list (cons first pfirst))))
      (declare (type (integer 0 #x7fffffff) first end pfirst stop))
      (tagbody next
        (if pending nil (return-from done nil))
        (let* ((state (car pending)) (index (car state)) (position (cdr state))
               (next index) (pnext position) (recursive nil))
          (declare (type (integer 0 #x7fffffff) index position next pnext))
          (setq pending (cdr pending))
          (if (>= position stop) (if (= index end) (return-from done t) (go next)))
          (tagbody pattern-end
            (if (= (char-code (char pattern pnext)) 47) (go pattern-found))
            (setq pnext (the (integer 0 #x7fffffff) (1+ pnext))) (go pattern-end)
           pattern-found)
          (setq recursive (if (= (- pnext position) 2)
                              (if (= (char-code (char pattern position)) 42) (= (char-code (char pattern (1+ position))) 42) nil)
                              nil))
          (if recursive (setq pending (cons (cons index (1+ pnext)) pending)))
          (if (>= index end) (go next))
          (tagbody text-end
            (if (= (char-code (char text next)) 47) (go text-found))
            (setq next (the (integer 0 #x7fffffff) (1+ next))) (go text-end)
           text-found)
          (if recursive
              (setq pending (cons (cons (1+ next) position) pending))
              (if (let ((bounds (list index next position pnext))) (declare (type cons bounds)) (values (block done
    (let* ((index (car bounds)) (tail (cdr bounds))
           (end (car tail)) (tail2 (cdr tail))
           (position (car tail2)) (stop (car (cdr tail2)))
           (star -1) (retry 0))
      (declare (type (integer 0 #x7fffffff) index end position stop retry)
               (type (integer -1 #x7fffffff) star))
      (tagbody match
        (if (>= index end)
            (tagbody stars
              (if (>= position stop) (return-from done (= position stop)))
              (if (= (char-code (char pattern position)) 42) nil (return-from done nil))
              (setq position (the (integer 0 #x7fffffff) (1+ position)))
              (go stars)))
        (if (< position stop)
            (let ((code (char-code (char pattern position))))
              (if (= code 42)
                  (progn (setq star position retry index position (the (integer 0 #x7fffffff) (1+ position))) (go match)))
              (if (= code (char-code (char text index)))
                  (progn (setq position (the (integer 0 #x7fffffff) (1+ position)) index (the (integer 0 #x7fffffff) (1+ index))) (go match)))))
        (if (< star 0) (return-from done nil))
        (setq position (the (integer 0 #x7fffffff) (1+ star)) retry (the (integer 0 #x7fffffff) (1+ retry)) index retry)
        (go match))))))
                  (setq pending (cons (cons (1+ next) (1+ pnext)) pending)))))
        (go next))))))
(defun format-small-integer (value)
  (declare (optimize (safety 0) (debug 0)))
  (values (if (integerp value)
      (if (<= (- #x3fffffff) value) (if (<= value #x3fffffff) value
          (error 'type-error :datum value :expected-type (list 'integer (- #x3fffffff) #x3fffffff)))
          (error 'type-error :datum value :expected-type (list 'integer (- #x3fffffff) #x3fffffff)))
      (error 'type-error :datum value :expected-type 'integer))))
