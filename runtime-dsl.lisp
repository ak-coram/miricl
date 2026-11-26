;;;; Host Lisp abstractions for composing the guest runtime's WAT.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defun runtime-wat-type (type)
  "Expand common guest reference types; leave native WAT types unchanged."
  (case type
    (:object '(ref eq))
    (:nullable-object '(ref null eq))
    (:digits '(ref $digits))
    (otherwise type)))

(defun runtime-wat-sequence (&rest fragments)
  "Join rendered templates and runtime sections without quoting their text."
  (wat-fragment
   (format nil "~{~A~^~%~}"
           (mapcar (lambda (fragment)
                     (etypecase fragment
                       (string fragment)
                       (wat-fragment (wat-fragment-text fragment))))
                   fragments))))

(defmacro define-runtime-section (name lambda-list &body fragments)
  "Define a host function that assembles related guest definitions."
  `(defun ,name ,lambda-list
     (runtime-wat-sequence ,@fragments)))

(defun runtime-wat-forms (forms)
  "Render a list of generated WAT trees as a sequence of instructions."
  (apply #'runtime-wat-sequence (mapcar #'wat-string forms)))

(defvar *runtime-function-definitions* nil)

(defun runtime-function-definition (name definition)
  "Keep standalone generators usable while optionally collecting module functions."
  (if *runtime-function-definitions*
      (progn (vector-push-extend (cons name definition) *runtime-function-definitions*)
             (wat-fragment ""))
      definition))

(defun runtime-function-layout (definitions &key (reserved-functions 0) references)
  "Give frequently referenced functions short binary indices.
Only declaration order changes: all names, signatures, and bodies are retained.
Keep source order within each index range to preserve compression locality."
  (let ((counts (make-hash-table :test 'equal)))
    (labels ((count-calls (text)
               ;; This is a layout heuristic, not a WAT transformation. Even a
               ;; call-like string or comment can only affect the ordering.
               ;; Count folded runtime WAT and the flat linked compiler WAT,
               ;; including tail calls and function references.
               (dolist (marker '("call " "return_call " "ref.func "))
                 (loop with start = 0
                       for call = (search marker text :start2 start) while call do
                   (let* ((name-start (+ call (length marker)))
                          (end (or (position-if
                                     (lambda (char) (find char '(#\Space #\Tab #\Newline #\Return #\))))
                                     text :start name-start)
                                   (length text))))
                     (when (or (zerop call)
                               (find (char text (1- call)) '(#\( #\Space #\Tab #\Newline #\Return)))
                       (incf (gethash (subseq text name-start end) counts 0)))
                     (setf start end))))))
      (loop for (name . definition) across definitions do
        (count-calls (wat-fragment-text definition)))
      (when references (count-calls references)))
    (let* ((ranked (stable-sort (copy-seq definitions) #'>
                     :key (lambda (entry) (gethash (wat-string (car entry)) counts 0))))
           (short (subseq ranked 0 (min (max 0 (- 128 reserved-functions)) (length ranked)))))
      (apply #'runtime-wat-sequence
        (map 'list #'cdr
          (stable-sort (copy-seq definitions) #'>
            :key (lambda (entry) (if (find entry short :test #'eq) 1 0))))))))

(defun runtime-live-function-layout (records counts)
  ;; The browser runtime is import-free and fits in two ULEB widths. Keep
  ;; anonymous and inline-export declarations in their live source slots.
  (unless (< 128 (length records) 16384)
    (return-from runtime-live-function-layout records))
  (flet ((movable-p (record)
           (and (stringp (svref record 0)) (not (svref record 4)))))
    (let* ((mobile (loop for record in records when (movable-p record) collect record))
           (short-slots (loop for record in records repeat 128 count (movable-p record)))
           (ranked (stable-sort (copy-list mobile) #'>
                     :key (lambda (record) (gethash (svref record 0) counts 0))))
           (short (make-hash-table :test 'eq)))
      (loop for record in ranked repeat short-slots do (setf (gethash record short) 1))
      (setf mobile (stable-sort mobile #'> :key (lambda (record) (gethash record short 0))))
      (mapcar (lambda (record) (if (movable-p record) (pop mobile) record)) records))))

(defun runtime-prune-wat (text &key layout-functions)
  "Remove unreferenced named functions from a generated core WAT module.
Keep all non-function fields byte-for-byte. Every symbolic function name in
those fields is a root; every matching name in a function is an edge. This
deliberately overestimates references through identifier namespace collisions.
Anonymous and imported definitions are retained; numeric references keep TEXT.
LAYOUT-FUNCTIONS gives frequently referenced live definitions short indices
in import-free images while leaving anonymous and inline exports in place.
This private host pass accepts trusted generated text, not arbitrary user WAT;
the assembler still validates its output."
  (check-type text string)
  (let* ((size (length text)) (position 0) (depth 0) (frames nil)
         (functions (make-hash-table :test 'equal)) (roots nil) (records nil)
         (owner nil) (pending-reference nil) (module-seen nil)
         (layout-supported layout-functions)
         (counts (and layout-functions (make-hash-table :test 'equal)))
         (previous nil) (before-previous nil))
    (labels ((unsupported () (return-from runtime-prune-wat (values text nil nil)))
             (spacep (char) (find char '(#\Space #\Tab #\Newline #\Return #\Page)))
             (namedp (token) (and (> (length token) 1) (char= (char token 0) #\$)))
             (reference (token)
               (if owner (push token (svref owner 3)) (push token roots)))
             (count-reference (token)
               (if owner
                   (push token (svref owner 5))
                   (incf (gethash token counts 0))))
             (numericp (token)
               (let ((first (char token 0)))
                 (or (digit-char-p first) (find first "+-"))))
             (type-operand-p ()
               ;; The assembler can synthesize types for inline signatures.
               ;; Reordering functions can change those types' numeric indices.
               (or (member previous '("type" "ref" "ref.null" "sub" "call_ref" "return_call_ref")
                           :test #'equal)
                   (and (member previous '("null" "final") :test #'equal)
                        (member before-previous '("ref" "sub") :test #'equal))
                   (equal before-previous "array.copy")
                   (and previous
                        (or (and (>= (length previous) 7)
                                 (string= previous "struct." :end1 7))
                            (and (>= (length previous) 6)
                                 (string= previous "array." :end1 6))))))
             (atom-token (token)
               (let* ((frame (car frames)) (head (and frame (svref frame 0))))
                 (unless frame (unsupported))
                 (when layout-supported
                   (when (and (numericp token) (type-operand-p))
                     (setf layout-supported nil))
                   (setf before-previous previous previous token))
                 (when pending-reference
                   (unless (namedp token) (unsupported))
                   (when (and layout-supported (eq pending-reference :counted))
                     (count-reference token))
                   (setf pending-reference nil))
                 (cond
                   ((null head)
                    (setf (svref frame 0) token)
                    (when (= depth 1)
                      (unless (and (not module-seen) (string= token "module")) (unsupported))
                      (setf module-seen t))
                    (when (and (= depth 2) (string= token "func"))
                      ;; Name, source span, reachability edges, inline roots,
                      ;; and call/reference operands used for layout.
                      (setf owner (vector nil (svref frame 1) nil nil nil nil))
                      (push owner records))
                    (when (and (<= 2 depth 3) (string= token "import"))
                      (setf layout-supported nil))
                    (when (and owner (member token '("export" "import") :test #'string=))
                      (setf (svref owner 4) t)))
                   ((and (= depth 2) (string= head "func") (null (svref owner 0)))
                    (if (namedp token)
                        (progn
                          (when (gethash token functions) (unsupported))
                          (setf (svref owner 0) token (gethash token functions) owner))
                        (setf (svref owner 0) :anonymous))))
                 (when (namedp token) (reference token))
                 (cond
                   ((member token '("call" "return_call" "ref.func") :test #'string=)
                    (setf pending-reference :counted))
                   ((string= token "start") (setf pending-reference t)))
                 ;; Function descriptors in explicit export fields and old
                 ;; bare element lists may use numeric indices. Do not alter
                 ;; indices without a complete binary relocation pass.
                 (when (and (string= token "func") (cdr frames)
                            (string= (or (svref (second frames) 0) "") "export"))
                   (setf pending-reference t))
                 (when (and (string= (or head "") "elem") (numericp token))
                   (unsupported)))))
      (loop while (< position size) for char = (char text position) do
        (cond
          ((spacep char) (incf position))
          ((and (char= char #\;) (< (1+ position) size)
                (char= (char text (1+ position)) #\;))
           (setf position (or (position #\Newline text :start (+ position 2)) size)))
          ((and (char= char #\() (< (1+ position) size)
                (char= (char text (1+ position)) #\;))
           (incf position 2)
           (let ((comments 1))
             (loop while (plusp comments) do
               (when (>= position size) (unsupported))
               (cond
                 ((and (< (1+ position) size) (char= (char text position) #\()
                       (char= (char text (1+ position)) #\;))
                  (incf comments) (incf position 2))
                 ((and (< (1+ position) size) (char= (char text position) #\;)
                       (char= (char text (1+ position)) #\)))
                  (decf comments) (incf position 2))
                 (t (incf position))))))
          ((char= char #\")
           (when pending-reference (unsupported))
           (setf previous nil before-previous nil)
           (incf position)
           (loop
             (when (>= position size) (unsupported))
             (let ((value (char text position)))
               (incf position)
               (cond ((char= value #\\) (incf position))
                     ((char= value #\") (return))))))
          ((char= char #\()
           (when pending-reference (unsupported))
           (setf previous nil before-previous nil)
           (when (and owner (= depth 2) (null (svref owner 0)))
             (setf (svref owner 0) :anonymous))
           (push (vector nil position) frames) (incf depth) (incf position))
          ((char= char #\))
           (when (or pending-reference (zerop depth)) (unsupported))
           (setf previous nil before-previous nil)
           (when (and owner (= depth 2))
             (unless (svref owner 0) (setf (svref owner 0) :anonymous))
             (setf (svref owner 2) (1+ position) owner nil))
           (pop frames) (decf depth) (incf position))
          ((char= char #\;) (unsupported))
          (t
           (let ((start position))
             (loop while (and (< position size)
                              (not (spacep (char text position)))
                              (not (find (char text position) "();\""))) do (incf position))
             (when (= start position) (unsupported))
             (atom-token (subseq text start position))))))
      (when (or (not module-seen) (plusp depth) owner pending-reference) (unsupported))
      (let ((live (make-hash-table :test 'equal)) (pending roots) (removed nil) (kept nil))
        (dolist (record records)
          (if (eq (svref record 0) :anonymous)
              (dolist (callee (svref record 3)) (push callee pending))
              (when (svref record 4) (push (svref record 0) pending))))
        (loop while pending for name = (pop pending) do
          (unless (gethash name live)
            (setf (gethash name live) t)
            (let ((function (gethash name functions)))
              (when function (dolist (callee (svref function 3)) (push callee pending))))))
        (dolist (record records)
          (if (or (eq (svref record 0) :anonymous) (gethash (svref record 0) live))
              (when layout-supported (push record kept))
              (push record removed)))
        (when layout-supported
          (dolist (record kept)
            (dolist (callee (svref record 5)) (incf (gethash callee counts 0)))))
        (values
          (with-output-to-string (out)
            (if layout-supported
                (let ((start 0) (original kept) (layout (runtime-live-function-layout kept counts)))
                  (dolist (record (nreverse records))
                    (write-string text out :start start :end (svref record 1))
                    (when (eq record (first original))
                      (pop original)
                      (let ((replacement (pop layout)))
                        (write-string text out :start (svref replacement 1) :end (svref replacement 2))))
                    (setf start (svref record 2)))
                  (write-string text out :start start))
                (let ((start 0))
                  (dolist (record removed)
                    (write-string text out :start start :end (svref record 1))
                    (setf start (svref record 2)))
                  (write-string text out :start start))))
          (mapcar (lambda (record) (svref record 0)) removed) t)))))

(defmacro define-runtime-function (name parameters results &body body)
  "Define a host generator with the same $NAME as its guest function.
PARAMETERS and :LOCALS contain (name type) pairs; RESULTS is a list of
types. Optional :EXPORT, :EXPORT-RAW and :LOCALS clauses precede ordinary Lisp
expressions producing WAT instructions, usually #@ templates. The
generator returns a WAT-FRAGMENT; calling it never executes guest code.
:EXPORT-RAW lets a reentrant host bridge propagate guest transfers instead
of converting them to the ordinary top-level host trap."
  (let ((exports nil) (raw-exports nil) (locals nil))
    (loop while (and (consp (first body))
                     (member (caar body) '(:export :export-raw :locals)))
          for clause = (pop body)
          do (ecase (car clause)
               (:export (setf exports (cdr clause)))
               (:export-raw (setf exports (cdr clause) raw-exports t))
               (:locals (setf locals (cdr clause)))))
    `(defun ,name ()
       (runtime-function-definition ',name (wat-fragment
        (wat-string
         (list 'func ',name
               ,@(loop for export in exports collect `(list 'export ,export))
               ,@(loop for (parameter type) in parameters
                       collect `(list 'param ',parameter ',(runtime-wat-type type)))
               ,@(when results
                   (list `(quote (result ,@(mapcar #'runtime-wat-type results)))))
               ,@(loop for (local type) in locals
                       collect `(list 'local ',local ',(runtime-wat-type type)))
               ,(if (and exports (not raw-exports))
                    `(runtime-wat-sequence
                      #@(block $export-failed
                          (try_table (catch $transfer $export-failed)
                            ,(runtime-wat-sequence ,@body)
                            (return)))
                      #@(call $trap) #@(unreachable))
                    `(runtime-wat-sequence ,@body)))))))))

(defvar *runtime-output-literals* nil)
(defvar *runtime-output-bytes* nil)
(defvar *runtime-pack-text-literals* t)

(defun runtime-text-literal-entry (string)
  (or (gethash string *runtime-output-literals*)
      (setf (gethash string *runtime-output-literals*)
        (let ((offset (length *runtime-output-bytes*)))
          (cffi:with-foreign-string ((data length) string :encoding :utf-8)
            (loop for i below (1- length) do
              (vector-push-extend (cffi:mem-aref data :uint8 i) *runtime-output-bytes*)))
          (cons offset (- (length *runtime-output-bytes*) offset))))))

(defun wat-text-literal (string)
  (if (and *runtime-pack-text-literals* *runtime-output-literals* (>= (length string) 8)
           (every (lambda (char) (< (char-code char) 128)) string))
      (let ((entry (runtime-text-literal-entry string)))
        `(call $ascii-text-literal (i32.const ,(car entry)) (i32.const ,(cdr entry))))
      `(call $text (array.new_fixed $digits ,(length string)
                     ,@(loop for char across string collect `(i32.const ,(char-code char)))))))

(defun emit-text (string)
  ;; Static printer text can go directly to its UTF-8 output buffer. Keep
  ;; ordinary string literals fresh: callers may expose or mutate those.
  (cond ((zerop (length string)) '(nop))
        ((= (length string) 1) `(call $emit-char (i32.const ,(char-code (char string 0)))))
        (*runtime-output-literals*
         (let ((entry (runtime-text-literal-entry string)))
           `(call $emit-literal (i32.const ,(car entry)) (i32.const ,(cdr entry)))))
        (t `(call $print-text ,(wat-text-literal string)))))

(defun wat-fail (error)
  "Signal a named guest error, preserving the bootstrap error codes."
  (wat-fragment #@(call $fail (i32.const ,(runtime-error-code error)))))

(defun wat-fail-when (condition error)
  "Signal ERROR when a rendered WAT CONDITION is nonzero."
  (wat-fragment #@(if ,(runtime-wat-sequence condition) (then ,(wat-fail error)))))

(defun wat-new-object (tag &key (n #@(i32.const 0)) (f #@(f64.const 0))
                              (digits #@(ref.null $digits))
                              (a #@(ref.null eq)) (b #@(ref.null eq))
                              (c #@(ref.null eq)))
  "Allocate an object, spelling out only its populated fields.
TAG is a named object kind or a rendered WAT expression. Field arguments
are rendered expressions, evaluated once in their struct field order."
  (let ((tag (if (keywordp tag) #@(i32.const ,(runtime-object-tag tag)) tag)))
    (wat-fragment
     #@(struct.new $obj (call $allocate-object-tag ,(runtime-wat-sequence tag)) ,(runtime-wat-sequence n)
                   ,(runtime-wat-sequence f) ,(runtime-wat-sequence digits)
                   ,(runtime-wat-sequence a) ,(runtime-wat-sequence b)
                   ,(runtime-wat-sequence c)))))

(defmacro wat-list-loop ((list &key (done '$done) (next '$next)) &body body)
  "Walk a guest list local, advancing it after each iteration.
BODY produces WAT instructions and can branch to DONE or NEXT. Choose
explicit labels for nested loops. The list local is consumed."
  `(wat-fragment
    (wat-string
     (list 'block ',done
           (list 'loop ',next
                 '(br_if ,done (ref.is_null (local.get ,list)))
                 (runtime-wat-sequence ,@body)
                 '(local.set ,list (call $cdr (local.get ,list)))
                 '(br ,next))))))

(defun wat-arity (min &optional (max min) checked-count)
  "Check $ARGS, or reuse CHECKED-COUNT while its validated spine is unchanged."
  (cond
    (checked-count
     (wat-fragment #@(call $arity-count ,(runtime-wat-sequence checked-count) (i32.const ,min) (i32.const ,max))))
    ((and (= min max) (<= 0 min 3))
     (wat-fragment (wat-string `(call ,(nth min '($arity0 $arity1 $arity2 $arity3)) (local.get $args)))))
    (t (wat-fragment #@(call $arity (local.get $args) (i32.const ,min) (i32.const ,max))))))

(defparameter *runtime-dynamic-state*
  '($dynamic-package $dynamic-variables $handlers $restarts $restart-associations $catches $targets))

(defun wat-dynamic-call (expression &key cleanup)
  "Restore dynamic scope on ordinary returns and guest nonlocal transfers."
  (runtime-wat-sequence
   (runtime-wat-forms
    (loop for name in *runtime-dynamic-state* collect `(local.set ,name (global.get ,name))))
   #@(block $returned
       (block $thrown
         (try_table (catch $transfer $thrown)
           (local.set $result ,(runtime-wat-sequence expression))
           (br $returned)))
       (local.set $thrown (i32.const 1)))
   (runtime-wat-forms
    (loop for name in *runtime-dynamic-state* collect `(global.set ,name (local.get ,name))))
   (or cleanup (runtime-wat-sequence))
   #@(if (local.get $thrown) (then (throw $transfer)))
   #@(local.get $result)))

(defun wat-integer-branch (value ids body)
  "Branch on a nonempty integer ID set. VALUE must have no side effects."
  (let ((ranges nil))
    ;; Consecutive IDs share one unsigned range test. Subtraction keeps both
    ;; endpoints exact even when a signed input lies below the interval.
    ;; Keep invalid I32.CONST inputs as singletons for assembler rejection.
    (dolist (id (sort (copy-list ids) #'<))
      (if (and ranges (= id (1+ (cdar ranges)))
               (<= (- #x80000000) (caar ranges)) (< id #x100000000)
               (< (- id (caar ranges)) #x100000000))
          (setf (cdar ranges) id)
          (push (cons id id) ranges)))
    (let ((condition
            (reduce (lambda (a b) `(i32.or ,a ,b))
                    (mapcar (lambda (range)
                              (if (= (car range) (cdr range))
                                  `(i32.eq ,value (i32.const ,(car range)))
                                  `(i32.le_u (i32.sub ,value (i32.const ,(car range)))
                                              (i32.const ,(- (cdr range) (car range))))))
                            (nreverse ranges)))))
      (wat-fragment (wat-string `(if ,condition (then ,body)))))))

(defun wat-symbol-branch (value symbols body)
  ;; Some runtime generators use a single symbol group without a full CASE.
  (wat-integer-branch value (mapcar #'runtime-symbol-id symbols) body))

(defun wat-index-dispatch (value entries done)
  "Branch to shared clause labels using compact tables and a sparse search tree."
  (let* ((first (caar entries)) (last (caar (last entries)))
         (span (1+ (- last first))))
    (cond
      ((<= (length entries) 3)
       (apply #'runtime-wat-sequence
         (append (loop for (id . label) in entries collect
                   (wat-string `(br_if ,label (i32.eq ,value (i32.const ,id)))))
                 (list (wat-string `(br ,done))))))
      ((<= span (* 6 (length entries)))
       (let ((targets (make-array span :initial-element done)))
         (dolist (entry entries) (setf (aref targets (- (car entry) first)) (cdr entry)))
         (wat-fragment (wat-string `(br_table ,@(coerce targets 'list) ,done
                                     (i32.sub ,value (i32.const ,first)))))))
      (t
       (let* ((middle (floor (length entries) 2)) (pivot (car (nth middle entries))))
         (wat-fragment (wat-string
           `(if (i32.lt_s ,value (i32.const ,pivot))
              (then ,(wat-index-dispatch value (subseq entries 0 middle) done))
              (else ,(wat-index-dispatch value (nthcdr middle entries) done))))))))))

(defun wat-integer-dispatch (value clauses)
  "Dispatch on integer IDs, keeping each body once across sparse tables.
VALUE must have no side effects. Clauses contain (IDS . BODY), where IDS
is a nonempty list of distinct IDs and BODY can fall through to the end."
  (when (< (loop for (ids . body) in clauses sum (length ids)) 4)
    (return-from wat-integer-dispatch
      (apply #'runtime-wat-sequence
        (loop for (ids . body) in clauses collect (wat-integer-branch value ids body)))))
  (let* ((done (gensym "$dispatch-done"))
         (labels (loop repeat (length clauses) collect (gensym "$dispatch-case")))
         (entries nil)
         (seen (make-hash-table)))
    (loop for (ids . body) in clauses for label in labels do
      (dolist (id ids)
        (when (gethash id seen) (error "Duplicate WAT dispatch integer ~S." id))
        (setf (gethash id seen) t)
        (push (cons id label) entries)))
    (let ((dispatch (wat-index-dispatch value (sort entries #'< :key #'car) done)))
      (loop for (ids . body) in clauses for label in labels do
        (setf dispatch (runtime-wat-sequence
          (wat-string `(block ,label ,dispatch)) body (wat-string `(br ,done)))))
      (wat-fragment (wat-string `(block ,done ,dispatch))))))

(defun wat-symbol-dispatch (value clauses)
  (wat-integer-dispatch value
    (loop for (symbols . body) in clauses collect
      (cons (mapcar #'runtime-symbol-id symbols) body))))

(defmacro wat-symbol-case (value &body clauses)
  "Dispatch on a bootstrap symbol ID. Each clause names a symbol or list.
Clause bodies produce WAT instructions and explicitly RETURN when done;
the value expression should have no side effects, since tests reuse it."
  (let ((expression (gensym "VALUE")))
    `(let ((,expression (runtime-wat-sequence ,value)))
       (wat-symbol-dispatch ,expression (list
        ,@(loop for (symbols . body) in clauses
                collect `(cons ',(if (listp symbols) symbols (list symbols))
                               (runtime-wat-sequence ,@body))))))))
