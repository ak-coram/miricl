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

(defun wat-arity (min &optional (max min))
  "Check the builtin's $ARGS list, with an exact arity by default."
  (if (and (= min max) (<= 0 min 3))
      (wat-fragment (wat-string `(call ,(nth min '($arity0 $arity1 $arity2 $arity3)) (local.get $args))))
      (wat-fragment #@(call $arity (local.get $args) (i32.const ,min) (i32.const ,max)))))

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

(defun wat-symbol-branch (value symbols body)
  (let ((condition
          (reduce (lambda (a b) `(i32.or ,a ,b))
                  (mapcar (lambda (symbol)
                            `(i32.eq ,value (i32.const ,(runtime-symbol-id symbol))))
                          symbols))))
    (wat-fragment (wat-string `(if ,condition (then ,body))))))

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

(defun wat-symbol-dispatch (value clauses)
  "Keep each body once, including when sparse symbol IDs need several tables."
  (when (< (loop for (symbols . body) in clauses sum (length symbols)) 4)
    (return-from wat-symbol-dispatch
      (apply #'runtime-wat-sequence
        (loop for (symbols . body) in clauses collect (wat-symbol-branch value symbols body)))))
  (let* ((done (gensym "$dispatch-done"))
         (labels (loop repeat (length clauses) collect (gensym "$dispatch-case")))
         (entries nil)
         (seen (make-hash-table)))
    (loop for (symbols . body) in clauses for label in labels do
      (dolist (symbol symbols)
        (let ((id (runtime-symbol-id symbol)))
          (when (gethash id seen) (error "Duplicate WAT dispatch symbol ~S." symbol))
          (setf (gethash id seen) t)
          (push (cons id label) entries))))
    (let ((dispatch (wat-index-dispatch value (sort entries #'< :key #'car) done)))
      (loop for (symbols . body) in clauses for label in labels do
        (setf dispatch (runtime-wat-sequence
          (wat-string `(block ,label ,dispatch)) body (wat-string `(br ,done)))))
      (wat-fragment (wat-string `(block ,done ,dispatch))))))

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
