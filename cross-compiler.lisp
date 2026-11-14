;;;; Host tools only. Do not include this adapter in distributed guest images.
(in-package #:miricl-cross)

(defun require-target-macro-expansion (expander form environment)
  (declare (ignore expander environment))
  ;; A host expansion can silently call implementation-private functions,
  ;; even for SETF of a standard place such as SLOT-VALUE. Fail before emitting
  ;; that dependency. The initial interface accepts core forms, or an explicit
  ;; target expansion hook with the standard *MACROEXPAND-HOOK* convention.
  (miricl-compiler::compiler-input-fail "cross-compilation macro" form
    "a target macro expansion hook is required; host expansions are not portable"))

(defun require-target-load-time-evaluation (form)
  (miricl-compiler::compiler-input-fail "cross-compilation LOAD-TIME-VALUE" form
    "a target load-time evaluator is required; host evaluation is not portable"))

(defun compile-lambda (source &key name global-specials
                                 (macroexpand-hook #'require-target-macro-expansion)
                                 (load-time-evaluator #'require-target-load-time-evaluation))
  "Return target Wasm bytes, literals and debug metadata, without executing SOURCE.
SOURCE uses Common Lisp core forms. Macro expansion requires an explicit target
MACROEXPAND-HOOK; host implementation macros are rejected by default. Target
global special bindings are supplied through GLOBAL-SPECIALS, not host state.
LOAD-TIME-VALUE requires an explicit target LOAD-TIME-EVALUATOR.
No compiler is bootstrapped in Wasm, and no guest function bindings are changed."
  (let ((miricl-compiler::*compiler-state* :interpreted)
        (miricl-compiler::*hir-load-time-evaluator*
          (or load-time-evaluator #'require-target-load-time-evaluation))
        (*macroexpand-hook* macroexpand-hook))
    (miricl-compiler:emit-native-module
      (miricl-compiler:compile-to-hir source :name name :global-specials global-specials))))

(defun compile-module (definitions &key (entry-points nil entriesp) (inline t)
                                       (macroexpand-hook #'require-target-macro-expansion)
                                       (load-time-evaluator #'require-target-load-time-evaluation))
  "Cross-compile a closed DEFUN group; return Wasm bytes, literals, debug and HIR.
Only ENTRY-POINTS escape; omitted ENTRY-POINTS exposes every supplied definition.
Macro expansion and load-time evaluation have the same explicit target contract
as COMPILE-LAMBDA."
  (let ((miricl-compiler::*compiler-state* :interpreted)
        (miricl-compiler::*hir-load-time-evaluator*
          (or load-time-evaluator #'require-target-load-time-evaluation))
        (*macroexpand-hook* macroexpand-hook))
    (let ((unit (if entriesp
                    (miricl-compiler:compile-module-to-hir definitions
                      :entry-points entry-points :inline inline)
                    (miricl-compiler:compile-module-to-hir definitions :inline inline))))
      (multiple-value-bind (bytes literals debug) (miricl-compiler:emit-native-module unit)
        (values bytes literals debug unit)))))

(defvar *kernel-cache* (make-hash-table :test 'equal))
(defvar *kernel-backend* nil)

(defun kernel-macroexpand-hook (expander form environment)
  ;; The host's SETF expansion may name private operators; a SETF SVREF
  ;; function binding is not required on the host. Preserve place/value order.
  (if (and (consp form) (eq (first form) 'setf) (= (length form) 3)
           (consp (second form))
           (or (member (first (second form)) '(svref char))
               (and (eq (first (second form)) 'row-major-aref)
                    (eq (cdr (assoc '(setf row-major-aref) miricl-compiler::*hir-inline-policy*
                                    :test #'equal)) 'inline)))
           (= (length (second form)) 3))
      (let ((vector (gensym "VECTOR")) (index (gensym "INDEX")))
        `(let ((,vector ,(second (second form))) (,index ,(third (second form))))
           (funcall #',(list 'setf (first (second form))) ,(third form) ,vector ,index)))
      (if (and (consp form) (eq (first form) 'setf) (= (length form) 3)
               (consp (second form))
               (member (first (second form)) '(elt char row-major-aref gethash symbol-value symbol-plist symbol-function))
               (= (length (second form))
                  (if (member (first (second form)) '(symbol-value symbol-plist symbol-function)) 2 3)))
          ;; Supply the target's accessor-function ABI explicitly. Host SETF
          ;; expanders may instead name implementation-private setters.
          (let* ((place (second form)) (arguments (cdr place))
                 (temporaries (mapcar (lambda (argument) (declare (ignore argument)) (gensym "PLACE")) arguments)))
            `(let ,(mapcar #'list temporaries arguments)
               (funcall (fdefinition (cons 'setf (cons ',(car place) nil)))
                        ,(third form) ,@temporaries)))
          (require-target-macro-expansion expander form environment))))

(defun kernel-backend ()
  ;; REPL reloads can change an emitter or proof helper while kernel source
  ;; stays unchanged. Cache against the actual host definitions, not a build
  ;; timestamp, and discard old generations instead of retaining their code.
  (list (fdefinition 'kernel-worker-wat) (fdefinition 'kernel-literal-plan)
    (fdefinition 'kernel-macroexpand-hook)
    (copy-tree miricl-compiler::*native-imports*)
    (loop for name in miricl-compiler::*native-compiler-seeds*
          when (and (fboundp name) (not (macro-function name)))
            collect (cons name (fdefinition name)))))

(defparameter *kernel-source-files*
  '("runtime-kernel-library.lisp" "runtime-character-kernels.lisp"
    "runtime-list-mapping-kernels.lisp" "runtime-reader-kernels.lisp"
    "runtime-integer-field-kernels.lisp" "runtime-reduce-kernels.lisp"
    "runtime-sequence-edit-kernels.lisp" "runtime-count-kernels.lisp"
    "runtime-string-case-loop-kernels.lisp" "runtime-search-kernels.lisp"
    "runtime-mismatch-kernels.lisp" "runtime-set-kernels.lisp"
    "runtime-character-sequence-kernels.lisp" "runtime-package-kernels.lisp"
    "runtime-tree-kernels.lisp"))

(defun kernel-definitions ()
  ;; One closed group lets private helpers inline across migration batches.
  ;; Read the sources on each query so persistent host REPL edits invalidate
  ;; the existing source-keyed cache without rebuilding the compiler image.
  (loop for file in *kernel-source-files* append
    (with-open-file (stream (asdf:system-relative-pathname :miricl file))
      (let ((*package* (find-package :miricl-runtime)) (*read-eval* nil))
        (loop for form = (read stream nil stream) until (eq form stream)
              when (and (consp form) (eq (car form) 'defun)) collect form)))))

(defun kernel-export-index (bytes name)
  (let ((input (miricl-compiler::wasm-input bytes 8)))
    (loop while (< (miricl-compiler::wasm-input-position input) (length bytes)) do
      (let* ((kind (miricl-compiler::module-byte input))
             (size (miricl-compiler::module-uleb input))
             (end (+ size (miricl-compiler::wasm-input-position input))))
        (when (= kind 7)
          (dotimes (i (miricl-compiler::module-uleb input))
            (let* ((text (map 'string #'code-char (miricl-compiler::module-name input)))
                   (export-kind (miricl-compiler::module-byte input))
                   (index (miricl-compiler::module-uleb input)))
              (when (and (zerop export-kind) (string= text name))
                (return-from kernel-export-index index)))))
        (setf (miricl-compiler::wasm-input-position input) end))))
  (error "Missing compiled runtime export ~S." name))

(defun kernel-literal-plan (ops imports literals)
  "Resolve bootstrap symbols and scalar integers without a closure/literal pool."
  (let ((replacements (make-hash-table)) (skipped (make-hash-table))
        (flow (map 'vector #'copy-seq ops)))
    (loop for op across ops for i from 0
          when (and (= (miricl-compiler::native-op-code op) #x10)
                    (< (miricl-compiler::native-op-argument op) (length imports))
                    (string= (second (aref imports (miricl-compiler::native-op-argument op))) "native-literal")) do
      (unless (and (>= i 2)
                   (= (miricl-compiler::native-op-code (aref ops (- i 2))) #x20)
                   (zerop (miricl-compiler::native-op-argument (aref ops (- i 2))))
                   (= (miricl-compiler::native-op-code (aref ops (1- i))) #x41))
        (error "Runtime kernel has a nonconstant literal lookup."))
      (let* ((slot (miricl-compiler::native-op-argument (aref ops (1- i))))
             (literal (and (<= 0 slot) (< slot (length literals)) (aref literals slot)))
             (id (and (symbolp literal) (<= 0 slot) (< slot (length literals))
                      (position literal miricl::*runtime-symbols*))))
        (unless (or id (and (integerp literal) (<= -9223372036854775808 literal 9223372036854775807)))
          (error "Runtime kernel requires a non-bootstrap literal at slot ~S: ~S." slot literal))
        (setf (gethash (- i 2) replacements) (if id (cons :symbol id) (cons :integer literal))
              (gethash (1- i) skipped) t (gethash i skipped) t
              ;; Remove only the proved constant lookup's closure dependency
              ;; from the ABI liveness check; every other read stays visible.
              (miricl-compiler::native-op-code (aref flow (- i 2))) #xd0
              (miricl-compiler::native-op-argument (aref flow (- i 2))) -17)))
    (values replacements skipped flow)))

(defun kernel-worker-wat (bytes index name arity result interruptible &optional (literals #()))
  (declare (notinline kernel-literal-plan))
  (multiple-value-bind (body imports offset signature)
      (miricl-compiler::native-disassembly-body bytes index)
    (declare (ignore offset))
    (unless (equal signature '(("externref" "externref" "externref" "externref" "externref") ("externref")))
      (error "Unsupported runtime worker signature: ~S." signature))
    (let* ((ops (miricl-compiler::native-read-ops body))
           (input (miricl-compiler::wasm-input body)) (locals nil) (raw-truth nil)
           (vector-read nil) (vector-write nil) (vector-index nil) (vector-object nil) (vector-value nil)
           (call-slots 0) (call-base 0) (integer-call nil) (integer-left 0) (integer-right 0))
      (dotimes (i (miricl-compiler::module-uleb input))
        (let ((count (miricl-compiler::module-uleb input))
              (type (miricl-compiler::module-byte input)))
          (unless (member type '(#x7f #x7e #x6f)) (error "Unsupported runtime local type: ~D." type))
          (dotimes (j count) (push (case type (#x7f "i32") (#x7e "i64") (otherwise "(ref null eq)")) locals))))
      (multiple-value-bind (literal-replacements literal-skips flow-ops)
          (kernel-literal-plan ops imports literals)
      ;; An entry has no closure object or padded arguments. Prove those
      ;; private ABI inputs dead before changing their slots into zeroed locals.
      (miricl-compiler::native-op-liveness flow-ops (+ 5 (length locals))
        (miricl-compiler::native-op-flow flow-ops))
      (let ((live (miricl-compiler::native-op-live-in (aref flow-ops 0))))
        (when (or (logbitp 0 live)
                  (loop for i from (1+ arity) below 5 thereis (logbitp i live)))
          (error "Runtime kernel depends on a closure or padded argument: ~S." name)))
      (loop for op across ops
            when (and (= (miricl-compiler::native-op-code op) #x10)
                      (< (miricl-compiler::native-op-argument op) (length imports))) do
        (let ((target (second (aref imports (miricl-compiler::native-op-argument op)))))
          (when (string= target "native-vector-ref-inline") (setf vector-read t))
          (when (string= target "native-vector-set-inline") (setf vector-write t))
          (let ((arity (position target '("native-call0" "native-call1" "native-call2" "native-call3" "native-call4") :test #'equal)))
            (when arity (setf call-slots (max call-slots (1+ arity)))))
          (when (member target '("native-call" "native-character-code") :test #'equal)
            (setf call-slots (max call-slots 2)))
          (when (string= target "native-character-ref-inline") (setf call-slots (max call-slots 2)))
          (when (string= target "native-character-set-inline") (setf call-slots (max call-slots 3)))
          (when (string= target "native-simple-character-set-inline")
            (setf integer-call t call-slots (max call-slots 2)))
          (when (string= target "native-code-character-inline") (setf call-slots (max call-slots 1)))
          (when (string= target "native-logand-general-inline") (setf call-slots (max call-slots 2)))
          (when (member target '("native-mod-general-inline" "native-truncate-primary-inline") :test #'equal)
            (setf call-slots (max call-slots 2)))
          (when (string= target "native-character-code-at-inline") (setf call-slots (max call-slots 2)))
          (when (string= target "native-character-code-at64-inline")
            (setf integer-call t call-slots (max call-slots 1)))
          (when (string= target "native-character-code-inline") (setf call-slots (max call-slots 1)))
          (when (equal (cdr (assoc target miricl-compiler::*native-imports* :test #'equal))
                       '((:object :i64 :i64) :object))
            (setf integer-call t call-slots (max call-slots 1)))
          (when (string= target "native-character-ref") (setf call-slots (max call-slots 3)))))
      (setf vector-index (+ 5 (length locals)) vector-object (1+ vector-index) vector-value (+ 2 vector-index)
            call-base (+ vector-index (if vector-write 3 (if vector-read 1 0)))
            integer-left (+ call-base call-slots) integer-right (1+ integer-left)
            locals (nreverse locals))
      (with-output-to-string (out)
        (format out "(func ~A (param~{ ~A~}) (result ~A)~%" name
          (make-list arity :initial-element "(ref null eq)")
          (if (eq result :truth) "i32" "(ref null eq)"))
        (format out "(local~{ ~A~})~%" (append (make-list (- 5 arity) :initial-element "(ref null eq)")
                                                        locals
                                                        (when (or vector-read vector-write) '("i32"))
                                                        (when vector-write '("(ref null eq)" "(ref null eq)"))
                                                        (make-list call-slots :initial-element "(ref null eq)")
                                                        (when integer-call '("i64" "i64"))))
        (labels ((return-next-p (index)
                   (loop for i from (1+ index) below (length ops)
                         for next = (aref ops i) for code = (miricl-compiler::native-op-code next)
                         do (cond
                              ((and (= code #x10) (< (miricl-compiler::native-op-argument next) (length imports))
                                    (member (second (aref imports (miricl-compiler::native-op-argument next)))
                                      '("native-poll" "native-return1") :test #'equal)))
                              (t (return (or (= code #x0f)
                                             (and (= code #x0b) (= (1+ i) (length ops))))))))))
        (loop for op across ops for index from 0 for code = (miricl-compiler::native-op-code op)
              for argument = (miricl-compiler::native-op-argument op)
              for last = (= (miricl-compiler::native-op-end op) (length body)) do
          (cond
            ((gethash index literal-skips))
            ((nth-value 1 (gethash index literal-replacements))
             (let ((replacement (gethash index literal-replacements)))
               (if (eq (car replacement) :symbol)
                   (format out "i32.const ~D call $symbol~%" (cdr replacement))
                   (format out "i64.const ~D call $int64~%" (cdr replacement)))))
            ((and (= code #x0b) last)
             (when (and (eq result :truth) (not raw-truth)) (write-line "ref.is_null i32.eqz" out)))
            ((= code #x0f)
             (when (and (eq result :truth) (not raw-truth)) (write-line "ref.is_null i32.eqz" out))
             (write-line "return" out) (setq raw-truth nil))
            ((member code '(#x20 #x21 #x22))
             (format out "~A ~D~%" (case code (#x20 "local.get") (#x21 "local.set") (#x22 "local.tee"))
               (cond ((zerop argument) arity) ((<= argument arity) (1- argument)) (t argument))))
            ((member code '(#x10 #x12 #xd2))
             (unless (and (= code #x10) (< argument (length imports)))
               (error "Runtime kernel retains a general or indirect call."))
             (let ((target (second (aref imports argument))))
               (cond
                 ;; Raw callers own value state. Preserve backedge polling
                 ;; for interruptible kernels; the other kernels explicitly
                 ;; match their original non-suspending primitive contract.
                 ((string= target "native-poll")
                  (when interruptible (write-line "call $thread-checkpoint" out)))
                 ((string= target "native-return1"))
                 ((string= target "native-boolean")
                  (if (and (eq result :truth) (return-next-p index))
                      (progn (write-line "i32.eqz i32.eqz" out) (setq raw-truth t))
                      (write-line "call $bool" out)))
                 ((member target '("native-car-list-inline" "native-car-inline-checked") :test #'equal)
                  (write-line "call $car" out))
                 ((member target '("native-cdr-list-inline" "native-cdr-inline-checked") :test #'equal)
                  (write-line "call $cdr" out))
                 ((string= target "native-eq-inline") (write-line "ref.eq call $bool" out))
                 ((string= target "native-eql-inline") (write-line "call $eql call $bool" out))
                 ((string= target "native-eql-test") (write-line "call $eql" out))
                 ((string= target "native-cons") (write-line "call $cons" out))
                 ((string= target "native-function-reference") (write-line "call $fdefinition" out))
                 ((string= target "native-environment") (write-line "ref.null eq" out))
                 ((string= target "native-check-apply-list") (write-line "call $check-apply-list" out))
                 ((string= target "native-call")
                  (format out "local.set ~D local.set ~D drop local.get ~D local.get ~D call $apply call $value-list call $clear-values~%"
                    (1+ call-base) call-base call-base (1+ call-base)))
                 ((string= target "native-primary") (write-line "call $car" out))
                 ((string= target "native-argument") (write-line "call $int call $native-argument-cl" out))
                 ((string= target "native-character-code-at64-inline")
                  (format out "local.set ~D local.set ~D~%local.get ~D extern.convert_any local.get ~D call $~A any.convert_extern ref.cast (ref null eq)~%"
                    integer-left call-base call-base integer-left target))
                 ((equal (cdr (assoc target miricl-compiler::*native-imports* :test #'equal))
                         '((:object :i64 :i64) :object))
                  ;; Preserve the resolved function guard and full scalar
                  ;; operands when a newly proved range selects this ABI.
                  (format out "local.set ~D local.set ~D local.set ~D~%local.get ~D extern.convert_any local.get ~D local.get ~D call $~A any.convert_extern ref.cast (ref null eq)~%"
                    integer-right integer-left call-base call-base integer-left integer-right target))
                 ((member target '("native-call0" "native-call1" "native-call2" "native-call3" "native-call4" "native-character-code" "native-character-ref" "native-character-ref-inline" "native-character-code-inline" "native-character-code-at-inline") :test #'equal)
                  (let ((count (cond ((string= target "native-character-code") 2)
                                     ((string= target "native-character-ref") 3)
                                     ((string= target "native-character-code-inline") 1)
                                     ((string= target "native-character-ref-inline") 2)
                                     ((string= target "native-character-code-at-inline") 2)
                                     (t (1+ (position target '("native-call0" "native-call1" "native-call2" "native-call3" "native-call4") :test #'equal))))))
                    (loop for slot downfrom (1- count) to 0 do (format out "local.set ~D~%" (+ call-base slot)))
                    (dotimes (slot count) (format out "local.get ~D extern.convert_any~%" (+ call-base slot)))
                    (format out "call $~A any.convert_extern ref.cast (ref null eq)~%" target)))
                 ((string= target "native-int64") (write-line "call $int64" out))
                 ((string= target "native-unbox-int64") (write-line "ref.as_non_null call $scalar-integer-value" out))
                 ;; These imports already represent proven simple-vector
                 ;; operations. Lower their access directly in the core ABI;
                 ;; extra helper calls substantially slow the swap loop.
                 ((string= target "native-vector-ref-inline")
                  (format out "i32.wrap_i64 local.set ~D call $vector-data local.get ~D array.get $objects~%"
                    vector-index vector-index))
                 ((string= target "native-vector-set-inline")
                  (format out "i32.wrap_i64 local.set ~D local.set ~D local.set ~D~%"
                    vector-index vector-object vector-value)
                  (format out "local.get ~D call $vector-data local.get ~D local.get ~D array.set $objects local.get ~D~%"
                    vector-object vector-index vector-value vector-value))
                 ((string= target "native-vector-length-inline") (write-line "call $vector-data array.len call $int" out))
                 ((string= target "native-sbit-ref-inline")
                  (write-line "call $checked-sbit-ref" out))
                 ((string= target "native-row-major-ref-inline")
                  (write-line "ref.null eq i32.const 0 call $array-element" out))
                 ((string= target "native-row-major-set-inline")
                  (write-line "call $checked-row-major-set" out))
                 ((string= target "native-length-inline")
                  (write-line "extern.convert_any call $native-length-inline any.convert_extern ref.cast (ref null eq)" out))
                 ((string= target "native-list-length-inline")
                  (write-line "call $list-length-native" out))
                 ((member target '("native-consp-inline" "native-consp-test") :test #'equal)
                  (write-line "call $tag i32.const 7 i32.eq" out)
                  (when (string= target "native-consp-inline") (write-line "call $bool" out)))
                 ((string= target "native-simple-vector-p-inline")
                  (write-line "call $tag i32.const 34 i32.eq call $bool" out))
                 ((string= target "native-vector-p-inline")
                  (write-line "call $vectorp-native call $bool" out))
                 ((string= target "native-string-p-inline")
                  (write-line "call $stringp-native call $bool" out))
                 ((string= target "native-integer-p-inline")
                  (write-line "call $tag i32.const 1 i32.eq call $bool" out))
                 ((string= target "native-character-p-inline")
                  (write-line "call $tag i32.const 16 i32.eq call $bool" out))
                 ((string= target "native-integer-length-inline")
                  (write-line "i32.const 1 call $integer-bit-measure" out))
                 ((string= target "native-ordered-compare-inline")
                  (write-line "call $ordered-compare-native" out))
                 ((string= target "native-zero-inline")
                  (write-line "call $number call $zero call $bool" out))
                 ((string= target "native-ash-general-inline")
                  (write-line "call $ash-native" out))
                 ((string= target "native-logand-general-inline")
                  (format out "local.set ~D local.set ~D i32.const 8 local.get ~D local.get ~D i32.const 0 call $boole-native~%"
                    (1+ call-base) call-base call-base (1+ call-base)))
                 ((member target '("native-code-character-inline" "native-make-character-vector-inline" "native-make-string-inline") :test #'equal)
                  (format out "extern.convert_any call $~A any.convert_extern ref.cast (ref null eq)~%" target))
                 ((string= target "native-make-vector-inline")
                  (write-line "call $make-vector-native" out))
                 ((string= target "native-character-set-inline")
                  (format out "local.set ~D local.set ~D local.set ~D local.get ~D extern.convert_any local.get ~D extern.convert_any local.get ~D extern.convert_any call $native-character-set-inline any.convert_extern ref.cast (ref null eq)~%"
                    (+ call-base 2) (1+ call-base) call-base call-base (1+ call-base) (+ call-base 2)))
                 ((string= target "native-simple-character-set-inline")
                  (format out "local.set ~D local.set ~D local.set ~D local.get ~D extern.convert_any local.get ~D extern.convert_any local.get ~D call $native-simple-character-set-inline any.convert_extern ref.cast (ref null eq)~%"
                    integer-left (1+ call-base) call-base call-base (1+ call-base) integer-left))
                 ((member target '("native-mod-general-inline" "native-truncate-primary-inline") :test #'equal)
                  (format out "local.set ~D local.set ~D local.get ~D extern.convert_any local.get ~D extern.convert_any call $~A any.convert_extern ref.cast (ref null eq)~%"
                    (1+ call-base) call-base call-base (1+ call-base) target))
                 (t (error "Unsupported runtime kernel dependency: ~S." target)))))
            ((= code #xd0)
             (if (and (eq result :truth) (return-next-p index))
                 (progn (write-line "i32.const 0" out) (setq raw-truth t))
                 (write-line "ref.null eq" out)))
            ((= code #xfb)
             ;; Change the module's external reference view to the core's
             ;; raw eqref ABI. All kernel inputs and permitted primitives use
             ;; this representation, so these view conversions are identities.
             (let ((position (1+ (miricl-compiler::native-op-start op))) (subcode nil))
               (multiple-value-setq (subcode position) (miricl-compiler::native-read-uleb body position))
               (case subcode
                 ((#x1a #x1b))
                 (#x1c (write-line "ref.i31" out))
                 (#x1d (write-line "i31.get_s" out))
                 (#x14
                  (unless (= (miricl-compiler::native-read-sleb body position) 1)
                    (error "Unsupported runtime heap type test."))
                  (write-line "ref.test (ref $obj)" out))
                 ((2 5) (let ((type nil) (field nil))
                      (multiple-value-setq (type position) (miricl-compiler::native-read-uleb body position))
                      (multiple-value-setq (field position) (miricl-compiler::native-read-uleb body position))
                      (unless (= type 1) (error "Unsupported runtime field type: ~D." type))
                      (when (and (= subcode 5) (not (member field '(4 5))))
                        (error "Runtime kernel writes a non-cons field: ~D." field))
                      (format out "struct.~A $obj ~D~%" (if (= subcode 5) "set" "get") field)))
                 ((#x16 #x17)
                  (let ((type (miricl-compiler::native-read-sleb body position)))
                    (cond ((= type 1) (format out "ref.cast (ref ~A$obj)~%" (if (= subcode #x17) "null " "")))
                          ((= type -20) (format out "ref.cast (ref ~Ai31)~%" (if (= subcode #x17) "null " "")))
                          ((and (= subcode #x17) (= type -19)))
                          (t (error "Unsupported runtime reference cast: ~D." type)))))
                 (otherwise (error "Unsupported runtime GC operation: ~D." subcode)))))
            (t (write-line (miricl-compiler::native-disassembly-instruction op body imports) out)))))
        (write-line ")" out))))))

(defun runtime-kernel-wat (wat-name entry &key (result :object) interruptible)
  "Cross-compile one single-valued internal runtime kernel.
Compile its closed helper group with inlining; link only the verified worker.
INTERRUPTIBLE preserves compiler backedge polls as core scheduler checkpoints.
Fixed bootstrap symbols and ordinary Lisp calls are linked explicitly.
Unsupported dependencies and captures are errors, never fallbacks."
  ;; The cache tracks replacements of this linker entry. Same-file calls may
  ;; otherwise retain the compiled definition on hosts such as ECL.
  (declare (notinline kernel-worker-wat kernel-macroexpand-hook))
  (unless (member result '(:object :truth)) (error "Unsupported runtime result ABI: ~S." result))
  (let ((backend (kernel-backend)))
    (unless (equal backend *kernel-backend*)
      (clrhash *kernel-cache*) (setq *kernel-backend* backend)))
  (let* ((definitions (kernel-definitions)) (key (list wat-name entry result interruptible definitions)))
    (or (gethash key *kernel-cache*)
        (setf (gethash key *kernel-cache*)
          (let ((miricl-compiler::*compiler-state* :interpreted)
                (miricl-compiler::*hir-load-time-evaluator* #'require-target-load-time-evaluation)
                (*macroexpand-hook* (fdefinition 'kernel-macroexpand-hook)))
            (let* ((unit (miricl-compiler:compile-module-to-hir definitions :entry-points (list entry)))
                   (children (rest (miricl-compiler:hir-unit-functions unit))))
              (unless (and children (null (cdr children)))
                (error "Runtime helper group did not inline into one entry: ~S." entry))
              (when (miricl-compiler::hir-function-captures (first children))
                (error "Runtime kernel retains lexical captures: ~S." entry))
              ;; The discarded factory owns module definitions, not runtime
              ;; state. Promote the closed entry so its literals belong to it.
              (setf (miricl-compiler::hir-function-parent (first children)) nil
                    (miricl-compiler:hir-unit-functions unit) children)
              (let ((arity (miricl-compiler::native-fixed-arity (first children))))
                (unless arity (error "Runtime kernel requires fixed arity: ~S." entry))
                (multiple-value-bind (bytes literals)
                    (miricl-compiler:emit-native-module unit :entries children)
                  (kernel-worker-wat bytes (kernel-export-index bytes (format nil "direct-entry0-~D" arity))
                    (miricl:wat-string wat-name) arity result interruptible (first literals))))))))))
