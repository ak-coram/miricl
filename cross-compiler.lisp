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

(defun kernel-backend ()
  ;; REPL reloads can change an emitter or proof helper while kernel source
  ;; stays unchanged. Cache against the actual host definitions, not a build
  ;; timestamp, and discard old generations instead of retaining their code.
  (list (copy-tree miricl-compiler::*native-imports*)
    (loop for name in miricl-compiler::*native-compiler-seeds*
          when (and (fboundp name) (not (macro-function name)))
            collect (cons name (fdefinition name)))))

(defun kernel-definitions ()
  (with-open-file (stream (asdf:system-relative-pathname :miricl "runtime-kernel-library.lisp"))
    (let ((*package* (find-package :miricl-runtime)) (*read-eval* nil))
      (loop for form = (read stream nil stream) until (eq form stream)
            when (and (consp form) (eq (car form) 'defun)) collect form))))

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

(defun kernel-worker-wat (bytes index name arity result interruptible)
  (multiple-value-bind (body imports offset signature)
      (miricl-compiler::native-disassembly-body bytes index)
    (declare (ignore offset))
    (unless (equal signature '(("externref" "externref" "externref" "externref" "externref") ("externref")))
      (error "Unsupported runtime worker signature: ~S." signature))
    (let* ((ops (miricl-compiler::native-read-ops body))
           (input (miricl-compiler::wasm-input body)) (locals nil) (raw-truth nil))
      (dotimes (i (miricl-compiler::module-uleb input))
        (let ((count (miricl-compiler::module-uleb input))
              (type (miricl-compiler::module-byte input)))
          (unless (member type '(#x7f #x7e #x6f)) (error "Unsupported runtime local type: ~D." type))
          (dotimes (j count) (push (case type (#x7f "i32") (#x7e "i64") (otherwise "(ref null eq)")) locals))))
      ;; An entry has no closure object or padded arguments. Prove those
      ;; private ABI inputs dead before changing their slots into zeroed locals.
      (miricl-compiler::native-op-liveness ops (+ 5 (length locals)) nil)
      (let ((live (miricl-compiler::native-op-live-in (aref ops 0))))
        (when (or (logbitp 0 live)
                  (loop for i from (1+ arity) below 5 thereis (logbitp i live)))
          (error "Runtime kernel depends on a closure or padded argument: ~S." name)))
      (with-output-to-string (out)
        (format out "(func ~A (param~{ ~A~}) (result ~A)~%" name
          (make-list arity :initial-element "(ref null eq)")
          (if (eq result :truth) "i32" "(ref null eq)"))
        (format out "(local~{ ~A~})~%" (append (make-list (- 5 arity) :initial-element "(ref null eq)")
                                                        (nreverse locals)))
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
                 ((string= target "native-int64") (write-line "call $int64" out))
                 ((string= target "native-unbox-int64") (write-line "ref.as_non_null call $scalar-integer-value" out))
                 ((member target '("native-consp-inline" "native-consp-test") :test #'equal)
                  (write-line "call $tag i32.const 7 i32.eq" out)
                  (when (string= target "native-consp-inline") (write-line "call $bool" out)))
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
                          ((and (= subcode #x17) (= type -19)))
                          (t (error "Unsupported runtime reference cast: ~D." type)))))
                 (otherwise (error "Unsupported runtime GC operation: ~D." subcode)))))
            (t (write-line (miricl-compiler::native-disassembly-instruction op body imports) out)))))
        (write-line ")" out)))))

(defun runtime-kernel-wat (wat-name entry &key (result :object) interruptible)
  "Cross-compile one single-valued internal runtime kernel.
Compile its closed helper group with inlining; link only the verified worker.
INTERRUPTIBLE preserves compiler backedge polls as core scheduler checkpoints.
Unsupported captures, literals and general calls are errors, never fallbacks."
  (unless (member result '(:object :truth)) (error "Unsupported runtime result ABI: ~S." result))
  (let ((backend (kernel-backend)))
    (unless (equal backend *kernel-backend*)
      (clrhash *kernel-cache*) (setq *kernel-backend* backend)))
  (let* ((definitions (kernel-definitions)) (key (list wat-name entry result interruptible definitions)))
    (or (gethash key *kernel-cache*)
        (setf (gethash key *kernel-cache*)
          (let ((miricl-compiler::*compiler-state* :interpreted)
                (miricl-compiler::*hir-load-time-evaluator* #'require-target-load-time-evaluation)
                (*macroexpand-hook* #'require-target-macro-expansion))
            (let* ((unit (miricl-compiler:compile-module-to-hir definitions :entry-points (list entry)))
                   (children (rest (miricl-compiler:hir-unit-functions unit))))
              (unless (and children (null (cdr children)))
                (error "Runtime helper group did not inline into one entry: ~S." entry))
              (let ((arity (miricl-compiler::native-fixed-arity (first children))))
                (unless arity (error "Runtime kernel requires fixed arity: ~S." entry))
                (multiple-value-bind (bytes literals)
                    (miricl-compiler:emit-native-module unit :entries children)
                  (unless (every (lambda (group) (zerop (length group))) literals)
                    (error "Runtime kernel retains literals: ~S." literals))
                  (kernel-worker-wat bytes (kernel-export-index bytes (format nil "direct-entry0-~D" arity))
                    (miricl:wat-string wat-name) arity result interruptible)))))))))
