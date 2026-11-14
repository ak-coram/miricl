(in-package #:miricl-test)
(in-suite :miricl)
(defvar *cross-helper-output* nil)

(defun cross-helper-wat (definitions entry &key (result :object) interruptible (name "$entry") defer)
  (let ((miricl-compiler::*compiler-state* :interpreted)
        (*macroexpand-hook* #'miricl-cross::kernel-macroexpand-hook))
    (let ((wat (miricl-cross::kernel-helper-group-wat
                 (miricl-compiler:compile-module-to-hir definitions :entry-points (list entry) :inline nil)
                 entry name result interruptible defer)))
      (when *cross-helper-output* (format *cross-helper-output* "~%;; ~S~%~A" entry wat))
      wat)))

(defun cross-helper-module (worker probes)
  ;; A small checked core model, using the actual generated worker bodies.
  ;; Pending multiple values deliberately start dirty before every probe.
  (wt:wat-to-wasm
    (format nil "(module
      (type $obj (struct (field i32) (field (mut i32)) (field f64)
        (field (ref null eq)) (field (mut (ref null eq)))
        (field (mut (ref null eq))) (field (mut (ref null eq)))))
      (global $count (mut i32) (i32.const 2))
      (global $extra (mut (ref null eq)) (ref.null eq))
      (global $polls (mut i32) (i32.const 0))
      (func $thread-checkpoint (global.set $polls (i32.add (global.get $polls) (i32.const 1))))
      (func $clear-values (global.set $count (i32.const 1)) (global.set $extra (ref.null eq)))
      (func $int64 (param i64) (result (ref null eq)) local.get 0 i32.wrap_i64 ref.i31)
      (func $symbol (param i32) (result (ref null eq)) local.get 0 ref.i31)
      (func $fdefinition (param (ref null eq)) (result (ref null eq))
        i32.const 8 local.get 0 ref.cast (ref i31) i31.get_s i32.const 1 i32.add
        f64.const 0 ref.null eq ref.null eq ref.null eq ref.null eq struct.new $obj)
      (func $kernel-function-reference (param (ref null eq)) (result (ref null eq))
        local.get 0 call $fdefinition)
      (func $native-call1 (param externref externref) (result externref)
        local.get 0 any.convert_extern ref.cast (ref $obj) struct.get $obj 1
        i32.const ~D i32.ne if unreachable end
        local.get 1 any.convert_extern ref.cast (ref i31) i31.get_s
        i32.const 1 i32.add ref.i31 extern.convert_any)
      (func $native-call2 (param externref externref externref) (result externref) unreachable)
      (func $native-add-int64-checked (param externref i64 i64) (result externref)
        local.get 1 local.get 2 i64.add i32.wrap_i64 ref.i31 extern.convert_any)
      (func $native-sub-int64-checked (param externref i64 i64) (result externref)
        local.get 1 local.get 2 i64.sub i32.wrap_i64 ref.i31 extern.convert_any)
      (func $bool (param i32) (result (ref null eq)) local.get 0
        if (result (ref null eq)) i32.const 1 ref.i31 else ref.null eq end)
      (func $scalar-integer-value (param (ref eq)) (result i64)
        local.get 0 ref.cast (ref i31) i31.get_s i64.extend_i32_s)
      (func $cons (param (ref null eq) (ref null eq)) (result (ref null eq))
        i32.const 7 i32.const 0 f64.const 0 ref.null eq local.get 0 local.get 1 ref.null eq struct.new $obj)
      (func $car (param (ref null eq)) (result (ref null eq))
        local.get 0 ref.is_null if (result (ref null eq)) ref.null eq
        else local.get 0 ref.cast (ref $obj) struct.get $obj 4 end)
      (func $cdr (param (ref null eq)) (result (ref null eq))
        local.get 0 ref.is_null if (result (ref null eq)) ref.null eq
        else local.get 0 ref.cast (ref $obj) struct.get $obj 5 end)
      (func $return-values (param $values (ref null eq)) (result (ref null eq))
        (local $scan (ref null eq))
        i32.const 0 global.set $count
        local.get $values local.set $scan
        block $done loop $next
          local.get $scan ref.is_null br_if $done
          global.get $count i32.const 1 i32.add global.set $count
          local.get $scan call $cdr local.set $scan br $next
        end end
        local.get $values call $cdr global.set $extra
        local.get $values call $car)
      (func (export ~S) (result i32) global.get $count)
      (func (export ~S) (result i32) global.get $extra ref.is_null)
      (func (export ~S) (result i32) global.get $polls)
      ~A ~A)" (1+ (miricl::runtime-symbol-id '1+))
      "value-count" "no-extra-values" "polls" worker probes)))

(defun call-cross-helper-probes (bytes callback)
  (wt:with-engine (engine)
    (wt:with-store ((store context) engine)
      (let ((compiled (wt:compile-module engine bytes)))
        (unwind-protect
            (wt:with-instance (instance context compiled)
              (funcall callback (lambda (name &rest args)
                                  (apply #'wt:instance-call context instance name args))))
          (wt::wasmtime-module-delete compiled))))))

(test runtime-kernel-module-helper-sharing
  (let* ((definitions
           '((defun shared-recursive-helper (n cell)
               (declare (type (integer 0 100) n) (type cons cell)
                        (optimize (safety 0) (debug 0))
                        (inline zerop 1- 1+ car rplaca))
               (if (zerop n) (car cell)
                   (progn (rplaca cell (1+ (the fixnum (car cell))))
                          (shared-recursive-helper (1- n) cell))))
             (defun shared-first-entry (n cell)
               (declare (optimize (debug 0)))
               (values (shared-recursive-helper n cell)))
             (defun shared-second-entry (n cell)
               (declare (optimize (debug 0)) (inline 1+))
               (values (1+ (the fixnum (shared-recursive-helper n cell)))))))
         (first (cross-helper-wat definitions 'shared-first-entry
                  :name "$first" :interruptible t :defer t))
         (second (cross-helper-wat definitions 'shared-second-entry
                   :name "$second" :interruptible t :defer t))
         (probes "(func (export \"first\") (param i32) (result i32)
                    local.get 0 ref.i31 i32.const 0 ref.i31 ref.null eq call $cons
                    call $first ref.cast (ref i31) i31.get_s)
                  (func (export \"second\") (param i32) (result i32)
                    local.get 0 ref.i31 i32.const 0 ref.i31 ref.null eq call $cons
                    call $second ref.cast (ref i31) i31.get_s)"))
    (labels ((link (share)
               (let ((miricl-cross::*kernel-shared-workers*
                       (when share (make-hash-table :test 'equal))))
                 (concatenate 'string (miricl-cross::kernel-link-helper-group first)
                                      (miricl-cross::kernel-link-helper-group second)))))
      (let* ((ordinary (cross-helper-module (link nil) probes))
             (shared-wat (link t))
             (shared (cross-helper-module shared-wat probes)))
        (is (< (length shared) (length ordinary)))
        ;; Cached groups must work in fresh assemblies without retained names
        ;; or ownership from the previous module.
        (is (string= shared-wat (link t)))
        (dolist (bytes (list ordinary shared))
          (call-cross-helper-probes bytes
            (lambda (call)
              (is (= 5 (funcall call "first" 5)))
              (is (= 6 (funcall call "second" 5)))
              (is (= 1 (funcall call "value-count")))
              (is (= 1 (funcall call "no-extra-values")))
              (is (plusp (funcall call "polls"))))))))))

(test runtime-kernel-helper-sharing-resolved-literals
  (labels ((group (literal name)
             (cross-helper-wat
               `((defun literal-leaf (object)
                   (declare (optimize (debug 0)))
                   (values (if object ',literal nil)))
                 (defun literal-parent (object)
                   (declare (optimize (debug 0)))
                   (values (literal-leaf object)))
                 (defun literal-entry (object)
                   (declare (optimize (debug 0)))
                   (values (literal-parent object))))
               'literal-entry :name name :defer t)))
    (let* ((first (group 'car "$first")) (second (group 'cdr "$second"))
           (miricl-cross::*kernel-shared-workers* (make-hash-table :test 'equal))
           (workers (concatenate 'string (miricl-cross::kernel-link-helper-group first)
                                        (miricl-cross::kernel-link-helper-group second)))
           (bytes (cross-helper-module workers
                    "(func (export \"first\") (result i32)
                       i32.const 1 ref.i31 call $first ref.cast (ref i31) i31.get_s)
                     (func (export \"second\") (result i32)
                       i32.const 1 ref.i31 call $second ref.cast (ref i31) i31.get_s)")))
      (call-cross-helper-probes bytes
        (lambda (call)
          (is (= (miricl::runtime-symbol-id 'car) (funcall call "first")))
          (is (= (miricl::runtime-symbol-id 'cdr) (funcall call "second"))))))))

(test runtime-kernel-retained-closed-helpers
  ;; Mutation travels through an explicit shared argument. The recursive
  ;; lexical binding remains sealed even if the corresponding global changes.
  (let* ((definitions
           '((defun kernel-recursive-helper (n cell)
               (declare (type (integer 0 100) n) (type cons cell)
                        (optimize (safety 0) (debug 0))
                        (inline zerop 1- 1+ car rplaca))
               (if (zerop n) (car cell)
                   (progn (rplaca cell (1+ (the fixnum (car cell))))
                          (kernel-recursive-helper (1- n) cell))))
             (defun kernel-recursive-entry (n cell)
               (declare (type (integer 0 100) n) (type cons cell)
                        (optimize (safety 0) (debug 0)) (inline +))
               (values (+ (the fixnum (kernel-recursive-helper n cell))
                          (the fixnum (kernel-recursive-helper n cell)))))))
         (worker (cross-helper-wat definitions 'kernel-recursive-entry :interruptible t))
         (bytes (cross-helper-module worker
                  "(func (export \"run\") (param i32) (result i32)
                     (local $cell (ref null eq))
                     i32.const 0 ref.i31 ref.null eq call $cons local.set $cell
                     local.get 0 ref.i31 local.get $cell call $entry
                     ref.cast (ref i31) i31.get_s
                     local.get $cell call $car ref.cast (ref i31) i31.get_s i32.add)")))
    (is (search "return_call" worker))
    (call-cross-helper-probes bytes
      (lambda (call)
        (is (= 15 (funcall call "run" 3)))
        (is (= 0 (funcall call "run" 0)))
        (is (= 1 (funcall call "value-count")))
        (is (= 1 (funcall call "no-extra-values")))
        (is (>= (funcall call "polls") (* 2 3)))))))

(test runtime-kernel-helper-literals-and-truth
  (let* ((worker (cross-helper-wat
                   '((defun kernel-literal-first (flag)
                       (declare (optimize (debug 0))) (values (if flag 'integer 42)))
                     (defun kernel-literal-second (flag)
                       (declare (optimize (debug 0))) (values (if flag 7 'list)))
                     (defun kernel-literal-entry (flag)
                       (declare (optimize (safety 0) (debug 0)) (inline cons))
                       (values (cons (kernel-literal-first flag) (kernel-literal-second flag)))))
                   'kernel-literal-entry))
         (bytes (cross-helper-module worker
                  "(func (export \"first\") (result i32)
                     i32.const 1 ref.i31 call $entry call $car ref.cast (ref i31) i31.get_s)
                   (func (export \"second\") (result i32)
                     ref.null eq call $entry call $cdr ref.cast (ref i31) i31.get_s)")))
    (call-cross-helper-probes bytes
      (lambda (call)
        (is (= (miricl::runtime-symbol-id 'integer) (funcall call "first")))
        (is (= (miricl::runtime-symbol-id 'list) (funcall call "second"))))))
  (let* ((worker (cross-helper-wat
                   '((defun kernel-truth-helper (flag)
                       (declare (optimize (debug 0))) (values flag))
                     (defun kernel-truth-entry (flag)
                       (declare (optimize (debug 0))) (values (kernel-truth-helper flag))))
                   'kernel-truth-entry :result :truth))
         (bytes (cross-helper-module worker
                  "(func (export \"yes\") (result i32) i32.const 7 ref.i31 call $entry)
                   (func (export \"no\") (result i32) ref.null eq call $entry)")))
    (call-cross-helper-probes bytes
      (lambda (call) (is (= 1 (funcall call "yes"))) (is (= 0 (funcall call "no")))))))

(test runtime-kernel-helper-malformed-dependencies
  (let* ((miricl-compiler::*compiler-state* :interpreted)
         (*macroexpand-hook* #'miricl-cross::kernel-macroexpand-hook)
         (unit (miricl-compiler:compile-module-to-hir
                 '((defun kernel-initialized-helper (x) (declare (optimize (debug 0))) (values x))
                   (defun kernel-initialized-entry (x) (declare (optimize (debug 0)))
                     (values (kernel-initialized-helper x))))
                 :entry-points '(kernel-initialized-entry) :inline nil))
         (factory (first (miricl-compiler:hir-unit-functions unit))))
    ;; Structurally valid public HIR can still violate the closed-binding
    ;; contract: replace its initialization with an ordinary NIL value.
    (dolist (block (miricl-compiler::hir-function-blocks factory))
      (dolist (instruction (miricl-compiler::hir-block-instructions block))
        (when (eq (miricl-compiler::hir-instruction-opcode instruction) :cell-write)
          (let* ((cell (first (miricl-compiler::hir-instruction-inputs instruction)))
                 (initial (first (miricl-compiler::hir-instruction-inputs
                                   (miricl-compiler::hir-value-producer cell)))))
            (setf (second (miricl-compiler::hir-instruction-inputs instruction)) initial)))))
    (signals miricl-compiler::compiler-input-error
      (miricl-cross::kernel-closed-helper-unit unit 'kernel-initialized-entry)))
  (signals miricl-compiler::compiler-input-error
    (cross-helper-wat
      '((defun kernel-captured-entry (object)
          (declare (optimize (debug 0)))
          (flet ((change () (setq object (cons nil object))))
            (declare (notinline change))
            (values (change))))) 'kernel-captured-entry)))

(test runtime-kernel-helper-primary-values
  (let* ((definitions
           '((defun kernel-values-helper (flag)
               (declare (optimize (safety 0) (debug 0)))
               (if flag (values 17 29) (values)))
             (defun kernel-values-entry (flag)
               (declare (optimize (safety 0) (debug 0)))
               (values (or (kernel-values-helper flag) 0)))))
         (worker (cross-helper-wat definitions 'kernel-values-entry))
         (bytes (cross-helper-module worker
                  "(func (export \"some\") (result i32)
                     i32.const 1 ref.i31 call $entry ref.cast (ref i31) i31.get_s)
                   (func (export \"none\") (result i32)
                     ref.null eq call $entry ref.cast (ref i31) i31.get_s)")))
    (call-cross-helper-probes bytes
      (lambda (call)
        (is (= 17 (funcall call "some")))
        (is (= 1 (funcall call "value-count")))
        (is (= 1 (funcall call "no-extra-values")))
        (is (= 0 (funcall call "none")))
        (is (= 1 (funcall call "value-count")))
        (is (= 1 (funcall call "no-extra-values")))))))

(test runtime-kernel-helper-boundaries-and-recovery
  (let ((prefix '((defun kernel-boundary-helper (object)
                   (declare (optimize (safety 3) (debug 0)) (inline car))
                   (values (car object))))))
    ;; Escaping identity and a full-value consumer retain the ordinary ABI;
    ;; this bounded raw linker rejects them before producing executable code.
    (dolist (body '((cons #'kernel-boundary-helper object)
                    (multiple-value-call #'list (kernel-boundary-helper object))
                    (kernel-boundary-helper object object)))
      (signals error
        (cross-helper-wat
          (append prefix `((defun kernel-boundary-entry (object)
                             (declare (optimize (safety 3) (debug 0)) (inline cons))
                             ,body))) 'kernel-boundary-entry)))
    (let* ((worker (cross-helper-wat
                     (append prefix '((defun kernel-boundary-entry (object)
                                        (declare (optimize (safety 3) (debug 0)))
                                        (values (kernel-boundary-helper object)))))
                     'kernel-boundary-entry))
           (bytes (cross-helper-module worker
                    "(func (export \"bad\") (result i32)
                       i32.const 7 ref.i31 call $entry ref.cast (ref i31) i31.get_s)
                     (func (export \"good\") (result i32)
                       i32.const 42 ref.i31 ref.null eq call $cons call $entry
                       ref.cast (ref i31) i31.get_s)")))
      (call-cross-helper-probes bytes
        (lambda (call)
          (signals error (funcall call "bad"))
          (is (= 42 (funcall call "good")))))))
  ;; Cyclic source structure fails at the checked compiler boundary and does
  ;; not damage subsequent compilations.
  (let ((bad (list 'defun 'kernel-bad-entry nil nil)))
    (setf (fourth bad) bad)
    (signals miricl-compiler::compiler-input-error
      (miricl-cross:compile-module (list bad) :entry-points '(kernel-bad-entry))))
  (is (stringp (cross-helper-wat
                 '((defun kernel-recovery-helper (object)
                     (declare (optimize (debug 0))) (values object))
                   (defun kernel-recovery-entry (object)
                     (declare (optimize (debug 0)))
                     (values (kernel-recovery-helper object)))) 'kernel-recovery-entry))))
