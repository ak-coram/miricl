;;;; Private callback contracts, beyond ANSI. The small substrate distinguishes
;;;; saved and replacement callbacks and observes scheduler/value-state effects.
;;;; Loaded after MIRICL by scripts/check-host-compilation.lisp.
(let* ((directory (merge-pathnames
                   (format nil "miricl-kernel-callbacks-~D-~D/" (get-universal-time) (random most-positive-fixnum))
                   (uiop:temporary-directory)))
       (primitive 'miricl::%%generic-dispatch-metadata)
       (descriptor (list (list 0 primitive)))
       (manifest nil))
  (ensure-directories-exist (merge-pathnames "fixture" directory))
  (labels ((rejected (object)
             (assert (handler-case
                       (progn (miricl-cross::kernel-fixed-callback-descriptor object) nil)
                       (miricl-compiler::compiler-input-error () t)))
             ;; A rejection must leave later requests usable.
             (assert (equal descriptor (miricl-cross::kernel-fixed-callback-descriptor descriptor))))
           (fixture (name definitions runs &optional (contract descriptor) supplied-text)
             (let* ((unit (unless supplied-text
                            (miricl-compiler:compile-module-to-hir definitions :entry-points '(callback-test-entry) :inline nil)))
                    (text (or supplied-text (miricl-cross::kernel-helper-group-wat unit 'callback-test-entry "$entry" :object nil nil contract)))
                    (image (merge-pathnames (format nil "~A.wasm" name) directory)))
               (with-open-file (out image :direction :output :element-type '(unsigned-byte 8))
                 (write-sequence (wasmtime:wat-to-wasm
                   (concatenate 'string
"(module
 (global $thread-active (export \"active\") (mut i32) (i32.const 1))
 (global $polls (export \"polls\") (mut i32) (i32.const 0))
 (global $clears (export \"clears\") (mut i32) (i32.const 0))
 (global $direct (export \"direct\") (mut i32) (i32.const 0))
 (global $ordinary (export \"ordinary\") (mut i32) (i32.const 0))
 (tag $transfer)
 (func $thread-checkpoint (global.set $polls (i32.add (global.get $polls) (i32.const 1))))
 (func $clear-values (global.set $clears (i32.add (global.get $clears) (i32.const 1))))
 (func $bool (param i32) (result (ref null eq))
   local.get 0 if (result (ref null eq)) i32.const 1 ref.i31 else ref.null eq end)
 (func $int64 (param i64) (result (ref null eq)) local.get 0 i32.wrap_i64 ref.i31)
 (func $box-int64 (param i64) (result (ref null eq)) local.get 0 struct.new $boxed-integer)
 (func $scalar-integer-value (param (ref eq)) (result i64)
   local.get 0 ref.test (ref i31)
   if (result i64) local.get 0 ref.cast (ref i31) i31.get_s i64.extend_i32_s
   else local.get 0 ref.cast (ref $boxed-integer) struct.get $boxed-integer 0 end)
 (func $eql (param (ref null eq) (ref null eq)) (result i32) local.get 0 local.get 1 ref.eq)
 (func $generic-dispatch-metadata-value (param (ref null eq) (ref null eq) (ref null eq)) (result (ref null eq))
   (global.set $direct (i32.add (global.get $direct) (i32.const 1)))
   local.get 0 ref.cast (ref i31) i31.get_s i32.const -1 i32.eq if throw $transfer end
   local.get 1)
 (func $native-call3 (param externref externref externref externref) (result externref)
   (global.set $ordinary (i32.add (global.get $ordinary) (i32.const 1)))
   (if (global.get $thread-active) (then (call $thread-checkpoint)))
   local.get 0 any.convert_extern ref.cast (ref i31) i31.get_s
   if (result externref) local.get 3
   else local.get 1 any.convert_extern ref.cast (ref null eq)
        local.get 2 any.convert_extern ref.cast (ref null eq)
        local.get 3 any.convert_extern ref.cast (ref null eq)
        call $generic-dispatch-metadata-value extern.convert_any end
   call $clear-values)
 (func $native-call1 (param externref externref) (result externref)
   (global.set $ordinary (i32.add (global.get $ordinary) (i32.const 1)))
   (if (global.get $thread-active) (then (call $thread-checkpoint)))
   throw $transfer)
" text
 ;; Append the substrate type so explicit worker type indices stay unchanged.
"(type $boxed-integer (struct (field i64)))
 (func (export \"run\") (param i32 i32 i32 i32) (result i32)
   local.get 0 ref.i31 local.get 1 ref.i31 local.get 2 ref.i31 local.get 3 ref.i31
   call $entry ref.as_non_null call $scalar-integer-value i32.wrap_i64))")) out))
               (push (list name (namestring image) runs) manifest))))
    (let ((copy (miricl-cross::kernel-fixed-callback-descriptor descriptor)))
      (assert (equal copy descriptor))
      (assert (not (eq copy descriptor)))
      (assert (not (eq (first copy) (first descriptor)))))
    (dolist (bad (list '(0) '((0)) (list (list 0 primitive 'extra))
                       (list (list -1 primitive)) (list (list 4 primitive))
                       (list (list 0 primitive) (list 0 primitive))
                       '((0 function)) '((0 car)) '((0 miricl::%%slot-operation))
                       (cons (list 0 primitive) 'tail)))
      (rejected bad))
    (let ((cycle (list (list 0 primitive))))
      (setf (cdr cycle) cycle) (rejected cycle))
    (let ((cycle (list 0 primitive)))
      (setf (cddr cycle) cycle) (rejected (list cycle)))
    (let* ((source (merge-pathnames "cache-source.lisp" directory))
           (definitions '((defun callback-test-entry (callback selector object extra)
                            (declare (optimize (debug 0) (safety 0)))
                            (values (funcall callback selector object extra)))))
           (supplied (copy-tree descriptor)))
      (with-open-file (out source :direction :output)
        (let ((*package* (find-package :miricl-runtime)))
          (dolist (form definitions) (write form :stream out :readably t))))
      (let ((miricl-cross::*kernel-source-files* (list source))
            (miricl-cross::*kernel-cache* (make-hash-table :test 'equal))
            (miricl-cross::*kernel-backend* nil))
        (fixture "cache-original" nil '((0 7 23 41 23 :direct)) nil
          (miricl-cross:runtime-kernel-wat 'miricl::$entry 'callback-test-entry :fixed-callbacks supplied))
        ;; The caller may later mutate its descriptor. A new contract must
        ;; neither mutate the cached key nor retrieve a differently proved body.
        (setf (caar supplied) 1)
        (fixture "cache-mutated" nil '((1 0 23 41 41 :ordinary)) nil
          (miricl-cross:runtime-kernel-wat 'miricl::$entry 'callback-test-entry :fixed-callbacks supplied))
        (with-open-file (out source :direction :output :if-exists :supersede)
          (let ((*package* (find-package :miricl-runtime)))
            (write '(defun callback-test-entry (callback)
                      (declare (optimize (debug 0) (safety 0)))
                      (values (funcall callback 7 nil nil))) :stream out :readably t)))
        (assert (handler-case
                  (progn (miricl-cross:runtime-kernel-wat 'miricl::$entry 'callback-test-entry
                           :fixed-callbacks (list (list 1 primitive))) nil)
                  (miricl-compiler::compiler-input-error () t)))
        (assert (stringp (miricl-cross:runtime-kernel-wat 'miricl::$entry 'callback-test-entry
                           :fixed-callbacks descriptor)))))
    ;; The opaque callback is deliberately represented by an integer in this
    ;; private substrate: zero is the saved primitive; one is a replacement.
    (fixture "copied"
      '((defun callback-test-entry (callback selector object extra)
          (declare (optimize (debug 0) (safety 0)))
          (let ((copy callback)) (values (funcall copy selector object extra)))))
      '((0 7 23 41 23 :direct) (0 -1 23 41 :transfer :direct) (0 7 31 43 31 :direct)))
    (fixture "ordinary"
      '((defun callback-test-entry (callback selector object extra)
          (declare (optimize (debug 0) (safety 0)))
          (values (funcall callback selector object extra))))
      '((1 7 23 41 41 :ordinary)) nil)
    (fixture "replaced"
      '((defun callback-test-entry (callback replacement object extra)
          (declare (optimize (debug 0) (safety 0)))
          (setq callback replacement)
          (values (funcall callback 7 object extra))))
      '((0 1 23 41 41 :ordinary)))
    (fixture "wrong-arity"
      '((defun callback-test-entry (callback selector object extra)
          (declare (ignore selector extra) (optimize (debug 0) (safety 0)))
          (values (funcall callback object))))
      '((0 7 23 41 :transfer :ordinary)))
    (fixture "callee-snapshot"
      '((defun callback-test-entry (callback replacement object extra)
          (declare (optimize (debug 0) (safety 0)))
          (funcall callback (progn (setq callback replacement) 7) object extra)
          (values (funcall callback 7 object extra))))
      '((0 1 23 41 41 :mixed)))
    (fixture "joined"
      '((defun callback-test-entry (callback replacement object extra)
          (declare (inline eql) (optimize (debug 0) (safety 0)))
          (values (funcall (if (eql object 23) callback replacement) 7 object extra))))
      '((0 1 23 41 23 :ordinary) (0 1 31 41 41 :ordinary)))
    ;; The condition is unknown at emission. Both executable incoming
    ;; primaries retain the saved callback, even with different value groups.
    (fixture "same-callback-join"
      '((defun callback-test-entry (callback selector object extra)
          (declare (inline eql) (optimize (debug 0) (safety 0)))
          (values (funcall (if (eql object 23) (values callback extra) (values callback))
                           selector object extra))))
      '((0 7 23 41 23 :direct) (0 7 31 43 31 :direct)
        (0 -1 23 41 :transfer :direct) (0 7 23 41 23 :direct)))
    ;; Object-valued joins cross the native worker's external-reference ABI
    ;; into the runtime's GC reference ABI. Validate and execute the adapted
    ;; WAT on both incoming paths, including transfer and later recovery.
    (let ((definitions
            '((defun callback-test-entry (callback selector object extra)
                (declare (inline eql) (optimize (debug 0) (safety 0)))
                (let ((answer (if (eql object 23)
                                  (funcall callback selector object extra)
                                  (funcall callback selector extra object))))
                  (values (funcall callback selector answer extra)))))))
      (fixture "callback-result-join" definitions
        '((0 7 23 41 23 :direct) (0 7 31 43 43 :direct)
          (0 -1 23 41 :transfer :direct) (0 -1 31 43 :transfer :direct)
          (0 7 23 41 23 :direct) (0 7 31 43 43 :direct)))
      ;; The fixed descriptor promises the saved primitive's identity.
      ;; Ordinary callbacks use a separately compiled worker without it.
      (fixture "ordinary-result-join" definitions
        '((1 7 23 41 41 :ordinary) (1 7 31 43 43 :ordinary)) nil))
    ;; Scalar joins retain their Wasm result type while the surrounding worker
    ;; changes reference ABI. Round-trip actual optimized bytes through the
    ;; listing used by the adapter, then execute both numeric incoming values.
    (let* ((bytes (miricl-compiler::optimize-wasm-module
                   (wasmtime:wat-to-wasm
"(module
 (import \"m\" \"call3\" (func $callback (param externref externref externref externref) (result externref)))
 (import \"m\" \"int64\" (func $integer (param i64) (result externref)))
 (func (export \"entry\") (param externref externref externref externref externref) (result externref)
   (local i64)
   local.get 1 local.get 2
   block $done
     local.get 3 any.convert_extern ref.cast (ref i31) i31.get_s i32.const 23 i32.eq
     if i64.const 17 local.set 5 br $done end
     i64.const 29 local.set 5
   end
   local.get 5 call $integer
   local.get 4 call $callback))")))
           (worker (miricl-cross::kernel-worker-wat bytes
                     (miricl-cross::kernel-export-index bytes "entry") "$entry" 4 :object nil)))
      (fixture "integer-result-join" nil
        '((0 7 23 41 17 :ordinary) (0 7 31 43 29 :ordinary)
          (0 -1 23 41 :transfer :ordinary) (0 7 31 43 29 :ordinary)) nil worker))
    ;; Tiered integer workers retain nullable i31 locals and test their
    ;; physical representation. Preserve both the initialized and null paths
    ;; when adapting external references into the runtime's raw reference ABI.
    (let* ((bytes (miricl-compiler::optimize-wasm-module
                   (wasmtime:wat-to-wasm
"(module
 (import \"m\" \"call3\" (func $callback (param externref externref externref externref) (result externref)))
 (import \"m\" \"int64\" (func $integer (param i64) (result externref)))
 (func (export \"entry\") (param externref externref externref externref externref) (result externref)
   (local (ref null i31))
   local.get 3 any.convert_extern ref.cast (ref i31) i31.get_s i32.const 23 i32.eq
   if i32.const 17 ref.i31 local.set 5 end
   local.get 1 local.get 2
   local.get 5 ref.test (ref i31)
   if (result i64)
     local.get 5 ref.cast (ref i31) i31.get_s i64.extend_i32_s
   else i64.const 29 end
   call $integer local.get 4 call $callback))")))
           (worker (miricl-cross::kernel-worker-wat bytes
                     (miricl-cross::kernel-export-index bytes "entry") "$entry" 4 :object nil)))
      (fixture "i31-local-test" nil
        '((0 7 23 41 17 :ordinary) (0 7 31 43 29 :ordinary)
          (0 -1 23 41 :transfer :ordinary) (0 7 23 41 17 :ordinary)) nil worker))
    ;; A positive type index can have the same decoded integer as an abstract
    ;; reference type byte. Its longer signed-LEB encoding still selects the
    ;; function type, whose scalar result must survive listing and adaptation.
    (let* ((types (with-output-to-string (out)
                    (dotimes (i #x6f) (write-line "(type (func))" out))
                    (write-line "(type $typed-join (func (result i64)))" out)))
           (bytes (miricl-compiler::optimize-wasm-module
                   (wasmtime:wat-to-wasm
                    (concatenate 'string "(module " types
"(import \"m\" \"call3\" (func $callback (param externref externref externref externref) (result externref)))
 (import \"m\" \"int64\" (func $integer (param i64) (result externref)))
 (func (export \"entry\") (param externref externref externref externref externref) (result externref)
   local.get 1 local.get 2
   block (type $typed-join)
     local.get 3 any.convert_extern ref.cast (ref i31) i31.get_s i32.const 23 i32.eq
     if i64.const 17 br 1 end
     i64.const 29
   end
   call $integer local.get 4 call $callback))"))))
           (worker (concatenate 'string types
                     (miricl-cross::kernel-worker-wat bytes
                       (miricl-cross::kernel-export-index bytes "entry") "$entry" 4 :object nil))))
      (fixture "indexed-result-join" nil
        '((0 7 23 41 17 :ordinary) (0 7 31 43 29 :ordinary)) nil worker))
    (fixture "joined-helper"
      '((defun callback-test-helper (callback selector object extra)
          (declare (optimize (debug 0) (safety 0)))
          (values (funcall callback selector object extra)))
        (defun callback-test-entry (callback selector object extra)
          (declare (inline eql) (optimize (debug 0) (safety 0)))
          (values (callback-test-helper (if (eql object 23) callback (values callback extra))
                                       selector object extra))))
      '((0 7 23 41 23 :direct) (0 7 31 43 31 :direct)))
    (fixture "known-helper"
      '((defun callback-test-helper (callback selector object extra)
          (declare (optimize (debug 0) (safety 0)))
          (values (funcall callback selector object extra)))
        (defun callback-test-entry (callback selector object extra)
          (declare (optimize (debug 0) (safety 0)))
          (values (callback-test-helper callback selector object extra))))
      '((0 7 23 41 23 :direct)))
    (fixture "mixed-helper"
      '((defun callback-test-helper (callback selector object extra)
          (declare (optimize (debug 0) (safety 0)))
          (values (funcall callback selector object extra)))
        (defun callback-test-entry (callback replacement object extra)
          (declare (optimize (debug 0) (safety 0)))
          (callback-test-helper callback 7 object extra)
          (values (callback-test-helper replacement 7 object extra))))
      '((0 1 23 41 41 :ordinary)))
    (fixture "recursive-entry"
      '((defun callback-test-entry (callback replacement object extra)
          (declare (optimize (debug 0) (safety 0)))
          (if replacement
              (values (callback-test-entry replacement nil object extra))
              (values (funcall callback 7 object extra)))))
      ;; The raw wrapper supplies zero as a non-NIL callback, then the Lisp NIL
      ;; supplied by the recursive edge stops recursion with the replacement.
      '((0 1 23 41 41 :ordinary)))
    (setf manifest (nreverse manifest))
    (let ((json (merge-pathnames "fixtures.json" directory)))
      (with-open-file (out json :direction :output)
        (write-char #\[ out)
        (loop for (name image runs) in manifest for first = t then nil do
          (unless first (write-char #\, out))
          (format out "{~S:~S,~S:~S,~S:[" "name" name "image" image "runs")
          (loop for run in runs for first-run = t then nil do
            (unless first-run (write-char #\, out))
            (format out "[~{~A~^,~}]" (mapcar (lambda (value)
              (if (keywordp value) (format nil "~S" (string-downcase value)) value)) run)))
          (write-string "]}" out))
        (write-char #\] out))
      (uiop:run-program (list "node" (namestring (asdf:system-relative-pathname :miricl "tests/runtime-kernel-callbacks.mjs"))
                             (namestring json)) :output *standard-output* :error-output *error-output*))))
