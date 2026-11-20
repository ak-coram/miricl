;;;; Wasmtime host services for the compiler running inside Wasm.
(in-package #:miricl)

(defvar *native-service-interpreters* (make-hash-table))

(defstruct host-native-module instance entries descriptors code storage)

(defun host-native-fail (reason &rest arguments)
  (error "Invalid native module: ~A" (apply #'format nil reason arguments)))

(defun host-native-value (values index)
  (cffi:mem-aptr values '(:struct wt::wasmtime-val) index))

(defun call-with-host-native-results (interpreter name arguments count receiver &optional instance)
  ;; Arguments are borrowed rooted values from this store, or numeric (kind . value)
  ;; pairs. A successful Wasmtime call gives us ownership of every result root.
  (let ((context (interpreter-context interpreter)))
    (wt:with-export (export context (or instance (interpreter-instance interpreter)) name 0)
      (cffi:with-foreign-objects ((args '(:struct wt::wasmtime-val) (max 1 (length arguments)))
                                (out '(:struct wt::wasmtime-val) (max 1 count)) (trap :pointer))
        (loop for argument in arguments for i from 0 for to = (host-native-value args i) do
          (if (consp argument) (wt:write-value to (car argument) (cdr argument))
              (wt::copy-struct to argument '(:struct wt::wasmtime-val))))
        (setf (cffi:mem-ref trap :pointer) (cffi:null-pointer))
        (wt:check-error (wt::wasmtime-func-call context (wt:extern-payload export 'wt::func)
                                             args (length arguments) out count trap))
        (wt::check-trap (cffi:mem-ref trap :pointer))
        (unwind-protect (funcall receiver out)
          (dotimes (i count) (wt::wasmtime-val-unroot (host-native-value out i))))))))

(defmacro with-host-native-results ((values interpreter name arguments count &optional instance) &body body)
  `(call-with-host-native-results ,interpreter ,name ,arguments ,count
                                  (lambda (,values) ,@body) ,instance))

(defun host-native-clone (value)
  (let ((owned (cffi:foreign-alloc '(:struct wt::wasmtime-val))))
    (wt::wasmtime-val-clone value owned) owned))

(defun host-native-free (value)
  (when value (wt::wasmtime-val-unroot value) (cffi:foreign-free value)))

(defun host-native-function-value (function)
  (let ((value (cffi:foreign-alloc '(:struct wt::wasmtime-val))))
    (setf (cffi:foreign-slot-value value '(:struct wt::wasmtime-val) 'wt::kind) 5)
    (wt::copy-struct
      (cffi:foreign-slot-pointer (cffi:foreign-slot-pointer value '(:struct wt::wasmtime-val) 'wt::of)
                                '(:union wt::wasmtime-valunion) 'wt::funcref)
      function '(:struct wt::wasmtime-func))
    value))

(defun host-native-entry-valid-p (interpreter value direct)
  (and (= 5 (cffi:foreign-slot-value value '(:struct wt::wasmtime-val) 'wt::kind))
       (with-host-native-results (result interpreter "native-entry-valid" (list value (cons 0 direct)) 1)
         (not (zerop (wt:read-value result))))))

(defun host-native-module-description (bytes)
  ;; These private readers run only after Wasmtime has validated the whole
  ;; module. Independently validate our bounded instantiation/table protocol.
  (let ((reader (native-link-reader bytes 8)) (imports nil) (exports nil)
        (metadata nil) (table nil) (table-import nil))
    (loop while (< (native-link-reader-position reader) (length bytes)) do
      (let* ((kind (native-link-byte reader))
             (payload (native-link-span reader (native-link-integer reader)))
             (section (native-link-reader payload)))
        (case kind
          (0
           (when (string= (native-link-name section) "miricl.entries")
             (when metadata (host-native-fail "duplicate entry metadata"))
             (labels ((word ()
                        (let* ((start (native-link-reader-position section))
                               (value (native-link-integer section))
                               (end (native-link-reader-position section)))
                          (when (and (> end (1+ start)) (zerop (aref payload (1- end))))
                            (host-native-fail "noncanonical entry metadata integer"))
                          value)))
               (unless (= 1 (word)) (host-native-fail "unsupported entry metadata version"))
               (let* ((count (word)) (mode (native-link-byte section)))
                 (unless (<= 1 count (length bytes)) (host-native-fail "entry count exceeds module bounds"))
                 (unless (member mode '(0 1)) (host-native-fail "unknown entry descriptor mode"))
                 (unless (= (- (native-link-reader-limit section) (native-link-reader-position section))
                            (if (= mode 1) count 0))
                   (host-native-fail "entry descriptor extent"))
                 (let ((descriptors (if (= mode 1) (native-link-span section count)
                                        (make-array count :element-type '(unsigned-byte 8) :initial-element 0)))
                       (direct-count 0))
                   (loop for descriptor across descriptors
                         for arity = (logand descriptor 7) for usage = (ash descriptor -3) do
                     (cond ((zerop arity) (unless (zerop descriptor) (host-native-fail "usage without direct entry")))
                           (t (unless (and (<= arity 5) (or (zerop usage) (< (1- usage) (ash 1 (1- arity)))))
                                (host-native-fail "invalid direct arity or argument usage"))
                              (incf direct-count))))
                   (unless (<= (+ count direct-count) (length bytes))
                     (host-native-fail "entry table exceeds module bounds"))
                   (setq metadata (list count (+ count direct-count) descriptors)))))))
          (2
           (dotimes (i (native-link-integer section))
             (let ((namespace (native-link-name section)) (name (native-link-name section))
                   (kind (native-link-byte section)))
               (unless (string= namespace "m") (host-native-fail "unsupported import namespace ~S" namespace))
               (case kind
                 (0 (native-link-integer section) (push (cons (concatenate 'string "native-" name) 0) imports))
                 (4 (unless (and (string= name "transfer") (zerop (native-link-byte section)))
                      (host-native-fail "unsupported exception tag"))
                    (native-link-integer section) (push (cons "native-transfer" 5) imports))
                 (1 (setq table-import t) (host-native-fail "imported tables are not permitted"))
                 (otherwise (host-native-fail "unsupported runtime import kind ~D" kind))))))
          (4
           (when table (host-native-fail "duplicate table section"))
           (unless (and (= 1 (native-link-integer section)) (= #x70 (native-link-byte section))
                        (= 1 (native-link-integer section)))
             (host-native-fail "expected one fixed funcref table"))
           (setq table (list (native-link-integer section) (native-link-integer section)))
           (native-link-finished section))
          (7
           (dotimes (i (native-link-integer section))
             (let ((name (native-link-name section)) (kind (native-link-byte section))
                   (index (native-link-integer section)))
               (push (list name kind index) exports))))
          ((5 11 12) (host-native-fail "native modules may not define memories or data segments"))
          (8 (host-native-fail "start function is not permitted")))))
    (let ((entry-table (assoc "entries" exports :test #'string=)))
      (cond (metadata
             (unless (and (not table-import) (equal table (list (second metadata) (second metadata)))
                          (equal (cdr entry-table) '(1 0)))
               (host-native-fail "entry table does not match metadata")))
            ((or entry-table table) (host-native-fail "missing metadata for entry table"))))
    (values (nreverse imports) metadata exports)))

(defun host-native-module-entry (interpreter module name)
  (wt:with-export (export (interpreter-context interpreter) (host-native-module-instance module) name 0)
    (let ((value (host-native-function-value (wt:extern-payload export 'wt::func))))
      (unless (host-native-entry-valid-p interpreter value 0)
        (host-native-free value) (host-native-fail "entry ~S has the wrong signature" name))
      value)))

(defun host-native-compile-module (interpreter bytes storage start length)
  (let ((engine (interpreter-engine interpreter)) (context (interpreter-context interpreter)))
    (native-link-validate engine bytes)
    (multiple-value-bind (imports metadata exports) (host-native-module-description bytes)
      (declare (ignore exports))
      (let ((module (wt:compile-module engine bytes))
            (instance (cffi:foreign-alloc '(:struct wt::wasmtime-instance)))
            (raw-imports nil) (entries nil) (code nil) (owned-storage nil) (success nil))
        (unwind-protect
             (progn
               (dolist (import imports)
                 (let ((value (cffi:foreign-alloc '(:struct wt::wasmtime-extern))))
                   (handler-case
                       (wt::get-export context (interpreter-instance interpreter) (car import) value (cdr import))
                     (error (condition) (cffi:foreign-free value) (error condition)))
                   (push value raw-imports)))
               (wt:instantiate context module instance (reverse raw-imports))
               (when metadata
                 (wt:with-export (table context instance "entries" 2)
                   (unless (= (wt::wasmtime-table-size context (wt:extern-payload table 'wt::table)) (second metadata))
                     (host-native-fail "actual entry table size mismatch"))
                   (dotimes (i (second metadata))
                     (let ((entry (cffi:foreign-alloc '(:struct wt::wasmtime-val))))
                       (unless (wt::wasmtime-table-get context (wt:extern-payload table 'wt::table) i entry)
                         (cffi:foreign-free entry) (host-native-fail "missing entry table slot ~D" i))
                       (push entry entries)
                       (unless (host-native-entry-valid-p interpreter entry (if (< i (first metadata)) 0 1))
                         (host-native-fail "entry ~D has the wrong signature" i))))))
               (with-host-native-results (value interpreter "snapshot"
                         (list (cons 0 start) (cons 0 length)) 1 (interpreter-native-service-instance interpreter))
                 (setq code (host-native-clone value)))
               (setq owned-storage (host-native-clone storage))
               (prog1 (make-host-native-module :instance instance :entries (and entries (coerce (nreverse entries) 'vector))
                          :descriptors (and metadata (third metadata)) :code code :storage owned-storage)
                 (setq success t)))
          (wt::wasmtime-module-delete module)
          (dolist (import raw-imports) (wt::wasmtime-extern-delete import) (cffi:foreign-free import))
          (unless success
            (dolist (entry entries) (host-native-free entry))
            (host-native-free code) (host-native-free owned-storage) (cffi:foreign-free instance)))))))

(defun host-native-close-module (module)
  (loop for value across (or (host-native-module-entries module) #()) do (host-native-free value))
  (host-native-free (host-native-module-code module))
  (host-native-free (host-native-module-storage module))
  (cffi:foreign-free (host-native-module-instance module)))

(defun close-interpreter-native-services (interpreter)
  (when (interpreter-context interpreter)
    (remhash (cffi:pointer-address (interpreter-context interpreter)) *native-service-interpreters*))
  (maphash (lambda (bytes modules) (declare (ignore bytes))
             (dolist (module modules) (host-native-close-module module)))
           (interpreter-native-modules interpreter))
  (clrhash (interpreter-native-modules interpreter))
  (when (interpreter-native-service-instance interpreter)
    (cffi:foreign-free (interpreter-native-service-instance interpreter))
    (setf (interpreter-native-service-instance interpreter) nil)))

(defun host-native-error-result (interpreter condition results)
  ;; Only the host bridge's own memory is scratch. Build a Lisp string using
  ;; checked runtime constructors; the bridge signals after this C frame exits.
  (let* ((message (format nil "Native module rejected: ~A" condition))
         (text (subseq message 0 (min 1024 (length message))))
         (bytes (map '(simple-array (unsigned-byte 8) (*))
                     (lambda (char) (let ((code (char-code char))) (if (< code 128) code 63))) text))
         (bridge (interpreter-native-service-instance interpreter)))
    (wt:with-export (export (interpreter-context interpreter) bridge "memory" 3)
      (let ((memory (wt:extern-payload export 'wt::memory)))
        (let ((data (wt:wasmtime-memory-data (interpreter-context interpreter) memory)))
          (dotimes (i (length bytes)) (setf (cffi:mem-aref data :uint8 i) (aref bytes i))))))
    (with-host-native-results (value interpreter "message" (list (cons 0 (length bytes))) 1 bridge)
      (wt::wasmtime-val-clone value results)
      (wt:write-value (host-native-value results 1) 0 1))))

(defun host-native-install (interpreter args results)
  ;; The Wasm bridge checked the input representation, copied bytes and checked
  ;; the entry index before crossing C. Do not reenter guest input validators.
  (let* ((context (interpreter-context interpreter))
         (storage (host-native-value args 0))
         (literals (host-native-value args 1)) (name (host-native-value args 2))
         (start (wt:read-value (host-native-value args 3)))
         (length (wt:read-value (host-native-value args 4)))
         (index (wt:read-value (host-native-value args 5))))
    (unless (and (typep start '(unsigned-byte 32)) (<= 8 length (* 64 1024 1024)))
      (host-native-fail "code byte range exceeds supported bounds"))
    (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
      (wt:with-export (export context (interpreter-instance interpreter) "memory" 3)
        (let ((memory (wt:extern-payload export 'wt::memory)))
          (unless (<= (+ start length) (wt:wasmtime-memory-data-size context memory))
            (host-native-fail "code byte range exceeds guest memory"))
          (let ((data (wt:wasmtime-memory-data context memory)))
            (dotimes (i length) (setf (aref bytes i) (cffi:mem-aref data :uint8 (+ start i)))))))
      (let* ((compiled
               (or (find-if
                     (lambda (module)
                       (with-host-native-results (same interpreter "same-storage"
                           (list storage (host-native-module-storage module)) 1
                           (interpreter-native-service-instance interpreter))
                         (not (zerop (wt:read-value same)))))
                     (gethash bytes (interpreter-native-modules interpreter)))
                   (let ((module (host-native-compile-module interpreter bytes storage start length)))
                     (push module (gethash bytes (interpreter-native-modules interpreter))) module)))
             (entries (host-native-module-entries compiled))
             (count (length (host-native-module-descriptors compiled)))
             (entry (if entries
                        (progn (unless (< -1 index count) (host-native-fail "entry index exceeds table bounds"))
                               (aref entries index))
                        (host-native-module-entry interpreter compiled
                          (if (minusp index) "entry" (format nil "entry~D" index))))))
        (unwind-protect
             (with-host-native-results (function interpreter "core-native-function"
                 (list entry literals name (cons 0 length) (host-native-module-code compiled)) 1)
               ;; Checked packet entries are sufficient for bootstrap. No
               ;; compiler metadata grants certified-call authority.
               (wt::wasmtime-val-clone function results)
               (wt:write-value (host-native-value results 1) 0 0))
          (unless entries (host-native-free entry)))))))

(cffi:defcallback interpreter-native-service :pointer
    ((env :pointer) (caller :pointer) (args wt::p-wasmtime-val) (nargs wt::size)
     (results wt::p-wasmtime-val) (nresults wt::size))
  (declare (ignore env))
  (let* ((context (wt::wasmtime-caller-context caller))
         (interpreter (gethash (cffi:pointer-address context) *native-service-interpreters*)))
    (handler-case
        (progn
          (unless (and interpreter (= nargs 6) (= nresults 2)) (error "Unknown native service caller"))
          (wt:with-export (global context (interpreter-instance interpreter) "javascript-depth" 1)
            (cffi:with-foreign-object (previous '(:struct wt::wasmtime-val))
              (let ((handle (wt:extern-payload global 'wt::global)))
                (wt::wasmtime-global-get context handle previous)
                (unwind-protect
                     (cffi:with-foreign-object (current '(:struct wt::wasmtime-val))
                       (wt:write-value current 0 (1+ (wt:read-value previous)))
                       (wt:check-error (wt::wasmtime-global-set context handle current))
                       (handler-case (host-native-install interpreter args results)
                         (error (condition) (host-native-error-result interpreter condition results))))
                  (wt:check-error (wt::wasmtime-global-set context handle previous))))))
          (cffi:null-pointer))
      (error (condition)
        ;; Failures in the embedding itself cannot be turned into guest values.
        (cffi:with-foreign-string ((text length) (princ-to-string condition) :encoding :utf-8)
          (wt::wasmtime-trap-new text (1- length)))))))

(defparameter *host-native-service-bridge*
  "(module
    (import \"runtime\" \"bytes\" (func $bytes (param externref) (result i32 i32)))
    (import \"runtime\" \"index\" (func $index (param externref) (result i32)))
    (import \"runtime\" \"storage\" (func $storage (param externref) (result externref)))
    (import \"runtime\" \"code\" (func $code (param externref i32 i32) (result externref)))
    (import \"runtime\" \"make-string\" (func $make-string (param externref) (result externref)))
    (import \"runtime\" \"code-char\" (func $code-char (param externref) (result externref)))
    (import \"runtime\" \"set-char\" (func $set-char (param externref externref externref) (result externref)))
    (import \"runtime\" \"error\" (func $error (param externref) (result externref)))
    (import \"host\" \"service\" (func $service (param externref externref externref i32 i32 i32) (result externref i32)))
    (memory (export \"memory\") 1 1)
    (func $integer (param i32) (result externref) local.get 0 ref.i31 extern.convert_any)
    (func (export \"same-storage\") (param externref externref) (result i32)
      local.get 0 any.convert_extern ref.cast (ref null eq)
      local.get 1 any.convert_extern ref.cast (ref null eq) ref.eq)
    (func (export \"snapshot\") (param i32 i32) (result externref)
      ref.null extern local.get 0 local.get 1 call $code)
    (func (export \"message\") (param $length i32) (result externref)
      (local $string externref) (local $i i32)
      local.get $length call $integer call $make-string local.set $string
      block $done loop $next
        local.get $i local.get $length i32.ge_u br_if $done
        local.get $i i32.load8_u call $integer call $code-char
        local.get $string local.get $i call $integer call $set-char drop
        local.get $i i32.const 1 i32.add local.set $i br $next
      end end local.get $string)
    (func (export \"service\") (param externref externref externref) (result externref)
      (local $start i32) (local $length i32) (local $index i32) (local $failed i32) (local $value externref)
      (local $storage externref)
      local.get 0 call $storage local.set $storage
      local.get 0 call $bytes local.set $length local.set $start
      local.get 0 call $index local.set $index
      local.get $storage local.get 1 local.get 2
      local.get $start local.get $length local.get $index call $service
      local.set $failed local.set $value
      local.get $failed if local.get $value call $error drop unreachable end
      local.get $value))")

(defun install-interpreter-native-services (interpreter)
  (let ((context (interpreter-context interpreter)) (imports nil)
        (bridge (cffi:foreign-alloc '(:struct wt::wasmtime-instance))) (success nil))
    (setf (gethash (cffi:pointer-address context) *native-service-interpreters*) interpreter)
    (unwind-protect
         (wt:with-callback-func (function context (cffi:callback interpreter-native-service)
                                         :parameters '(:externref :externref :externref :i32 :i32 :i32)
                                         :results '(:externref :i32))
           (dolist (name '("native-code-bytes" "native-code-entry" "native-code-storage" "core-native-code" "native-make-string-inline"
                           "native-code-character-inline" "native-character-set-inline" "native-module-error"))
             (let ((export (cffi:foreign-alloc '(:struct wt::wasmtime-extern))))
               (handler-case (wt::get-export context (interpreter-instance interpreter) name export 0)
                 (error (condition) (cffi:foreign-free export) (error condition)))
               (push export imports)))
           (cffi:with-foreign-object (host '(:struct wt::wasmtime-extern))
             (setf (cffi:foreign-slot-value host '(:struct wt::wasmtime-extern) 'wt::kind) 0)
             (wt::copy-struct (wt:extern-payload host 'wt::func) function '(:struct wt::wasmtime-func))
             (wt::with-module (module (interpreter-engine interpreter) *host-native-service-bridge*)
               (wt:instantiate context module bridge (append (reverse imports) (list host)))
               (wt:with-export (service context bridge "service" 0)
                 (wt:with-export (table context (interpreter-instance interpreter) "native-services" 2)
                   (let ((value (host-native-function-value (wt:extern-payload service 'wt::func))))
                     (unwind-protect
                          (wt:check-error (wt::wasmtime-table-set context (wt:extern-payload table 'wt::table) 0 value))
                       (host-native-free value)))))))
           (setf (interpreter-native-service-instance interpreter) bridge success t))
      (dolist (import imports) (wt::wasmtime-extern-delete import) (cffi:foreign-free import))
      (unless success (cffi:foreign-free bridge)))))

(defun bootstrap-native-compiler-manifest (&key (fuel (1- (ash 1 64))))
  "Run the interpreted and native guest compiler stages using only CL and Wasmtime."
  (with-interpreter (interpreter :fuel fuel :native-service t)
    (load-source interpreter (asdf:system-relative-pathname :miricl "scripts/compiler-image-bootstrap.lisp"))
    (let ((start (get-internal-real-time)) (native-start nil))
      (evaluate-string interpreter "(miricl-compiler::bootstrap-compiler-native-functions)")
      (setq native-start (get-internal-real-time))
      (evaluate-string interpreter "(miricl-compiler::bootstrap-compiler-image)")
      (format t "~&Compiler bootstrap phase seconds: interpreted=~,3F native=~,3F~%"
              (/ (- native-start start) internal-time-units-per-second)
              (/ (- (get-internal-real-time) native-start) internal-time-units-per-second)))
    (let ((text (evaluate-string interpreter
                  "(with-open-file (input \"/compiler-image.lisp\") (let ((text (make-string (file-length input)))) (read-sequence text input) text))")))
      (with-standard-io-syntax
        (let ((*read-eval* nil)) (read-from-string text))))))
