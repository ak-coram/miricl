;;;; Host-only linking of native compiler modules into the runtime's WAT.
;;;; The guest compiler and runtime never depend on a JavaScript decoder.
(in-package #:miricl)

(defstruct (native-link-reader (:constructor native-link-reader (bytes &optional (position 0) (limit (length bytes)))))
  bytes position limit)

(defun native-link-fail (input reason &rest arguments)
  (miricl-compiler::compiler-input-fail "native module linker" input
    (apply #'format nil reason arguments)))

(defun native-link-byte (input)
  (when (>= (native-link-reader-position input) (native-link-reader-limit input))
    (native-link-fail (native-link-reader-bytes input) "truncated native module"))
  (prog1 (aref (native-link-reader-bytes input) (native-link-reader-position input))
    (incf (native-link-reader-position input))))

(defun native-link-integer (input &optional signed)
  ;; Share the compiler's checked LEB readers; additionally enforce this
  ;; section/body's boundary and the immediate's physical Wasm width.
  (let ((start (native-link-reader-position input)))
    (multiple-value-bind (value next)
        (funcall (if signed #'miricl-compiler::native-read-sleb #'miricl-compiler::native-read-uleb)
                 (native-link-reader-bytes input) start)
      (unless (and (<= next (native-link-reader-limit input))
                   (<= (- next start) (if (eql signed 64) 10 5))
                   (typep value (if signed `(signed-byte ,signed) '(unsigned-byte 32))))
        (native-link-fail (native-link-reader-bytes input) "invalid or truncated Wasm integer"))
      (setf (native-link-reader-position input) next)
      value)))

(defun native-link-span (input count)
  (let ((start (native-link-reader-position input)))
    (unless (<= count (- (native-link-reader-limit input) start))
      (native-link-fail (native-link-reader-bytes input) "byte range exceeds section or function body"))
    (incf (native-link-reader-position input) count)
    (subseq (native-link-reader-bytes input) start (+ start count))))

(defun native-link-name (input)
  ;; Supported linker identifiers are ASCII. Reject other names instead of
  ;; silently changing a UTF-8 import/export into a different runtime name.
  (let* ((length (native-link-integer input))
         (bytes (progn
                  (when (> length 1024)
                    (native-link-fail (native-link-reader-bytes input) "native link name exceeds the supported limit"))
                  (native-link-span input length))))
    (unless (every (lambda (byte) (< byte 128)) bytes)
      (native-link-fail bytes "unsupported non-ASCII native link name"))
    (map 'string #'code-char bytes)))

(defun native-link-finished (input)
  (unless (= (native-link-reader-position input) (native-link-reader-limit input))
    (native-link-fail (native-link-reader-bytes input) "unexpected bytes after native section")))

(defun native-link-identifier-p (name)
  (and (stringp name) (plusp (length name))
       (find (char name 0) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
       (every (lambda (char) (find char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-")) name)))

(defun native-link-octets (code)
  (let ((length
          (cond ((vectorp code) (length code))
                ((listp code) (miricl-compiler::compiler-list-length code "native module bytes"))
                (t (native-link-fail code "expected a vector or proper list of bytes")))))
    (unless (<= 8 length (* 64 1024 1024))
      (native-link-fail code "native module size is outside the supported bounds"))
    (unless (every (lambda (byte) (typep byte '(unsigned-byte 8))) code)
      (native-link-fail code "expected unsigned byte elements"))
    ;; Own the validated bytes through decoding. COERCE may retain a caller's
    ;; already specialized vector, allowing later mutation to change the
    ;; module whose bounds and types the engine checked.
    (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
      (replace bytes code)
      bytes)))

(defun native-link-validate (engine bytes)
  ;; Semantic validation precedes the restricted linker decoder. The decoder
  ;; never substitutes for Wasm's type, branch, index or instruction checks.
  (cffi:with-pointer-to-vector-data (data bytes)
    (handler-case
        (wt:check-error (wt::wasmtime-module-validate engine data (length bytes)))
      (error (condition) (native-link-fail bytes "invalid compiler module: ~A" condition)))))

(defun native-link-value-type (input)
  (let ((code (native-link-byte input)))
    (or (cdr (assoc code '((#x7f . "i32") (#x7e . "i64") (#x6f . "externref")
                          (#x70 . "funcref") (#x6c . "i31ref"))))
        (native-link-fail (native-link-reader-bytes input) "unsupported native value type #x~X" code))))

(defun native-link-signature (type bytes)
  (unless (and type (= (aref type 0) #x60))
    (native-link-fail bytes "missing native function signature"))
  (flet ((types (types)
           (mapcar (lambda (bytes)
                     (let ((input (native-link-reader (coerce bytes 'vector))))
                       (prog1 (native-link-value-type input) (native-link-finished input)))) types)))
    (list (types (aref type 1)) (types (aref type 2)))))

(defun native-link-module (bytes module-index prefix engine)
  (native-link-validate engine bytes)
  (let ((input (native-link-reader bytes 8)) (types #()) (imports nil) (tags nil)
        (function-types nil) (bodies nil) (entry-index nil) (canonical-objects nil)
        (declarations nil) (call-counts (make-hash-table :test 'equal)))
    ;; Full Wasm validation makes the existing CL type decoder safe to reuse.
    ;; Keep aggregate slots in its type-index space; only the compiler's exact
    ;; canonical object types can be adapted to the runtime's $obj identity.
    (loop while (< (native-link-reader-position input) (length bytes)) do
      (let* ((kind (native-link-byte input)) (payload (native-link-span input (native-link-integer input)))
             (section (native-link-reader payload)))
        (case kind
          (0)
          (1
           (handler-case (setq types (miricl-compiler::module-types payload))
             (miricl-compiler::unsupported-wasm-optimization ()
               (native-link-fail bytes "unsupported native type section")))
           (let* ((expected (coerce (miricl-compiler::with-wasm-bytes
                                     (miricl-compiler::emit-native-object-types)) 'vector))
                  (start (progn (native-link-integer section) (native-link-reader-position section))))
             (setq canonical-objects
               (and (<= (+ start (length expected)) (length payload))
                    (equalp expected (subseq payload start (+ start (length expected))))))))
          (2
           (dotimes (i (native-link-integer section))
             (let* ((namespace (native-link-name section)) (name (native-link-name section))
                    (kind (native-link-byte section)))
               (unless (string= namespace "m") (native-link-fail bytes "unsupported native runtime namespace"))
               (case kind
                 (0
                  (unless (and (plusp (length name))
                               (every (lambda (char) (find char "abcdefghijklmnopqrstuvwxyz0123456789-")) name))
                    (native-link-fail bytes "invalid native runtime helper name"))
                  (native-link-integer section)
                  (push (miricl-cross::kernel-import-name (list namespace name)) imports))
                 (4
                  (unless (string= name "transfer") (native-link-fail bytes "unsupported native exception tag"))
                  (unless (zerop (native-link-byte section)) (native-link-fail bytes "unsupported native exception tag attribute"))
                  (let ((signature (native-link-signature (aref types (native-link-integer section)) bytes)))
                    (unless (equal signature '(nil nil))
                      (native-link-fail bytes "native transfer tag must have no payload")))
                  (push "$transfer" tags))
                 (otherwise (native-link-fail bytes "unsupported native import kind ~D" kind)))))
           (native-link-finished section))
          (3
           (setq function-types (loop repeat (native-link-integer section) collect (native-link-integer section)))
           (native-link-finished section))
          (7
           (unless (and (= (native-link-integer section) 1) (string= (native-link-name section) "entry")
                        (zerop (native-link-byte section)))
             (native-link-fail bytes "expected one native function export named entry"))
           (setq entry-index (native-link-integer section))
           (native-link-finished section))
          (9
           ;; The final linked table declares every private function. Native
           ;; modules use only declarative function segments, never tables or
           ;; instantiation-time element writes.
           (dotimes (i (native-link-integer section))
             (unless (and (= (native-link-integer section) 3) (zerop (native-link-byte section)))
               (native-link-fail bytes "unsupported native element segment"))
             (dotimes (j (native-link-integer section)) (native-link-integer section)))
           (native-link-finished section))
          (10
           (unless (= (native-link-integer section) (length function-types))
             (native-link-fail bytes "native function and code counts differ"))
           (setq bodies (loop repeat (length function-types)
                              collect (native-link-span section (native-link-integer section))))
           (native-link-finished section))
          (otherwise (native-link-fail bytes "unsupported native section ~D" kind)))))
    (unless (and entry-index bodies) (native-link-fail bytes "native module has no entry or code"))
    (setq imports (coerce (nreverse imports) 'vector) tags (coerce (nreverse tags) 'vector))
    (labels ((function-name (index)
               (unless (< index (+ (length imports) (length bodies)))
                 (native-link-fail bytes "native function index exceeds module bounds"))
               (if (< index (length imports)) (concatenate 'string "$" (aref imports index))
                   (format nil "$~A-~D-~D" prefix module-index (- index (length imports)))))
             (tag-name (index)
               (unless (< index (length tags)) (native-link-fail bytes "unsupported native exception tag index"))
               (aref tags index))
             (block-type (input)
               (let ((code (native-link-byte input)))
                 (if (= code #x40) ""
                     (let ((type (cdr (assoc code '((#x7f . "i32") (#x7e . "i64") (#x6f . "externref"))))))
                       (unless type (native-link-fail bytes "unsupported native block type #x~X" code))
                       (format nil " (result ~A)" type)))))
             (object-types ()
               (unless canonical-objects (native-link-fail bytes "native object types do not match the runtime layout"))))
      (let ((entry (function-name entry-index)))
        (when (< entry-index (length imports)) (native-link-fail bytes "native entry must be a defined function"))
        (values
          (with-output-to-string (out)
            (loop for body in bodies for type-index in function-types for index from 0 do
              (let* ((function (function-name (+ index (length imports))))
                     (signature (native-link-signature (aref types type-index) bytes))
                     (input (native-link-reader body)) (locals nil))
                (when (string= function entry)
                  (unless (equal signature '(("externref" "externref") ("externref")))
                    (native-link-fail bytes "native factory must accept the checked packet and return one Lisp value")))
                (push function declarations)
                (dotimes (i (native-link-integer input))
                  (let ((count (native-link-integer input)) (type (native-link-value-type input)))
                    (push (make-list count :initial-element type) locals)))
                (format out "(func ~A (param~{ ~A~})~@[ (result~{ ~A~})~]~%" function (first signature) (second signature))
                (dolist (group (nreverse locals)) (when group (format out "(local~{ ~A~})~%" group)))
                (loop while (< (native-link-reader-position input) (length body)) do
                  (let ((code (native-link-byte input)))
                    (case code
                      ((#x02 #x03 #x04) (format out "~A~A~%" (case code (2 "block") (3 "loop") (4 "if")) (block-type input)))
                      (#x0b (when (< (native-link-reader-position input) (length body)) (write-line "end" out)))
                      (#x08 (format out "throw ~A~%" (tag-name (native-link-integer input))))
                      ((#x0c #x0d) (format out "~A ~D~%" (if (= code #x0c) "br" "br_if") (native-link-integer input)))
                      (#x0e (format out "br_table~{ ~D~}~%"
                              (loop repeat (1+ (native-link-integer input)) collect (native-link-integer input))))
                      ((#x10 #x12 #xd2)
                       (let ((index (native-link-integer input)))
                         (when (and (= code #x10) (< index (length imports))) (incf (gethash (aref imports index) call-counts 0)))
                         (format out "~A ~A~%" (case code (#x10 "call") (#x12 "return_call") (#xd2 "ref.func")) (function-name index))))
                      (#x1f
                       (format out "try_table~A" (block-type input))
                       (dotimes (i (native-link-integer input))
                         (unless (zerop (native-link-byte input)) (native-link-fail bytes "unsupported native exception catch kind"))
                         (format out " (catch ~A ~D)" (tag-name (native-link-integer input)) (native-link-integer input)))
                       (terpri out))
                      ((#x20 #x21 #x22) (format out "~A ~D~%" (case code (#x20 "local.get") (#x21 "local.set") (#x22 "local.tee"))
                                              (native-link-integer input)))
                      ((#x41 #x42) (format out "~A ~D~%" (if (= code #x41) "i32.const" "i64.const")
                                         (native-link-integer input (if (= code #x41) 32 64))))
                      (#xd0
                       (unless (= (native-link-byte input) #x6f) (native-link-fail bytes "unsupported native null reference"))
                       (write-line "ref.null extern" out))
                      (#xfb
                       (let ((gc (native-link-integer input)))
                         (case gc
                           ((2 5)
                            (unless (= (native-link-integer input) 1) (native-link-fail bytes "unsupported native struct type"))
                            (object-types)
                            (format out "struct.~A $obj ~D~%" (if (= gc 5) "set" "get") (native-link-integer input)))
                           ((#x14 #x16 #x17)
                            (let* ((type (native-link-integer input 32))
                                   (name (cond ((= type 1) (object-types) "$obj")
                                               ((and (= gc #x17) (= type -19)) "eq")
                                               ((and (/= gc #x17) (= type -20)) "i31")
                                               (t (native-link-fail bytes "unsupported native heap type ~D" type)))))
                              (format out "~A (ref ~A~A)~%" (if (= gc #x14) "ref.test" "ref.cast")
                                (if (= gc #x17) "null " "") name)))
                           (otherwise
                            (let ((name (cdr (assoc gc '((#x1a . "any.convert_extern") (#x1b . "extern.convert_any")
                                                        (#x1c . "ref.i31") (#x1d . "i31.get_s"))))))
                              (unless name (native-link-fail bytes "unsupported native GC instruction #x~X" gc))
                              (write-line name out))))))
                      (otherwise
                       ;; Reuse the compiler disassembler's numeric opcode
                       ;; vocabulary; new arithmetic opcodes have one CL source.
                       (handler-case
                           (write-line (miricl-compiler::native-disassembly-instruction
                             (miricl-compiler::make-native-op code nil 0 1) (vector code) #()) out)
                         (error () (native-link-fail bytes "unsupported native instruction #x~X" code)))))))
                (write-line ")" out))))
          (list entry) (nreverse declarations) call-counts)))))

(defun link-native-modules (codes &key (prefix "compiler") (table-name "compiler-entries")
                                      (table-export "core-compiler-functions"))
  "Link compiler modules into runtime WAT; return WAT, entries, bodies and call counts."
  (miricl-compiler::compiler-list-length codes "native modules")
  (unless (and (native-link-identifier-p prefix) (native-link-identifier-p table-name)
               (native-link-identifier-p table-export))
    (native-link-fail (list prefix table-name table-export) "invalid native link identifier"))
  (let ((entries nil) (declarations nil) (calls (make-hash-table :test 'equal)) (bodies nil))
    (wt:with-engine (engine)
      (loop for code in codes for index from 0 do
        (multiple-value-bind (wat module-entries module-declarations module-calls)
            (native-link-module (native-link-octets code) index prefix engine)
          (push wat bodies)
          (setq entries (append entries module-entries) declarations (append declarations module-declarations))
          (maphash (lambda (name count) (incf (gethash name calls 0) count)) module-calls))))
    (let ((functions (remove-duplicates (append entries declarations) :test #'equal :from-end t)))
      (values
        (with-output-to-string (out)
          (dolist (body (nreverse bodies)) (write-string body out) (terpri out))
          (format out "(table $~A (export ~S) ~D ~D funcref)~%" table-name table-export (length functions) (length functions))
          (format out "(elem (table $~A) (i32.const 0) func~{ ~A~})~%" table-name functions))
        entries declarations calls))))
