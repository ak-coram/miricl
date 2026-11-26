;;;; Host-only core-module adapter for the native bytecode optimization passes.
;;;; Preserve public function identities when rewriting code and names.
(in-package :miricl-compiler)

(define-condition unsupported-wasm-optimization (error) ())
(defun unsupported-wasm-optimization () (error 'unsupported-wasm-optimization))

(defstruct (wasm-input (:constructor wasm-input (bytes &optional (position 0)))) bytes position)
(defun module-byte (input)
  (prog1 (aref (wasm-input-bytes input) (wasm-input-position input))
    (incf (wasm-input-position input))))
(defun module-uleb (input)
  (multiple-value-bind (value position)
      (native-read-uleb (wasm-input-bytes input) (wasm-input-position input))
    (setf (wasm-input-position input) position) value))
(defun module-sleb (input)
  (multiple-value-bind (value position)
      (native-read-sleb (wasm-input-bytes input) (wasm-input-position input))
    (setf (wasm-input-position input) position) value))
(defun module-span (input count)
  (let ((start (wasm-input-position input)))
    (incf (wasm-input-position input) count)
    (subseq (wasm-input-bytes input) start (wasm-input-position input))))
(defun module-name (input) (module-span input (module-uleb input)))
(defun module-value-type (input)
  ;; Preserve the exact signed heap-type encoding, including typed GC refs.
  (let ((start (wasm-input-position input)) (code (module-byte input)))
    (cond ((member code '(#x63 #x64)) (module-uleb input))
          ((not (member code '(#x7f #x7e #x7d #x7c #x7b #x70 #x6f #x6e #x6d #x6c #x6b #x6a #x69 #x68 #x67 #x66 #x78 #x77)))
           (unsupported-wasm-optimization)))
    (coerce (subseq (wasm-input-bytes input) start (wasm-input-position input)) 'list)))
(defun module-type-vector (input)
  (loop repeat (module-uleb input) collect (module-value-type input)))

(defun module-types (bytes)
  ;; Records: composite kind, parameter types, result types, struct field count.
  ;; Recursive groups are flattened only in this metadata, never in the module.
  (let ((input (wasm-input bytes)) (types nil))
    (labels ((field ()
               (if (member (aref bytes (wasm-input-position input)) '(#x77 #x78))
                   (module-byte input) (module-value-type input))
               (module-byte input))
             (subtype ()
               (let ((kind (module-byte input)))
                 (when (member kind '(#x4f #x50))
                   (dotimes (i (module-uleb input)) (module-uleb input))
                   (setq kind (module-byte input)))
                 (push (case kind
                         (#x60 (let ((parameters (module-type-vector input)))
                                  (vector kind parameters (module-type-vector input) 0)))
                         (#x5f (let ((count (module-uleb input)))
                                  (dotimes (i count) (field)) (vector kind nil nil count)))
                         (#x5e (field) (vector kind nil nil 0))
                         (#x5d (module-uleb input) (vector kind nil nil 0))
                         (otherwise (unsupported-wasm-optimization))) types))))
      (dotimes (i (module-uleb input))
        (if (= (aref bytes (wasm-input-position input)) #x4e)
            (progn (module-byte input) (dotimes (j (module-uleb input)) (subtype)))
            (subtype))))
    (coerce (nreverse types) 'vector)))

(defun module-import-functions (bytes)
  (let ((input (wasm-input bytes)) (functions nil))
    (labels ((limits ()
               (let ((flags (module-uleb input)))
                 (module-uleb input) (when (logbitp 0 flags) (module-uleb input)))))
      (dotimes (i (module-uleb input))
        (module-name input) (module-name input)
        (case (module-byte input)
          (0 (push (module-uleb input) functions))
          (1 (module-value-type input) (limits))
          (2 (limits))
          (3 (module-value-type input) (module-byte input))
          (4 (module-byte input) (module-uleb input))
          (otherwise (unsupported-wasm-optimization)))))
    (nreverse functions)))

(defun module-small-f64-integer (bytes)
  ;; Inspect IEEE bits without host float arithmetic (notably signaling NaNs).
  ;; Signed i32 integers convert exactly to f64 and have a shorter encoding.
  (let* ((bits (loop for byte across bytes for shift from 0 by 8 sum (ash byte shift)))
         (exponent (- (ldb (byte 11 52) bits) 1023)))
    (cond ((zerop bits) 0)
          ((and (<= 0 exponent 31)
                (zerop (ldb (byte (- 52 exponent) 0) bits)))
           (let ((value (ash (logior (ldb (byte 52 0) bits) (ash 1 52)) (- exponent 52))))
             (when (logbitp 63 bits) (setq value (- value)))
             (and (typep value '(signed-byte 32)) value))))))

(defun module-read-body (bytes signature types functions effects
                        &optional observe-function observe-instruction observe-label)
  (let* ((input (wasm-input bytes)) (parameters (aref signature 1))
         (locals nil) (ops nil))
    (dotimes (i (module-uleb input))
      (let* ((count (module-uleb input)) (type (module-value-type input)))
        (when effects (dotimes (j count) (push type locals)))))
    (labels ((u () (module-uleb input))
             (label-depth ()
               (let* ((start (wasm-input-position input)) (depth (u)))
                 (when observe-label
                   (funcall observe-label depth start (wasm-input-position input)))
                 depth))
             (skip (count) (dotimes (i count) (u)))
             (block-type ()
               (let ((start (wasm-input-position input)))
                 (if (member (aref bytes start) '(#x63 #x64)) (module-value-type input) (u))
                 (if (and (= (wasm-input-position input) (1+ start)) (= (aref bytes start) #x40)) #x40 t)))
             (call-effect (index indirect)
               (let ((type (aref types index)))
                 (unless (= (aref type 0) #x60) (unsupported-wasm-optimization))
                 (cons (+ indirect (length (aref type 1))) (length (aref type 2))))))
      (loop while (< (wasm-input-position input) (length bytes)) do
        (let* ((start (wasm-input-position input)) (code (module-byte input))
               (normalized code) (argument nil) (effect (cons 0 0)))
          (cond
            ((member code '(#x00 #x01 #x05 #x0b #x0f)))
            ((member code '(#x02 #x03 #x04)) (setq argument (block-type)))
            ((= code #x1f)
             (block-type)
             (dotimes (i (u))
               (let ((kind (module-byte input)))
                 (unless (<= 0 kind 3) (unsupported-wasm-optimization))
                 ;; The instruction observer pushes TRY_TABLE after its
                 ;; catches have read labels in the surrounding scope.
                 (when (< kind 2) (u)) (label-depth))))
            ((= code #x08) (u))
            ((= code #x0a) (setq normalized #x08))
            ((member code '(#x0c #x0d)) (setq argument (label-depth)))
            ((= code #x0e) (setq argument (loop repeat (1+ (u)) collect (label-depth))))
            ((member code '(#x10 #x12))
             (setq argument (u) effect (call-effect (aref functions argument) 0)
                   normalized (if (= code #x12) #x08 #x10))
             (when observe-function
               (funcall observe-function code argument start (wasm-input-position input))))
            ((member code '(#x11 #x13 #x14 #x15))
             (setq effect (call-effect (u) 1))
             (when (member code '(#x11 #x13)) (u))
             (setq normalized (if (member code '(#x13 #x15)) #x08 #x10)))
            ((= code #x1a) (setq effect (cons 1 0)))
            ((member code '(#x1b #x1c))
             (when (= code #x1c) (module-type-vector input))
             (setq effect (cons 3 1)))
            ((member code '(#x20 #x21 #x22)) (setq argument (u)))
            ((member code '(#x23 #x24 #x25 #x26))
             (setq argument (u)) (setq effect (case code (#x23 (cons 0 1)) (#x24 (cons 1 0))
                                      (#x25 (cons 1 1)) (#x26 (cons 2 0)))))
            ((<= #x28 code #x3e)
             (let ((alignment (u)))
               (when (logbitp 6 alignment) (u))
               (unless (< alignment 128) (unsupported-wasm-optimization))
               (u))
             (setq effect (if (<= code #x35) (cons 1 1) (cons 2 0))))
            ((member code '(#x3f #x40)) (u) (setq effect (cons (if (= code #x40) 1 0) 1)))
            ((member code '(#x41 #x42)) (setq argument (module-sleb input) effect (cons 0 1)))
            ((member code '(#x43 #x44))
             (let ((bits (module-span input (if (= code #x43) 4 8))))
               (when (= code #x44) (setq argument (module-small-f64-integer bits))))
             (setq effect (cons 0 1)))
            ((<= #x45 code #xc4)
             (setq effect (cons (if (or (= code #x45) (= code #x50)
                                       (<= #x67 code #x69) (<= #x79 code #x7b)
                                       (<= #x8b code #x91) (<= #x99 code #x9f)
                                       (>= code #xa7)) 1 2) 1)))
            ((= code #xd0) (u) (setq effect (cons 0 1)))
            ((= code #xd2)
             (let ((index (u)))
               (when observe-function
                 (funcall observe-function code index start (wasm-input-position input))))
             (setq effect (cons 0 1)))
            ((member code '(#xd1 #xd4)) (setq effect (cons 1 1)))
            ((= code #xd3) (setq effect (cons 2 1)))
            ((member code '(#xd5 #xd6)) (setq argument (label-depth) normalized #x0d))
            ((= code #xfc)
             (let ((sub (u)))
               (cond ((<= 0 sub 7) (setq effect (cons 1 1)))
                     ((member sub '(8 10 12 14)) (skip 2) (setq effect (cons 3 0)))
                     ((member sub '(9 13)) (u))
                     ((member sub '(11 17)) (u) (setq effect (cons 3 0)))
                     ((= sub 15) (u) (setq effect (cons 2 1)))
                     ((= sub 16) (u) (setq effect (cons 0 1)))
                     (t (unsupported-wasm-optimization)))))
            ((= code #xfb)
             (let ((sub (u)))
               (case sub
                 (0 (setq effect (cons (aref (aref types (u)) 3) 1)))
                 (1 (u) (setq effect (cons 0 1)))
                 ((2 3 4) (skip 2) (setq effect (cons 1 1)))
                 (5 (skip 2) (setq effect (cons 2 0)))
                 (6 (u) (setq effect (cons 2 1)))
                 (7 (u) (setq effect (cons 1 1)))
                 (8 (u) (setq effect (cons (u) 1)))
                 ((9 10) (skip 2) (setq effect (cons 2 1)))
                 ((11 12 13) (u) (setq effect (cons 2 1)))
                 (14 (u) (setq effect (cons 3 0)))
                 (15 (setq effect (cons 1 1)))
                 (16 (u) (setq effect (cons 4 0)))
                 (17 (skip 2) (setq effect (cons 5 0)))
                 ((18 19) (skip 2) (setq effect (cons 4 0)))
                 ((20 21 22 23) (u) (setq effect (cons 1 1)))
                 ((24 25) (module-byte input) (setq argument (label-depth) normalized #x0d) (skip 2))
                 ((26 27 28 29 30) (setq effect (cons 1 1)))
                 (otherwise (unsupported-wasm-optimization)))))
            ;; SIMD, atomics and experimental instructions keep their original
            ;; function body. Unknown instructions must never be modeled as NOPs.
            (t (unsupported-wasm-optimization)))
          (when observe-instruction
            (funcall observe-instruction code argument start (wasm-input-position input)))
          (when effects
            (let ((op (make-native-op normalized argument start (wasm-input-position input))))
              (setf (gethash op effects) effect) (push op ops))))))
    (when effects
      (values (coerce (nreverse ops) 'simple-vector) (length parameters) (length locals)
              (coerce (append parameters (nreverse locals)) 'vector)))))

(defun module-write-bytes (bytes) (loop for byte across bytes do (wasm-byte byte)))
(defun module-write-section (id bytes)
  (wasm-byte id) (wasm-u32 (length bytes)) (module-write-bytes bytes))

(defun module-integer-zero-comparisons (bytes ops)
  ;; Use decoded instruction boundaries: opcode-looking bytes in an operand
  ;; are not instructions. Integer EQ against zero has the unary EQZ result.
  (let ((replacements nil))
    (loop for i below (1- (length ops))
          for constant = (aref ops i) for comparison = (aref ops (1+ i))
          for code = (aref bytes (native-op-start constant))
          when (and (eql (native-op-argument constant) 0)
                    (= (native-op-end constant) (native-op-start comparison))
                    (or (and (= code #x41)
                             (= (aref bytes (native-op-start comparison)) #x46))
                        (and (= code #x42)
                             (= (aref bytes (native-op-start comparison)) #x51))))
            do (push (list (native-op-start constant) (native-op-end comparison)
                           (if (= code #x41) #x45 #x50)) replacements))
    (if (null replacements) bytes
        (with-wasm-bytes
          (let ((position 0))
            (dolist (replacement (nreverse replacements))
              (wasm-append bytes position (first replacement))
              (wasm-byte (third replacement))
              (setq position (second replacement)))
            (wasm-append bytes position))))))

(defun module-label-mapping (ops bytes)
  (let ((next 0))
    (coerce (loop for op across ops
                  when (member (aref bytes (native-op-start op)) '(#x02 #x03 #x04 #x1f))
                  collect (when (member (native-op-code op) '(#x02 #x03 #x04 #x1f))
                            (prog1 next (incf next)))) 'vector)))

(defun module-local-names (bytes mappings label-mappings)
  (let ((input (wasm-input bytes)))
    (with-wasm-bytes
      (wasm-u32 (module-uleb input))
      ;; The custom-section name is always "name" here.
      (module-write-bytes (module-span input 4))
      (loop while (< (wasm-input-position input) (length bytes)) do
        (let* ((id (module-byte input)) (payload (module-span input (module-uleb input))))
          (if (not (member id '(2 3))) (module-write-section id payload)
              (let ((names (wasm-input payload)))
                (module-write-section id
                  (with-wasm-bytes
                    (let ((count (module-uleb names)))
                      (wasm-u32 count)
                      (dotimes (i count)
                        (let* ((function (module-uleb names))
                               (mapping (gethash function (if (= id 2) mappings label-mappings)))
                               (entries nil) (seen (make-hash-table :test 'eql)))
                          (dotimes (j (module-uleb names))
                            (let* ((old (module-uleb names)) (name (module-name names))
                                   (new (if mapping (and (< old (length mapping)) (aref mapping old)) old)))
                              (when (and new (not (gethash new seen)))
                                (setf (gethash new seen) t) (push (cons new name) entries))))
                          (setq entries (sort entries #'< :key #'car))
                          (wasm-u32 function) (wasm-u32 (length entries))
                          (dolist (entry entries)
                            (wasm-u32 (car entry)) (wasm-u32 (length (cdr entry)))
                            (module-write-bytes (cdr entry)))))))))))))))

(defun optimize-wasm-module (bytes &key (default-outline-policy 25) runtime-call-pairs)
  "Apply the existing local/stack passes to a core module, without changing ABI.
Unsupported instruction sets retain their original function bodies. Returns
the module, the number of smaller bodies, the number of unsupported bodies,
and the first appended helper index when outlining adds helpers."
  (unless (and (>= (length bytes) 8) (equalp (subseq bytes 0 8) #(0 97 115 109 1 0 0 0)))
    (error "Expected a core Wasm module, before component packaging."))
  (let ((input (wasm-input bytes 8)) (sections nil) (types #()) (imported nil) (defined nil)
        (mappings (make-hash-table :test 'eql)) (label-mappings (make-hash-table :test 'eql))
        (changed 0) (unsupported 0))
    (handler-case
        (progn
          (loop while (< (wasm-input-position input) (length bytes)) do
            (let* ((id (module-byte input)) (payload (module-span input (module-uleb input))))
              (push (cons id payload) sections)
              (case id
                (1 (setq types (module-types payload)))
                (2 (setq imported (module-import-functions payload)))
                (3 (let ((functions (wasm-input payload)))
                     (setq defined (loop repeat (module-uleb functions) collect (module-uleb functions)))))))))
      (unsupported-wasm-optimization () (return-from optimize-wasm-module (values bytes 0 0))))
    (setq sections (nreverse sections))
    (let ((functions (coerce (append imported defined) 'vector))
          (*native-module-stack-effects* nil) (*wasm-function-relocations* nil)
          (*wasm-current-function* nil))
      (dolist (section sections)
        (when (= (car section) 10)
          (let ((code (wasm-input (cdr section))))
            (setf (cdr section)
              (with-wasm-bytes
                (let ((count (module-uleb code)))
                  (unless (= count (length defined)) (error "Mismatched Wasm function/code sections."))
                  (wasm-u32 count)
                  (dotimes (i count)
                    (let* ((body (module-span code (module-uleb code)))
                           (function (+ (length imported) i))
                           (*native-module-stack-effects* (make-hash-table :test 'eq))
                           (decoded (handler-case
                                      (multiple-value-list (module-read-body body
                                        (aref types (aref functions function)) types functions
                                        *native-module-stack-effects*))
                                      (unsupported-wasm-optimization () (incf unsupported) nil))))
                      (when decoded
                        (multiple-value-bind (optimized mapping)
                            (apply #'native-optimize-body body decoded)
                          (setq optimized
                            (handler-case
                                (module-integer-zero-comparisons optimized
                                  (module-read-body optimized
                                    (aref types (aref functions function)) types functions
                                    (make-hash-table :test 'eq)))
                              (unsupported-wasm-optimization () optimized)))
                          (when (< (length optimized) (length body))
                            (setf (gethash function label-mappings)
                              (module-label-mapping (first decoded) body))
                            (setq body optimized) (incf changed)
                            (setf (gethash function mappings) mapping))))
                      (wasm-u32 (length body)) (module-write-bytes body)))))))))
      (multiple-value-bind (module first-helper)
          (outline-wasm-module
          (if (zerop changed) bytes
              (coerce (with-wasm-bytes
                (module-write-bytes (subseq bytes 0 8))
                (dolist (section sections)
                  (let ((payload (cdr section)))
                    (when (and (zerop (car section)) (>= (length payload) 5)
                               (equalp (subseq payload 0 5) #(4 110 97 109 101)))
                      (setq payload (module-local-names payload mappings label-mappings)))
                    (module-write-section (car section) payload))))
                '(simple-array (unsigned-byte 8) (*))))
            *native-outlining-budget* default-outline-policy runtime-call-pairs)
        (values module changed unsupported first-helper)))))
;; Host-only WAT identities survive GC type adaptation and whole-image linking.
(defvar *wasm-outline-policies* (make-hash-table :test 'equal))
(defun record-wasm-outline-policy (name policy)
  (when (and (plusp (length name)) (char= (char name 0) #\$)) (setq name (subseq name 1)))
  (let ((previous (gethash name *wasm-outline-policies*)))
    (setf (gethash name *wasm-outline-policies*)
      (if previous
          (+ (max (logand previous 3) (logand policy 3))
             (* 4 (min (logand 3 (ash previous -2)) (logand 3 (ash policy -2))))
             (* 16 (max (ash previous -4) (ash policy -4))))
          policy))))

(defun module-outline-policies (sections)
  (dolist (section sections)
    (when (zerop (car section))
      (let ((input (wasm-input (cdr section))))
        (when (equalp (module-name input) #(109 105 114 105 99 108 46 112 111 108 105 99 121))
          (let* ((count (module-uleb input)) (end (length (cdr section))))
            (unless (= (+ (wasm-input-position input) count) end)
              (error "Malformed Wasm compilation policies."))
            (let ((policies (module-span input count)))
              (unless (every (lambda (policy) (< policy 64)) policies)
                (error "Invalid Wasm compilation policy."))
              (return-from module-outline-policies policies))))))))

(defun module-outline-names (sections policies)
  ;; Keep metadata out of the runtime's exports and literal graph. WAT name
  ;; sections are still present here, before distribution stripping.
  (dolist (section sections)
    (when (zerop (car section))
      (let ((input (wasm-input (cdr section))))
        (when (equalp (module-name input) #(110 97 109 101))
          (loop while (< (wasm-input-position input) (length (cdr section))) do
            (let* ((id (module-byte input)) (payload (module-span input (module-uleb input))))
              (when (= id 1)
                (let ((names (wasm-input payload)))
                  (dotimes (i (module-uleb names))
                    (let* ((index (module-uleb names)) (name (map 'string #'code-char (module-name names)))
                           (policy (gethash name *wasm-outline-policies*)))
                      (when (and policy (< index (length policies))) (setf (aref policies index) policy)))))))))))))

(defun native-module-outline-policies (bytes module)
  ;; Recover the named linker identities of a compiler-produced module.
  (let ((input (wasm-input bytes 8)) (sections nil))
    (loop while (< (wasm-input-position input) (length bytes)) do
      (let ((id (module-byte input))) (push (cons id (module-span input (module-uleb input))) sections)))
    (let ((policies (module-outline-policies sections)))
      (unless policies (error "Native compiler module has no compilation policies."))
      (loop for policy across policies for i from 0 collect
        (cons (format nil "compiler-~D-~D" module i) policy)))))

(defstruct module-call-pair
  helper sites (gross-saving 0))

(defun module-runtime-export-indices (exports count)
  (let ((indices (make-array count :element-type 'bit :initial-element 0))
        (input (wasm-input exports)))
    (dotimes (i (module-uleb input) indices)
      (let ((name (map 'string #'code-char (module-name input)))
            (kind (module-byte input))
            (index (module-uleb input)))
        (when (and (zerop kind) (assoc name *native-imports* :test #'string=))
          (setf (sbit indices index) 1))))))

(defun module-copyable-call-op-p (body op parameter-count copyable-callee-p)
  (let ((start (native-op-start op)))
    (case (aref body start)
      (#x10 (funcall copyable-callee-p (native-op-argument op)))
      (#x20 (< (native-op-argument op) parameter-count))
      (#xfb
       ;; GC reads, tests, casts and representation conversions. No writes,
       ;; object allocations, function references, or control transfers.
       (member (native-read-uleb body (1+ start))
               '(2 3 4 11 12 13 15 20 21 22 23 26 27 29 30)))
      (otherwise
       (let ((code (aref body start)))
         (or (member code '(#x1a #x41 #x42 #x43 #x44 #xd0 #xd1 #xd3 #xd4))
             (<= #x45 code #xc4)))))))

(defun module-call-pair-body (body ops prefix-count second)
  (with-wasm-bytes
    (wasm-byte 0)
    ;; Leave the other arguments to the second call under the copied first
    ;; body, which uses only its own parameters and its original stack suffix.
    (dotimes (i prefix-count)
      (wasm-byte #x20) (wasm-u32 i))
    (loop for i below (1- (length ops))
          for op = (aref ops i) for start = (native-op-start op) do
      (if (= (aref body start) #x20)
          (progn (wasm-byte #x20) (wasm-u32 (+ prefix-count (native-op-argument op))))
          (wasm-append body start (native-op-end op))))
    ;; Execute the second call with the original caller's continuation and
    ;; handlers. Its live effects occur after the unchanged first body.
    (wasm-byte #x12) (wasm-u32 second) (wasm-byte #x0b)))

(defun module-select-call-pairs (records candidates function-index type-index)
  (let* ((count (length records))
         (occupied (make-array count :initial-element nil))
         (replacements (make-array count :initial-element nil))
         (helpers nil))
    (labels ((helper-cost (helper)
               (destructuring-bind (body parameters results) helper
                 (+ (length body) (wasm-u32-width (length body))
                    1 (wasm-u32-width (length parameters)) (wasm-u32-width (length results))
                    (loop for type in parameters sum (length type))
                    (loop for type in results sum (length type))
                    (wasm-u32-width type-index)
                    ;; Match the ordinary outliner's allowance for section growth.
                    16)))
             (available-p (function start end)
               (let ((bits (aref occupied function)))
                 (or (null bits)
                     (loop for pos from start below end always (zerop (sbit bits pos))))))
             (occupy (function start end)
               (let ((bits (or (aref occupied function)
                               (setf (aref occupied function)
                                 (make-array (length (first (aref records function)))
                                             :element-type 'bit :initial-element 0)))))
                 (loop for pos from start below end do (setf (sbit bits pos) 1))
                 (push (list start end function-index) (aref replacements function)))))
      (loop for candidate in (stable-sort candidates #'> :key #'module-call-pair-gross-saving)
            for call-width = (1+ (wasm-u32-width function-index))
            for helper = (module-call-pair-helper candidate)
            for sites = nil for gain = 0
            for previous-function = -1 for previous-end = 0
            while (< (length helpers) 64) do
        ;; Sites were discovered in increasing function and instruction order.
        (dolist (site (nreverse (module-call-pair-sites candidate)))
          (destructuring-bind (function start end) site
            (let ((saving (- end start call-width)))
              (when (and (plusp saving)
                         (or (/= function previous-function) (>= start previous-end))
                         (available-p function start end))
                (push site sites)
                (incf gain saving)
                (setf previous-function function previous-end end)))))
        (when (and (cdr sites) (>= (- gain (helper-cost helper)) *native-outlining-minimum-saving*))
          (dolist (site sites) (apply #'occupy site))
          (push helper helpers)
          (incf function-index)
          (incf type-index))))
    (values
      (loop for record across records for sites across replacements for body = (first record)
            collect
        (if sites
            (with-wasm-bytes
              (let ((position 0))
                (dolist (site (sort sites #'< :key #'first))
                  (destructuring-bind (start end index) site
                    (wasm-append body position start)
                    (wasm-byte #x10) (wasm-u32 index)
                    (setq position end)))
                (wasm-append body position)))
            body))
      (nreverse helpers))))

(defun module-outline-call-pairs (records signatures policies runtime-entries function-index type-index work)
  ;; Function indices coincide with record indices in this import-free module.
  ;; The export identities are supplied only by the host's private runtime build.
  (let ((copyable (make-hash-table :test 'eql))
        (groups (make-hash-table :test 'equal))
        (candidates nil) (site-count 0))
    (labels ((copyable-p (function depth)
               (when (and (plusp depth) (< function (length records)))
                 (destructuring-bind (body ops local-types effects) (aref records function)
                   (declare (ignore effects))
                   (when (and ops (<= (length body) 64) (<= 2 (length ops) 17))
                     (let ((key (+ (* function 4) (1- depth)))
                           (parameters (length (first (aref signatures function)))))
                       (multiple-value-bind (known present) (gethash key copyable)
                         (if present known
                             (setf (gethash key copyable)
                               (and (= (length local-types) parameters)
                                    (= (native-op-code (aref ops (1- (length ops)))) #x0b)
                                    (loop for i below (1- (length ops))
                                          always (module-copyable-call-op-p body (aref ops i) parameters
                                                   (lambda (callee) (copyable-p callee (1- depth))))))))))))))
             (helper (first second)
               (destructuring-bind (first-parameters first-results) (aref signatures first)
                 (destructuring-bind (second-parameters second-results) (aref signatures second)
                   (when (and (= (length first-results) 1)
                              (<= (length second-results) 1)
                              second-parameters
                              (equal (car first-results) (car (last second-parameters)))
                              (<= (+ (length first-parameters) (length second-parameters)) 9)
                              (copyable-p first 4))
                     (let* ((prefix (butlast second-parameters))
                            (record (aref records first)))
                       (list (module-call-pair-body (first record) (second record) (length prefix) second)
                             (append prefix first-parameters) second-results)))))))
      (loop for record across records for policy across policies for function from 0
            while (and (plusp work) (< site-count 32768)) do
        (destructuring-bind (body ops local-types effects) record
          (declare (ignore local-types effects))
          (when (and ops policy
                     (<= 1 (ldb (byte 2 2) policy))
                     (<= (logand policy 3) (ldb (byte 2 2) policy))
                     (<= (length body) work)
                     ;; Large bodies also obey the caller's compilation policy.
                     (<= (length body) (case (ash policy -4)
                                         (0 200000) (1 100000) (2 25000) (otherwise 4000))))
            (decf work (length body))
            (unless (find #x1f ops :key (lambda (op) (native-op-code op)))
              (loop for i below (1- (length ops))
                    for first-op = (aref ops i) for second-op = (aref ops (1+ i))
                    for start = (native-op-start first-op) for end = (native-op-end second-op)
                    for saving = (- end start 1 (wasm-u32-width function-index))
                    while (< site-count 32768)
                    ;; The decoder normalizes indirect and ref calls to #x10.
                    when (and (plusp saving)
                              (= (aref body start) #x10)
                              (= (aref body (native-op-start second-op)) #x10)) do
                (let ((first (native-op-argument first-op)) (second (native-op-argument second-op)))
                  (when (and (< first (length records)) (< second (length records))
                             (= 1 (sbit runtime-entries first)) (= 1 (sbit runtime-entries second)))
                    (let ((key (cons first second)))
                      (multiple-value-bind (group present) (gethash key groups)
                        (unless (or present (>= (hash-table-count groups) 4096))
                          (let ((helper (helper first second)))
                            (when helper
                              (setq group (make-module-call-pair :helper helper))
                              (push group candidates))
                            (setf (gethash key groups) group)))
                        (when group
                          (push (list function start end) (module-call-pair-sites group))
                          (incf (module-call-pair-gross-saving group) saving)
                          (incf site-count)))))))))))
      (module-select-call-pairs records (nreverse candidates) function-index type-index))))

(defun module-outline-record (body signature types functions)
  (let ((effects (make-hash-table :test 'eq)))
    (handler-case
        (multiple-value-bind (ops parameters locals local-types)
            (module-read-body body signature types functions effects)
          (declare (ignore parameters locals))
          (list body ops local-types effects))
      (unsupported-wasm-optimization () (list body nil nil nil)))))

(defun outline-wasm-module (bytes &optional (budget *native-outlining-budget*) (default-policy 25)
                                           runtime-call-pairs)
  ;; Host build adapter. Keep the shared matching/rewriting core independent of
  ;; recursive GC type parsing and the rest of the full-image binary format.
  (unless (plusp budget) (return-from outline-wasm-module bytes))
  (let ((input (wasm-input bytes 8)) (sections nil) (types nil)
        (imported nil) (defined nil) (records nil) (helpers nil) (replacements nil) (policies nil))
    (handler-case
        (loop while (< (wasm-input-position input) (length bytes)) do
          (let* ((id (module-byte input)) (payload (module-span input (module-uleb input))))
            (push (cons id payload) sections)
            (case id
              (1 (setq types (module-types payload)))
              (2 (setq imported (module-import-functions payload)))
              (3 (let ((functions (wasm-input payload)))
                   (setq defined (loop repeat (module-uleb functions) collect (module-uleb functions))))))))
      (unsupported-wasm-optimization () (return-from outline-wasm-module bytes)))
    (setq sections (nreverse sections))
    (let* ((functions (coerce (append imported defined) 'vector))
           (signatures (map 'vector (lambda (index) (let ((type (aref types index)))
                              (list (aref type 1) (aref type 2)))) functions)))
      (let ((code (wasm-input (cdr (assoc 10 sections)))))
        (unless (= (module-uleb code) (length defined)) (error "Mismatched Wasm code section."))
        (dotimes (i (length defined))
          (push (module-outline-record (module-span code (module-uleb code))
                   (aref types (aref functions (+ i (length imported)))) types functions)
                records)))
      (setq records (coerce (nreverse records) 'vector))
      (let* ((explicit (module-outline-policies sections))
             (named (make-array (length functions) :initial-element nil))
             (imports (cdr (assoc 2 sections)))
             (exports (cdr (assoc 7 sections))))
        (when (and explicit (/= (length explicit) (length records)))
          (error "Mismatched Wasm compilation policy count."))
        (module-outline-names sections named)
        (let ((recorded (loop for i below (length records)
                              collect (or (aref named (+ i (length imported)))
                                          (and explicit (aref explicit i))) into result
                              finally (return (coerce result 'vector)))))
          (setq policies (map 'vector (lambda (policy) (or policy default-policy)) recorded))
          (multiple-value-setq (replacements helpers)
            (native-outline-code records signatures
              (map 'vector (lambda (record policy) (and (second record) policy)) records policies) budget))
          (when (and runtime-call-pairs exports (or (null imports) (zerop (module-uleb (wasm-input imports)))))
            (let* ((type-count (length types))
                   (function-count (length functions))
                   (extended-types (concatenate 'vector types
                                     (loop for (body parameters results) in helpers
                                           collect (vector #x60 parameters results 0))))
                   (extended-functions (concatenate 'vector functions
                                         (loop repeat (length helpers) for i from type-count collect i))))
              ;; Decode only replacements changed by ordinary outlining. Its
              ;; helpers may be called from these bodies and need signatures.
              (loop for body in replacements for i from 0
                    unless (eq body (first (aref records i))) do
                (setf (aref records i)
                  (module-outline-record body (aref types (aref functions i))
                                         extended-types extended-functions)))
              (multiple-value-bind (bodies pair-helpers)
                  (module-outline-call-pairs records signatures recorded
                    (module-runtime-export-indices exports function-count)
                    (length extended-functions) (length extended-types) budget)
                (when pair-helpers
                  (setq replacements bodies helpers (append helpers pair-helpers)))))))))
    (unless helpers (return-from outline-wasm-module bytes))
    (values (with-wasm-bytes
      (wasm-append bytes 0 8)
      (dolist (section sections)
        (module-write-section (car section)
          (case (car section)
            ((1 3)
             (let ((payload (wasm-input (cdr section))))
               (with-wasm-bytes
                 (wasm-u32 (+ (module-uleb payload) (length helpers)))
                 (wasm-append (wasm-input-bytes payload) (wasm-input-position payload))
                 (loop for helper in helpers for i from (length types) do
                   (if (= (car section) 3) (wasm-u32 i)
                       (progn
                         (wasm-byte #x60)
                         (dolist (list (cdr helper))
                           (wasm-u32 (length list))
                           (dolist (type list) (dolist (byte type) (wasm-byte byte))))))))))
            (10 (with-wasm-bytes
                  (wasm-u32 (+ (length replacements) (length helpers)))
                  (dolist (body (append replacements (mapcar #'first helpers)))
                    (wasm-u32 (length body)) (wasm-append body))))
            (0 (let ((input (wasm-input (cdr section))))
                 (if (equalp (module-name input) #(109 105 114 105 99 108 46 112 111 108 105 99 121))
                     (with-wasm-bytes
                       (wasm-name "miricl.policy") (wasm-u32 (+ (length policies) (length helpers)))
                       (loop for policy across policies do (wasm-byte policy))
                       (dotimes (i (length helpers)) (wasm-byte 0)))
                     (cdr section))))
            (otherwise (cdr section))))))
      (+ (length imported) (length defined)))))

(defun module-layout-constant-expression (input pin)
  ;; Initializers in the linked core are ordinary Wasm constant expressions,
  ;; including GC allocation. Engine validation precedes this operand scan.
  (loop
    (case (module-byte input)
      (#x0b (return))
      ((#x41 #x42 #xd0) (module-sleb input))
      (#x43 (module-span input 4))
      (#x44 (module-span input 8))
      (#xd2 (funcall pin (module-uleb input)))
      (#xfb (case (module-uleb input)
              ((0 1 6 7) (module-uleb input))
              (8 (module-uleb input) (module-uleb input))
              (otherwise (unsupported-wasm-optimization))))
      (otherwise (unsupported-wasm-optimization)))))

(defun module-layout-check-end (input)
  (unless (= (wasm-input-position input) (length (wasm-input-bytes input)))
    (unsupported-wasm-optimization)))

(defun module-linked-code-sections (bytes)
  (let ((input (wasm-input bytes 8)))
    (loop while (< (wasm-input-position input) (length bytes))
          for id = (module-byte input)
          ;; Custom metadata could contain offsets. Imports and start
          ;; functions require additional index handling for helper placement.
          do (unless (member id '(1 3 4 5 6 7 9 10 11 12 13))
               (unsupported-wasm-optimization))
          collect (cons id (module-span input (module-uleb input))))))

(defun module-write-control-edits (body edits)
  (if (null edits) body
      (with-wasm-bytes
        (let ((position 0))
          (loop for (from to opcodes depth) in edits do
            (wasm-append body position from)
            (dolist (opcode opcodes) (wasm-byte opcode))
            (when depth (wasm-u32 depth))
            (setf position to))
          (wasm-append body position)))))

(defun module-terminal-branches (body signature types functions)
  (let ((frames (list nil)) (edits nil)
        (previous nil) (previous-argument nil) (previous-start nil)
        (prior nil) (prior-argument nil) (prior-start nil))
    (module-read-body body signature types functions nil nil
      (lambda (opcode argument start end)
        (declare (ignore end))
        (case opcode
          ((#x02 #x03 #x04 #x1f) (push opcode frames))
          (#x0b
           (let ((frame (pop frames)))
             ;; Keep the frame boundaries through validation. A taken branch
             ;; may discard operands that normal exit from its frame rejects.
             (cond ((and (null frame) (eql previous #x0f))
                    (push (list previous-start start) edits))
                   ((member frame '(#x02 #x04))
                    (cond ((and (eql previous #x0c) (zerop previous-argument))
                           (push (list previous-start start) edits))
                          ((and (eql previous #x0c) (eql prior #x0d) (zerop prior-argument))
                           (push (list prior-start start '(#x45 #x0d) previous-argument) edits))
                          ((and (eql previous #x0d) (zerop previous-argument))
                           (push (list previous-start start '(#x1a)) edits))))))))
        (psetf prior previous prior-argument previous-argument prior-start previous-start
               previous opcode previous-argument argument previous-start start)))
    (module-write-control-edits body (nreverse edits))))

(defstruct module-control-frame
  parent opcode start open-end close-start close-end void-p targeted-p removed-p level)

(defun module-adjacent-blocks-p (body inner outer)
  (and (eql (module-control-frame-opcode inner) #x02)
       (eql (module-control-frame-opcode outer) #x02)
       (= (module-control-frame-start inner) (module-control-frame-open-end outer))
       (= (module-control-frame-close-end inner) (module-control-frame-close-start outer))
       ;; Identical parameters give both frames the same operand base.
       (not (mismatch body body
                      :start1 (1+ (module-control-frame-start inner))
                      :end1 (module-control-frame-open-end inner)
                      :start2 (1+ (module-control-frame-start outer))
                      :end2 (module-control-frame-open-end outer)))))

(defun module-untargeted-scopes (body signature types functions)
  (let* ((root (make-module-control-frame :level 0))
         (stack (make-array 8 :fill-pointer 0 :adjustable t))
         (frames (list root)) (references nil) (edits nil))
    (vector-push-extend root stack)
    (module-read-body body signature types functions nil nil
      (lambda (opcode argument start end)
        (case opcode
          ((#x02 #x03 #x04 #x1f)
           (let ((frame (make-module-control-frame
                          :parent (aref stack (1- (length stack))) :opcode opcode
                          :start start :open-end end :void-p (eql argument #x40))))
             (push frame frames)
             (vector-push-extend frame stack)))
          (#x0b
           (let ((frame (vector-pop stack)))
             (setf (module-control-frame-close-start frame) start
                   (module-control-frame-close-end frame) end)))))
      (lambda (depth start end)
        (let* ((top (1- (length stack)))
               (source (aref stack top)) (target (aref stack (- top depth))))
          (setf (module-control-frame-targeted-p target) t)
          (push (list start end depth source target) references))))
    (dolist (frame frames)
      (let ((parent (module-control-frame-parent frame)))
        (when parent
          (cond ((module-adjacent-blocks-p body frame parent)
                 ;; Branches to either frame return to the same continuation.
                 ;; Transfer uses before the parent's removal decision.
                 (when (module-control-frame-targeted-p frame)
                   (setf (module-control-frame-targeted-p parent) t))
                 (setf (module-control-frame-removed-p frame) t))
                ((and (module-control-frame-void-p frame)
                      (member (module-control-frame-opcode frame) '(#x02 #x03))
                      (not (module-control-frame-targeted-p frame)))
                 (setf (module-control-frame-removed-p frame) t))))))
    (loop for frame in (nreverse frames) for parent = (module-control-frame-parent frame)
          when parent do
            (setf (module-control-frame-level frame)
              (+ (module-control-frame-level parent)
                 (if (module-control-frame-removed-p frame) 0 1)))
            (when (module-control-frame-removed-p frame)
              (push (list (module-control-frame-start frame) (module-control-frame-open-end frame)) edits)
              (push (list (module-control-frame-close-start frame) (module-control-frame-close-end frame)) edits)))
    (when edits
      (loop for (start end old-depth source target) in references
            for depth = (- (module-control-frame-level source) (module-control-frame-level target))
            unless (= depth old-depth) do (push (list start end nil depth) edits)))
    (module-write-control-edits body (sort edits #'< :key #'car))))

(defun rewrite-linked-wasm-control (bytes rewrite-body)
  "Propose a control-flow rewrite of validated, stripped and fully linked core code."
  (handler-case
      (let* ((sections (module-linked-code-sections bytes))
             (function-section (assoc 3 sections)) (code-section (assoc 10 sections)))
        (unless (and function-section code-section (assoc 1 sections))
          (return-from rewrite-linked-wasm-control bytes))
        (let* ((function-input (wasm-input (cdr function-section)))
               (count (module-uleb function-input))
               (functions (make-array count))
               (types (module-types (cdr (assoc 1 sections))))
               (code (wasm-input (cdr code-section)))
               (changed nil))
          (dotimes (i count)
            (setf (aref functions i) (module-uleb function-input)))
          (module-layout-check-end function-input)
          (unless (= (module-uleb code) count) (unsupported-wasm-optimization))
          (setf (cdr code-section)
            (with-wasm-bytes
              (wasm-u32 count)
              (dotimes (i count)
                (let* ((body (module-span code (module-uleb code)))
                       (rewritten
                         (handler-case
                             (funcall rewrite-body body (aref types (aref functions i)) types functions)
                           (unsupported-wasm-optimization () body))))
                  (unless (eq body rewritten) (setf changed t))
                  (wasm-u32 (length rewritten))
                  (wasm-append rewritten)))))
          (module-layout-check-end code)
          (if changed
              (let ((result (with-wasm-bytes
                              (wasm-append bytes 0 8)
                              (loop for (id . payload) in sections do
                                (module-write-section id payload)))))
                (if (< (length result) (length bytes))
                    (coerce result '(simple-array (unsigned-byte 8) (*)))
                    bytes))
              bytes)))
    (unsupported-wasm-optimization () bytes)))

(defun module-pin-function-indices (sections count)
  (let ((pinned (make-array count :element-type 'bit :initial-element 0)))
    (flet ((pin (index) (setf (sbit pinned index) 1)))
      (loop for (id . payload) in sections
            when (member id '(4 6 7 9 11)) do
              (let ((input (wasm-input payload)))
                (dotimes (i (module-uleb input))
                  (case id
                    (4 ; Plain funcref table with minimum and optional maximum.
                     (unless (= (module-byte input) #x70) (unsupported-wasm-optimization))
                     (case (module-uleb input)
                       (0 (module-uleb input))
                       (1 (module-uleb input) (module-uleb input))
                       (otherwise (unsupported-wasm-optimization))))
                    (6 (module-value-type input) (module-byte input)
                       (module-layout-constant-expression input #'pin))
                    (7 (module-name input)
                       (let* ((kind (module-byte input)) (index (module-uleb input)))
                         (when (zerop kind) (pin index))))
                    (9 ; Active index vector with explicit table and i32 offset.
                     (unless (= (module-uleb input) 2) (unsupported-wasm-optimization))
                     (module-uleb input)
                     (unless (= (module-byte input) #x41) (unsupported-wasm-optimization))
                     (module-sleb input)
                     (unless (and (= (module-byte input) #x0b) (= (module-byte input) 0))
                       (unsupported-wasm-optimization))
                     (dotimes (entry (module-uleb input)) (pin (module-uleb input))))
                    (11 ; Passive data at this point contains no core indices.
                     (unless (= (module-uleb input) 1) (unsupported-wasm-optimization))
                     (let ((size (module-uleb input)))
                       (incf (wasm-input-position input) size)))))
                (module-layout-check-end input))))
    pinned))

(defun module-write-compact-body (body edits indices)
  (let ((start 0))
    (loop for (from end kind argument operand-end) in edits do
      (ecase kind
        (:call
         (let ((index (aref indices argument)))
           (when index
             (wasm-append body start from)
             (wasm-u32 index)
             (setf start end))))
        (:null-branch
         (wasm-append body start from)
         (wasm-byte #xd5) ; br_on_null with the original br_if depth
         (wasm-append body argument operand-end)
         (setf start end))))
    (wasm-append body start)))

(defun compact-linked-wasm-code (bytes first-helper)
  "Shorten helper calls and null branches in a validated, stripped and fully linked core."
  (handler-case
      (let ((sections (module-linked-code-sections bytes)))
        (unless (and (assoc 1 sections) (assoc 3 sections) (assoc 10 sections))
          (return-from compact-linked-wasm-code bytes))
        (let* ((function-input (wasm-input (cdr (assoc 3 sections))))
               (count (module-uleb function-input))
               (pinned (when first-helper (module-pin-function-indices sections count)))
               (types (module-types (cdr (assoc 1 sections))))
               (functions (make-array count))
               (bodies (make-array count))
               (edits (make-array count :initial-element nil))
               (counts (make-array count :initial-element 0))
               (changed nil)
               (code (wasm-input (cdr (assoc 10 sections)))))
          (dotimes (i count)
            (setf (aref functions i) (module-uleb function-input)))
          (module-layout-check-end function-input)
          (unless (= (module-uleb code) count) (unsupported-wasm-optimization))
          (dotimes (i count)
            (let ((body (module-span code (module-uleb code)))
                  (local nil) (test-start nil) (branch-operand nil) (branch-end nil))
              (setf (aref bodies i) body)
              (module-read-body body (aref types (aref functions i)) types functions nil
                (when first-helper
                  (lambda (opcode index start end)
                    (if (= opcode #xd2)
                        (setf (sbit pinned index) 1)
                        (progn
                          (incf (aref counts index))
                          (push (list (1+ start) end :call index) (aref edits i))))))
                (lambda (opcode argument start end)
                  (cond
                    ((and branch-end (= opcode #x20) (= argument local))
                     ;; The taken edge discards the null reference. The
                     ;; fallthrough keeps the same reference that the second
                     ;; local.get would read. Retain the initial get or tee.
                     (push (list test-start end :null-branch branch-operand branch-end)
                           (aref edits i))
                     (setf changed t local nil test-start nil branch-end nil))
                    ((and test-start (not branch-end) (= opcode #x0d))
                     (setf branch-operand (1+ start) branch-end end))
                    ((and local (not test-start) (= opcode #xd1))
                     (setf test-start start))
                    (t
                     (setf local (when (member opcode '(#x20 #x22)) argument)
                           test-start nil branch-end nil)))))
              (setf (aref edits i) (nreverse (aref edits i)))))
          (module-layout-check-end code)
          (let ((indices (make-array count :initial-element nil)))
            (when first-helper
              (let ((short (loop for i below (min 128 first-helper)
                                 when (zerop (sbit pinned i)) collect i))
                    (helpers (loop for i from (max 128 first-helper) below count
                                   when (zerop (sbit pinned i)) collect i)))
                ;; Stable ordering gives ties to the lower original indices.
                (flet ((uses (index) (aref counts index)))
                  (loop for low in (stable-sort short #'< :key #'uses)
                        for high in (stable-sort helpers #'> :key #'uses)
                        while (< (uses low) (uses high))
                        do (setf (aref indices low) high (aref indices high) low changed t)))))
            (unless changed (return-from compact-linked-wasm-code bytes))
            ;; A function's body and type move together. All escaping indices
            ;; are pinned. For relocation, rewrite only the immediates of
            ;; direct calls and direct tail calls found in the final bodies.
            (let ((result
                    (with-wasm-bytes
                      (wasm-append bytes 0 8)
                      (loop for (id . payload) in sections do
                        (module-write-section id
                          (case id
                            (3 (with-wasm-bytes
                                 (wasm-u32 count)
                                 (dotimes (i count)
                                   (wasm-u32 (aref functions (or (aref indices i) i))))))
                            (10 (with-wasm-bytes
                                  (wasm-u32 count)
                                  (dotimes (i count)
                                    (let* ((index (or (aref indices i) i))
                                           (body (aref bodies index))
                                           (body-edits (aref edits index))
                                           (rewritten
                                             (if (some (lambda (edit)
                                                         (or (eq (third edit) :null-branch)
                                                             (aref indices (fourth edit))))
                                                       body-edits)
                                                 (with-wasm-bytes
                                                   (module-write-compact-body body body-edits indices))
                                                 body)))
                                      (wasm-u32 (length rewritten))
                                      (wasm-append rewritten)))))
                            (otherwise payload)))))))
              (if (< (length result) (length bytes))
                  (coerce result '(simple-array (unsigned-byte 8) (*)))
                  bytes)))))
    (unsupported-wasm-optimization () bytes)))
