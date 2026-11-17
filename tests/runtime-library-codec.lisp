;;;; Bootstrap packing round trips through the production reader. Test exports
;;;; exist only in this temporary artifact, never in a distributed image.
(labels ((unpack-bytes (packed expected-size)
           ;; Independent reference reader for the byte envelope, before the
           ;; production Wasm reader reconstructs any Lisp source objects.
           (let ((bytes (make-array expected-size :element-type '(unsigned-byte 8)))
                 (cursor 0) (used 0))
             (loop while (< cursor (length packed)) do
               (let ((token (aref packed cursor)))
                 (incf cursor)
                 (if (< token 128)
                     (dotimes (index (1+ token))
                       (assert (< cursor (length packed)))
                       (assert (< used expected-size))
                       (setf (aref bytes used) (aref packed cursor))
                       (incf cursor) (incf used))
                     (progn
                       (assert (< (1+ cursor) (length packed)))
                       (let ((distance (+ (aref packed cursor)
                                          (* 256 (aref packed (1+ cursor))))))
                         (incf cursor 2)
                         (assert (<= 1 distance used))
                         (dotimes (index (+ 4 (- token 128)))
                           (assert (< used expected-size))
                           (setf (aref bytes used) (aref bytes (- used distance)))
                           (incf used)))))))
             (assert (= used expected-size))
             bytes))
         (check (bytes)
           (let ((packed (miricl::compress-runtime-library-bytes bytes)))
             (assert (equalp bytes (unpack-bytes packed (length bytes)))))))
  ;; Cross literal and match token boundaries, exercise overlapping matches,
  ;; and move older matches outside the 16-bit distance window.
  (dolist (size '(0 1 3 4 127 128 129 131 132 256 512 65535 65536 65700))
    (check (make-array size :element-type '(unsigned-byte 8) :initial-element 42))
    (check (map '(vector (unsigned-byte 8)) (lambda (index) (mod index 256))
                (loop for index below size collect index))))
  (let* ((size (+ 65536 1024))
         (bytes (make-array size :element-type '(unsigned-byte 8)))
         (state 17))
    (dotimes (index size)
      (setf state (mod (+ (* state 1664525) 1013904223) (ash 1 32))
            (aref bytes index) (ldb (byte 8 24) state)))
    (replace bytes bytes :start1 65536 :start2 0 :end2 512)
    (check bytes))
  (let ((repeated (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 17)))
    (assert (< (length (miricl::compress-runtime-library-bytes repeated)) (length repeated)))))

(let* ((first-name (symbol-name (gensym "MIRICL-BOOTSTRAP-PACKAGE-")))
       (second-name (symbol-name (gensym "MIRICL-BOOTSTRAP-PACKAGE-")))
       (shared (make-symbol "SHARED"))
       (earlier (make-symbol "EARLIER"))
       (later (make-symbol "LATER")))
  (labels ((preamble (name imported exported)
             `(let ((package (or (find-package ,name)
                                 (make-package ,name :use '("CL")))))
                (shadowing-import ',imported package)
                (export ',exported package))))
    (let* ((initial (preamble first-name (list shared earlier) (list shared)))
           (subsequent (preamble first-name (list earlier later shared) (list shared earlier later)))
           (other-package (preamble second-name (list shared) (list shared)))
           ;; Ordinary source package operations must stay at their original
           ;; evaluation points, even when they mention an already seen symbol.
           (ordinary `(shadowing-import ',(list shared) (find-package ,first-name)))
           (forms (list initial ordinary subsequent other-package))
           (compacted (miricl::compact-bootstrap-package-forms forms)))
      (assert (equal (first compacted) initial))
      (assert (eq (second compacted) ordinary))
      (assert (equal (third compacted)
                     (preamble first-name (list later) (list earlier later))))
      (assert (equal (fourth compacted) other-package))
      ;; Compare EQ symbol objects as well as package contents: replacing a
      ;; private symbol with a freshly interned namesake breaks native linkage.
      (unwind-protect
          (progn
            (mapc #'eval compacted)
            (dolist (symbol (list shared earlier later))
              (multiple-value-bind (found status) (find-symbol (symbol-name symbol) first-name)
                (assert (eq found symbol))
                (assert (eq status :external))))
            (assert (eq shared (find-symbol (symbol-name shared) second-name))))
        (dolist (name (list first-name second-name))
          (let ((package (find-package name)))
            (when package (delete-package package))))))))

(let* ((miricl::*runtime-library-optimize-prefixes* nil)
       (directory (merge-pathnames
                   (format nil "miricl-library-codec-~D-~D/" (get-universal-time) (random most-positive-fixnum))
                   (uiop:temporary-directory)))
       (image (merge-pathnames "reader.wasm" directory))
       (fixture (merge-pathnames "forms.bin" directory))
       (strings-fixture (merge-pathnames "strings.bin" directory))
       (strings-expected (merge-pathnames "strings.json" directory))
       (compression-fixture (merge-pathnames "compression.json" directory))
       (compression-bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
       (strings
         (append
          (loop for length in '(0 1 4 5 63 64 127 128 8191 8192)
                collect (make-string length :initial-element (code-char 255)))
          (list (map 'string #'code-char (loop for code below 256 collect code))
                "ordinary ASCII text"
                (map 'string #'code-char '(0 127 128 255 256 1114111))
                (make-array 8 :element-type 'character :fill-pointer 5
                              :initial-element (code-char 255))
                (make-array 5 :element-type 'character
                              :displaced-to (make-string 8 :initial-element (code-char 255))
                              :displaced-index-offset 2))))
       (forms (list '(1 2 3) '(1 2 3) '(4 5 6) '(4 5 6)
                    '("marker" "marker") '("marker" "marker") '("marker" "marker")
                    '(7 . 8) '(7 . 8) (make-list 32 :initial-element 9)
                    (make-list 40 :initial-element 10)
                    '((1 2 3) (4 5 6)) '((1 2 3) (4 5 6)))))
  ;; Cross the first ULEB ID-width boundary and then replay every input.
  (let ((boundary (loop for index below (+ 128 16)
                        collect (list (+ 1000 index) (+ 2000 index) (+ 3000 index)))))
    (setf forms (append forms boundary boundary)))
  (ensure-directories-exist image)
  (with-open-file (out fixture :direction :output :element-type '(unsigned-byte 8))
    (write-sequence (miricl::runtime-library-bytes forms nil 24) out))
  (let* ((noise (make-array (+ 65536 1024) :element-type '(unsigned-byte 8)))
         (state 17)
         (cases (append
                  (list (miricl::runtime-library-bytes forms nil 24))
                  (loop for size in '(0 1 3 4 127 128 129 131 132 1024)
                        collect (make-array size :element-type '(unsigned-byte 8) :initial-element 255))
                  (list noise (make-array 5 :element-type '(unsigned-byte 8) :initial-element 42))))
         (malformed '(((128 0 0) 4) ; Zero backward distance.
                      ((128 1 0) 4) ; No earlier output.
                      ((0 42 128 2 0) 5) ; Distance exceeds prefix.
                      ((2 1 2) 3) ; Truncated literals.
                      ((128) 4) ((128 1) 4) ; Truncated distances.
                      ((0 42) 0) ; Literal overrun.
                      ((0 42 128 1 0) 4) ; Match overrun.
                      ((0 42) 2) (nil 1) ; Output underrun.
                      ((0 42 0 43) 1) ; Trailing token.
                      (nil 16777216) (nil 4294967295)))) ; Huge lengths.
    (dotimes (index (length noise))
      (setf state (mod (+ (* state 1664525) 1013904223) (ash 1 32))
            (aref noise index) (ldb (byte 8 24) state)))
    (replace noise noise :start1 65536 :start2 0 :end2 512)
    (with-open-file (out compression-fixture :direction :output)
      (write-string "{\"valid\":[" out)
      (loop for bytes in cases for first = t then nil
            for packed = (miricl::compress-runtime-library-bytes bytes) do
        (unless first (write-char #\, out))
        (format out "[[~{~D~^,~}],~D,~D]" (coerce bytes 'list)
                (length compression-bytes) (length packed))
        (loop for byte across packed do (vector-push-extend byte compression-bytes)))
      (write-string "],\"invalid\":[" out)
      (loop for (bytes length) in malformed for first = t then nil do
        (unless first (write-char #\, out))
        (format out "[~D,~D,~D]" (length compression-bytes) (length bytes) length)
        (dolist (byte bytes) (vector-push-extend byte compression-bytes)))
      (write-string "]}" out)))
  ;; A separate first-string encoding makes the writer's size decision visible
  ;; without relying on the dictionary IDs of the larger fixture.
  (dolist (string strings)
    (let* ((bytes (miricl::runtime-library-bytes (list string) nil 24))
           (length (length string))
           (header-width (max 1 (ceiling (integer-length length) 7)))
           (ordinary-size (+ 1 header-width
                             (loop for char across string
                                   sum (max 1 (ceiling (integer-length (char-code char)) 7)))))
           (byte-string-p (every (lambda (char) (<= (char-code char) 255)) string))
           (raw-size (+ 1 5 length)))
      (assert (= (length bytes) (if byte-string-p (min ordinary-size raw-size) ordinary-size)))
      (let ((header 0) (shift 0))
        (loop for index from 1 for byte = (aref bytes index) do
          (setf header (logior header (ash (logand byte 127) shift)))
          (unless (logbitp 7 byte) (return))
          (incf shift 7))
        (assert (= (logand header #x7fffffff) length))
        (assert (eq (logbitp 31 header) (and byte-string-p (< raw-size ordinary-size)))))))
  ;; First occurrence introduces a string ID; the later source spines may
  ;; replay completed forms containing references, retaining fresh strings.
  (with-open-file (out strings-fixture :direction :output :element-type '(unsigned-byte 8))
    (write-sequence
     (miricl::runtime-library-bytes
      (loop for string in strings append (loop repeat 3 collect (list 'list string string))) nil 24) out))
  (with-open-file (out strings-expected :direction :output)
    (format out "[~{[~{~D~^,~}]~^,~}]~%"
            (mapcar (lambda (string) (map 'list #'char-code string)) strings)))
  (let* ((text (miricl::interpreter-wat :compiler nil))
         (hooks (format nil "
(func (export ~S) (param $length i32)
 (global.set $text-output-length (i32.const 0))
 (global.set $cursor (i32.const 0)) (global.set $limit (local.get $length))
 (call $prepare-library-reader))
(func (export ~S) (result externref) (extern.convert_any (call $read-library-literal)))
(func (export ~S) (param $value externref)
 (drop (call $set-symbol-value (call $symbol (i32.const ~D))
   (ref.cast (ref null eq) (any.convert_extern (local.get $value))))))
(func (export ~S) (param externref externref) (result i32)
 (ref.eq (ref.cast (ref null eq) (any.convert_extern (local.get 0)))
         (ref.cast (ref null eq) (any.convert_extern (local.get 1)))))
(func (export ~S) (param externref) (result externref)
 (extern.convert_any (call $car (ref.cast (ref null eq) (any.convert_extern (local.get 0))))))
(func (export ~S) (param externref) (result externref)
 (extern.convert_any (call $cdr (ref.cast (ref null eq) (any.convert_extern (local.get 0))))))
(func (export ~S) (global.set $library-read-budget (i32.const 0)))
 (func (export ~S) (param $start i32) (param $size i32) (param $length i32)
  (call $unpack-library-bytes
    (array.new_data $bootstrap-bytes $test-library-packed-data (local.get $start) (local.get $size))
    (local.get $length)))
~A
" "test-library-prepare" "test-library-read" "test-library-bind"
                    (miricl::runtime-symbol-id '*) "test-library-same" "test-library-car"
                    "test-library-cdr" "test-library-exhaust" "test-library-unpack"
                    (format nil "(data $test-library-packed-data \"~{\\~2,'0X~}\")"
                            (coerce compression-bytes 'list)))))
    (with-open-file (out image :direction :output :element-type '(unsigned-byte 8))
      (write-sequence
       (wasmtime:wat-to-wasm
        (miricl::runtime-prune-wat (concatenate 'string (subseq text 0 (position #\) text :from-end t)) hooks ")"))) out)))
  (uiop:run-program (list "node" (namestring (asdf:system-relative-pathname :miricl "tests/runtime-library-codec.mjs"))
                         (namestring image) (namestring fixture)
                         (namestring strings-fixture) (namestring strings-expected)
                         (namestring compression-fixture))
                    :output *standard-output* :error-output *error-output*))
