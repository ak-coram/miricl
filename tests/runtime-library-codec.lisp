;;;; Bootstrap packing round trips through the production reader. Test exports
;;;; exist only in this temporary artifact, never in a distributed image.
(let* ((miricl::*runtime-library-optimize-prefixes* nil)
       (directory (merge-pathnames
                   (format nil "miricl-library-codec-~D-~D/" (get-universal-time) (random most-positive-fixnum))
                   (uiop:temporary-directory)))
       (image (merge-pathnames "reader.wasm" directory))
       (fixture (merge-pathnames "forms.bin" directory))
       (strings-fixture (merge-pathnames "strings.bin" directory))
       (strings-expected (merge-pathnames "strings.json" directory))
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
" "test-library-prepare" "test-library-read" "test-library-bind"
                    (miricl::runtime-symbol-id '*) "test-library-same" "test-library-car"
                    "test-library-cdr" "test-library-exhaust")))
    (with-open-file (out image :direction :output :element-type '(unsigned-byte 8))
      (write-sequence
       (wasmtime:wat-to-wasm
        (miricl::runtime-prune-wat (concatenate 'string (subseq text 0 (position #\) text :from-end t)) hooks ")"))) out)))
  (uiop:run-program (list "node" (namestring (asdf:system-relative-pathname :miricl "tests/runtime-library-codec.mjs"))
                         (namestring image) (namestring fixture)
                         (namestring strings-fixture) (namestring strings-expected))
                    :output *standard-output* :error-output *error-output*))
