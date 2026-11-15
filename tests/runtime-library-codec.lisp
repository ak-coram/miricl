;;;; Bootstrap packing round trips through the production reader. Test exports
;;;; exist only in this temporary artifact, never in a distributed image.
(let* ((directory (merge-pathnames
                   (format nil "miricl-library-codec-~D-~D/" (get-universal-time) (random most-positive-fixnum))
                   (uiop:temporary-directory)))
       (image (merge-pathnames "reader.wasm" directory))
       (fixture (merge-pathnames "forms.bin" directory))
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
(func (export ~S) (global.set $library-read-budget (i32.const 0)))
" "test-library-prepare" "test-library-read" "test-library-bind"
                    (miricl::runtime-symbol-id '*) "test-library-same" "test-library-car" "test-library-exhaust")))
    (with-open-file (out image :direction :output :element-type '(unsigned-byte 8))
      (write-sequence
       (wasmtime:wat-to-wasm
        (miricl::runtime-prune-wat (concatenate 'string (subseq text 0 (position #\) text :from-end t)) hooks ")"))) out)))
  (uiop:run-program (list "node" (namestring (asdf:system-relative-pathname :miricl "tests/runtime-library-codec.mjs"))
                         (namestring image) (namestring fixture))
                    :output *standard-output* :error-output *error-output*))
