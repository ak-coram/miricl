;;;; Load after benchmark-runtime-kernels.lisp in a persistent SBCL host.
(in-package #:cl-user)
(defun stream-sequence-benchmark-fields (stream selector packet)
  ;; Equivalent one-step host substrate. The CL worker owns every graph edge.
  (ecase selector
    (0 (unless (open-stream-p stream) (error 'stream-error :stream stream))
       (cond ((typep stream 'broadcast-stream) 43) ((typep stream 'synonym-stream) 41)
             ((typep stream 'two-way-stream) 40) ((typep stream 'echo-stream) 46) (t 22)))
    (1 (cond ((typep stream 'synonym-stream) (symbol-value (synonym-stream-symbol stream)))
             ((typep stream 'two-way-stream) (two-way-stream-output-stream stream))
             ((typep stream 'echo-stream) (echo-stream-output-stream stream))
             (t (error 'type-error :datum stream :expected-type 'stream))))
    (2 (broadcast-stream-streams stream))
    (3 (let ((seq (svref packet 0)) (first (svref packet 1))
             (count (svref packet 2)) (mode (svref packet 3)))
         (ecase mode
           (1 (write-sequence seq stream :start first :end (+ first count)))
           (2 (write-string seq stream :start first :end (+ first count)))
           (3 (write-line seq stream :start first :end (+ first count))))))))
(defparameter *stream-sequence-bench-operations*
  (vector #'length #'read-byte #'read-char #'write-byte #'write-char
          #'write-sequence #'write-string #'write-line #'stream-sequence-benchmark-fields
          #'make-hash-table #'gethash #'(setf gethash) #'remhash #'eq #'error))
(setf miricl-port-bench:*cases*
 (append (remove-if (lambda (case) (search "stream-sequence/" (first case))) miricl-port-bench:*cases*)
  '(("stream-sequence/control-default" stream-sequence-control-values
     (list (list "abcdefghijklmnop" (make-string-output-stream)) 1 *stream-sequence-bench-operations*) :calls 100000)
    ("stream-sequence/control-keywords" stream-sequence-control-values
     (list (list (coerce "aλ🙂b" 'list) (make-string-output-stream) :start 1 :end 3) 1 *stream-sequence-bench-operations*) :calls 100000)
    ("stream-sequence/text-list" stream-sequence-text-value
     (list (coerce "aλ🙂bcdef" 'list) 1 6 *stream-sequence-bench-operations*) :calls 50000)
    ("stream-sequence/text-vector" stream-sequence-text-value
     (list (coerce "aλ🙂bcdef" 'vector) 1 6 *stream-sequence-bench-operations*) :calls 50000)
    ("stream-sequence/read-characters" stream-sequence-transfer-value
     (list (make-array 16) (make-string-input-stream (make-string 32016 :initial-element (code-char 955)))
           #(0 16 0 0 nil) *stream-sequence-bench-operations*) :calls 2000)
    ("stream-sequence/write-characters" stream-sequence-transfer-value
     (list "aλ🙂bcdefghijklmn" (make-string-output-stream) #(0 16 0 3 nil)
           *stream-sequence-bench-operations*) :calls 2000)
    ("stream-sequence/copy-vector" stream-sequence-transfer-value
     (list (make-array 16) (make-array 16 :initial-element (code-char 955)) #(0 16 0 2 nil)
           *stream-sequence-bench-operations*) :calls 100000)
    ("stream-sequence/broadcast-shared" stream-broadcast-sequence-value
     (let* ((out (make-string-output-stream)) (nested (make-broadcast-stream out out))
            (root (make-broadcast-stream nested nested)))
       (list "aλ🙂bcdef" root #("aλ🙂bcdef" 0 8 1) *stream-sequence-bench-operations*)) :calls 2000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun stream-sequence-diagnostic-wat ()
  (runtime-function-definition '$test-stream-sequence-install
   (wat-fragment #@(func $test-stream-sequence-install (export "test-stream-sequence-install")
     (drop (call $stream-sequence-function (i32.const 0)))
     (drop (call $set-symbol-value
       (call $intern ,(wat-text-literal "*STREAM-SEQUENCE-BENCH-OPERATIONS*") (global.get $current-package))
       (global.get $stream-sequence-functions)))))))
(in-package #:miricl-port-bench)
(defparameter *stream-sequence-names*
 '("stream-sequence-control-values" "stream-sequence-text-value" "stream-sequence-transfer-value"
   "stream-broadcast-sequence-value"))
(defun write-stream-sequence-benchmark (path &key catalog wat-path)
 (let ((original (fdefinition 'miricl::$stream-sequence-function)))
  (unwind-protect
    (progn
      (setf (fdefinition 'miricl::$stream-sequence-function)
            (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::stream-sequence-diagnostic-wat))))
      (write-benchmark-image path :catalog catalog :wat-path wat-path :names *stream-sequence-names*))
    (setf (fdefinition 'miricl::$stream-sequence-function) original))))
