;;;; Stream sequence migration regression suite.
(in-package #:miricl-test)
(in-suite :miricl)

(defun stream-kernel-test-size (sequence)
  ;; Host LENGTH is not required to terminate on circular lists. The target's
  ;; canonical LENGTH boundary deliberately rejects them before unchecked code.
  (if (vectorp sequence) (length sequence)
      (or (miricl-runtime:list-length-or-cycle sequence)
          (error 'type-error :datum sequence :expected-type 'list))))

(defun stream-kernel-test-operations (&optional fields)
  (vector #'stream-kernel-test-size #'read-byte #'read-char #'write-byte #'write-char
          #'write-sequence #'write-string #'write-line fields #'make-hash-table #'gethash
          (lambda (value key table) (setf (gethash key table) value)) #'remhash #'eq #'error))

(test stream-sequence-control-kernel
  (let ((stream (make-string-output-stream)))
    (dolist (kind '(string vector list))
      (let ((sequence (ecase kind (string "abcd") (vector #(#\a #\b #\c #\d))
                                       (list '(#\a #\b #\c #\d)))))
        (dolist (bounds '((0 nil 0 4) (1 3 1 3) (4 4 4 4)))
          (destructuring-bind (first stop expected-first expected-stop) bounds
            (let ((state (miricl-runtime:stream-sequence-control-values
                           (list sequence stream :start first :end stop) 1 (stream-kernel-test-operations))))
              (is (eq sequence (svref state 0))) (is (eq stream (svref state 1)))
              (is (= expected-first (svref state 2))) (is (= expected-stop (svref state 3))))))))
    (is (equalp (vector "abcd" stream 1 3)
          (miricl-runtime:stream-sequence-control-values
            (list "abcd" stream :start 1 :start -9 :end 3 :end -8) 1 (stream-kernel-test-operations))))
    (is (equalp #("abcd" nil 0 4)
          (miricl-runtime:stream-sequence-control-values '("abcd") 2 (stream-kernel-test-operations))))
    (is (miricl-runtime:stream-sequence-control-values
          (list "abcd" stream :unknown 9 :allow-other-keys t) 1 (stream-kernel-test-operations)))
    (dolist (arguments (list nil (list "abcd") (list "abcd" stream :start)
                            (list "abcd" stream :unknown 9)
                            (list "abcd" stream :unknown 9 :allow-other-keys nil :allow-other-keys t)))
      (is (null (miricl-runtime:stream-sequence-control-values arguments 1 (stream-kernel-test-operations)))))
    (dolist (bounds '((-1 nil) (0 5) (4 3) (nil nil) (0 -1) (1.5 nil) (0 1.5)
                      (100000000000000000000 nil) (0 100000000000000000000)))
      (signals type-error
        (miricl-runtime:stream-sequence-control-values
          (list "abcd" stream :start (first bounds) :end (second bounds)) 1 (stream-kernel-test-operations))))
    (signals type-error
      (miricl-runtime:stream-sequence-control-values (list #(1 2) stream) 2 (stream-kernel-test-operations)))
    (signals type-error
      (miricl-runtime:stream-sequence-control-values (list '(#\a . #\b) stream) 1 (stream-kernel-test-operations)))
    (let ((cycle (list "abcd" stream)))
      (setf (cddr cycle) cycle)
      (signals type-error
        (miricl-runtime:stream-sequence-control-values cycle 1 (stream-kernel-test-operations))))
    (is (miricl-runtime:stream-sequence-control-values (list "ok" stream) 1 (stream-kernel-test-operations)))))

(test stream-sequence-text-kernel
  (dolist (source (list "aλ🙂bc" (coerce "aλ🙂bc" 'list) (coerce "aλ🙂bc" 'vector)
                        (make-array 5 :element-type 'character :displaced-to "xaλ🙂bcz" :displaced-index-offset 1)
                        (make-array 7 :element-type 'character :initial-contents "aλ🙂bcde" :fill-pointer 5)))
    (dolist (bounds '((0 0) (0 5) (1 3) (5 0)))
      (destructuring-bind (first count) bounds
        (is (string= (coerce (subseq source first (+ first count)) 'string)
                     (miricl-runtime:stream-sequence-text-value source first count (stream-kernel-test-operations)))))))
  (signals type-error (miricl-runtime:stream-sequence-text-value '(#\a 42 #\b) 0 3 (stream-kernel-test-operations))))

(test stream-sequence-transfer-kernel
  (let ((operations (stream-kernel-test-operations)))
    (dolist (kind '(string vector list))
      (let* ((target (ecase kind (string (make-string 5 :initial-element #\x))
                                      (vector (make-array 5 :initial-element #\x))
                                      (list (make-list 5 :initial-element #\x))))
             (stream (make-string-input-stream "aλ🙂")))
        (is (= 4 (miricl-runtime:stream-sequence-transfer-value target stream #(1 5 0 0 nil) operations)))
        (is (equal '(#\x #\a #\λ #\🙂 #\x) (coerce target 'list)))
        (is (= 2 (miricl-runtime:stream-sequence-transfer-value target stream #(2 5 0 0 nil) operations)))))
    (dolist (source (list (coerce "aλ🙂bc" 'vector) (coerce "aλ🙂bc" 'list)))
      (let ((stream (make-string-output-stream)))
        (is (= 4 (miricl-runtime:stream-sequence-transfer-value source stream #(1 4 0 3 nil) operations)))
        (is (string= "λ🙂b" (get-output-stream-string stream)))))
    (let ((data (copy-seq "abcde")))
      (is (= 5 (miricl-runtime:stream-sequence-transfer-value data data #(1 5 0 2 nil) operations)))
      (is (string= "aaaaa" data)))
    (let ((target (vector nil nil nil)))
      (setf (svref operations 2)
        (let ((next 0)) (lambda (&rest args) (declare (ignore args))
                         (incf next) (values (if (= next 3) nil #\a) :ignored))))
      (is (= 2 (miricl-runtime:stream-sequence-transfer-value target nil #(0 3 0 0 nil) operations)))
      (is (equalp #(#\a #\a nil) target)))))

(test stream-sequence-broadcast-kernel
  (let* ((a (make-string-output-stream)) (b (make-string-output-stream))
         (root (make-string-output-stream)) (alias (make-string-output-stream))
         (metadata (make-hash-table :test 'eq)) (trace nil))
    ;; Actual streams are used for conditions and leaf effects; the side table
    ;; models representation access without depending on the host's layouts.
    (setf (gethash a metadata) (list 22 nil))
    (setf (gethash b metadata) (list 22 nil))
    (setf (gethash root metadata) (list 43 (list alias b a)))
    (setf (gethash alias metadata) (list 41 a))
    (flet ((fields (node selector packet)
             (let ((data (gethash node metadata)))
               (unless data (error 'type-error :datum node :expected-type 'stream))
               (case selector
                 (0 (first data)) (1 (second data))
                 (2 (stream-kernel-test-size (second data)) (second data))
                 (3 (push node trace)
                    (let ((sequence (svref packet 0)) (first (svref packet 1))
                          (count (svref packet 2)) (mode (svref packet 3)))
                      (funcall (case mode (1 #'write-sequence) (2 #'write-string) (3 #'write-line))
                               sequence node :start first :end (+ first count))))))))
      (let ((operations (stream-kernel-test-operations #'fields)))
        (is (null (miricl-runtime:stream-broadcast-sequence-value "abcd" root #("abcd" 1 2 1) operations)))
        (is (equal (list a b a) (nreverse trace)))
        (is (string= "bcbc" (get-output-stream-string a)))
        (is (string= "bc" (get-output-stream-string b)))
        (setf (gethash alias metadata) (list 41 root))
        (signals stream-error
          (miricl-runtime:stream-broadcast-sequence-value "x" root #("x" 0 1 1) operations))
        (setf (gethash alias metadata) (list 41 a))
        (is (null (miricl-runtime:stream-broadcast-sequence-value "ok" alias #("ok" 0 2 3) operations)))
        (is (string= (format nil "ok~%") (get-output-stream-string a)))))))

(test stream-sequence-raw-value-packets
  (let ((original (fdefinition 'miricl::stream-sequence-runtime-wat))
        (cases
          '(("controls" "" "(call $stream-sequence-arguments (call $cons (local.get $text) (call $cons (local.get $stream) (ref.null eq))) (i32.const 1)) (drop) (drop) (drop) (drop)" nil)
            ("text" "" "(drop (call $stream-sequence-text (local.get $vector) (i32.const 0) (i32.const 3)))" nil)
            ("read" "" "(drop (call $stream-sequence-transfer (local.get $vector) (local.get $input) (i32.const 0) (i32.const 3) (i32.const 0) (i32.const 0) (i32.const 0)))" nil)
            ("overlap" "" "(drop (call $stream-sequence-transfer (local.get $text) (local.get $text) (i32.const 1) (i32.const 5) (i32.const 0) (i32.const 2) (i32.const 0)))" nil)
            ("broadcast" "" "(call $write-stream-sequence (local.get $text) (local.get $root) (i32.const 0) (i32.const 3) (i32.const 1))" nil)
            ("missing-key-value" "" "(call $stream-sequence-arguments (call $cons (local.get $text) (call $cons (local.get $stream) (call $cons (call $symbol (i32.const START-KEY-ID)) (ref.null eq)))) (i32.const 1)) (drop) (drop) (drop) (drop)" program-error)
            ("negative-text-region" "" "(drop (call $stream-sequence-text (local.get $text) (i32.const -1) (i32.const 1)))" type-error)
            ("invalid-character" "(array.set $objects (call $vector-data (local.get $vector)) (i32.const 1) (call $int (i32.const 42)))" "(drop (call $stream-sequence-text (local.get $vector) (i32.const 0) (i32.const 3)))" type-error)
            ("read-region" "" "(drop (call $stream-sequence-transfer (local.get $vector) (local.get $input) (i32.const 0) (i32.const 4) (i32.const 0) (i32.const 0) (i32.const 0)))" type-error)
            ("broadcast-cycle" "(struct.set $obj $a (ref.cast (ref $obj) (local.get $root)) (call $cons (local.get $root) (ref.null eq)))" "(call $write-stream-sequence (local.get $text) (local.get $root) (i32.const 0) (i32.const 3) (i32.const 1))" stream-error)
            ("broadcast-spine-cycle" "(local.set $children (call $cons (local.get $stream) (ref.null eq))) (struct.set $obj $b (ref.cast (ref $obj) (local.get $children)) (local.get $children)) (struct.set $obj $a (ref.cast (ref $obj) (local.get $root)) (local.get $children))" "(call $write-stream-sequence (local.get $text) (local.get $root) (i32.const 0) (i32.const 3) (i32.const 1))" type-error)
            ("invalid-write-mode" "" "(call $write-stream-sequence (local.get $text) (local.get $stream) (i32.const 0) (i32.const 1) (i32.const 0))" program-error))))
    (unwind-protect
        (progn
          (setf (fdefinition 'miricl::stream-sequence-runtime-wat)
            (lambda ()
              (miricl::runtime-wat-sequence
                (funcall original)
                (miricl::wat-fragment
                  (format nil
                    "(func (export ~S) (param $mode i32) (result i32)
                       (local $saved (ref null eq)) (local $text (ref null eq)) (local $vector (ref null eq))
                       (local $stream (ref null eq)) (local $input (ref null eq)) (local $root (ref null eq))
                       (local $children (ref null eq)) (local $caught i32) (local $ok i32)
                       (local.set $text (call $text (array.new_fixed $digits 5 (i32.const 97) (i32.const 98) (i32.const 99) (i32.const 100) (i32.const 101))))
                       (local.set $vector (call $vector-new (array.new_fixed $objects 3
                         (call $node (i32.const 16) (i32.const 97) (ref.null eq) (ref.null eq) (ref.null eq))
                         (call $node (i32.const 16) (i32.const 955) (ref.null eq) (ref.null eq) (ref.null eq))
                         (call $node (i32.const 16) (i32.const 128578) (ref.null eq) (ref.null eq) (ref.null eq)))))
                       (local.set $stream (call $report-stream))
                       (local.set $input (call $make-input-stream (local.get $text) (call $int (i32.const 0)) (ref.null eq)))
                       (local.set $root (call $node (i32.const 43) (i32.const 0)
                         (call $cons (local.get $stream) (call $cons (local.get $stream) (ref.null eq))) (local.get $stream) (ref.null eq)))
                       (global.set $extra-values (call $cons (call $int (i32.const 42)) (ref.null eq)))
                       (global.set $value-count (i32.const 2))
                       (local.set $saved (global.get $extra-values))
                       (block $rejected (try_table (catch $transfer $rejected)
                         ~{~A~}
                         (local.set $caught (i32.const 1))))
                       (local.set $ok (i32.and
                         (i32.and (i32.eq (global.get $value-count) (i32.const 2)) (ref.eq (local.get $saved) (global.get $extra-values)))
                         ~A))
                       (global.set $error (i32.const 0))
                       (global.set $last-condition (ref.null eq))
                       (global.set $transfer-target (ref.null eq))
                       (global.set $transfer-values (ref.null eq))
                       (local.get $ok))"
                    "stream-sequence-packets"
                    (loop for (name setup call condition) in cases for mode from 0 collect
                      (format nil "(if (i32.eq (local.get $mode) (i32.const ~D)) (then ~A ~A))"
                              mode setup
                              (if (search "START-KEY-ID" call)
                                  (concatenate 'string (subseq call 0 (search "START-KEY-ID" call))
                                    (write-to-string (miricl::runtime-symbol-id :start))
                                    (subseq call (+ (search "START-KEY-ID" call) (length "START-KEY-ID")))) call)))
                    (format nil "(if (result i32) (i32.lt_u (local.get $mode) (i32.const 5)) (then (local.get $caught)) (else (i32.and (i32.eqz (local.get $caught)) ~A)))"
                      (format nil "(block (result i32) ~{~A~} (i32.const 0))"
                        (loop for (name setup call condition) in cases for mode from 0 when condition collect
                          (format nil "(if (i32.eq (local.get $mode) (i32.const ~D)) (then (br 1 (call $typep (global.get $last-condition) (call $symbol (i32.const ~D))))))"
                                  mode (miricl::runtime-symbol-id condition))))))))))
          (let* ((miricl::*interpreter-only-wasm* nil)
                 (miricl::*interpreter-wasm* (m:interpreter-wasm :compiler nil)))
            (m:with-interpreter (vm)
              (loop for (name) in cases for mode from 0 do
                (is (= 1 (wt:instance-call (miricl::interpreter-context vm) (miricl::interpreter-instance vm)
                                           "stream-sequence-packets" mode)) "Raw packet case ~A" name)
                (is (= 42 (m:evaluate-string vm "(+ 20 22)")))))))
      (setf (fdefinition 'miricl::stream-sequence-runtime-wat) original))))
