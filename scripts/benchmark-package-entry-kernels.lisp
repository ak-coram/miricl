;;;; Load after benchmark-runtime-kernels.lisp in the persistent host REPL.
(in-package #:cl-user)
(defvar *package-entry-benchmark-cache* (make-hash-table :test #'eq))
(defun package-entry-benchmark-fields (entry selector ignored)
  (declare (ignore ignored) (type simple-vector entry) (type fixnum selector)
           (optimize (speed 3) (safety 0) (debug 0)))
  (svref entry (- selector 4)))
(defun package-entry-benchmark-present (name package)
  ;; Allocate the host representation once during fixture setup. Its fields
  ;; implement the same entry ABI; no snapshots are allocated inside timing.
  (let* ((table (or (gethash package *package-entry-benchmark-cache*)
                    (setf (gethash package *package-entry-benchmark-cache*) (make-hash-table :test #'equal))))
         (cached (gethash name table)))
    (or cached
        (multiple-value-bind (symbol status) (find-symbol name package)
          (when (member status '(:internal :external))
            (setf (gethash name table) (vector symbol nil (if (eq status :external) 2 1))))))))
(defun package-entry-benchmark-inherited (name package)
  (declare (ignore name package)) nil)
(defparameter *package-entry-fields* #'package-entry-benchmark-fields)
(defparameter *package-entry-present* #'package-entry-benchmark-present)
(defparameter *package-entry-inherited* #'package-entry-benchmark-inherited)
(setf miricl-port-bench:*cases*
      (append
       (remove-if (lambda (case) (search "package-entry/" (first case))) miricl-port-bench:*cases*)
       '(("package-entry/hash-ascii" package-name-hash (list "ALPHABETICAL" 255))
         ("package-entry/hash-unicode" package-name-hash (list "αλφαβητικός🙂" 255))
         ("package-entry/present" find-present-package-entry
          (let ((p (or (find-package "ENTRY-BENCH-P") (make-package "ENTRY-BENCH-P" :use nil))))
            (intern "ALPHA" p)
            (list "ALPHA" (funcall *package-entry-present* "ALPHA" p) *package-entry-fields* #'symbol-name)) :calls 100000)
         ("package-entry/present-miss" find-present-package-entry
          (let ((p (or (find-package "ENTRY-BENCH-P") (make-package "ENTRY-BENCH-P" :use nil))))
            (intern "ALPHA" p)
            (list "MISS" (funcall *package-entry-present* "ALPHA" p) *package-entry-fields* #'symbol-name)) :calls 100000)
         ("package-entry/inherited" find-inherited-package-entry
          (let* ((a (or (find-package "ENTRY-BENCH-A") (make-package "ENTRY-BENCH-A" :use nil)))
                 (b (or (find-package "ENTRY-BENCH-B") (make-package "ENTRY-BENCH-B" :use nil)))
                 (symbol (intern "ALPHA" a)))
            (export symbol a) (import symbol b) (export symbol b)
            (funcall *package-entry-present* "ALPHA" a) (funcall *package-entry-present* "ALPHA" b)
            (list "ALPHA" (list a b) *package-entry-fields* *package-entry-present*)) :calls 50000)
         ("package-entry/inherited-empty" find-inherited-package-entry
          (list "MISSING" nil *package-entry-fields* *package-entry-present*))
         ("package-entry/accessible" find-accessible-package-entry
          (let ((p (or (find-package "ENTRY-BENCH-P") (make-package "ENTRY-BENCH-P" :use nil))))
            (intern "ALPHA" p) (funcall *package-entry-present* "ALPHA" p)
            (list "ALPHA" p *package-entry-present* *package-entry-inherited*)) :calls 100000)
         ("package-entry/accessible-miss" find-accessible-package-entry
          (let ((p (or (find-package "ENTRY-BENCH-EMPTY") (make-package "ENTRY-BENCH-EMPTY" :use nil))))
            (list "MISSING" p *package-entry-present* *package-entry-inherited*)) :calls 100000)
         ("package-entry/shadow" package-symbol-shadowed-p (list :c '(:a :b :c :d)))
         ("package-entry/shadow-miss" package-symbol-shadowed-p (list :missing '(:a :b :c :d)))
         ("package-entry/conflict" package-entry-conflict-p (list :incoming :present '(:a :b :c :d)))
         ("package-entry/conflict-shadowed" package-entry-conflict-p (list :incoming :present '(:a :present :c)))
         ("package-entry/conflict-identical" package-entry-conflict-p (list :present :present nil)))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(define-runtime-function $package-field-direct-bench (($count i32)) (i32)
  (:export "bench-package-entry-direct-field")
  (:locals ($entry :nullable-object) ($sum i32))
  #@(local.set $entry (call $present ,(wat-text-literal "CAR") (global.get $cl-package)))
  #@(block $done (loop $next
    (br_if $done (i32.eqz (local.get $count)))
    (local.set $sum (i32.add (local.get $sum)
      (i32.eqz (ref.is_null (call $package-node-ref (local.get $entry) (i32.const 4) (ref.null eq))))))
    (local.set $count (i32.sub (local.get $count) (i32.const 1))) (br $next)))
  #@(local.get $sum))
(define-runtime-function $package-field-dispatch-bench (($count i32)) (i32)
  (:export "bench-package-entry-dispatched-field")
  (:locals ($entry :nullable-object) ($function :nullable-object) ($sum i32))
  #@(local.set $entry (call $present ,(wat-text-literal "CAR") (global.get $cl-package)))
  #@(local.set $function (call $package-entry-function (i32.const 0)))
  #@(block $done (loop $next
    (br_if $done (i32.eqz (local.get $count)))
    (local.set $sum (i32.add (local.get $sum)
      (i32.eqz (ref.is_null (any.convert_extern
        (call $native-call3 (extern.convert_any (local.get $function)) (extern.convert_any (local.get $entry))
          (extern.convert_any (call $int (i32.const 4))) (extern.convert_any (ref.null eq))))))))
    (local.set $count (i32.sub (local.get $count) (i32.const 1))) (br $next)))
  #@(local.get $sum))
(defun package-entry-diagnostic-wat ()
  (runtime-wat-sequence
   ($package-field-direct-bench) ($package-field-dispatch-bench)
   (runtime-function-definition '$test-package-entry-install
    (wat-fragment #@(func $test-package-entry-install (export "test-package-entry-install")
      ,@(loop for name in '("*PACKAGE-ENTRY-FIELDS*" "*PACKAGE-ENTRY-PRESENT*" "*PACKAGE-ENTRY-INHERITED*")
              for index from 0 collect
              `(drop (call $set-symbol-value
                (call $intern ,(wat-text-literal name) (global.get $current-package))
                (call $package-entry-function (i32.const ,index))))))))))

(in-package #:miricl-port-bench)
(defun write-package-entry-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::$package-entry-function))
        (names '("package-name-hash" "find-present-package-entry" "find-inherited-package-entry"
                 "find-accessible-package-entry" "package-symbol-shadowed-p" "package-entry-conflict-p")))
    (unwind-protect
         (progn
           (setf (fdefinition 'miricl::$package-entry-function)
                 (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::package-entry-diagnostic-wat))))
           (write-benchmark-image path :catalog catalog :wat-path wat-path :names names))
      (setf (fdefinition 'miricl::$package-entry-function) original))))
