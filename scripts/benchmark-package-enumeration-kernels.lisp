;;;; Load after benchmark-runtime-kernels.lisp in a persistent host REPL.
(in-package #:cl-user)
(defvar *package-enumeration-benchmark-entries* (make-hash-table :test 'eq))
(defun package-enumeration-benchmark-fields (node selector ignored)
  (declare (ignore ignored))
  (case selector
    (10 (list-length node))
    (2 (package-use-list node))
    (7 (or (gethash node *package-enumeration-benchmark-entries*)
           (setf (gethash node *package-enumeration-benchmark-entries*)
             (let ((result nil))
               (do-symbols (symbol node)
                 (let ((status (nth-value 1 (find-symbol (symbol-name symbol) node))))
                   (unless (eq status :inherited)
                     (push (vector symbol (if (eq status :external) 2 1) nil) result))))
               result))))
    (4 (svref node 0)) (6 (svref node 1))
    (8 (find-package node))
    (9 (or (find-package node) (error 'package-error :package node)))
    (otherwise (error 'program-error))))
(defun package-enumeration-benchmark-present (name package)
  (find name (package-enumeration-benchmark-fields package 7 nil)
    :key (lambda (entry) (symbol-name (svref entry 0))) :test #'string=))
(defun package-enumeration-benchmark-inherited (name package)
  (miricl-runtime:find-inherited-package-entry name (package-use-list package)
    #'package-enumeration-benchmark-fields #'package-enumeration-benchmark-present))
(defparameter *package-enumeration-fields* #'package-enumeration-benchmark-fields)
(defparameter *package-enumeration-callbacks*
  (vector #'package-enumeration-benchmark-fields #'package-enumeration-benchmark-present
          #'package-enumeration-benchmark-inherited #'symbol-name))
(defparameter *package-enumeration-state*
  (lambda (packages mask)
    (miricl-runtime:make-package-iteration-state packages mask *package-enumeration-callbacks*)))

(setf miricl-port-bench:*cases*
  (append (remove-if (lambda (case) (search "package-enumeration/" (first case))) miricl-port-bench:*cases*)
    '(("package-enumeration/names-free" package-names-available-p
       (list '("ENUM-BENCH-FREE-A" "ENUM-BENCH-FREE-B") nil *package-enumeration-fields*) :calls 100000)
      ("package-enumeration/names-conflict" package-names-available-p
       (list '("COMMON-LISP") nil *package-enumeration-fields*) :calls 100000)
      ("package-enumeration/local" collect-package-symbols
       (let ((p (or (find-package "ENUM-BENCH-LOCAL") (make-package "ENUM-BENCH-LOCAL" :use nil))))
         (intern "A" p) (intern "B" p) (import (list nil) p)
         (funcall *package-enumeration-fields* p 7 nil)
         (list p nil nil *package-enumeration-callbacks*)) :calls 100000)
      ("package-enumeration/inherited" collect-package-symbols
       (let* ((a (or (find-package "ENUM-BENCH-SOURCE") (make-package "ENUM-BENCH-SOURCE" :use nil)))
              (b (or (find-package "ENUM-BENCH-SECOND") (make-package "ENUM-BENCH-SECOND" :use nil)))
              (p (or (find-package "ENUM-BENCH-USING") (make-package "ENUM-BENCH-USING" :use nil)))
              (shared (intern "SHARED" a)))
         (export shared a) (import shared b) (export shared b) (use-package (list a b) p)
         (intern "LOCAL" p)
         (funcall *package-enumeration-fields* a 7 nil) (funcall *package-enumeration-fields* b 7 nil)
         (funcall *package-enumeration-fields* p 7 nil)
         (list p nil t *package-enumeration-callbacks*)) :calls 50000)
      ("package-enumeration/all" collect-all-package-symbols
       (let* ((a (or (find-package "ENUM-BENCH-ALL-A") (make-package "ENUM-BENCH-ALL-A" :use nil)))
              (b (or (find-package "ENUM-BENCH-ALL-B") (make-package "ENUM-BENCH-ALL-B" :use nil)))
              (shared (intern "SHARED" a)))
         (intern "LOCAL" b) (import shared b) (import (list nil) a)
         (funcall *package-enumeration-fields* a 7 nil) (funcall *package-enumeration-fields* b 7 nil)
         (list (list a b) *package-enumeration-fields*)) :calls 50000)
      ("package-enumeration/make-state" make-package-iteration-state
       (let ((p (or (find-package "ENUM-BENCH-STATE") (make-package "ENUM-BENCH-STATE" :use nil))))
         (list (list p p) 7 *package-enumeration-callbacks*)) :calls 50000)
      ("package-enumeration/next-local" next-package-symbol
       (let* ((p (or (find-package "ENUM-BENCH-NEXT") (make-package "ENUM-BENCH-NEXT" :use nil)))
              (state (funcall *package-enumeration-state* (list p) 1)))
         (intern "ONLY" p)
         (let ((entries (funcall *package-enumeration-fields* p 7 nil)))
           (setf (svref state 1) nil (svref state 2) p (svref state 3) entries
                 (svref state 7) (length entries) (svref state 9) 0))
         (list state)) :calls 100000)
      ("package-enumeration/next-empty" next-package-symbol
       (list (funcall *package-enumeration-state* nil 7)) :calls 100000))))

(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)
(defun package-enumeration-diagnostic-wat ()
  (runtime-function-definition '$test-package-enumeration-install
    (wat-fragment #@(func $test-package-enumeration-install (export "test-package-enumeration-install")
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*PACKAGE-ENUMERATION-FIELDS*") (global.get $current-package))
        (call $package-entry-function (i32.const 0))))
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*PACKAGE-ENUMERATION-CALLBACKS*") (global.get $current-package))
        (call $package-entry-callbacks)))
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*PACKAGE-ENUMERATION-STATE*") (global.get $current-package))
        (call $node (i32.const 8) (i32.const ,(+ +clos-primitive-base+ (clos-primitive-op '%%package-iterator-state)))
          (ref.null eq) (ref.null eq) (ref.null eq))))
      (drop (call $set-symbol-value
        (call $intern ,(wat-text-literal "*PACKAGE-ENUMERATION-NEXT*") (global.get $current-package))
        (call $node (i32.const 8) (i32.const ,(+ +clos-primitive-base+ (clos-primitive-op '%%package-iterator-next)))
          (ref.null eq) (ref.null eq) (ref.null eq))))))))

(in-package #:miricl-port-bench)
(defvar *package-enumeration-original-wrapper* (fdefinition 'wrapper-wat))
(defvar *package-enumeration-original-driver* (fdefinition 'install-driver))
(defun package-enumeration-wrapper (entry worker arity truth)
  (let ((text (funcall *package-enumeration-original-wrapper* entry worker arity truth)))
    (if (eq entry 'miricl-runtime:next-package-symbol)
        (let* ((loop-start (search "(block $done" text))
               (sum-start (search "(local.set $sum" text :start2 loop-start)))
          (concatenate 'string
            (subseq text 0 (1+ (position #\Newline text)))
            "(local $reset-entries (ref null eq)) (local $reset-count (ref null eq))" (string #\Newline)
            (subseq text (1+ (position #\Newline text)) loop-start)
            "(local.set $reset-entries (array.get $objects (call $vector-data (local.get $a0)) (i32.const 3)))"
            "(local.set $reset-count (array.get $objects (call $vector-data (local.get $a0)) (i32.const 7)))"
            (subseq text loop-start sum-start)
            "(array.set $objects (call $vector-data (local.get $a0)) (i32.const 3) (local.get $reset-entries))"
            "(array.set $objects (call $vector-data (local.get $a0)) (i32.const 7) (local.get $reset-count))"
            (subseq text sum-start)))
        text)))
(defun package-enumeration-driver (entry arity)
  (if (eq entry 'miricl-runtime:next-package-symbol)
      (compile (driver-name entry)
        '(lambda (count fixture)
           (declare (type fixnum count) (optimize (speed 3) (safety 0) (debug 0))
                    (notinline miricl-runtime:next-package-symbol))
           (let* ((state (car fixture)) (entries (svref state 3)) (entry-count (svref state 7)) (sum 0))
             (declare (type simple-vector state) (type fixnum sum))
             (dotimes (i count sum)
               (setf (svref state 3) entries (svref state 7) entry-count)
               (when (miricl-runtime:next-package-symbol state) (incf sum))))))
      (funcall *package-enumeration-original-driver* entry arity)))
(defun write-package-enumeration-benchmark (path &key catalog wat-path)
  (let ((original (fdefinition 'miricl::$package-entry-function))
        (wrapper (fdefinition 'wrapper-wat)) (driver (fdefinition 'install-driver))
        (names '("package-names-available-p" "collect-package-symbols" "collect-all-package-symbols"
                 "make-package-iteration-state" "next-package-symbol")))
    (unwind-protect
         (progn
           (setf (fdefinition 'miricl::$package-entry-function)
             (lambda () (miricl::runtime-wat-sequence (funcall original) (miricl::package-enumeration-diagnostic-wat))))
           (setf (fdefinition 'wrapper-wat) #'package-enumeration-wrapper
                 (fdefinition 'install-driver) #'package-enumeration-driver)
           (write-benchmark-image path :catalog catalog :wat-path wat-path :names names))
      (setf (fdefinition 'miricl::$package-entry-function) original
            (fdefinition 'wrapper-wat) wrapper (fdefinition 'install-driver) driver))))
(defun benchmark-package-enumeration-sbcl (path)
  (let ((driver (fdefinition 'install-driver)))
    (unwind-protect
         (progn
           (setf (fdefinition 'install-driver) #'package-enumeration-driver)
           (benchmark-sbcl path :names '("package-names-available-p" "collect-package-symbols"
             "collect-all-package-symbols" "make-package-iteration-state" "next-package-symbol")))
      (setf (fdefinition 'install-driver) driver))))
