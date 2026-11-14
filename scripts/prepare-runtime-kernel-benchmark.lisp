;;;; Load in a host REPL and call MIRICL::WRITE-RUNTIME-KERNEL-BENCHMARK, or:
;;;; sbcl --load quicklisp/setup.lisp --script scripts/prepare-runtime-kernel-benchmark.lisp OUTPUT.wasm
(require :asdf)
(unless (and (find-package :miricl)
             (fboundp (find-symbol "WRITE-INTERPRETER-WASM" :miricl)))
  (let ((root (uiop:pathname-parent-directory-pathname (uiop:pathname-directory-pathname *load-truename*))))
    (asdf:load-asd (merge-pathnames "miricl.asd" root))
    (asdf:load-system :miricl)))
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(define-runtime-function $benchmark-property-kernel (($count i32) ($size i32) ($key i32)) (i32)
  (:export-raw "benchmark-property-cell")
  (:locals ($items :nullable-object) ($result :nullable-object) ($key-object :nullable-object))
  #@(loop $make
    (if (local.get $size)
      (then (local.set $size (i32.sub (local.get $size) (i32.const 1)))
        (local.set $items (call $cons (call $int (local.get $size))
          (call $cons (call $int (local.get $size)) (local.get $items)))) (br $make))))
  #@(local.set $key-object (call $int (local.get $key)))
  #@(loop $run
    (if (local.get $count)
      (then (local.set $result (call $property-cell (local.get $items) (local.get $key-object)))
        (local.set $count (i32.sub (local.get $count) (i32.const 1))) (br $run))))
  #@(if (result i32) (ref.is_null (local.get $result)) (then (i32.const -1))
    (else (call $natural-count (call $car (local.get $result))))))

(define-runtime-function $benchmark-member-kernel (($count i32) ($size i32) ($key i32)) (i32)
  (:export-raw "benchmark-member-eq")
  (:locals ($items :nullable-object) ($result i32) ($key-object :nullable-object))
  #@(loop $make
    (if (local.get $size)
      (then (local.set $size (i32.sub (local.get $size) (i32.const 1)))
        (local.set $items (call $cons (call $int (local.get $size)) (local.get $items))) (br $make))))
  #@(local.set $key-object (call $int (local.get $key)))
  #@(loop $run
    (if (local.get $count)
      (then (local.set $result (call $member-eq (local.get $key-object) (local.get $items)))
        (local.set $count (i32.sub (local.get $count) (i32.const 1))) (br $run))))
  #@(local.get $result))

(define-runtime-function $benchmark-list-eq-kernel (($count i32) ($size i32) ($key i32)) (i32)
  (:export-raw "benchmark-list-eq")
  (:locals ($left :nullable-object) ($right :nullable-object) ($result :nullable-object))
  #@(loop $make
    (if (local.get $size)
      (then (local.set $size (i32.sub (local.get $size) (i32.const 1)))
        (local.set $left (call $cons (call $int (local.get $size)) (local.get $left)))
        (local.set $right (call $cons
          (call $int (if (result i32) (i32.eq (local.get $size) (local.get $key))
            (then (i32.const -1)) (else (local.get $size)))) (local.get $right)))
        (br $make))))
  #@(loop $run
    (if (local.get $count)
      (then (local.set $result (call $list-eq (local.get $left) (local.get $right)))
        (local.set $count (i32.sub (local.get $count) (i32.const 1))) (br $run))))
  #@(i32.eqz (ref.is_null (local.get $result))))

(define-runtime-function $benchmark-reverse-onto (($count i32) ($size i32) ($mutate i32)) (i32)
  (:export-raw "benchmark-reverse-onto")
  (:locals ($items :nullable-object) ($original :nullable-object) ($n i32))
  #@(local.set $n (local.get $size))
  #@(loop $make
    (if (local.get $n)
      (then (local.set $n (i32.sub (local.get $n) (i32.const 1)))
        (local.set $items (call $cons (call $int (local.get $n)) (local.get $items))) (br $make))))
  #@(local.set $original (local.get $items))
  #@(loop $run
    (if (local.get $count)
      (then
        (local.set $items (call $reverse-onto (local.get $items) (ref.null eq) (local.get $mutate)))
        (local.set $items (call $reverse-onto (local.get $items) (ref.null eq) (local.get $mutate)))
        (local.set $count (i32.sub (local.get $count) (i32.const 1))) (br $run))))
  #@(if (i32.and (local.get $mutate) (i32.eqz (ref.eq (local.get $items) (local.get $original))))
    (then (return (i32.const -1))))
  #@(if (call $natural-count (call $car (local.get $items))) (then (return (i32.const -2))))
  #@(call $length (local.get $items)))

(define-runtime-function $benchmark-reverse-copy (($count i32) ($size i32) ($key i32)) (i32)
  (:export-raw "benchmark-reverse-copy")
  #@(call $benchmark-reverse-onto (local.get $count) (local.get $size) (i32.const 0)))

(define-runtime-function $benchmark-reverse-mutate (($count i32) ($size i32) ($key i32)) (i32)
  (:export-raw "benchmark-reverse-mutate")
  #@(call $benchmark-reverse-onto (local.get $count) (local.get $size) (i32.const 1)))

(defun write-runtime-kernel-benchmark (pathname)
  "Write a compiler-less diagnostic image. Production images gain no exports.
In a REPL this uses the currently installed generators, allowing paired builds
after reloading only the affected compiler/runtime definitions."
  (let ((property (symbol-function '$property-cell)) (member (symbol-function '$member-eq))
        (list-eq (symbol-function '$list-eq)) (reverse (symbol-function '$reverse-onto))
        (*interpreter-only-wasm* nil))
    (unwind-protect
        (progn
          (setf (symbol-function '$property-cell)
                  (lambda () (runtime-wat-sequence (funcall property) ($benchmark-property-kernel)))
                (symbol-function '$member-eq)
                  (lambda () (runtime-wat-sequence (funcall member) ($benchmark-member-kernel)))
                (symbol-function '$list-eq)
                  (lambda () (runtime-wat-sequence (funcall list-eq) ($benchmark-list-eq-kernel)))
                (symbol-function '$reverse-onto)
                  (lambda () (runtime-wat-sequence (funcall reverse) ($benchmark-reverse-onto)
                    ($benchmark-reverse-copy) ($benchmark-reverse-mutate))))
          (ensure-directories-exist pathname)
          (write-interpreter-wasm pathname :compiler nil))
      (setf (symbol-function '$property-cell) property (symbol-function '$member-eq) member
            (symbol-function '$list-eq) list-eq (symbol-function '$reverse-onto) reverse))))

(when (uiop:command-line-arguments)
  (write-runtime-kernel-benchmark (first (uiop:command-line-arguments))))
