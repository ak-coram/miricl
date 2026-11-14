;;;; Exact integer extraction. Representation-specific ASH/LOGAND and integer
;;;; measurement remain shared substrate operations, never calls back to LDB.
(in-package #:miricl-runtime)
(export '(extract-integer-field))

(defun extract-integer-field (value size position predicate)
  "Extract SIZE two's-complement bits at POSITION, or test their nonzero value.
Unbounded counts are checked before scalar/range reasoning. Support bounds
avoid enormous masks for zero/sign-extended fields and full positive windows."
  (declare (optimize (safety 0) (debug 0))
           (inline integerp < > >= = - + 1+ 1- zerop not ash logand integer-length))
  (if (integerp value) nil (error 'type-error :datum value :expected-type 'integer))
  (if (integerp size) nil (error 'type-error :datum size :expected-type 'unsigned-byte))
  (if (< size 0) (error 'type-error :datum size :expected-type 'unsigned-byte))
  (if (integerp position) nil (error 'type-error :datum position :expected-type 'unsigned-byte))
  (if (< position 0) (error 'type-error :datum position :expected-type 'unsigned-byte))
  (values
   (block result
     (if (zerop size) (return-from result (if predicate nil 0)))
     (let ((length (integer-length value)) (negative (< value 0)))
       (if negative
           ;; Above INTEGER-LENGTH a negative integer contains only ones.
           ;; A predicate crossing that boundary is true without allocating.
           (if predicate
               (if (>= position length) (return-from result t)
                   (if (> size (- length position)) (return-from result t))))
           (progn
             (if (>= position length) (return-from result (if predicate nil 0)))
             (if (> size (- length position)) (setq size (- length position)))))
       ;; Keep the existing target result-allocation limit. Predicate support
       ;; bounds above make even arbitrarily large requested widths cheap.
       (if predicate nil (if (> size #x1ffffffe) (error 'storage-condition)))
       (let ((shifted (ash value (- position))))
         (if negative nil
             (if (>= size (- length position))
                 (return-from result (if predicate (not (zerop shifted)) shifted))))
         ;; At the largest admitted result width, constructing 2^SIZE first
         ;; would need one bit beyond the target allocation limit. Construct
         ;; that sole endpoint through a smaller mask instead.
         (let* ((mask (if (= size #x1ffffffe)
                          (let ((half (1- (ash 1 (1- size))))) (1+ (+ half half)))
                          (1- (ash 1 size))))
                (field (logand shifted mask)))
           (if predicate (not (zerop field)) field)))))))
