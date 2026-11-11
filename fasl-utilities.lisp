;;;; Shared FASL byte and UTF-8 operations, independent of compiler passes.
(in-package :miricl-compiler)

(defun compiler-primitive (operation &rest arguments)
  (declare (type (unsigned-byte 32) operation) (type list arguments))
  ;; This is a private bootstrap primitive, deliberately not interned in the
  ;; guest MIRICL package. Its identity is carried by the bootstrap/FASL ABI.
  (when (fboundp 'miricl::%%compile)
    (return-from compiler-primitive (values (apply 'miricl::%%compile operation arguments) t)))
  (values nil nil))

(defun utf8-bytes (text)
  (declare (type string text))
  (multiple-value-bind (bytes native) (compiler-primitive 12 text)
    (when native (return-from utf8-bytes bytes)))
  (let ((bytes (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (labels ((emit-byte (n) (vector-push-extend n bytes)))
      (loop for character across text for n = (char-code character) do
        (cond ((< n #x80) (emit-byte n))
              ((< n #x800) (emit-byte (logior #xc0 (ash n -6))) (emit-byte (logior #x80 (logand n 63))))
              ((< n #x10000)
               (when (<= #xd800 n #xdfff) (error "Invalid Unicode scalar."))
               (emit-byte (logior #xe0 (ash n -12))) (emit-byte (logior #x80 (logand (ash n -6) 63)))
               (emit-byte (logior #x80 (logand n 63))))
              ((<= n #x10ffff)
               (emit-byte (logior #xf0 (ash n -18))) (emit-byte (logior #x80 (logand (ash n -12) 63)))
               (emit-byte (logior #x80 (logand (ash n -6) 63))) (emit-byte (logior #x80 (logand n 63))))
              (t (error "Invalid Unicode scalar.")))))
    bytes))

(defun payload-text (stream end)
  ;; END is an absolute file position, including preceding bundle units.
  (declare (type stream stream) (type (integer 0 *) end))
  (multiple-value-bind (text native) (compiler-primitive 14 stream (file-position stream) end)
    (when native (return-from payload-text text)))
  (let ((text (make-array 128 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop while (< (file-position stream) end) for byte = (read-byte stream) do
      (let* ((count (cond ((< byte #x80) 0) ((<= #xc2 byte #xdf) 1)
                          ((<= #xe0 byte #xef) 2) ((<= #xf0 byte #xf4) 3)
                          (t (error "Invalid UTF-8 payload."))))
             (n (logand byte (1- (ash 1 (- 7 count))))))
        (dotimes (i count)
          (when (>= (file-position stream) end) (error "Truncated UTF-8 payload."))
          (let ((next (read-byte stream)))
            (unless (<= #x80 next #xbf) (error "Invalid UTF-8 payload."))
            (setq n (logior (ash n 6) (logand next 63)))))
        (unless (and (>= n (nth count '(0 #x80 #x800 #x10000)))
                     (<= n #x10ffff) (not (<= #xd800 n #xdfff)))
          (error "Invalid UTF-8 scalar."))
        (vector-push-extend (or (code-char n) (error "Unsupported Unicode scalar.")) text)))
    text))

(defun read-uleb (stream)
  (declare (type stream stream))
  (loop with value = 0 for shift from 0 below 35 by 7 for byte = (read-byte stream) do
    (when (and (= shift 28) (> byte 15)) (error "Invalid FASL length."))
    (setq value (logior value (ash (logand byte 127) shift)))
    (when (< byte 128) (return-from read-uleb value)))
  (error "Invalid FASL length."))

(defun write-uleb (number stream)
  (declare (type (unsigned-byte 32) number) (type stream stream))
  (loop (write-byte (logior (logand number 127) (if (> number 127) 128 0)) stream)
        (setq number (ash number -7)) (when (zerop number) (return))))
