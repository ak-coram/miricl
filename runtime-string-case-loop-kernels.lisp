;;;; String case traversal. The public native entry checks string designators
;;;; and bounds; canonical character operations are supplied independently of
;;;; the host's character repertoire and mutable global function bindings.
(in-package #:miricl-runtime)
(export '(case-string-region-value))

(defun case-string-region-value (string first stop state)
  "Convert a validated region, copying only upon the first changed character.
STATE is #(flags uppercase lowercase alphanumeric copy-region). Flag bit 0 permits mutation;
bit 1 selects downcase, bit 2 selects capitalization. Character callbacks are
canonical checked target operations, not user callbacks. Bounds refer to the
initial logical string region; CHAR continues to resolve live displacement."
  (declare (type string string) (type (integer 0 #x7fffffff) first stop)
           (type simple-vector state) (optimize (safety 0) (debug 0))
           (inline svref char (setf char) eql = + logand zerop not))
  (values
   (let* ((flags (svref state 0)) (upper (svref state 1))
          (lower (svref state 2)) (alphanumeric (svref state 3))
          (copy-region (svref state 4))
          (write (not (zerop (logand flags 1))))
          (down (not (zerop (logand flags 2))))
          (capitalize (not (zerop (logand flags 4))))
          (word nil) (index first))
     (declare (type (integer 0 7) flags)
              (type function upper lower alphanumeric copy-region)
              (type (integer 0 #x7fffffff) index))
     (block result
       (tagbody next
          (if (= index stop) (return-from result string))
          (let* ((character (char string index)) (direction down) (convert t)
                 (mapped character))
            (declare (type character character mapped))
            (if capitalize
                (if (funcall alphanumeric character)
                    (setq direction word word t)
                    (setq word nil convert nil)))
            (if convert (setq mapped (funcall (if direction lower upper) character)))
            (if (eql character mapped) nil
                (progn
                  (if write nil (setq string (funcall copy-region string 0) write t))
                  (setf (char string index) mapped))))
          (setq index (the (integer 0 #x7fffffff) (+ index 1)))
          (go next))))))
