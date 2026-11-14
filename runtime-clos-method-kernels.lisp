;;;; CLOS initialization and effective-slot selection over checked target getters.
(in-package #:miricl-runtime)
(export '(clos-initarg-cell-value clos-default-initargs-value
          clos-shared-initialize-value clos-find-slot-value))

(declaim (inline clos-initarg-cell-value))

(defun clos-initarg-cell-value (arguments names metadata)
  "Return the value cell of the leftmost initarg naming any member of NAMES."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr eq))
  (funcall metadata 10 arguments nil)
  (funcall metadata 9 names nil)
  (values
   (block found
     (let ((scan arguments) (keys nil))
       (tagbody argument
          (if scan nil (return-from found nil))
          (setq keys names)
        name
          (if keys nil (go advance))
          (if (eq (car scan) (car keys)) (return-from found (cdr scan)))
          (setq keys (cdr keys)) (go name)
        advance
          (setq scan (cdr (cdr scan))) (go argument))))))

(defun clos-default-initargs-value (class arguments metadata)
  "Append unsupplied defaults in effective order, copying only when needed."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr cons eq rplacd clos-initarg-cell-value <))
  (funcall metadata 10 arguments nil)
  (if (funcall metadata 0 class nil) nil (funcall metadata 1 class nil))
  (let ((defaults (funcall metadata 2 class nil))
        (result arguments) (tail nil) (entry nil) (name nil) (value nil) (pair nil))
    (funcall metadata 9 defaults nil)
    ;; User initfunctions may mutate class metadata: own the traversal spine.
    (setq defaults (let ((copy-scan defaults) (copy-head nil) (copy-tail nil) (copy-cell nil))
       ;; The complete spine was validated immediately above this copy.
       (tagbody copy-next
          (if copy-scan nil (go copy-done))
          (setq copy-cell (cons (car copy-scan) nil))
          (if copy-tail (rplacd (the cons copy-tail) copy-cell) (setq copy-head copy-cell))
          (setq copy-tail copy-cell copy-scan (cdr copy-scan)) (go copy-next)
        copy-done)
       copy-head))
    (tagbody default
       (if defaults nil (go done))
       (setq entry (car defaults))
       (if (< (the fixnum (funcall metadata 9 entry nil)) 3)
           (funcall metadata 12 entry nil))
       (setq name (car entry))
       (if (clos-initarg-cell-value result (cons name nil) metadata) nil
           (progn
             (setq value (car (cdr (cdr entry))))
             (if (funcall metadata 14 value nil) nil (funcall metadata 12 entry nil))
             (setq value (funcall value))
             ;; The callback can mutate the caller's argument list, so validate
             ;; again before copying/traversing it. Do not copy any value.
             (if tail nil
                 (progn
                   (funcall metadata 10 arguments nil)
                   (setq result (let ((copy-scan arguments) (copy-head nil) (copy-tail nil) (copy-cell nil))
       ;; The complete spine was validated immediately above this copy.
       (tagbody copy-next
          (if copy-scan nil (go copy-done))
          (setq copy-cell (cons (car copy-scan) nil))
          (if copy-tail (rplacd (the cons copy-tail) copy-cell) (setq copy-head copy-cell))
          (setq copy-tail copy-cell copy-scan (cdr copy-scan)) (go copy-next)
        copy-done)
       copy-head) tail result)
                   (tagbody end
                      (if tail nil (go copied))
                      (if (cdr tail) nil (go copied))
                      (setq tail (cdr tail)) (go end)
                    copied)))
             (setq pair (cons value nil))
             (if tail (rplacd (the cons tail) (cons name pair))
                 (setq result (cons name pair)))
             (setq tail pair)))
       (setq defaults (cdr defaults)) (go default)
     done)
    (values result)))

(defun clos-shared-initialize-value (object slot-names arguments metadata)
  "Fill supplied slots, then selected unbound slots with initfunctions."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr cons eq rplacd make-array svref (setf svref)
                   clos-initarg-cell-value))
  (funcall metadata 10 arguments nil)
  (if (eq slot-names t) nil (funcall metadata 9 slot-names nil))
  (let* ((class (funcall metadata 11 object nil))
         (slots (funcall metadata 3 class nil))
         (slot nil) (name nil) (keys nil) (cell nil) (function nil)
         (value nil) (scan nil) (packet (make-array 3)))
    (declare (type simple-vector packet))
    (funcall metadata 9 slots nil)
    (setq slots (let ((copy-scan slots) (copy-head nil) (copy-tail nil) (copy-cell nil))
       ;; The complete spine was validated immediately above this copy.
       (tagbody copy-next
          (if copy-scan nil (go copy-done))
          (setq copy-cell (cons (car copy-scan) nil))
          (if copy-tail (rplacd (the cons copy-tail) copy-cell) (setq copy-head copy-cell))
          (setq copy-tail copy-cell copy-scan (cdr copy-scan)) (go copy-next)
        copy-done)
       copy-head))
    (setf (svref packet 0) class)
    (setf (svref packet 1) object)
    (tagbody slot
       (if slots nil (go done))
       (setq slot (car slots) name (funcall metadata 4 slot nil)
             keys (funcall metadata 5 slot nil)
             cell (clos-initarg-cell-value arguments keys metadata))
       (setf (svref packet 2) slot)
       (if cell (setq value (car cell))
           (progn
             (if (eq slot-names t) nil
                 (progn
                   (funcall metadata 9 slot-names nil)
                   (setq scan slot-names)
                   (tagbody selected
                      (if scan nil (go skip))
                      (if (eq name (car scan)) (go selection-done))
                      (setq scan (cdr scan)) (go selected)
                    selection-done)))
             (if (funcall metadata 6 packet nil) (go skip))
             (setq function (funcall metadata 7 slot nil))
             (if function nil (go skip))
             (if (funcall metadata 14 function nil) nil (funcall metadata 12 slot nil))
             (setq value (funcall function))))
       (funcall metadata 8 packet value)
     skip
       (setq slots (cdr slots)) (go slot)
     done)
    (values object)))

(defun clos-find-slot-value (slots name direct-names metadata)
  "Return the last matching effective slot, preserving metadata identity."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr eq cons rplacd))
  (funcall metadata 9 slots nil)
  (let ((scan (let ((copy-scan slots) (copy-head nil) (copy-tail nil) (copy-cell nil))
       ;; The complete spine was validated immediately above this copy.
       (tagbody copy-next
          (if copy-scan nil (go copy-done))
          (setq copy-cell (cons (car copy-scan) nil))
          (if copy-tail (rplacd (the cons copy-tail) copy-cell) (setq copy-head copy-cell))
          (setq copy-tail copy-cell copy-scan (cdr copy-scan)) (go copy-next)
        copy-done)
       copy-head)) (slot nil) (found nil))
    (tagbody next
       (if scan nil (go done))
       (setq slot (car scan))
       (if (eq name (funcall metadata 13 slot direct-names)) (setq found slot))
       (setq scan (cdr scan)) (go next)
     done)
    (values found)))
