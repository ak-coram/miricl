;;;; Boolean/value/predicate TYPEP control above checked target services.
(in-package #:miricl-runtime)
(export '(compound-typep-value))

(defun compound-type-cursor-value (type cursor metadata)
  "Callbacks may mutate or detach CURSOR. Check both before reading live CDR."
  (declare (type function metadata) (optimize (safety 0) (debug 0)))
  (funcall metadata 5 type nil)
  (if (funcall metadata 3 cursor nil) nil (funcall metadata 4 cursor 3))
  (values nil))

(defun compound-typep-value (object type metadata)
  "Evaluate AND/OR/NOT/MEMBER/EQL/SATISFIES. Return exactly one boolean.
METADATA supplies child TYPEP, named predicate application, finite
spine length, malformed-control signaling and nonexecuting control validation.
Only the named predicate service executes a SATISFIES predicate."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr consp symbolp eq eql =))
  (values
   (block answer
     (if (consp type) nil (funcall metadata 4 type 2))
     (let* ((name (car type)) (arguments (cdr type))
            (count (funcall metadata 3 arguments nil)) (result nil))
       (if count nil (funcall metadata 4 type 3))
       (case name
         ((not eql satisfies)
          (if (= (the integer count) 1) nil (funcall metadata 4 type 1))
          (case name
            (eql (return-from answer (if (eql object (car arguments)) t nil)))
            (satisfies
             (if (symbolp (car arguments)) nil (funcall metadata 4 type 2))
             (setq result (funcall metadata 2 object (car arguments))))
            (not
             (funcall metadata 5 type nil)
             (setq result (funcall metadata 0 object (car arguments)))))
          (return-from answer (if (eq name 'not) (if result nil t) (if result t nil))))
         ((and or member)
          ;; The finite spine completely validates empty controls and literal
          ;; MEMBER data. Only recursive controls need the graph validator.
          (if arguments
              (if (eq name 'member) nil (funcall metadata 5 type nil)))
          (tagbody next
             (if arguments nil (go done))
             (setq result (if (eq name 'member) (eql object (car arguments))
                              (funcall metadata 0 object (car arguments))))
             (if (eq name 'and)
                 (if result nil (return-from answer nil))
                 (if result (return-from answer t)))
             ;; A short circuit needs no further access. Otherwise callbacks
             ;; invalidate every proof about the live control and cursor.
             (if (eq name 'member) nil (compound-type-cursor-value type arguments metadata))
             (setq arguments (cdr arguments)) (go next)
            done)
          (return-from answer (if (eq name 'and) t nil)))
         (otherwise (funcall metadata 4 type 2)))))))
