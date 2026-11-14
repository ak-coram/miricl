;;;; Generic dispatch decisions over checked target metadata and representations.
(in-package #:miricl-runtime)
(export '(dispatch-eql-key-value generic-key-match-value
          generic-cache-permitted-value generic-fixed-arity-value))

(defun dispatch-eql-key-value (object specializers metadata)
  "Return the first interned specializer whose object is EQL to OBJECT."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr eql))
  (funcall metadata 0 specializers nil)
  (values
   (block found
     (tagbody next
        (if specializers nil (go done))
        (if (eql object (funcall metadata 1 (car specializers) nil))
            (return-from found (car specializers)))
        (setq specializers (cdr specializers)) (go next)
       done)
     nil)))

(defun generic-fixed-arity-value (parameters count metadata)
  "Recognize a fixed generic lambda-list for a bounded direct caller."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr consp symbolp eq eql +))
  (funcall metadata 9 count nil)
  (values
   (block fixed
     (let ((index 0) (name nil))
       (declare (type (integer 0 4) index count))
       (tagbody next
          (if (eql index count) (go done))
          ;; Bounded traversal also rejects circular lists without chasing them.
          (if (consp parameters) nil (return-from fixed nil))
          (setq name (car parameters))
          (if (symbolp name) nil (return-from fixed nil))
          (case name
            ((&optional &rest &body &key &allow-other-keys &aux &whole &environment)
             (return-from fixed nil)))
          (setq parameters (cdr parameters) index (+ index 1)) (go next)
         done)
       (eq parameters nil)))))

(defun generic-cache-permitted-value (generic metadata)
  "Use a current cache proof, otherwise retain the extensible protocol call."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr eql))
  (values
   (if (funcall metadata 3 nil nil) t
       (let ((proof (funcall metadata 4 generic nil)))
         (if proof
             (progn
               (funcall metadata 10 proof nil)
               (if (eql (car proof) (funcall metadata 5 nil nil))
                   (if (cdr proof) t nil)
                   (if (funcall metadata 6 generic nil) t nil)))
             (if (funcall metadata 6 generic nil) t nil))))))

(defun generic-key-match-value (key state metadata)
  "STATE is #(generic arguments class-a class-b class-c class-d mode).
Consume the cache key in reverse argument order, retaining EQL object identity."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr eq eql svref + - logand ash))
  (funcall metadata 7 state nil)
  (funcall metadata 0 key nil)
  (let ((mode (svref (the simple-vector state) 6))
        (arguments (svref (the simple-vector state) 1))
        (position 0) (index 0) (scan nil) (object nil) (profile nil))
    (declare (type (integer 0 8191) mode) (type (integer 0 3) position index))
    (funcall metadata 0 arguments nil)
    (values
     (block match
       (if (eql (logand mode 7) 0) (return-from match (eq key nil)))
       (setq position (- (logand mode 7) 1))
       (tagbody next
          (if (eql (logand mode (ash 16 position)) 0) (go next-position))
          (setq scan arguments index position)
        argument
          (if (eql index 0) (go argument-found))
          (setq scan (cdr scan) index (- index 1)) (go argument)
        argument-found
          ;; A malformed short argument list supplies no matching cache proof.
          (if scan nil (return-from match nil))
          (setq object (car scan))
          (if (eql (logand mode (ash 256 position)) 0) (go class))
          (setq profile (funcall metadata 2 (svref (the simple-vector state) 0) nil))
          (funcall metadata 0 profile nil)
          (setq index position)
        profile
          (if (eql index 0) (go profile-found))
          (setq profile (cdr profile) index (- index 1)) (go profile)
        profile-found
          ;; A short profile supplies no proof for this selected position.
          (if profile nil (return-from match nil))
          (if key nil (return-from match nil))
          (if (eq (car key) (dispatch-eql-key-value object (car profile) metadata))
              nil (return-from match nil))
          (setq key (cdr key))
        class
          (if key nil (return-from match nil))
          (if (eq (car key) (svref (the simple-vector state) (+ position 2)))
              nil (return-from match nil))
          (setq key (cdr key))
        next-position
          (if (eql position 0) (return-from match (eq key nil)))
          (setq position (- position 1)) (go next))
       nil))))
