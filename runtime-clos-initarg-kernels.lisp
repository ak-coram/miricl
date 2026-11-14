;;;; Initialization validity and allocation policy over checked target services.
(in-package #:miricl-runtime)
(export '(clos-check-initargs-value clos-allocate-instance-value))

(defun clos-check-initargs-value (object arguments state metadata)
  "Check slot and applicable-method initargs. STATE is #(names slots previous)."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr cons eq rplacd svref < >))
  (funcall metadata 20 state nil)
  (funcall metadata 10 arguments nil)
  (let ((names (svref (the simple-vector state) 0))
        (slot-names (svref (the simple-vector state) 1))
        (previous (svref (the simple-vector state) 2))
        (class nil)
        (slots nil) (allowed nil) (allow nil) (keys nil) (methods nil)
        (name nil) (actual nil) (ll nil) (mode nil) (param nil) (key nil) (size 0))
    (declare (type fixnum size))
    (funcall metadata 9 names nil)
    ;; All three outer lists survive extensible metadata callbacks. Own their
    ;; spines; retain the identities of the actual slot and method objects.
    (setq names (let ((scan names) (head nil) (tail nil) (cell nil))
      (tagbody next (if scan nil (go done))
         (setq cell (cons (car scan) nil))
         (if tail (rplacd (the cons tail) cell) (setq head cell))
         (setq tail cell scan (cdr scan)) (go next) done) head))
    (setq class (funcall metadata 11 object nil)
          slots (funcall metadata 3 class nil))
    (funcall metadata 9 slots nil)
    (setq slots (let ((scan slots) (head nil) (tail nil) (cell nil))
      (tagbody next (if scan nil (go done))
         (setq cell (cons (car scan) nil))
         (if tail (rplacd (the cons tail) cell) (setq head cell))
         (setq tail cell scan (cdr scan)) (go next) done) head))
    (tagbody slot
       (if slots nil (go functions))
       (setq keys (funcall metadata 5 (car slots) nil))
       (funcall metadata 9 keys nil)
     slot-key
       (if keys nil (go next-slot))
       (setq allowed (cons (car keys) allowed) keys (cdr keys)) (go slot-key)
     next-slot
       (setq slots (cdr slots)) (go slot)
     functions
       (if names nil (go checked))
       (setq name (car names))
       ;; A previous callback may have changed the caller-owned argument list.
       (funcall metadata 10 arguments nil)
       (setq actual (cons object arguments))
       (if (eq name 'allocate-instance)
           (setq actual (cons (funcall metadata 11 object nil) arguments)))
       (if (eq name 'shared-initialize)
           (setq actual (cons object (cons slot-names arguments))))
       (if (eq name 'update-instance-for-different-class)
           (setq actual (cons previous (cons object arguments))))
       (setq methods (funcall metadata 16 (funcall metadata 15 name nil) actual))
       (funcall metadata 9 methods nil)
       (setq methods (let ((scan methods) (head nil) (tail nil) (cell nil))
         (tagbody next (if scan nil (go done))
            (setq cell (cons (car scan) nil))
            (if tail (rplacd (the cons tail) cell) (setq head cell))
            (setq tail cell scan (cdr scan)) (go next) done) head))
     method
       (if methods nil (go next-function))
       (setq ll (funcall metadata 17 (car methods) nil) mode nil)
       (funcall metadata 9 ll nil)
       ;; Do not cache by list identity: custom MOP readers can return a
       ;; mutable lambda-list spine, including one changed in place.
     parameter
       (if ll nil (go next-method))
       (setq param (car ll) ll (cdr ll))
       (if (eq param '&allow-other-keys) (setq allow t)
           (if (eq param '&key) (setq mode t)
               (if (eq param '&aux) (setq mode nil)
                   (if mode
                       (progn
                         (if (funcall metadata 21 param nil)
                             (progn
                               (if (funcall metadata 27 param nil) nil (funcall metadata 12 param nil))
                               (setq key (funcall metadata 18 param nil)))
                             (progn
                               (setq size (the fixnum (funcall metadata 9 param nil)))
                               (if (if (< size 1) t (> size 3)) (funcall metadata 12 param nil))
                               (if (eq size 3)
                                   (if (funcall metadata 27 (car (cdr (cdr param))) nil) nil
                                       (funcall metadata 12 param nil)))
                               (setq key (car param))
                               (if (funcall metadata 21 key nil)
                                   (progn
                                     (if (funcall metadata 27 key nil) nil (funcall metadata 12 param nil))
                                     (setq key (funcall metadata 18 key nil)))
                                   (progn
                                     (if (eq (funcall metadata 9 key nil) 2) nil
                                         (funcall metadata 12 param nil))
                                     (if (if (funcall metadata 21 (car key) nil)
                                             (funcall metadata 27 (car (cdr key)) nil) nil)
                                         nil (funcall metadata 12 param nil))
                                     (setq key (car key))))))
                         (setq allowed (cons key allowed)))))))
       (go parameter)
     next-method
       (setq methods (cdr methods)) (go method)
     next-function
       (setq names (cdr names)) (go functions)
     checked)
    (funcall metadata 10 arguments nil)
    (values (funcall metadata 19 arguments (cons allowed allow)))))

(defun clos-allocate-instance-value (class metadata)
  "Apply the standard allocation policy above native storage construction."
  (declare (type function metadata) (optimize (safety 0) (debug 0)))
  (setq class (funcall metadata 22 class nil))
  (if (funcall metadata 0 class nil) nil (funcall metadata 1 class nil))
  (let ((metaclass (funcall metadata 23 class nil)))
    (let ((object (funcall metadata 24 class nil)))
      (values (if (funcall metadata 25 class metaclass)
                  (funcall metadata 26 object nil) object)))))
