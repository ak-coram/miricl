;;;; Initialization validity and allocation policy over checked target services.
(in-package #:miricl-runtime)
(export '(clos-check-initargs-value clos-allocate-instance-value clos-initarg-profile-value clos-canonical-method-metadata-p
          clos-initarg-source-value clos-initarg-profile-matches-value))

(defun clos-initarg-profile-value (ll metadata)
  "Normalize checked method keyword metadata into an owned (keys . allow) pair."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr cons eq < >))
  (funcall metadata 9 ll nil)
  (let ((mode nil) (param nil) (key nil) (size 0)
        (method-keys nil) (method-allow nil))
    (declare (type fixnum size))
    (tagbody
     parameter
       (if ll nil (go done))
       (setq param (car ll) ll (cdr ll))
       (if (eq param '&allow-other-keys) (setq method-allow t)
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
                         (setq method-keys (cons key method-keys)))))))
       (go parameter)
     done)
    (cons method-keys method-allow)))

(defun clos-canonical-method-metadata-p (original canonical metadata)
  "Check that explicit executable keyword names describe the original MOP LL."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr eq symbolp symbol-name string=))
  (if (= (funcall metadata 9 original nil) (funcall metadata 9 canonical nil))
      nil (return-from clos-canonical-method-metadata-p nil))
  (let ((keyp nil))
    (tagbody
     next
       (if original nil (go done))
       (let ((source (car original)) (target (car canonical)) (marker nil))
         (setq original (cdr original) canonical (cdr canonical))
         (setq marker
           (if (eq source '&key) t
             (if (eq source '&aux) t
               (if (eq source '&allow-other-keys) t
                 (if (eq target '&key) t
                   (if (eq target '&aux) t (eq target '&allow-other-keys)))))))
         (if marker
             (progn
               (if (eq source target) nil (return-from clos-canonical-method-metadata-p nil))
               (if (eq source '&key) (setq keyp t))
               (if (eq source '&aux) (setq keyp nil)))
             (if keyp
                 (let ((variable nil) (explicit nil) (source-key nil) (source-size 1))
                   (declare (type fixnum source-size))
                   (if (symbolp source)
                       (setq variable source)
                       (progn
                         (setq source-size (the fixnum (funcall metadata 9 source nil)))
                         (if (if (< source-size 1) t (> source-size 3)) (funcall metadata 12 source nil))
                         (if (symbolp (car source))
                             (setq variable (car source))
                             (progn
                               (if (= 2 (funcall metadata 9 (car source) nil)) nil
                                   (funcall metadata 12 source nil))
                               (setq explicit t source-key (car (car source))
                                     variable (car (cdr (car source))))))))
                   (if (symbolp target) (return-from clos-canonical-method-metadata-p nil))
                   (if (= source-size (funcall metadata 9 target nil)) nil
                       (return-from clos-canonical-method-metadata-p nil))
                   (if (= 2 (funcall metadata 9 (car target) nil)) nil
                       (return-from clos-canonical-method-metadata-p nil))
                   (let ((key (car (car target))) (target-variable (car (cdr (car target)))))
                     (if (eq variable target-variable) nil (return-from clos-canonical-method-metadata-p nil))
                     (if explicit
                         (if (eq source-key key) nil (return-from clos-canonical-method-metadata-p nil))
                         (progn
                           (if (symbolp key) nil (return-from clos-canonical-method-metadata-p nil))
                           (if (symbolp variable) nil (return-from clos-canonical-method-metadata-p nil))
                           (if (string= (symbol-name key) (symbol-name variable)) nil
                               (return-from clos-canonical-method-metadata-p nil)))))
                   (if (= source-size 3)
                       (if (eq (car (cdr (cdr source))) (car (cdr (cdr target)))) nil
                           (return-from clos-canonical-method-metadata-p nil)))))))
       (go next)
     done)
    t))

(defun clos-initarg-source-value (ll metadata)
  "Own the keyword declaration shape, without retaining initform graphs."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr cons eq symbolp rplacd))
  (funcall metadata 9 ll nil)
  (let ((mode nil) (head nil) (tail nil) (cell nil) (param nil) (entry nil) (size 0))
    (declare (type fixnum size))
    (tagbody
     next
       (if ll nil (go done))
       (setq param (car ll) ll (cdr ll))
       (if (eq param '&key) (setq mode t)
           (if (eq param '&aux) (setq mode nil)
               (if (eq param '&allow-other-keys) nil
                   (if mode
                       (progn
                         (if (symbolp param)
                             (progn
                               (if (funcall metadata 27 param nil) nil (funcall metadata 12 param nil))
                               (setq entry param))
                             (progn
                               (setq size (the fixnum (funcall metadata 9 param nil)))
                               (if (if (< size 1) t (> size 3)) (funcall metadata 12 param nil))
                               (if (= size 3)
                                   (if (funcall metadata 27 (car (cdr (cdr param))) nil) nil
                                       (funcall metadata 12 param nil)))
                               (if (symbolp (car param))
                                   (progn
                                     (if (funcall metadata 27 (car param) nil) nil (funcall metadata 12 param nil))
                                     (setq entry (car param)))
                                   (progn
                                     (if (= 2 (funcall metadata 9 (car param) nil)) nil (funcall metadata 12 param nil))
                                     (if (funcall metadata 21 (car (car param)) nil) nil (funcall metadata 12 param nil))
                                     (if (funcall metadata 27 (car (cdr (car param))) nil) nil (funcall metadata 12 param nil))
                                     (setq entry (cons (car (car param)) (car (cdr (car param)))))))))
                         (setq cell (cons entry nil))
                         (if tail (rplacd (the cons tail) cell) (setq head cell))
                         (setq tail cell))))))
       (go next)
     done)
    head))

(defun clos-initarg-profile-matches-value (ll source profile metadata)
  "Validate borrowed/restored standard metadata against captured keyword identities."
  (declare (type function metadata) (optimize (safety 0) (debug 0))
           (inline car cdr eq cons consp symbolp symbol-name string=))
  (if (consp profile) nil (funcall metadata 12 profile nil))
  (let ((keys (car profile)) (stored-allow (cdr profile))
        (mode nil) (allow nil) (param nil) (variable nil) (explicit nil)
        (key nil) (entry nil) (size 0))
    (declare (type fixnum size))
    (funcall metadata 9 ll nil)
    (funcall metadata 9 source nil)
    (funcall metadata 9 keys nil)
    (if (if stored-allow (eq stored-allow t) t) nil (funcall metadata 12 profile nil))
    (tagbody
     next
       (if ll nil (go done))
       (setq param (car ll) ll (cdr ll))
       (if (eq param '&allow-other-keys) (setq allow t)
           (if (eq param '&key) (setq mode t)
               (if (eq param '&aux) (setq mode nil)
                   (if mode
                       (progn
                         (if source nil (return-from clos-initarg-profile-matches-value nil))
                         (if keys nil (return-from clos-initarg-profile-matches-value nil))
                         (setq explicit nil variable nil key nil)
                         (if (symbolp param)
                             (setq variable param)
                             (progn
                               (setq size (the fixnum (funcall metadata 9 param nil)))
                               (if (if (< size 1) t (> size 3)) (funcall metadata 12 param nil))
                               (if (= size 3)
                                   (if (funcall metadata 27 (car (cdr (cdr param))) nil) nil
                                       (funcall metadata 12 param nil)))
                               (if (symbolp (car param))
                                   (setq variable (car param))
                                   (progn
                                     (if (= 2 (funcall metadata 9 (car param) nil)) nil
                                         (funcall metadata 12 param nil))
                                     (setq explicit t key (car (car param)) variable (car (cdr (car param))))))))
                         (if (funcall metadata 27 variable nil) nil (funcall metadata 12 param nil))
                         (setq entry (car source))
                         (if (symbolp (car keys)) nil (funcall metadata 12 (car keys) nil))
                         (if (symbolp entry)
                             (progn
                               (if explicit (return-from clos-initarg-profile-matches-value nil))
                               (if (eq variable entry) nil (return-from clos-initarg-profile-matches-value nil))
                               (if (string= (symbol-name variable) (symbol-name (car keys))) nil
                                   (return-from clos-initarg-profile-matches-value nil)))
                             (progn
                               (if (consp entry) nil (funcall metadata 12 entry nil))
                               (if (symbolp (car entry)) nil (funcall metadata 12 entry nil))
                               (if (symbolp (cdr entry)) nil (funcall metadata 12 entry nil))
                               (if explicit nil (return-from clos-initarg-profile-matches-value nil))
                               (if (eq variable (cdr entry)) nil (return-from clos-initarg-profile-matches-value nil))
                               (if (eq key (car entry)) nil (return-from clos-initarg-profile-matches-value nil))
                               (if (eq key (car keys)) nil (return-from clos-initarg-profile-matches-value nil))))
                         (setq source (cdr source) keys (cdr keys)))))))
       (go next)
     done)
    (if source (return-from clos-initarg-profile-matches-value nil))
    (if keys (return-from clos-initarg-profile-matches-value nil))
    (eq allow stored-allow)))

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
        (name nil) (actual nil) (ll nil)
        (part nil) (method-keys nil) (method-allow nil))
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
       (if (funcall metadata 21 (car keys) nil) nil (funcall metadata 12 (car keys) nil))
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
       (setq ll (funcall metadata 17 (car methods) nil))
       ;; Only the standard-reader service can supply a private cached profile.
       ;; Custom readers keep ordinary live validation and normalization.
       (setq part (funcall metadata 28 (car methods) ll))
       (if part nil (setq part (clos-initarg-profile-value ll metadata)))
       (setq method-keys (car part) method-allow (cdr part))
     normalized
       (if method-allow (setq allow t))
     merge
       (if method-keys nil (go next-method))
       ;; Both the guarded profile service and the fallback normalizer return
       ;; fresh key cells. Consume that private spine, preserving the previous
       ;; reversal order without copying it again. No MOP source cells escape
       ;; into ALLOWED, even if the final key checker retains or mutates it.
       (let ((next (cdr method-keys)))
         (rplacd (the cons method-keys) allowed)
         (setq allowed method-keys method-keys next))
       (go merge)
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
