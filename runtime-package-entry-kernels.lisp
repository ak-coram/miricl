;;;; Indexed package entry algorithms. Native callbacks expose checked fields;
;;;; every hash, chain, visibility and membership traversal lives here.
(in-package #:miricl-runtime)
(export '(package-name-hash find-present-package-entry find-inherited-package-entry
          find-accessible-package-entry package-symbol-shadowed-p package-entry-conflict-p))

(defun package-name-hash (name mask)
  (declare (type string name) (type (integer 0 #x7fffffff) mask)
           (optimize (safety 0) (debug 0))
           (inline length char char-code logxor logand * = 1+))
  (values
   (let ((hash #x811c9dc5) (index 0) (size (length name)))
     (declare (type (unsigned-byte 32) hash)
              (type (integer 0 #x7fffffff) index size))
     (tagbody next
        (if (= index size) (go done))
        (setq hash (logand #xffffffff (* (logxor hash (char-code (char name index))) #x1000193)))
        (setq index (the (integer 0 #x7fffffff) (1+ index)))
        (go next)
       done)
     (logand hash mask))))

(defun find-present-package-entry (name entry fields symbol-name)
  "Search a checked hash bucket without replacing the package's index."
  (declare (type string name) (type function fields symbol-name)
           (optimize (safety 0) (debug 0))
           (inline eq length char char-code = 1+))
  (values
   (block found
     (let ((scan entry) (slow entry) (phase nil))
       (tagbody next
          (if scan nil (return-from found nil))
          (let* ((candidate (funcall symbol-name (funcall fields scan 4 nil)))
                 (size (length name)) (other-size (length (the string candidate))) (index 0))
            (declare (type (integer 0 #x7fffffff) size other-size index))
            (if (= size other-size)
                (tagbody character
                   (if (= index size) (return-from found scan))
                   (if (= (char-code (char name index)) (char-code (char (the string candidate) index)))
                       nil (go different))
                   (setq index (the (integer 0 #x7fffffff) (1+ index)))
                   (go character)
                  different)))
          (setq scan (funcall fields scan 5 nil))
          ;; A malformed private chain must terminate even if reached through
          ;; a future serialization or representation boundary.
          (if phase
              (progn (setq slow (funcall fields slow 5 nil))
                     (if scan (if (eq scan slow) (funcall fields nil -1 nil)))))
          (setq phase (if phase nil scan))
          (go next))))))

(defun find-inherited-package-entry (name uses fields present)
  "Return the last matching external entry, or a conflict marker."
  (declare (type string name) (type function fields present)
           (optimize (safety 0) (debug 0))
           (inline car cdr eq = list-length))
  (values
   (progn
     ;; Publicly reachable malformed lists must fail before the unchecked scan.
     (if uses (if (list-length uses) nil
                   (error 'type-error :datum uses :expected-type 'list)))
     (let ((scan uses) (found nil))
       (tagbody next
          (if scan nil (go done))
          (let ((entry (funcall present name (car scan))))
            (if entry
                (if (= (the fixnum (funcall fields entry 6 nil)) 2)
                    (progn
                      (if found
                          (if (eq (funcall fields found 4 nil) (funcall fields entry 4 nil)) nil
                              (return-from find-inherited-package-entry 0)))
                      (setq found entry)))))
          (setq scan (cdr scan)) (go next)
         done)
       found))))

(defun find-accessible-package-entry (name package present inherited)
  (declare (type function present inherited) (optimize (safety 0) (debug 0)))
  (values (let ((entry (funcall present name package)))
            (if entry entry (funcall inherited name package)))))

(defun package-symbol-shadowed-p (symbol shadows)
  (declare (optimize (safety 0) (debug 0)) (inline member-eq list-length))
  (values
   (progn
     (if shadows (if (list-length shadows) nil
                      (error 'type-error :datum shadows :expected-type 'list)))
     (if (member-eq symbol shadows) t nil))))

(defun package-entry-conflict-p (symbol existing shadows)
  (declare (optimize (safety 0) (debug 0)) (inline eq member-eq list-length))
  (values
   (progn
     (if shadows (if (list-length shadows) nil
                      (error 'type-error :datum shadows :expected-type 'list)))
     (if (eq symbol existing) nil (if (member-eq existing shadows) nil t)))))

(export '(package-names-available-p collect-package-symbols collect-all-package-symbols
          make-package-iteration-state next-package-symbol))

(defun package-names-available-p (names package fields)
  "Check normalized names against the global registry, ignoring local aliases."
  (declare (type function fields) (optimize (safety 0) (debug 0))
           (inline car cdr eq list-length))
  (values
   (progn
     (if (funcall fields names 10 nil) nil (error 'type-error :datum names :expected-type 'list))
     (let ((scan names))
       (tagbody next
          (if scan nil (return-from package-names-available-p (values t)))
          (let ((found (funcall fields (car scan) 8 nil)))
            (if found (if (eq found package) nil
                          (return-from package-names-available-p (values nil)))))
          (setq scan (cdr scan)) (go next))))))

(defun collect-package-symbols (package external inherited callbacks)
  "Enumerate checked entry spines; inherited symbols are deduplicated by EQ."
  (declare (type simple-vector callbacks) (optimize (safety 0) (debug 0))
           (inline svref car cdr cons eq = list-length gethash (setf gethash)))
  (values
   (let* ((fields (svref callbacks 0)) (present (svref callbacks 1))
          (inherit (svref callbacks 2)) (name (svref callbacks 3))
          (entries (funcall fields package 7 nil))
          (uses (if inherited (funcall fields package 2 nil)))
          (seen (if inherited (make-hash-table :test 'eq))) (result nil) (phase nil))
     (declare (type function fields present inherit name))
     (if (funcall fields uses 10 nil) nil (error 'type-error :datum uses :expected-type 'list))
     (tagbody source
        (if (funcall fields entries 10 nil) nil (error 'type-error :datum entries :expected-type 'list))
       next
        (if entries nil (go advance))
        (let* ((entry (car entries)) (status (funcall fields entry 6 nil)))
          (if (if phase (= (the fixnum status) 2)
                  (if external (= (the fixnum status) 2) t))
              (let ((symbol (funcall fields entry 4 nil)))
                (if phase
                    (let* ((text (funcall name symbol)) (found (funcall present text package)))
                      (if found nil (setq found (funcall inherit text package)))
                      (if found
                          (if (eq symbol (funcall fields found 4 nil)) nil (go skip))
                          (go skip))))
                (if inherited
                    (if (gethash symbol seen) (go skip)
                        (setf (gethash symbol seen) t)))
                (setq result (cons symbol result)))))
       skip
        (setq entries (cdr entries)) (go next)
       advance
        (if uses nil (go done))
        (setq phase t entries (funcall fields (car uses) 7 nil) uses (cdr uses))
        (go source)
       done)
     result)))

(defun collect-all-package-symbols (packages fields)
  "Collect each present symbol identity from the registry exactly once."
  (declare (type function fields) (optimize (safety 0) (debug 0))
           (inline car cdr cons list-length gethash (setf gethash)))
  (values
   (progn
     (if (funcall fields packages 10 nil) nil (error 'type-error :datum packages :expected-type 'list))
     (let ((scan packages) (seen (make-hash-table :test 'eq)) (result nil) (entries nil))
       (tagbody package
          (if scan nil (go done))
          (setq entries (funcall fields (car scan) 7 nil))
          (if (funcall fields entries 10 nil) nil (error 'type-error :datum entries :expected-type 'list))
         entry
          (if entries nil (go advance))
          (let ((symbol (funcall fields (car entries) 4 nil)))
            (if (gethash symbol seen) nil
                (progn (setf (gethash symbol seen) t) (setq result (cons symbol result)))))
          (setq entries (cdr entries)) (go entry)
         advance
          (setq scan (cdr scan)) (go package)
         done)
       result))))

(defun make-package-iteration-state (packages mask callbacks)
  "Resolve a package-list designator once and own the private cursor vector."
  (declare (type simple-vector callbacks) (optimize (safety 0) (debug 0))
           (inline svref (setf svref) consp car cdr cons integerp < > = list-length make-array))
  (if (integerp mask) (if (if (< mask 0) t (> mask 7)) (error 'program-error))
      (error 'type-error :datum mask :expected-type 'integer))
  (values
   (let* ((fields (svref callbacks 0))
          (scan (if packages (if (consp packages) packages (cons packages nil))))
          (count (funcall fields scan 10 nil)) (resolved nil) (state (make-array 10)))
     (declare (type function fields) (type simple-vector state))
     (if count nil (error 'type-error :datum packages :expected-type 'list))
     (tagbody next
        (if scan nil (go done))
        (setq resolved (cons (funcall fields (car scan) 9 nil) resolved) scan (cdr scan))
        (go next)
       done)
     (setf (svref state 0) mask)
     (setf (svref state 1) resolved)
     (setf (svref state 2) nil)
     (setf (svref state 3) nil)
     (setf (svref state 4) nil)
     (setf (svref state 5) 0)
     (setf (svref state 6) callbacks)
     (setf (svref state 7) 0)
     (setf (svref state 8) 0)
     (setf (svref state 9) count)
     state)))

(defun next-package-symbol (state)
  "Advance a checked cursor. Return #(symbol status package), or NIL once done."
  (declare (optimize (safety 0) (debug 0))
           (inline simple-vector-p svref (setf svref) length functionp integerp
                   consp car cdr eq eql = < > >= - 1+ 1- logand zerop list-length make-array))
  (if (simple-vector-p state) nil (error 'type-error :datum state :expected-type 'simple-vector))
  (if (= (length (the simple-vector state)) 10) nil (error 'program-error))
  (let* ((state (the simple-vector state)) (callbacks (svref state 6))
         (mask (svref state 0)) (phase (svref state 5)) (i 7))
    (declare (type (integer 7 10) i))
    (if (simple-vector-p callbacks) nil (error 'type-error :datum callbacks :expected-type 'simple-vector))
    (if (= (length (the simple-vector callbacks)) 4) nil (error 'program-error))
    (if (integerp mask) (if (if (< mask 0) t (> mask 7)) (error 'program-error))
        (error 'type-error :datum mask :expected-type 'integer))
    (if (if (eql phase 0) t (eql phase 1)) nil (error 'program-error))
    (tagbody validate
       (if (= i 10) (go validated))
       (let ((count (svref state i)))
         (if (integerp count) (if (if (< count 0) t (> count #x1ffffffe)) (error 'program-error))
             (error 'type-error :datum count :expected-type 'integer)))
       (setq i (the (integer 7 10) (1+ i))) (go validate)
      validated)
    (let ((fields (svref (the simple-vector callbacks) 0))
          (present (svref (the simple-vector callbacks) 1))
          (name (svref (the simple-vector callbacks) 3)))
      (if (functionp fields) nil (error 'program-error))
      (if (functionp present) nil (error 'program-error))
      (if (functionp name) nil (error 'program-error))
      ;; The cursor is a private object, but a forged or subsequently mutated
      ;; vector must still have finite spines before unchecked traversal.
      ;; Recompute the bounded counters rather than trusting serialized counts.
      (let ((packages (funcall fields (svref state 1) 10 nil))
            (entries (funcall fields (svref state 3) 10 nil))
            (uses (funcall fields (svref state 4) 10 nil)))
        (if packages nil (error 'type-error :datum (svref state 1) :expected-type 'list))
        (if entries nil (error 'type-error :datum (svref state 3) :expected-type 'list))
        (if uses nil (error 'type-error :datum (svref state 4) :expected-type 'list))
        (setf (svref state 9) packages)
        (setf (svref state 7) entries)
        (setf (svref state 8) uses))
      (values
       (block done
         (tagbody next
            (if (svref state 2) nil
                (let ((packages (svref state 1)) (count (the (integer 0 #x1ffffffe) (svref state 9))))
                  (if packages nil (return-from done nil))
                  (if (= count 0) (error 'program-error))
                  (if (consp packages) nil (error 'type-error :datum packages :expected-type 'list))
                  (let* ((package (funcall fields (car (the cons packages)) 9 nil))
                         (entries (funcall fields package 7 nil))
                         (uses (funcall fields package 2 nil))
                         (entry-count (funcall fields entries 10 nil)) (use-count (funcall fields uses 10 nil)))
                    (if entry-count nil (error 'type-error :datum entries :expected-type 'list))
                    (if use-count nil (error 'type-error :datum uses :expected-type 'list))
                    (setf (svref state 1) (cdr (the cons packages)))
                    (setf (svref state 9) (1- count))
                    (setf (svref state 2) package)
                    (setf (svref state 3) entries)
                    (setf (svref state 4) uses)
                    (setf (svref state 5) 0)
                    (setf (svref state 7) entry-count)
                    (setf (svref state 8) use-count))))
            (let ((entries (svref state 3)) (count (the (integer 0 #x1ffffffe) (svref state 7))))
              (if entries
                  (progn
                    (if (= count 0) (error 'program-error))
                    (if (consp entries) nil (error 'type-error :datum entries :expected-type 'list))
                    (let* ((entry (car (the cons entries))) (symbol (funcall fields entry 4 nil))
                           (status (the (integer 1 4) (funcall fields entry 6 nil)))
                           (package (svref state 2)))
                      (setf (svref state 3) (cdr (the cons entries)))
                      (setf (svref state 7) (1- count))
                      (if (= (the (integer 0 1) (svref state 5)) 1)
                          (progn
                            (if (= status 2) nil (go next))
                            (if (funcall present (funcall name symbol) package) (go next))
                            (setq status 4)))
                      (if (zerop (logand (the (integer 0 7) mask) status)) (go next))
                      (let ((result (make-array 3)))
                        (declare (type simple-vector result))
                        (setf (svref result 0) symbol)
                        (setf (svref result 1) status)
                        (setf (svref result 2) package)
                        (return-from done result))))))
            (setf (svref state 5) 1)
            (let ((uses (svref state 4)) (count (the (integer 0 #x1ffffffe) (svref state 8))))
              (if (if (zerop (logand (the (integer 0 7) mask) 4)) nil uses)
                  (progn
                    (if (= count 0) (error 'program-error))
                    (if (consp uses) nil (error 'type-error :datum uses :expected-type 'list))
                    (let* ((entries (funcall fields (car (the cons uses)) 7 nil))
                           (entry-count (funcall fields entries 10 nil)))
                      (if entry-count nil (error 'type-error :datum entries :expected-type 'list))
                      (setf (svref state 3) entries)
                      (setf (svref state 7) entry-count)
                      (setf (svref state 4) (cdr (the cons uses)))
                      (setf (svref state 8) (1- count))))
                  (setf (svref state 2) nil)))
            (go next)))))))
