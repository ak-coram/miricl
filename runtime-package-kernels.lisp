;;;; Package name traversal and DEFPACKAGE validation. Representation and
;;;; package-entry indexing remain in the checked native package substrate.
(in-package #:miricl-runtime)
(export '(package-bootstrap-name-list package-name-list collect-package-users collect-live-packages
          package-fold-name package-name-contains-folded-p filter-apropos-symbols
          check-package-options find-global-package-name find-local-package-name
          find-all-package-symbols))

(defun find-global-package-name (name packages primary nicknames)
  "Find a normalized name in the registry, including trusted early startup."
  (declare (type string name) (type function primary nicknames)
           (optimize (safety 0) (debug 0))
           (inline car cdr eq length char char-code = 1+))
  (values
   (block found
     (let ((scan packages))
       (tagbody next
          (if scan nil (return-from found nil))
          (let* ((package (car scan)) (candidate (funcall primary package))
                 (names (funcall nicknames package)))
            (tagbody compare
               (if candidate
                   (let ((size (the (integer 0 #x7fffffff) (length name)))
                         (other-size (the (integer 0 #x7fffffff) (length (the string candidate))))
                         (index 0))
                     (declare (type (integer 0 #x7fffffff) size other-size index))
                     (if (= size other-size)
                         (tagbody character
                            (if (= index size) (return-from found package))
                            (if (= (char-code (char name index))
                                   (char-code (char (the string candidate) index)))
                                nil (go different))
                            (setq index (the (integer 0 #x7fffffff) (1+ index)))
                            (go character)
                           different))))
               (if names
                   (progn (setq candidate (car names) names (cdr names)) (go compare)))))
          (setq scan (cdr scan)) (go next))))))

(defun find-local-package-name (name names)
  "Find a normalized name in the trusted current-package nickname ALIST."
  (declare (type string name) (optimize (safety 0) (debug 0))
           (inline car cdr eq length char char-code = 1+))
  (values
   (block found
     (let ((scan names))
       (tagbody next
          (if scan nil (return-from found nil))
          (let* ((entry (car scan)) (candidate (car entry))
                 (size (the (integer 0 #x7fffffff) (length name)))
                 (other-size (the (integer 0 #x7fffffff) (length (the string candidate))))
                 (index 0))
            (declare (type (integer 0 #x7fffffff) size other-size index))
            (if (= size other-size)
                (tagbody character
                   (if (= index size) (return-from found (cdr entry)))
                   (if (= (char-code (char name index))
                          (char-code (char (the string candidate) index)))
                       nil (go different))
                   (setq index (the (integer 0 #x7fffffff) (1+ index)))
                   (go character)
                  different)))
          (setq scan (cdr scan)) (go next))))))

(defun find-all-package-symbols (name packages state)
  "Collect distinct present symbols; a non-NIL status also represents NIL."
  (declare (type string name) (type simple-vector state)
           (optimize (safety 0) (debug 0))
           (inline car cdr cons svref member-eq eq))
  (values
   (let ((primary (svref state 0)) (find (svref state 1))
         (scan packages) (result nil))
     (declare (type function primary find))
     (tagbody next
        (if scan nil (go done))
        (let ((package (car scan)))
          (if (funcall primary package)
              (multiple-value-bind (symbol status) (funcall find name package)
                (if status
                    (if (eq status :inherited) nil
                        (if (member-eq symbol result) nil
                            (setq result (cons symbol result))))))))
        (setq scan (cdr scan)) (go next)
       done)
     result)))

(defun package-bootstrap-name-list (names)
  "Copy trusted static startup string names before Lisp symbols exist."
  (declare (type list names) (optimize (safety 0) (debug 0))
           (inline car cdr cons rplacd))
  (values
   (let ((scan names) (head nil) (tail nil))
     (tagbody next
        (if scan nil (go done))
        (let ((cell (cons (car scan) nil)))
          (if tail (rplacd (the cons tail) cell) (setq head cell))
          (setq tail cell scan (cdr scan)))
        (go next)
       done)
     head)))

(defun package-name-list (names)
  "Return a fresh spine of string designators, preserving name order."
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons consp stringp typep copy-seq rplacd not eq))
  (values
   (progn
     (let ((scan names) (slow names) (phase nil) (head nil) (tail nil))
       (tagbody next
          (if scan nil (go done))
          ;; Built-in startup package names are already strings and precede
          ;; installation of function bindings. Keep that path primitive-only.
          (if (consp scan) nil (error 'type-error :datum names :expected-type 'list))
          (let* ((name (car (the cons scan)))
                 (cell (cons (if (stringp name)
                                 (if (typep name 'simple-string) name (copy-seq name))
                                 (string name)) nil)))
            (if tail (rplacd (the cons tail) cell) (setq head cell))
            (setq tail cell scan (cdr (the cons scan))))
          (if phase
              (progn (setq slow (cdr (the cons slow)))
                     (if scan (if (eq scan slow)
                                  (error 'type-error :datum names :expected-type 'list)))))
          (setq phase (not phase))
          (go next)
         done)
       head))))

(defun collect-package-users (package packages use-list)
  "Find users in the private package registry, retaining its reverse order."
  (declare (type function use-list) (optimize (safety 0) (debug 0))
           (inline car cdr cons eq member-eq))
  (values
   (let ((scan packages) (result nil))
     (tagbody next
        (if scan nil (go done))
        (let ((other (car scan)))
          (if (member-eq package (funcall use-list other))
              (setq result (cons other result))))
        (setq scan (cdr scan))
        (go next)
       done)
     result)))

(defun collect-live-packages (packages name)
  "Return a fresh list omitting deleted package identities."
  (declare (type function name) (optimize (safety 0) (debug 0)) (inline car cdr cons))
  (values
   (let ((scan packages) (result nil))
     (tagbody next
        (if scan nil (go done))
        (let ((package (car scan)))
          (if (funcall name package) (setq result (cons package result))))
        (setq scan (cdr scan))
        (go next)
       done)
     result)))

(defun package-fold-name (name fold)
  "Copy NAME using a canonical target Unicode simple upcase callback."
  (declare (type string name) (type function fold)
           (optimize (safety 0) (debug 0))
           (inline length char char-code code-char make-string (setf char) = 1+
                   funcall))
  (values
   (let* ((size (length name)) (result (make-string size)) (index 0))
     (declare (type (integer 0 #x7fffffff) size index) (type simple-string result))
     (tagbody next
        (if (= index size) (go done))
        (setf (char result index)
              (the character (funcall fold (char name index))))
        (setq index (the (integer 0 #x7fffffff) (1+ index)))
        (go next)
       done)
     result)))

(defun package-name-contains-folded-p (pattern name fold)
  "Find a pre-folded pattern with the canonical target Unicode callback."
  (declare (type string pattern name) (type function fold)
           (optimize (safety 0) (debug 0))
           (inline length char char-code = > + - 1+ funcall))
  (values
   (block result
     (let ((size (length pattern)) (length (length name)) (index 0) (offset 0))
       (declare (type (integer 0 #x7fffffff) size length index offset))
       (if (> size length) (return-from result nil))
       (let ((stop (- length size)))
         (declare (type (integer 0 #x7fffffff) stop))
         (tagbody candidate
            (if (> index stop) (return-from result nil))
            (setq offset 0)
           compare
            (if (= offset size) (return-from result t))
            (if (= (char-code (char pattern offset))
                   (char-code (funcall fold (char name (+ index offset)))))
                (progn (setq offset (the (integer 0 #x7fffffff) (1+ offset))) (go compare)))
            (setq index (the (integer 0 #x7fffffff) (1+ index)))
            (go candidate)))))))

(defun filter-apropos-symbols (pattern symbols fold)
  "Destructively filter a fresh enumeration spine, preserving old reversal."
  (declare (type string pattern) (type function fold)
           (optimize (safety 0) (debug 0))
           (inline car cdr rplacd symbol-name length char char-code = > + - 1+))
  (values
   (let ((scan symbols) (result nil))
     (tagbody next
        (if scan nil (go done))
        (let ((next (cdr scan)))
          ;; Keep the same checked scalar scan inside this closed entry. The
          ;; runtime linker currently cannot retain a private helper entry.
          (if (block match
                (let* ((name (symbol-name (car scan)))
                       (size (length pattern)) (length (length name)) (index 0) (offset 0))
                  (declare (type string name)
                           (type (integer 0 #x7fffffff) size length index offset))
                  (if (> size length) (return-from match nil))
                  (let ((stop (- length size)))
                    (declare (type (integer 0 #x7fffffff) stop))
                    (tagbody candidate
                       (if (> index stop) (return-from match nil))
                       (setq offset 0)
                      compare
                       (if (= offset size) (return-from match t))
                       (if (= (char-code (char pattern offset))
                              (char-code (funcall fold (char name (+ index offset)))))
                           (progn (setq offset (the (integer 0 #x7fffffff) (1+ offset))) (go compare)))
                       (setq index (the (integer 0 #x7fffffff) (1+ index)))
                       (go candidate)))))
              (progn (rplacd (the cons scan) result) (setq result scan)))
          (setq scan next))
        (go next)
       done)
     result)))

(defun check-package-options (options)
  "Check finite option syntax and disjoint name groups before any mutation."
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr cons eq eql list-length
                   integerp stringp < = logand logior logxor rplacd))
  (values
   (progn
     (if (list-length options) nil (error 'type-error :datum options :expected-type 'list))
     (let ((scan options) (seen nil) (documentation nil) (size nil))
       (tagbody next
          (if scan nil (go done))
          (let* ((option (car scan)) (length (list-length option)))
            (if length nil (error 'type-error :datum option :expected-type 'list))
            (if (= length 0) (error 'program-error))
            (let* ((keyword (car option)) (items (cdr option)) (group 0))
              (declare (type (integer 0 16) group))
              (if (eq keyword :documentation)
                  (progn
                    (if documentation (error 'program-error))
                    (setq documentation t)
                    (if (= length 2) nil (error 'program-error))
                    (if (stringp (car items)) nil
                        (error 'type-error :datum (car items) :expected-type 'string)))
                  (if (eq keyword :size)
                      (progn
                        (if size (error 'program-error))
                        (setq size t)
                        (if (= length 2) nil (error 'program-error))
                        (if (integerp (car items))
                            (if (< (car items) 0)
                                (error 'type-error :datum (car items) :expected-type 'unsigned-byte))
                            (error 'type-error :datum (car items) :expected-type 'unsigned-byte)))
                      (if (eq keyword :shadow) (setq group 1)
                          (if (eq keyword :intern) (setq group 2)
                              (if (eq keyword :import-from) (setq group 4)
                                  (if (eq keyword :shadowing-import-from) (setq group 8)
                                      (if (eq keyword :export) (setq group 16)
                                          (if (eq keyword :nicknames) nil
                                              (if (eq keyword :use) nil
                                                  (if (eq keyword :local-nicknames) nil
                                                      (error 'program-error)))))))))))
              (if (if (= group 4) t (= group 8))
                  (progn
                    (if items nil (error 'program-error))
                    (setq items (cdr items))))
              (if (= group 0) nil
                  (tagbody item
                     (if items nil (go items-done))
                     (let* ((name (string (car items))) (names seen) (entry nil))
                       (tagbody find
                          (if names nil (go found))
                          (if (string= name (car (the cons (car names))))
                              (progn (setq entry (car names)) (go found)))
                          (setq names (cdr names)) (go find)
                         found)
                       (if entry nil
                           (progn (setq entry (cons name 0)) (setq seen (cons entry seen))))
                       (let ((mask (cdr (the cons entry))))
                         (declare (type (integer 0 31) mask))
                         (if (if (= group 16) nil
                                 (if (= 0 (logand mask (logxor 15 group))) nil t))
                             (error 'program-error))
                         (if (= group 16)
                             (if (= 0 (logand mask 2)) nil (error 'program-error))
                             (if (= group 2)
                                 (if (= 0 (logand mask 16)) nil (error 'program-error))))
                         (rplacd (the cons entry) (logior mask group))))
                     (setq items (cdr items)) (go item)
                    items-done))))
          (setq scan (cdr scan)) (go next)
         done)
       nil))))
