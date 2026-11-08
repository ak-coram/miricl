;;;; Development compiler transport. Application code never runs on the host.
;;;; Wire data uses the FASL object graph, with opaque references to target literals.
(defun %remote-print (object)
  (let ((*package* (find-package "CL-USER")) (*print-readably* t) (*print-circle* nil)
        (*print-pretty* nil) (*print-level* nil) (*print-length* nil)
        (*print-base* 10) (*print-radix* nil) (*print-case* :upcase))
    (write-to-string object)))
(defun %remote-read (text)
  (let ((*package* (find-package "CL-USER")) (*readtable* (copy-readtable nil))
        (*read-eval* nil) (*read-base* 10))
    (multiple-value-bind (object end) (read-from-string text)
      (unless (every (lambda (c) (find c '(#\Space #\Tab #\Newline #\Return))) (subseq text end))
        (error "Trailing compiler packet data.")) object)))
(defun %remote-pack (object &optional references)
  (let* ((%fasl-records (make-array 32 :adjustable t :fill-pointer 0))
         (%fasl-seen (make-hash-table :test 'eq)) (%file-load-values nil)
         (%file-object-effects nil) (root (%fasl-dump object)))
    (loop for marker in references for index from 0 do
      (multiple-value-bind (record present) (gethash marker %fasl-seen)
        (when present (setf (aref %fasl-records record) (list 11 index)))))
    (%remote-print (list 1 root (coerce %fasl-records 'list)))))
(defun %remote-unpack (packet &optional literals)
  (unless (and (listp packet) (= (length packet) 3) (eql (first packet) 1))
    (error "Invalid compiler packet."))
  (let* ((%fasl-records (coerce (third packet) 'vector))
         (%fasl-values (make-array (length %fasl-records)))
         (%fasl-states (make-array (length %fasl-records) :initial-element 0))
         (%fasl-readtable (copy-readtable nil)) (%fasl-pending nil))
    (dotimes (i (length %fasl-records))
      (let* ((record (aref %fasl-records i)) (kind (car record)))
        (cond
          ((eql kind 11)
           (unless (and literals (integerp (cadr record)) (<= 0 (cadr record))
                        (< (cadr record) (length literals))) (error "Invalid target literal reference."))
           (setf (aref %fasl-values i) (cdr (aref literals (cadr record)))
                 (aref %fasl-states i) 4))
          ((not (and (integerp kind) (<= 0 kind 8)))
           ;; MAKE-LOAD-FORM and LOAD-TIME-VALUE records execute code; neither
           ;; is allowed in a compiler request or response.
           (error "Executable objects are not allowed in compiler packets.")))
        (let ((package (case kind (4 (caddr record)) (8 (cadr record)))))
          (when (and package (not (find-package package))) (make-package package :use nil)))))
    (%fasl-object (second packet))))

(defun %remote-compile-request (text)
  (handler-case
      (let* ((request (%remote-unpack (%remote-read text)))
             (form (first request)) (references (second request)) (specials (third request)))
        (multiple-value-bind (code literals)
            (miricl-compiler:emit-native-module
              (miricl-compiler:compile-to-hir form :global-specials specials))
          (%remote-pack (list (coerce code 'simple-vector) literals) references)))
    (error (condition) (%remote-print (list 0 (princ-to-string condition))))))

(defun %remote-compile (lambda-expression &optional name)
  (let* ((%remote-literals (make-array 8 :adjustable t :fill-pointer 0))
         (%compilation-complete nil)
         (expanded (%compile-form (list 'function lambda-expression) nil))
         (seen (make-hash-table :test 'eq)) (work (list expanded)) (specials nil))
    ;; Include uninterned source symbols so returned global references retain
    ;; target identity. Package symbols use their portable package/name identity.
    (loop while work do
      (let ((object (pop work)))
        (unless (gethash object seen)
          (setf (gethash object seen) t)
          (cond ((consp object) (push (car object) work) (push (cdr object) work))
                ((symbolp object)
                 (when (miricl-compiler::compiler-primitive 18 object) (push object specials))
                 (when (and object (not (symbol-package object))
                            (not (find object %remote-literals :key #'car :test #'eq)))
                   (vector-push-extend (cons object object) %remote-literals)))))))
    (let* ((form (if (eq (car expanded) 'function) (second expanded) expanded))
           (request (%remote-pack (list form (map 'list #'car %remote-literals) specials)))
           (response (%remote-read (%%compile 22 request))))
      (when (eql (car response) 0) (error "Remote compiler: ~A" (second response)))
      (let ((result (%remote-unpack response %remote-literals)))
        (%%compile 17 (first result) (second result) name)))))

(defun %remote-evaluate (form)
  (let ((operator (and (consp form) (car form))))
    (case operator
      (progn
       (if (cdr form)
           (progn (dolist (child (butlast (cdr form))) (%remote-evaluate child))
                  (%remote-evaluate (car (last form)))) nil))
      (defun
       (let* ((name (second form)) (body (cdddr form)) (prefix nil) (documentation nil))
         (loop while (or (and (stringp (car body)) (cdr body) (not documentation))
                         (and (consp (car body)) (eq (caar body) 'declare))) do
           (let ((item (pop body)))
             (if (stringp item) (setq documentation item) (push item prefix))))
         (let ((function (%remote-compile
                           (list* 'lambda (third form)
                             (append (nreverse prefix)
                               (list (list* 'block (if (consp name) (second name) name) body)))) name)))
           (setf (fdefinition name) function)
           (when (and (symbolp name) (macro-function name)) (setf (macro-function name) nil))
           (setf (documentation name 'function) documentation
                 (documentation function 'function) documentation) name)))
      ;; These establish the target's compilation environment and retain the
      ;; evaluator's existing semantics. Their generated methods/macros can be
      ;; compiled explicitly; ordinary expressions and DEFUN bodies use the host.
      ((defvar defparameter)
       (if (and (<= (if (eq operator 'defvar) 2 3) (length form) 4) (symbolp (second form)))
           (progn
             (proclaim (list 'special (second form)))
             (if (and (cddr form) (or (eq operator 'defparameter) (not (boundp (second form)))))
                 (eval (list* operator (second form)
                         (list 'quote (%remote-evaluate (third form))) (cdddr form)))
                 (eval form)))
           (eval form)))
      ((defpackage in-package defmacro deftype defclass defstruct define-condition
        defmethod defgeneric defconstant declaim proclaim)
       (eval form))
      (otherwise
       (multiple-value-bind (expansion expanded) (macroexpand-1 form)
         (if expanded (%remote-evaluate expansion)
             (funcall (%remote-compile (list 'lambda nil form)))))))))
