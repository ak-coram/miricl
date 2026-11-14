;;;; Ordinary CL runtime kernels, executable on the host and cross-compiled
;;;; for the target. Keep this first slice independent of bootstrap metadata.
(defpackage #:miricl-runtime
  (:use #:cl)
  (:export #:property-cell #:member-eq #:list-eq #:reverse-copy-onto #:reverse-mutate-onto
           #:last-bounded #:copy-before-tail #:copy-butlast-bounded #:truncate-butlast-bounded))
(in-package #:miricl-runtime)

;; Keep checked error paths separate so the host does not diagnose a known
;; non-list CAR/CDR at compile time. Closed-module compilation can inline them.
(defun checked-car (object)
  (declare (optimize (safety 3)) (inline car))
  (car object))

(defun checked-cdr (object)
  (declare (optimize (safety 3)) (inline cdr))
  (cdr object))

(defun property-cell (plist key)
  "Find KEY's value cons without copying; an odd final key may return NIL.
This internal lookup deliberately leaves keyword-list validation to its caller."
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr eq consp))
  (values (block found
   (tagbody
   next
     (if plist
         (if (consp plist)
             (if (eq (car (the cons plist)) key)
                 (return-from found (cdr (the cons plist))))
             ;; Preserve ordinary Lisp errors for malformed storage. SAFETY 0
             ;; applies only after the structural checks above and below.
             (return-from found (checked-car plist)))
         (return-from found nil))
     (let ((tail (cdr (the cons plist))))
       (if tail
           (if (consp tail)
               (setq plist (cdr (the cons tail)))
               (return-from found (checked-cdr tail)))
           (return-from found nil)))
     (go next)))))

(defun member-eq (item items)
  "Internal identity membership predicate; return one Lisp boolean."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr eq consp))
  (values (block found
    (tagbody
     next
       (if items
           (if (consp items)
               (if (eq item (car (the cons items))) (return-from found t))
               (return-from found (checked-car items)))
           (return-from found nil))
       (setq items (cdr (the cons items)))
       (go next)))))

(defun list-eq (left right)
  "Internal pairwise identity comparison, retaining checked list access."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr eq consp))
  (values (block done
    (tagbody
     next
       (if left
           (if (consp left) nil (return-from done (checked-car left)))
           (return-from done (if right nil t)))
       ;; The old primitive reads LEFT even when RIGHT is NIL. Keep that
       ;; validation and error order before taking the unequal-length exit.
       (let ((item (car (the cons left))))
         (if right
             (if (consp right) nil (return-from done (checked-car right)))
             (return-from done nil))
         (if (eq item (car (the cons right))) nil (return-from done nil)))
       (setq left (cdr (the cons left)) right (cdr (the cons right)))
       (go next)))))

(defun reverse-copy-onto (items tail)
  "Copy and reverse a proper list onto an arbitrary, shared tail."
  (declare (optimize (safety 0) (debug 0)) (inline car cdr consp cons))
  (values (block done
    (tagbody
     next
       (if items
           (if (consp items) nil (return-from done (checked-cdr items)))
           (return-from done tail))
       (setq tail (cons (car (the cons items)) tail)
             items (cdr (the cons items)))
       (go next)))))

(defun reverse-mutate-onto (items tail)
  "Reverse a proper list by relinking its conses onto a shared tail."
  (declare (optimize (safety 0) (debug 0)) (inline cdr consp rplacd))
  (values (block done
    (tagbody
     next
       (if items
           (if (consp items) nil (return-from done (checked-cdr items)))
           (return-from done tail))
       (let ((rest (cdr (the cons items))))
         (rplacd (the cons items) tail)
         (setq tail items items rest))
       (go next)))))

(defun last-bounded (items count)
  "Find the last COUNT conses; the native boundary checks LIST and bounds COUNT."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline cdr consp zerop 1-))
  (values (block done
    (let ((ahead items))
      (tagbody
       advance
         (if (zerop count) (go scan))
         (if (consp ahead) nil (go scan))
         (setq ahead (cdr (the cons ahead))
               count (the (integer 0 #x7fffffff) (1- count)))
         (go advance)
       scan
         (if (consp ahead) nil (return-from done items))
         ;; AHEAD is COUNT conses ahead of ITEMS, so ITEMS is also a cons.
         (setq ahead (cdr (the cons ahead)) items (cdr (the cons items)))
         (go scan))))))

(defun copy-before-tail (items object)
  "Copy up to OBJECT, retaining a dotted terminator when OBJECT is absent.
The native boundary checks LIST before entering this sealed worker."
  ;; ITEMS can become an arbitrary dotted terminator after CDR. The entry
  ;; boundary proves LIST only on entry, not throughout this mutable binding.
  (declare (optimize (safety 0) (debug 0))
           (inline car cdr consp eql cons rplacd))
  (values (block done
    (if (eql items object) (return-from done nil))
    (if items nil (return-from done nil))
    (let* ((result (cons (car (the cons items)) nil)) (end result))
      (setq items (cdr (the cons items)))
      (tagbody
       next
         (if (eql items object) (return-from done result))
         (if (consp items) nil
             (progn (rplacd (the cons end) items) (return-from done result)))
         (let ((cell (cons (car (the cons items)) nil)))
           (rplacd (the cons end) cell)
           (setq end cell items (cdr (the cons items))))
         (go next))))))

(declaim (inline advance-list-tail))
(defun advance-list-tail (items count)
  "Advance at most COUNT conses. The boundary checks COUNT before this worker."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline cdr consp zerop 1-))
  (values (block done
    (tagbody next
      (if (zerop count) (return-from done items))
      (if (consp items) nil (return-from done items))
      (setq items (cdr (the cons items)) count (the (integer 0 #x7fffffff) (1- count)))
      (go next)))))

(defun copy-butlast-bounded (items count)
  "Copy all but COUNT trailing conses, dropping any dotted terminator."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline advance-list-tail car cdr consp cons rplacd))
  (values (block done
    (let ((ahead (advance-list-tail items count)) (result nil) (end nil))
      (tagbody next
        (if (consp ahead) nil (return-from done result))
        ;; AHEAD is a tail of ITEMS: a cons ahead proves the cursor is a cons.
        (let ((cell (cons (car (the cons items)) nil)))
          (if end (rplacd (the cons end) cell) (setq result cell))
          (setq end cell items (cdr (the cons items)) ahead (cdr (the cons ahead))))
        (go next))))))

(defun truncate-butlast-bounded (items count)
  "Drop COUNT trailing conses by changing the last retained cons's CDR."
  (declare (type (integer 0 #x7fffffff) count)
           (optimize (safety 0) (debug 0)) (inline advance-list-tail cdr consp rplacd))
  (values (block done
    (let ((ahead (advance-list-tail items count)) (scan items) (end nil))
      (tagbody next
        (if (consp ahead) nil (go finish))
        (setq end scan scan (cdr (the cons scan)) ahead (cdr (the cons ahead)))
        (go next)
       finish
        (if end
            (progn (rplacd (the cons end) nil) (return-from done items))
            (return-from done nil)))))))
