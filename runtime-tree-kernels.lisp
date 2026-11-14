;;;; Explicit traversal stack for the SUBST/NSUBST/SUBLIS family.
(in-package #:miricl-runtime)
(export '(substitute-tree-values copy-tree-value))

(defun substitute-tree-values (tree item new state)
  "Replace subtrees in depth-first callback order, copying after first change.
STATE is #(key resolved-function mode negate destructive-p alist-p). Native
keyword binding validates callables; every cons access remains structurally
checked after arbitrary callbacks. An active path rejects traversed cycles."
  (declare (type simple-vector state) (optimize (safety 0) (debug 0))
           (inline svref (setf svref) make-array consp car cdr cons rplaca rplacd
                   checked-path-car checked-path-cdr eq = 1+ 1- + zerop not))
  (values
   (block result
     (let* ((key (svref state 0)) (function (svref state 1)) (mode (svref state 2))
            (negate (svref state 3)) (write (svref state 4)) (alist (svref state 5))
            (alist-count (if alist (list-length item) 0))
            (scan tree) (head nil) (tail nil) (entered nil) (pending nil) (leaf-parent nil)
            (anchor tree) (span 0) (power 1)
            (active nil) (value nil) (terminal nil))
       (declare (type (integer 0 2) mode)
                (type (integer 0 #x3fffffffffffffff) span power))
       (if alist-count nil
           (error 'type-error :datum item :expected-type (list 'satisfies 'list-length)))
       (tagbody next
          (setq terminal nil)
          (if alist
              (let ((keyed (if key (funcall key scan) scan)) (entries item) (remaining alist-count))
                (tagbody lookup
                   (if (= remaining 0) (go looked))
                   (let ((entry (checked-path-car entries)))
                     (if entry
                         (progn
                           (if (consp entry) nil (error 'type-error :datum entry :expected-type 'list))
                           (let ((truth (funcall function keyed (car (the cons entry)))))
                             (if (if negate (not truth) truth)
                                 (progn (setq value (cdr (the cons entry)) terminal t) (go looked)))))))
                   (setq entries (checked-path-cdr entries) remaining (1- remaining))
                   (go lookup)
                  looked))
              (let* ((keyed (if key (funcall key scan) scan))
                     (truth (if (= mode 0) (funcall function item keyed) (funcall function keyed))))
                (if (if (= mode 0) (if negate (not truth) truth)
                        (if (= mode 2) (not truth) truth))
                    (setq value new terminal t))))
          (if terminal (go handle))
          (if (consp scan)
              (progn
                (if active
                    (progn
                      (if (gethash scan active)
                          (error 'type-error :datum scan :expected-type (list 'not (list 'eql scan))))
                      (setf (gethash scan active) t)))
                (setq entered (cons scan entered))
                ;; Atom CARs cannot descend further. Reuse scalar parent
                ;; state instead of allocating a return frame for each leaf.
                (if (consp (car (the cons scan))) nil
                    (progn (setq leaf-parent scan scan (car (the cons scan))) (go next)))
                ;; Flat spines use Brent's cycle check below. A precise active
                ;; hash is only needed when CAR traversal introduces branches;
                ;; seed it with the already visited spine before descending.
                (if active nil
                    (progn
                      (setq active (make-hash-table :test 'eq))
                      (let ((cursor entered))
                        (tagbody seed
                           (if cursor nil (go seeded))
                           (setf (gethash (car (the cons cursor)) active) t)
                           (setq cursor (cdr (the cons cursor)))
                           (go seed)
                          seeded))))
                (let ((frame (make-array 5)))
                  (declare (type simple-vector frame))
                  (setf (svref frame 0) tree)
                  (setf (svref frame 1) scan)
                  (setf (svref frame 2) head)
                  (setf (svref frame 3) tail)
                  (setf (svref frame 4) entered)
                  (setq pending (cons frame pending)))
                (setq tree (car (the cons scan)) scan tree head nil tail nil entered nil)
                (go next))
              (setq value scan terminal t))
         handle
          (if leaf-parent (setq scan leaf-parent leaf-parent nil terminal nil))
          (if write
              (progn
                (if terminal
                    (progn
                      (if tail (progn (rplacd (the cons tail) value) (setq value tree)))
                      (go deliver)))
                (if (consp scan) nil (error 'type-error :datum scan :expected-type 'cons))
                (rplaca (the cons scan) value)
                (setq tail scan))
              (progn
                (if (if tail nil (not (eq value (if terminal scan (checked-path-car scan)))))
                    (let ((prefix tree) (remaining (length entered)))
                      (declare (type (integer 0 #x3fffffffffffffff) remaining))
                      ;; A callback may have disconnected or cycled an earlier
                      ;; prefix. Bound the copy by the visited spine, and reject
                      ;; a malformed path instead of following it indefinitely.
                      (tagbody copy-prefix
                         (if (eq prefix scan) (go copied))
                         (if (zerop remaining)
                             (error 'type-error :datum prefix :expected-type (list 'eql scan)))
                         (let ((cell (cons (checked-path-car prefix) nil)))
                           (if tail (rplacd (the cons tail) cell) (setq head cell))
                           (setq tail cell))
                         (setq prefix (checked-path-cdr prefix)
                               remaining (the (integer 0 #x3fffffffffffffff) (1- remaining)))
                         (go copy-prefix)
                        copied)))
                (if terminal
                    (progn
                      (if tail (progn (rplacd (the cons tail) value) (setq value head))
                          (if (eq value scan) (setq value tree)))
                      (go deliver)))
                (if (if tail t (not (eq value (checked-path-car scan))))
                    (let ((cell (cons value nil)))
                      (if tail (rplacd (the cons tail) cell) (setq head cell))
                      (setq tail cell)))))
          (setq scan (checked-path-cdr scan))
          (if active nil
              (progn
                (setq span (the (integer 0 #x3fffffffffffffff) (1+ span)))
                (if (if scan (eq scan anchor) nil)
                    (error 'type-error :datum scan :expected-type (list 'not (list 'eql scan))))
                (if (= span power)
                    (setq anchor scan span 0 power (the (integer 0 #x3fffffffffffffff) (+ power power))))))
          (go next)
         deliver
          (tagbody clear-path
             (if entered nil (go cleared))
             (if active (remhash (car (the cons entered)) active))
             (setq entered (cdr (the cons entered)))
             (go clear-path)
            cleared)
          (if pending
              (let ((frame (car (the cons pending))))
                (declare (type simple-vector frame))
                (setq pending (cdr (the cons pending)) tree (svref frame 0) scan (svref frame 1)
                      head (svref frame 2) tail (svref frame 3) entered (svref frame 4) terminal nil)
                (go handle))
              (return-from result value)))))))

(defun copy-tree-value (tree)
  "Copy every cons occurrence without preserving shared input substructure.
The explicit stack avoids native recursion. Flat-spine Brent checks and a
lazy active-path table reject traversed CAR/CDR cycles before field access."
  (declare (optimize (safety 0) (debug 0))
           (inline make-array svref (setf svref) consp car cdr cons rplaca rplacd
                   eq = 1+ + checked-path-car checked-path-cdr))
  (values
   (block result
     (let ((scan tree) (head nil) (tail nil) (entered nil) (pending nil) (active nil)
           (anchor tree) (span 0) (power 1) (value nil))
       (declare (type (integer 0 #x3fffffffffffffff) span power))
       (tagbody next
          (if (consp scan) nil (go finished))
          (if active
              (progn
                (if (gethash scan active)
                    (error 'type-error :datum scan :expected-type (list 'not (list 'eql scan))))
                (setf (gethash scan active) t)))
          (setq entered (cons scan entered))
          (let ((cell (cons nil nil)) (child (car (the cons scan))))
            (if tail (rplacd (the cons tail) cell) (setq head cell))
            (setq tail cell)
            (if (consp child)
                (progn
                  (if active nil
                      (progn
                        (setq active (make-hash-table :test 'eq))
                        (let ((cursor entered))
                          (tagbody seed
                             (if cursor nil (go seeded))
                             (setf (gethash (car (the cons cursor)) active) t)
                             (setq cursor (cdr (the cons cursor)))
                             (go seed)
                            seeded))))
                  (let ((frame (make-array 4)))
                    (declare (type simple-vector frame))
                    (setf (svref frame 0) scan)
                    (setf (svref frame 1) head)
                    (setf (svref frame 2) tail)
                    (setf (svref frame 3) entered)
                    (setq pending (cons frame pending)))
                  (setq scan child head nil tail nil entered nil)
                  (go next))
                (rplaca (the cons cell) child)))
         advance
          (setq scan (cdr (the cons scan)))
          (if active nil
              (progn
                (setq span (the (integer 0 #x3fffffffffffffff) (1+ span)))
                (if (if scan (eq scan anchor) nil)
                    (error 'type-error :datum scan :expected-type (list 'not (list 'eql scan))))
                (if (= span power)
                    (setq anchor scan span 0 power (the (integer 0 #x3fffffffffffffff) (+ power power))))))
          (go next)
         finished
          (if tail (progn (rplacd (the cons tail) scan) (setq value head)) (setq value scan))
          (tagbody clear-path
             (if entered nil (go cleared))
             (if active (remhash (car (the cons entered)) active))
             (setq entered (cdr (the cons entered)))
             (go clear-path)
            cleared)
          (if pending
              (let ((frame (car (the cons pending))))
                (declare (type simple-vector frame))
                (setq pending (cdr (the cons pending)) scan (svref frame 0) head (svref frame 1)
                      tail (svref frame 2) entered (svref frame 3))
                (rplaca (the cons tail) value)
                (go advance))
              (return-from result value)))))))
