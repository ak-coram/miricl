;;;; Portable mapping loops. Representation access and callback invocation remain
;;;; supplied by the runtime substrate; list cursors avoid indexed list walks.
(in-package #:miricl-runtime)
(export '(mapping-argument-value map-list-values map-sequence-result-value))

(declaim (inline mapping-argument-value))
(defun mapping-argument-value (sequence index mode)
  (declare (type (integer 0 #x7fffffff) index mode)
           (optimize (safety 0) (debug 0))
           (inline >= <= consp car checked-path-car))
  (values (if (>= mode 2)
              (if (<= mode 3) sequence
                  (if (consp sequence) (car (the cons sequence)) (elt sequence index)))
              (checked-path-car sequence))))

(defun map-list-values (function inputs mode output)
  "Map separately owned cursors, passing fresh argument lists to callbacks."
  (declare (type (integer 0 #x7fffffff) mode)
           (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr consp car cdr cons rplaca rplacd
                   svref (setf svref) (setf row-major-aref) vectorp length = < <= >= 1+ 1- mapping-argument-value))
  (values (block done
    (let* ((function (if (symbolp function) (fdefinition function) function))
           (n (let ((count (list-length inputs)))
                (if count count (error 'type-error :datum inputs
                                       :expected-type (cons 'satisfies (cons 'list-length nil))))))
           (cursors (make-array n)) (first (checked-path-car inputs))
           (result output) (tail output) (index 0) (limit #x1ffffffe)
           (i 0) (scan inputs))
      (declare (type simple-vector cursors)
               (type (integer 0 #x7fffffff) n index limit i))
      (if (functionp function) nil (error 'type-error :datum function :expected-type 'function))
      (if (= n 0) (if (= mode 8) nil (error 'program-error)))
      (if (= mode 8)
          (setq limit (if (vectorp output) (array-total-size output)
                         (let ((count (list-length output)))
                           (if count count (error 'type-error :datum output
                                                  :expected-type (cons 'satisfies (cons 'list-length nil))))))))
      ;; Validate every input before a shorter sequence can terminate mapping.
      ;; Lists must be proper even when an earlier input happens to be empty.
      (tagbody initialize
        (if (= i n) (go initialized))
        (let* ((sequence (checked-path-car scan))
               (count (if (>= mode 4)
                          (if (vectorp sequence) (length sequence) (list-length sequence))
                          (list-length sequence))))
          (if count nil (error 'type-error :datum sequence
                               :expected-type (cons 'satisfies (cons 'list-length nil))))
          ;; Keep traversal finite if a callback destructively changes a list
          ;; spine. Such structural changes have undefined traversal results;
          ;; the boundary must nevertheless not permit an unbounded cycle.
          (if (< count limit) (setq limit count))
          (setf (svref cursors i) sequence))
        (setq scan (checked-path-cdr scan) i (the (integer 0 #x7fffffff) (1+ i)))
        (go initialize)
       initialized
       next
        (if (>= index limit) (go finish))
        (setq i 0)
       check
        (if (= i n) (go invoke))
        (let ((sequence (svref cursors i)))
          (if (>= mode 4)
              (if (vectorp sequence)
                  (if (>= index (length sequence)) (go finish))
                  (if sequence
                      (if (consp sequence) nil (error 'type-error :datum sequence :expected-type 'list))
                      (go finish)))
              (if sequence
                  (if (consp sequence) nil (error 'type-error :datum sequence :expected-type 'list))
                  (go finish))))
        (setq i (the (integer 0 #x7fffffff) (1+ i)))
        (go check)
       invoke
        (let ((value
                (if (= n 1)
                    (funcall function
                      (let ((sequence (svref cursors 0)))
                        (if (>= mode 2)
                            (if (<= mode 3) sequence
                                (if (consp sequence) (car (the cons sequence)) (elt sequence index)))
                            (checked-path-car sequence))))
                    (let ((arguments nil))
                      (setq i n)
                      (tagbody arguments
                        (if (= i 0) (go ready))
                        (setq i (the (integer 0 #x7fffffff) (1- i)))
                        (setq arguments
                          (cons (let ((sequence (svref cursors i)))
                                  (if (>= mode 2)
                                      (if (<= mode 3) sequence
                                          (if (consp sequence) (car (the cons sequence)) (elt sequence index)))
                                      (checked-path-car sequence)))
                                arguments))
                        (go arguments)
                       ready)
                      (apply function arguments)))))
          (tagbody
          (if (= mode 4) (if value nil (return-from done nil)))
          (if (= mode 5) (if value (return-from done value)))
          (if (= mode 0) (go collect))
          (if (= mode 2) (go collect))
          (if (= mode 6) (go store))
          (if (= mode 8) (go store))
          (go advance)
         collect
          (let ((cell (cons value nil)))
            (if tail (rplacd (the cons tail) cell) (setq result cell))
            (setq tail cell))
          (go advance)
         store
          (if (vectorp output)
              (setf (row-major-aref output index) value)
              (progn
                (if (consp tail) nil (error 'type-error :datum tail :expected-type 'cons))
                (rplaca (the cons tail) value)
                (setq tail (cdr (the cons tail)))))))
       advance
        (setq i 0)
       cursor
        (if (= i n) (go advanced))
        (let ((sequence (svref cursors i)))
          (if (consp sequence) (setf (svref cursors i) (cdr (the cons sequence)))))
        (setq i (the (integer 0 #x7fffffff) (1+ i)))
        (go cursor)
       advanced
        (setq index (the (integer 0 #x7fffffff) (1+ index)))
        (go next)
       finish)
      (if (= mode 8)
          (if (vectorp output)
              (if (array-has-fill-pointer-p output)
                  (funcall (fdefinition (cons 'setf (cons 'fill-pointer nil))) index output))))
      (if (= mode 4) (return-from done t))
      (if (= mode 1) (return-from done first))
      (if (= mode 3) (return-from done first))
      result))))

(defun map-sequence-result-value (type inputs)
  "Validate MAP inputs and allocate its result before invoking callbacks."
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr vectorp length <))
  (values (let ((scan inputs) (limit #x1ffffffe))
    (if (list-length inputs) nil
        (error 'type-error :datum inputs :expected-type (cons 'satisfies (cons 'list-length nil))))
    (if inputs nil (error 'program-error))
    (tagbody size
      (if scan nil (go sized))
      (let* ((sequence (checked-path-car scan))
             (length (if (vectorp sequence) (length sequence)
                         (let ((count (list-length sequence)))
                           (if count count (error 'type-error :datum sequence
                                                  :expected-type (cons 'satisfies (cons 'list-length nil))))))))
        (if (< length limit) (setq limit length)))
      (setq scan (checked-path-cdr scan))
      (go size)
     sized)
    (if type (make-sequence type limit) nil))))

(export 'search-list-values)
(defun search-list-values (items item key controls)
  "Search checked list cursors without copying a matching tail or association.
CONTROLS is the private four-item list (kind mode resolved-function negate)."
  (declare (optimize (safety 0) (debug 0))
           (inline checked-path-car checked-path-cdr car cdr consp eq not
                   integerp < > = 1+ +))
  (values (block done
    (let* ((kind (checked-path-car controls)) (tail (checked-path-cdr controls))
           (mode (checked-path-car tail)) (tail (checked-path-cdr tail))
           (function (checked-path-car tail)) (tail (checked-path-cdr tail))
           (negate (checked-path-car tail)) (tail (checked-path-cdr tail))
           (original items) (anchor items) (power 1) (span 0))
      (declare (type (integer 0 #x3fffffffffffffff) span))
      (if tail (error 'type-error :datum tail :expected-type 'null))
      (if (integerp kind)
          (if (< kind 0) (error 'type-error :datum kind :expected-type (list 'integer 0 2))
              (if (> kind 2) (error 'type-error :datum kind :expected-type (list 'integer 0 2))))
          (error 'type-error :datum kind :expected-type (list 'integer 0 2)))
      (if (integerp mode)
          (if (< mode 0) (error 'type-error :datum mode :expected-type (list 'integer 0 2))
              (if (> mode 2) (error 'type-error :datum mode :expected-type (list 'integer 0 2))))
          (error 'type-error :datum mode :expected-type (list 'integer 0 2)))
      ;; The boundary above owns validation; these private scalar bindings
      ;; expose its established ranges to both host and target compilation.
      (let ((kind (the (integer 0 2) kind)) (mode (the (integer 0 2) mode)))
      (declare (type (integer 0 2) kind mode))
      (tagbody next
        (if items nil (return-from done nil))
        (if (consp items) nil (error 'type-error :datum items :expected-type 'list))
        (let* ((entry (car (the cons items))) (value entry))
          (tagbody
            (if (= kind 0) nil
                (progn
                  ;; NIL alist entries are ignored, including when ITEM is NIL.
                  (if entry nil (go skip))
                  (if (consp entry) nil (error 'type-error :datum entry :expected-type 'list))
                  (setq value (if (= kind 1) (car (the cons entry)) (cdr (the cons entry))))))
            (if key (setq value (funcall key value)))
            (let ((truth (if (= mode 0) (funcall function item value) (funcall function value))))
              (if (if (= mode 0) (if negate (not truth) truth)
                      (if (= mode 2) (not truth) truth))
                  (return-from done (if (= kind 0) items entry))))
           skip))
        ;; Brent's visited-cursor check reads no future spine and introduces
        ;; no callback calls. Early matches retain dotted/circular tail sharing;
        ;; a miss cannot follow a stationary cycle indefinitely.
        (setq items (cdr (the cons items)) span (the (integer 0 #x3fffffffffffffff) (1+ span)))
        (if items
            (progn
              (if (eq items anchor)
                  (error 'type-error :datum original
                         :expected-type (cons 'satisfies (cons 'list-length nil))))
              (if (= span power) (setq anchor items span 0 power (+ power power)))))
        (go next)))))))

(export 'list-exclusive-values)
(defun list-exclusive-values (left right key controls)
  "Visit ordered pairs for SET-EXCLUSIVE-OR with an arbitrary predicate.
CONTROLS is (resolved-test . resolved-test-not). One30-bit immediate bitmap
word records every right element participating in at least one matching pair."
  (declare (optimize (safety 0) (debug 0))
           (inline consp car cdr cons checked-path-car checked-path-cdr integerp
                   callable-value = < > + 1+ ash logior logand zerop
                   svref (setf svref)))
  (values
   (block done
     (if (consp controls) nil (error 'type-error :datum controls :expected-type 'cons))
     (let* ((left-count (let ((n (list-length left)))
                          (if n n (error 'type-error :datum left
                                         :expected-type (cons 'satisfies (cons 'list-length nil))))))
            (right-count (let ((n (list-length right)))
                           (if n n (error 'type-error :datum right
                                          :expected-type (cons 'satisfies (cons 'list-length nil)))))))
       ;; Empty-set sharing needs no bitmap, scalar counter or callback use.
       ;; Both original spines have still passed the proper-list boundary.
       (if left nil (return-from done right))
       (if right nil (return-from done left))
       (if (integerp left-count) nil (error 'type-error :datum left-count :expected-type 'unsigned-byte))
       (if (< left-count 0) (error 'type-error :datum left-count :expected-type 'unsigned-byte))
       (if (integerp right-count) nil (error 'type-error :datum right-count :expected-type 'unsigned-byte))
       (if (< right-count 0) (error 'type-error :datum right-count :expected-type 'unsigned-byte))
       (if (> left-count #x1ffffffe) (error 'storage-condition))
       (if (> right-count #x1ffffffe) (error 'storage-condition))
       (let* ((negate (cdr (the cons controls)))
              (function (callable-value (if negate negate (car (the cons controls)))))
              (key (if key (callable-value key) nil))
              (left-count (the (integer 0 #x1ffffffe) left-count))
              (right-count (the (integer 0 #x1ffffffe) right-count))
              (marks (make-array (ceiling right-count 30) :initial-element 0))
              (scan-left left) (result nil) (left-index 0) (right-index 0)
              (word 0) (mask 1))
         (declare (type (integer 0 #x1ffffffe) left-count right-count left-index right-index word)
                  (type (integer 1 #x20000000) mask) (type simple-vector marks))
         (tagbody next-left
            (if (= left-index left-count) (go left-finished))
            (if (consp scan-left) nil (error 'type-error :datum scan-left :expected-type 'cons))
            (let ((value (car (the cons scan-left))) (scan-right right) (matched nil))
              (if key (setq value (funcall key value)))
              (setq right-index 0 word 0 mask 1)
              (tagbody next-right
                 (if (= right-index right-count) (go right-finished))
                 (if (consp scan-right) nil (error 'type-error :datum scan-right :expected-type 'cons))
                 (let ((other (car (the cons scan-right))))
                   (if key (setq other (funcall key other)))
                   (let ((match (funcall function value other)))
                     (if (if negate (not match) match)
                         (progn
                           (setq matched t)
                           (setf (svref marks word)
                                 (logior (the (integer 0 #x3fffffff) (svref marks word)) mask))))))
                 ;; Callbacks may mutate a spine. Read the next cursor after
                 ;; the call, retain checked loads, and cap work at the initial
                 ;; validated length rather than following a new cycle forever.
                 (setq scan-right (cdr (the cons scan-right))
                       right-index (the (integer 0 #x1ffffffe) (1+ right-index)))
                 (if (= mask #x20000000)
                     (setq mask 1 word (the (integer 0 #x1ffffffe) (1+ word)))
                     (setq mask (the (integer 1 #x20000000) (ash mask 1))))
                 (go next-right)
                right-finished)
              (if scan-right (error 'type-error :datum right
                                    :expected-type (cons 'satisfies (cons 'list-length nil))))
              (if matched nil (setq result (cons (car (the cons scan-left)) result))))
            (setq scan-left (cdr (the cons scan-left))
                  left-index (the (integer 0 #x1ffffffe) (1+ left-index)))
            (go next-left)
           left-finished
            (if scan-left (error 'type-error :datum left
                                  :expected-type (cons 'satisfies (cons 'list-length nil))))
            (setq right-index 0 word 0 mask 1)
           remaining
            (if (= right-index right-count) (go finish))
            (if (consp right) nil (error 'type-error :datum right :expected-type 'cons))
            (if (zerop (logand (the (integer 0 #x3fffffff) (svref marks word)) mask))
                (setq result (cons (car (the cons right)) result)))
            (setq right (cdr (the cons right))
                  right-index (the (integer 0 #x1ffffffe) (1+ right-index)))
            (if (= mask #x20000000)
                (setq mask 1 word (the (integer 0 #x1ffffffe) (1+ word)))
                (setq mask (the (integer 1 #x20000000) (ash mask 1))))
            (go remaining)
           finish)
         (if right (error 'type-error :datum right :expected-type 'null))
         result)))))
