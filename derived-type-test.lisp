;;;; Derived type lambda lists, lexical captures, and expansion contexts.
(in-package #:miricl-test)
(in-suite :miricl)

(test logical-class-subtype-proofs
  (m:with-interpreter (vm)
    (dolist (form
      '((subtypep '(or program-error type-error) 'condition)
        (subtypep 'program-error '(or program-error type-error))
        (subtypep 'simple-type-error '(and simple-error type-error))
        (subtypep '(and simple-error type-error) 'condition)
        (subtypep '(or (and simple-error type-error) program-error) 'error)
        (subtypep 'simple-type-error '(and condition (or warning type-error)))
        (subtypep '(or program-error symbol) 'condition)
        (subtypep 'condition '(and error type-error))))
      (let ((expected (multiple-value-list (eval form)))
            (actual (multiple-value-list (m:evaluate vm form))))
        ;; A host's permitted uncertainty must not reject a valid guest proof.
        (when (second expected) (is (equal expected actual) "Subtype mismatch: ~S" form))
        (is (or (not (first actual)) (second actual)))))
    (m:evaluate-string vm "(defclass logical-parent () ()) (defclass logical-child (logical-parent) ())")
    (is (equal '(t t) (m:evaluate-string vm "(multiple-value-list
      (subtypep '(or logical-parent logical-child) 'logical-parent))")))
    ;; Failure to prove containment in either arm does not prove that the
    ;; union fails to cover the source type.
    (is (eq t (m:evaluate-string vm "(multiple-value-bind (sub known)
      (subtypep 'logical-parent '(or logical-child (not logical-child)))
      (or sub (not known)))")))
    (is (eq t (m:evaluate-string vm "(multiple-value-bind (sub known)
      (subtypep '(or (cons (eql a) (eql x)) (cons (eql b) (eql y)))
                '(and (cons (member a b) (member x y)) (not (cons (eql a) (eql y)))))
      (or sub (not known)))")))
    (is (eq t (m:evaluate-string vm "(let ((type '(or (vector t 10) (vector t 5))))
      (if (subtypep type '(vector t))
          (equalp (map type #'identity '(1 2 3 4 5)) #(1 2 3 4 5)) t))")))))

(test native-complex-subtype-representations
  (m:with-interpreter (vm :fuel 100000000)
    ;; This matrix follows the three actual storage representations rather
    ;; than asking the host to predict implementation-dependent upgrading.
    (let ((types '((complex 7) ((complex) 7) ((complex *) 7)
                   ((complex real) 7) ((complex rational) 1)
                   ((complex integer) 1) ((complex ratio) 1) ((complex bit) 1)
                   ((complex (unsigned-byte 10000)) 1)
                   ((complex (signed-byte 10000)) 1)
                   ((complex (integer 0 1)) 1)
                   ((complex (integer (0) (0))) 0)
                   ((complex nil) 0) ((complex (member)) 0)
                   ((complex (eql 1/3)) 1) ((complex (member 1 2/3)) 1)
                   ((complex single-float) 2) ((complex short-float) 2)
                   ((complex double-float) 4) ((complex long-float) 4)
                   ((complex float) 6) ((complex (single-float 0.0 10.0)) 2)
                   ((complex (double-float (0d0) 10d0)) 4)
                   ((complex (single-float (0.0) (0.0))) 0)
                   ((complex (float 1d100 2d100)) 4))))
      (dolist (a types)
        (dolist (b types)
          (let* ((form `(multiple-value-list (subtypep ',(first a) ',(first b))))
                 (expected (list (= (logand (second a) (second b)) (second a)) t)))
            (is (equal expected (m:evaluate vm form)) "Complex subtype mismatch ~S" form))))
      (dolist (entry types)
        (dolist (target '(number atom t))
          (is (equal '(t t) (m:evaluate vm
            `(multiple-value-list (subtypep ',(first entry) ',target))))))
        (dolist (other '(integer (unsigned-byte 10000) (signed-byte 10000)
                         rational ratio real float single-float double-float
                         character symbol cons list sequence vector function))
          (is (equal (list (zerop (second entry)) t) (m:evaluate vm
            `(multiple-value-list (subtypep ',(first entry) ',other)))))
          (is (equal '(nil t) (m:evaluate vm
            `(multiple-value-list (subtypep ',other ',(first entry)))))))))
    (dolist (source
      '("(deftype complex-small-part () '(integer 0 10))
         (equal '(t t) (multiple-value-list
           (subtypep '(complex complex-small-part) '(complex rational))))"
        "(equal '(nil t) (multiple-value-list
           (subtypep '(complex single-float) '(complex (float 0.0 10.0)))))"
        "(equal '(t t) (multiple-value-list
           (subtypep '(complex (single-float 0.0 10.0)) '(complex (float 0.0 20.0)))))"
        "(equal '(t t) (multiple-value-list
           (subtypep '(complex single-float) '(complex float))))"
        "(equal '(t t) (multiple-value-list
           (subtypep '(integer (0) (0)) '(complex double-float))))"
        "(equal '(t t) (multiple-value-list
           (subtypep '(float (0d0) (0d0)) '(complex rational))))"
        "(equal '(t t) (multiple-value-list
           (subtypep '(complex single-float) (find-class 'number))))"
        "(equal '(t t) (multiple-value-list
           (subtypep (find-class 'complex) '(complex *))))"
        "(equal '(t t) (multiple-value-list (subtypep 'compiled-function 'atom)))"
        "(let ((calls 0))
           (flet ((forbidden-complex-p (x) (incf calls) (numberp x)))
             (let ((a (multiple-value-list (subtypep '(complex rational) '(satisfies forbidden-complex-p))))
                   (b (multiple-value-list (subtypep '(satisfies forbidden-complex-p) '(complex rational)))))
               (and (equal a '(nil nil)) (equal b '(nil nil)) (zerop calls)))))"
        "(deftype counted-complex-part () '(single-float 0.0 1.0))
         (macrolet ((check (&environment env)
                      (list 'quote (multiple-value-list
                        (subtypep '(complex counted-complex-part) '(complex single-float) env)))))
           (equal (check) '(t t)))"))
      (is (eq t (m:evaluate-string vm source)) "Complex subtype mismatch ~A" source))))

(defun reference-numeric-subtype-result (a b)
  ;; CCL rejects empty open intervals instead of recognizing the empty type.
  ;; Canonicalize those intervals for the host oracle, keeping the original
  ;; specifiers in every guest query.
  (flet ((canonical (type)
           (cond ((and (consp type) (eq (first type) 'rational)
                       (= (length type) 3) (integerp (second type))
                       (eql (second type) (third type)))
                  ;; ECL does not recognize singleton integer rationals.
                  `(integer ,(second type) ,(third type)))
                 ((and (consp type)
                       (member (first type) '(integer rational real float single-float double-float))
                       (= (length type) 3)
                       (or (consp (second type)) (consp (third type)))
                       (equal (if (consp (second type)) (car (second type)) (second type))
                              (if (consp (third type)) (car (third type)) (third type))))
                  nil)
                 (t type))))
    (multiple-value-list (subtypep (canonical a) (canonical b)))))

(test rational-interval-integer-and-ratio-targets
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (a '(rational real float single-float double-float
                (rational 0 10) (rational 1 1) (rational 1/3 1/3)
                (rational (0) (1)) (rational (0) 1) (rational 1/3 2/3)
                (rational (0) (0)) (real 1 1) (single-float 0.0 10.0)))
      (dolist (b '(integer ratio (integer 0 10) (integer 2 3) (signed-byte 8)
                  (unsigned-byte 8) (mod 3) complex atom))
        (let ((form `(multiple-value-list (subtypep ',a ',b))))
          (is (equalp (reference-numeric-subtype-result a b) (m:evaluate vm form)) "Rational target mismatch ~S" form))))
    (dolist (a '((integer -3 10) (signed-byte 8) (unsigned-byte 8) (integer (0) (0))))
      (dolist (b '(ratio float single-float double-float complex symbol cons list sequence atom
                  null boolean keyword character vector simple-vector string function))
        (let ((form `(multiple-value-list (subtypep ',a ',b))))
          (is (equalp (reference-numeric-subtype-result a b) (m:evaluate vm form)) "Integer target mismatch ~S" form))))
    (dolist (source
      '("(equal '(t t) (multiple-value-list (subtypep '(real 1/3 1/3) 'ratio)))"
        "(equal '(nil t) (multiple-value-list (subtypep '(real 1/3 1/3) 'integer)))"
        "(let ((n (1+ most-positive-fixnum)))
           (and (subtypep (list 'rational n n) 'bignum)
                (not (subtypep (list 'rational n n) 'fixnum))))"
        "(let ((n most-positive-fixnum))
           (and (subtypep (list 'rational n n) 'fixnum)
                (not (subtypep (list 'rational n n) 'bignum))))"
        "(equal '(nil t) (multiple-value-list
           (subtypep '(rational 1/3 1/3) '(unsigned-byte 100000000000000000000))))"))
      (is (eq t (m:evaluate-string vm source)) "Rational target mismatch ~A" source))))

(test native-subtype-class-state
  (m:with-interpreter (vm)
    (is (equal '(t nil t) (m:evaluate-string vm
      "(defclass subtype-parent-a () ()) (defclass subtype-parent-b () ())
       (defclass subtype-child (subtype-parent-a) ())
       (list (subtypep 'subtype-child 'subtype-parent-a)
             (subtypep 'subtype-child 'subtype-parent-b)
             (subtypep (find-class 'subtype-child) (find-class 'subtype-parent-a)))")))
    (is (equal '(nil t t) (m:evaluate-string vm
      "(defclass subtype-child (subtype-parent-b) ())
       (list (subtypep 'subtype-child 'subtype-parent-a)
             (subtypep 'subtype-child 'subtype-parent-b)
             (subtypep 'subtype-child 'atom))")))
    (is (eq t (m:evaluate-string vm
      "(setf (find-class 'subtype-alias) (find-class 'subtype-child))
       (subtypep 'subtype-alias 'subtype-parent-b)")))
    (is (equal '(nil t 1 t t 1) (m:evaluate-string vm
      "(defclass subtype-late-child (subtype-late-parent) ())
       (defclass subtype-late-parent () ())
       (defparameter *subtype-finalizations* 0)
       (defmethod mop:finalize-inheritance :around
           ((class (eql (find-class 'subtype-late-child))))
         (incf *subtype-finalizations*) (call-next-method))
       (list (mop:class-finalized-p (find-class 'subtype-late-child))
             (subtypep 'subtype-late-child 'subtype-late-parent)
             *subtype-finalizations*
             (mop:class-finalized-p (find-class 'subtype-late-child))
             (subtypep 'subtype-late-child 'subtype-late-parent)
             *subtype-finalizations*)")))
    (is (equal '(t 2) (m:evaluate-string vm
      "(defparameter *subtype-expansions* 0)
       (deftype subtype-expansion-probe () (incf *subtype-expansions*) 'integer)
       (list (subtypep 'subtype-expansion-probe 'subtype-expansion-probe) *subtype-expansions*)")))
    (is (eq t (m:evaluate-string vm
      "(let ((original (symbol-function 'subtypep)))
         (unwind-protect
           (progn (setf (symbol-function 'subtypep) (lambda (&rest args) :replacement))
             (and (eq :replacement (subtypep 'integer 'number))
                  (funcall original '(integer 1 3) '(integer 0 5))))
           (setf (symbol-function 'subtypep) original)))")))))

(test real-interval-subtypes
  (m:with-interpreter (vm :fuel 100000000)
    (let ((types '(real rational float single-float double-float (real) (rational) (float)
                   (real 0 10) (real (0) 10) (real 0 (10)) (real (0) (0))
                   (rational 1/3 1/3) (rational 0 10)
                   (rational (0) 10) (rational 0 (10)) (rational (0) (0))
                   (single-float 0.0 10.0) (single-float (0.0) 10.0)
                   (single-float 0.0 (10.0)) (single-float (0.0) (0.0))
                   (double-float 0.0d0 10.0d0) (double-float 0.0d0 (10.0d0))
                   (double-float (0.0d0) (0.0d0)) (float -0.0 0.0d0))))
      (dolist (a types)
        (dolist (b (append '(nil t number atom) types))
          (let ((form `(multiple-value-list (subtypep ',a ',b))))
            (is (equalp (reference-numeric-subtype-result a b) (m:evaluate vm form)) "Real interval mismatch ~S" form)))))
    (dolist (type '(single-float double-float))
      (let* ((zero (coerce 0 type)) (one (coerce 1 type))
             (least (if (eq type 'single-float) least-positive-single-float least-positive-double-float))
             (max (if (eq type 'single-float) most-positive-single-float most-positive-double-float))
             (next (coerce (+ 1 (expt 2 (if (eq type 'single-float) -23 -52))) type)))
        (dolist (a `((,type (,zero) ,least) (,type ,one (,next)) (,type ,(- zero) ,zero)))
          (dolist (b `(nil ,type (,type ,zero) (,type (,zero)) (,type ,one ,next)))
            (let ((form `(multiple-value-list (subtypep ',a ',b))))
              (is (equalp (eval form) (m:evaluate vm form)) "Float endpoint mismatch ~S" form))))
        ;; SBCL's interval reasoning does not establish these empty intervals.
        ;; Miricl has finite IEEE formats; no value lies between adjacent floats.
        (dolist (a `((,type (,zero) (,least)) (,type (,one) (,next))
                     (,type (,max)) (,type * (,(- max)))))
          (is (equal '(t t) (m:evaluate vm `(multiple-value-list (subtypep ',a nil))))))))
    (let ((huge (expt 2 2000)) (tiny (expt 2 -2000)))
      (dolist (a `((real ,huge) (real * ,(- huge)) (real ,tiny ,tiny)
                   (real ,(- tiny) ,tiny) (real ,huge ,huge)))
        ;; Host subtype implementations may overflow while normalizing huge
        ;; bounds. These intervals contain rationals and no nonzero floats.
        (dolist (b '(float single-float double-float))
          (is (equal '(nil t) (m:evaluate vm `(multiple-value-list (subtypep ',a ',b))))))
        (is (equal '(t t) (m:evaluate vm `(multiple-value-list (subtypep ',a 'real)))))
        (is (equal (if (equal a `(real ,(- tiny) ,tiny)) '(nil t) '(t t))
                   (m:evaluate vm `(multiple-value-list (subtypep ',a 'rational)))))))
    (is (equal '(t t) (m:evaluate-string vm
      "(multiple-value-list (subtypep '(real 1/3 1/3) 'rational))")))))

(defun reference-witness-subtype-result (a b witnesses)
  (list (notany (lambda (object) (and (typep object a) (not (typep object b)))) witnesses) t))

(test finite-and-cons-subtypes
  (m:with-interpreter (vm :fuel 100000000)
    ;; These representatives cover every membership partition of the types
    ;; in each matrix. Use host TYPEP: ECL's SUBTYPEP cannot prove many CONS
    ;; relationships, including their containment in LIST and SEQUENCE.
    (let ((witnesses (list nil t :key 'ordinary 0 1/2 1.0 #c(1 2)
                           #\a #\λ "" #() '(1) '(1 . 2) #'identity)))
      (dolist (a '(nil t null boolean keyword atom standard-char
                  symbol cons list sequence integer ratio number character string))
        (dolist (b '(nil t null boolean keyword atom standard-char
                    symbol cons list sequence integer ratio number character string))
          (let ((form `(multiple-value-list (subtypep ',a ',b))))
            (is (equalp (reference-witness-subtype-result a b witnesses) (m:evaluate vm form))
                "Atomic subtype mismatch ~S" form)))))
    (let* ((parts (list nil t :key 'ordinary -1 0 1 2 3 4 most-positive-fixnum
                        (1+ most-positive-fixnum) 1/2 1.0 #\a '(1)))
           (witnesses (loop for car in parts append
                        (loop for cdr in parts collect (cons car cdr)))))
      (dolist (a '(cons (cons) (cons *) (cons * *) (cons t t) (cons nil) (cons * nil)
                  (cons integer symbol) (cons fixnum null) (cons (integer 1 3) (cons * *))))
        (dolist (b '(nil t cons list sequence atom (cons) (cons * *) (cons number atom)
                    (cons symbol number) (cons integer symbol) (cons * nil)))
          (let ((form `(multiple-value-list (subtypep ',a ',b))))
            (is (equalp (reference-witness-subtype-result a b witnesses) (m:evaluate vm form))
                "CONS subtype mismatch ~S" form)))))
    (loop for form in
      '((let* ((one (copy-seq "abc")) (two (copy-seq "abc")))
          (list (multiple-value-list (subtypep (list 'eql one) (list 'eql two)))
                (multiple-value-list (subtypep (list 'member one) (list 'member two)))
                (multiple-value-list (subtypep (list 'cons (list 'eql one))
                                             (list 'cons (list 'eql two))))))
        (multiple-value-list (subtypep '(member 1 2 3) '(integer 0 3)))
        (multiple-value-list (subtypep '(member 1 2 4) '(integer 0 3)))
        (multiple-value-list (subtypep '(member) nil))
        (multiple-value-list (subtypep '(cons (integer (0) (0)) symbol) nil))
        (multiple-value-list (subtypep '(cons (eql 1) (member :x :y)) '(cons integer symbol)))
        (multiple-value-list (subtypep '(cons symbol integer) '(cons symbol symbol)))
        (multiple-value-list (subtypep '(cons * *) '(cons t t))))
      for expected in '(((nil t) (nil t) (nil t)) (t t) (nil t) (t t) (t t) (t t) (nil t) (t t)) do
      (is (equalp expected (m:evaluate vm form)) "Finite subtype mismatch ~S" form))
    (is (eq t (m:evaluate-string vm
      "(and (subtypep 'base-char 'character) (subtypep 'character 'base-char)
            (subtypep 'extended-char nil))")))
    (dolist (target '(logical-pathname broadcast-stream concatenated-stream echo-stream file-stream))
      (is (null (m:evaluate vm `(subtypep 'null ',target)))))
    (is (eq t (m:evaluate-string vm
      "(defparameter *subtype-predicate-calls* 0)
       (defun subtype-predicate-probe (x) (incf *subtype-predicate-calls*) (symbolp x))
       (subtypep '(member nil t) '(satisfies subtype-predicate-probe))
       (zerop *subtype-predicate-calls*)")))))

(test complex-part-types-and-coercion
  (m:with-interpreter (vm)
    ;; Upgrading describes Miricl's representations, which may differ from
    ;; the host's (CCL, for example, retains INTEGER and upgrades FLOAT to REAL).
    (dolist (entry '((nil nil) (integer rational) (rational rational) (ratio rational)
                     (fixnum rational) (bignum rational) (bit rational)
                     (signed-byte rational) (unsigned-byte rational)
                     (short-float single-float) (single-float single-float)
                     (double-float double-float) (long-float double-float)
                     (float float) (real real) ((integer 0 100) rational)
                     ((unsigned-byte 80) rational) ((signed-byte 80) rational)
                     ((eql 1) rational) ((eql 1.0) single-float) ((eql 1.0d0) double-float)))
      (destructuring-bind (type expected) entry
        (is (equal (list expected) (m:evaluate vm
              `(multiple-value-list (upgraded-complex-part-type ',type))))
            "Complex upgrade mismatch: ~S" type)))
    (dolist (object '(0 3 1/2 1.0 -1.0 1.0d0 #c(1 2) #c(1/2 1/3) #c(1.0 2.0) #c(1.0d0 2.0d0)))
      (dolist (type '(complex (complex) (complex *) (complex rational) (complex integer)
                     (complex single-float) (complex double-float) (complex real)))
        (let* ((part (if (consp type) (second type) 'real))
               (real (realpart object)) (imaginary (imagpart object))
               (value (case part
                        ((single-float double-float)
                         (complex (coerce real part) (coerce imaginary part)))
                        ((rational integer)
                         (if (and (rationalp real) (rationalp imaginary))
                             object :type-error))
                        (otherwise (complex real imaginary))))
               (expected (if (eq value :type-error) :type-error
                             (list value (typep (realpart value) 'single-float)
                                   (typep (realpart value) 'double-float)
                                   (rationalp (realpart value)) 1)))
               (form `(handler-case
                       (let ((result (coerce ',object ',type)))
                         (list result (typep (realpart result) 'single-float)
                               (typep (realpart result) 'double-float)
                               (rationalp (realpart result))
                               (length (multiple-value-list (coerce ',object ',type)))))
                       (type-error () :type-error))))
          (is (equalp expected (m:evaluate vm form)) "Complex coercion mismatch: ~S" form))))
    (dolist (source
      '("(typep #c(1/2 1/3) '(complex integer))"
        "(typep #c(1/2 1/3) '(complex (integer 0 10)))"
        "(not (typep #c(1.0 2.0) '(complex integer)))"
        ;; An unspecialized FLOAT part permits either supported float format.
        "(let ((z (coerce 1.0d0 '(complex float))))
           (and (typep z '(complex float)) (= (realpart z) 1) (zerop (imagpart z))))"
        "(let ((z #c(1.0 2.0))) (eq z (coerce z '(complex single-float))))"
        "(= (float-sign (imagpart (coerce -1.0 '(complex single-float)))) 1.0)"
        "(let ((z (complex -0.0d0 -0.0d0)))
           (and (= (float-sign (realpart z)) -1.0d0) (= (float-sign (imagpart z)) -1.0d0)))"
        "(deftype complex-part-alias () '(integer 0 10))
         (eq (upgraded-complex-part-type 'complex-part-alias) 'rational)"
        "(eq (upgraded-complex-part-type (find-class 'single-float)) 'single-float)"
        "(handler-case (upgraded-complex-part-type) (program-error () t))"
        "(handler-case (upgraded-complex-part-type 'real nil nil) (program-error () t))"))
      (is (eq t (m:evaluate-string vm source)) "Complex part mismatch: ~A" source))))

(test derived-type-definitions
  (m:with-interpreter (vm :fuel 100000000)
    (let ((name (gensym "TYPE")) (alias (gensym "ALIAS")))
      (dolist (form
        `((deftype ,name (&optional bound) (list 'integer 0 bound))
          (list (typep -1 ',name) (typep 0 ',name) (typep 100 ',name)
            (typep 3 '(,name 3)) (typep 4 '(,name 3)))
          (deftype ,name (&optional (bound 14)) (list 'integer 0 bound))
          (list (typep 14 ',name) (typep 15 ',name) (typep 4 '(,name 3)))
          (deftype ,name (&optional (x) &key y ((:z z) nil zp)) (list 'member x y z zp))
          (list (typep '* ',name) (typep nil ',name) (typep t ',name)
            (typep '* '(,name nil :y nil)) (typep t '(,name nil :z nil)))
          (deftype ,name ((a &optional b) &optional (c nil cp)) (list 'member a b c cp))
          (list (typep '* '(,name (1))) (typep nil '(,name (1))) (typep t '(,name (1) nil)))
          (deftype ,name (&whole form &rest args) (declare (ignore args)) (list 'eql (length form)))
          (list (typep 1 ',name) (typep 3 '(,name a b)))
          (deftype ,name () (return-from ,name 'integer))
          (typep 1 ',name)
          (deftype ,alias () ',name)
          (list (typep 1 ',alias) (typep "x" ',alias))
          (deftype ,name () 'string)
          (list (typep 1 ',alias) (typep "x" ',alias))
          (let ((bound 7)) (deftype ,name () (list 'integer 0 bound)))
          (list (typep 7 ',name) (typep 8 ',name))
          (deftype ,name () (declare (optimize (safety 3))) "old documentation"
            (declare (optimize (speed 1))) 'integer)
          (list (documentation ',name 'type) (setf (documentation ',name 'type) "new documentation")
            (documentation ',name 'type))
          (deftype ,name () 'character)
          ;; Host array upgrades can use different character type names.
          (list (subtypep (upgraded-array-element-type ',name) 'character)
                (subtypep (array-element-type (make-array 3 :element-type ',name)) 'character))
          (deftype ,name () 'double-float)
          (coerce 1/3 ',name)
          (deftype ,name (&optional (n 7)) (list 'integer 0 n))
          (list (multiple-value-list (subtypep '(,name 9) '(integer 0 9)))
            (multiple-value-list (subtypep '(integer 0 9) '(,name 9))))
          (multiple-value-list (typep (values 1 99) ',name nil))))
        (is (equalp (eval form) (m:evaluate vm form)) "Derived type mismatch: ~S" form))
      (dolist (form `((typep 1 '(,name 1 2)) (deftype 17 () 'integer)))
        (is (eq t (m:evaluate vm `(handler-case ,form (error () t)))))))))

(test derived-type-environments
  (m:with-interpreter (vm :fuel 100000000)
    (let ((name (gensym "ENV-TYPE")) (local (gensym "LOCAL")) (check (gensym "CHECK")))
      (dolist (form
        `((deftype ,name (&environment environment)
            (if (nth-value 1 (macroexpand '(,local) environment)) 'integer 'null))
          (defmacro ,check (&environment environment)
            (list 'quote (list (typep 1 ',name environment) (typep nil ',name nil)
              (typep nil ',name environment) (typep 1 ',name nil))))
          (list (typep nil ',name) (typep 1 ',name))))
        (is (equalp (eval form) (m:evaluate vm form)) "Type environment mismatch: ~S" form))
      ;; Implementations may ignore the environment for TYPEP (SBCL does).
      ;; Check this interpreter's supported lexical macro environment directly.
      (is (equal '(t t nil nil) (m:evaluate vm `(macrolet ((,local () t)) (,check)))))
      ;; The expansion context is restored after nonlocal exits as well.
      (m:evaluate vm `(deftype ,name (&environment environment) (declare (ignore environment)) (throw 'done :done)))
      (m:evaluate vm `(defmacro ,check (&environment environment)
        (list 'quote (catch 'done (typep 1 ',name environment)))))
      (is (eq :done (m:evaluate vm `(macrolet ((,local () t)) (,check)))))
      (is (eq t (m:evaluate vm `(progn
        (deftype ,name (&environment environment) (list 'eql (not (null environment))))
        (null (the ,name nil)))))))))

(test native-array-subtype-shapes
  (m:with-interpreter (vm :fuel 100000000)
    (let* ((types '(array (array) (array *) (array t) (array bit) (array character) (array nil)
                    (array * 0) (array * 1) (array * 2) (array t ()) (array t (0))
                    (array t (1)) (array t (2)) (array t (*)) (array t (1 *))
                    (array t (* 2)) (array t (0 *)) simple-array (simple-array * 1)
                    (simple-array t (*)) (simple-array character (2)) vector (vector t)
                    (vector nil) (vector bit) (vector character) (vector t 1)
                    simple-vector (simple-vector 2) string (string 1) simple-string (simple-string 2)
                    base-string (base-string 2) simple-base-string (simple-base-string 1)
                    bit-vector (bit-vector 2) simple-bit-vector (simple-bit-vector 1)
                    (and string string) (and simple-string string) (and string base-string)
                    (and string (array nil)) (and (array bit (* 2)) (simple-array bit (1 *)))))
           (shapes (append '(nil (0) (1) (2))
                     (loop for x below 3 append (loop for y below 3 collect (list x y)))))
           (forms (loop for shape in shapes append
                    (loop for element-type in '(t character bit nil) append
                      (loop for adjustable in '(nil t) collect
                        `(make-array ',shape :element-type ',element-type :adjustable ,adjustable)))))
           (memberships nil))
      (m:evaluate vm `(defparameter *array-subtype-witnesses* (list ,@forms)))
      ;; TYPEP provides an independent oracle over representatives of each
      ;; storage, shape, and simplicity partition exercised below. No host
      ;; array-upgrade choices enter this comparison.
      (dolist (type types)
        (let ((members (m:evaluate vm `(mapcar (lambda (a) (typep a ',type)) *array-subtype-witnesses*))))
          (is (some #'identity members) "Missing witnesses for ~S" type)
          (push (cons type members) memberships)))
      (dolist (a memberships)
        (dolist (b memberships)
          (let ((expected (every (lambda (left right) (or (not left) right)) (cdr a) (cdr b)))
                (form `(multiple-value-list (subtypep ',(car a) ',(car b)))))
            (is (equal (list expected t) (m:evaluate vm form)) "Array subtype mismatch: ~S" form)))))
    (dolist (entry '(((array t (17)) (array t (*)) t)
                     ((array t (*)) (array t (17)) nil)
                     ((array nil (*)) (array nil (0)) nil)
                     ((array nil (1)) nil nil)
                     ((array nil ()) nil nil)
                     ((array * 256) nil t)
                     ((array t (536870910 2)) nil t)
                     ((array t (536870910 2 0)) nil nil)
                     ((array t (536870910 2 *)) nil nil)
                     ((array t ()) integer nil)
                     ((array t (*)) sequence t)
                     ((array t (* *)) sequence nil)
                     ((and (array * (* 2)) (array * (1 *))) (array * (1 2)) t)
                     ((array * (1 2)) (and (array * (* 2)) (array * (1 *))) t)
                     ((and (array bit) (array character)) nil t)
                     ((and (array * 1) (array * 2)) nil t)
                     ((and (array t (1)) (array t (2))) nil t)
                     ((and (array t (536870910 *)) (array t (* 2))) nil t)
                     ((and (array t (536870910 2 *)) (array t (* * 0))) nil nil)
                     ((and t t) array nil)
                     (array (and t t) t)
                     ((and) atom nil)
                     ((and) (and) t)
                     ((array t (*)) (and (array * (*)) (simple-array * (*))) nil)))
      (destructuring-bind (a b expected) entry
        (is (equal (list expected t) (m:evaluate vm `(multiple-value-list (subtypep ',a ',b)))))))
    (is (eq t (m:evaluate vm '(let ((class (find-class 'array)))
      (and (subtypep class '(array)) (subtypep '(array *) class))))))
    (is (eq t (m:evaluate vm '(progn (setf (find-class 'array-alias) (find-class 'array))
      (and (subtypep 'array-alias '(array * *)) (subtypep '(array) 'array-alias))))))
    (is (eq t (m:evaluate vm '(let ((a (make-array '(536870910 2 0))))
      (and (zerop (array-total-size a)) (typep a '(array t (536870910 2 *))))))))))

(test array-element-upgrade-specializations
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (entry '((extended-char nil) ((member) nil) ((or) nil) ((not t) nil)
                     ((integer 1 0) nil) ((integer 0 (0)) nil)
                     ((integer (0) (2)) bit) ((integer 1 1) bit)
                     ((unsigned-byte 0) bit) ((unsigned-byte 1) bit) ((mod 1) bit) ((mod 2) bit)
                     ((member 0 1) bit) ((member 1 0 1) bit) ((eql 0) bit) ((eql 1) bit)
                     (null t) ((eql nil) t) ((member nil) t) ((member nil 0) t)
                     ((member #\a #\b) character) ((eql #\a) character)
                     ((member 0 #\a) t) ((integer -1 1) t) ((unsigned-byte 8) t)))
      (destructuring-bind (type expected) entry
        (is (eq expected (m:evaluate vm `(upgraded-array-element-type ',type))))
        (is (eq expected (m:evaluate vm `(array-element-type (make-array 0 :element-type ',type)))))))
    (dolist (size '(0 1))
      (is (equal '(t t nil nil)
        (m:evaluate vm `(let ((a (make-array ,size :element-type nil)))
          (list (stringp a) (typep a 'simple-string) (typep a 'base-string) (typep a 'simple-base-string)))))))
    (let ((name (gensym "ARRAY-ENV")) (local (gensym "LOCAL")) (check (gensym "CHECK")))
      (m:evaluate vm `(deftype ,name (&environment environment)
        (if (nth-value 1 (macroexpand '(,local) environment)) 'bit 'character)))
      (m:evaluate vm `(defmacro ,check (&environment environment)
        (list 'quote (list (upgraded-array-element-type ',name environment)
          (upgraded-array-element-type ',name nil)
          (multiple-value-list (subtypep '(array ,name) '(array bit) environment))))))
      (is (equal '(bit character (t t)) (m:evaluate vm `(macrolet ((,local () t)) (,check)))))
      (is (equal '(character) (m:evaluate vm `(multiple-value-list (upgraded-array-element-type ',name))))))
    (is (equal '(t 3) (m:evaluate vm '(progn
      (defparameter *array-element-expansions* 0)
      (deftype counted-array-bit () (incf *array-element-expansions*) 'bit)
      (list (subtypep '(and (array counted-array-bit (1 *)) (array counted-array-bit (* 2)))
                       '(array counted-array-bit (1 2))) *array-element-expansions*)))))
    (is (equal '(t 0) (m:evaluate vm '(progn
      (defparameter *array-predicate-calls* 0)
      (defun array-forbidden-predicate (object) (declare (ignore object))
        (incf *array-predicate-calls*) (error "Predicate must not run during array upgrading"))
      (list (subtypep '(array (satisfies array-forbidden-predicate)) '(array t)) *array-predicate-calls*)))))
    (dolist (form '((equalp (make-sequence '(vector (integer 0 255)) 3 :initial-element 17) #(17 17 17))
                    (equal (map '(vector bit) #'identity '(0 1 0 1)) #*0101)
                    (equal (concatenate '(vector character) '(#\a) '(#\b)) "ab")
                    (equalp (merge '(vector (integer 0 255)) '(1 3) '(2 4) #'<) #(1 2 3 4))
                    (equal (make-sequence '(vector (member #\a #\b)) 2 :initial-element #\a) "aa")))
      (is (eq t (m:evaluate vm form))))))

(test native-integer-width-types
  (m:with-interpreter (vm :fuel 100000000)
    (dolist (width '(1 2 14 15 16 28 29 30 31 44 45 46 59 60 61 127 128 129 1000))
      (let ((power (ash 1 width)))
        (dolist (number (list 0 1 -1 power (1- power) (1+ power) (- power) (- -1 power) (1- (- power))))
          (dolist (kind (if (zerop width) '(unsigned-byte) '(signed-byte unsigned-byte)))
            (let ((form `(typep ,number '(,kind ,width))))
              (is (eql (eval form) (m:evaluate vm form)) "Integer width mismatch: ~S" form))))))
    (dolist (number (list -536870913 -536870912 -536870911 -1 0 1 536870910 536870911 536870912
                         (ash 1 60) (- (ash 1 60))))
      (is (eql (<= -536870912 number 536870911) (m:evaluate vm `(typep ,number 'fixnum))))
      (is (eql (not (<= -536870912 number 536870911)) (m:evaluate vm `(typep ,number 'bignum)))))
    (is (eq t (m:evaluate vm '(let ((n (expt 2 100)))
      (and (typep -1 (list 'signed-byte n)) (typep 0 (list 'unsigned-byte n))
        (typep (expt 2 1000) (list 'signed-byte n)) (not (typep -1 (list 'unsigned-byte n))))))))))
