(in-package #:miricl-test)

(test package-definition-macro-expansion
  (m:with-interpreter (vm)
    (is (eq t (m:evaluate-string vm "(and (macro-function 'defpackage) (fboundp 'defpackage))")))
    (dolist (arguments '(nil (nil) (nil nil nil)))
      (is (eq t (m:evaluate vm `(handler-case (apply (macro-function 'defpackage) ',arguments)
                                 (program-error () t))))))
    (is (eq t (m:evaluate-string vm "(let ((form (macroexpand '(defpackage :expanded-package
           (:use) (:nicknames :expanded-first) (:nicknames :expanded-second)
           (:intern :private-symbol) (:export :public-symbol) (:documentation \"Package doc\")))))
      (and (not (find-package :expanded-package))
        (let ((p (eval form)))
          (and (eq p (find-package :expanded-first)) (eq p (find-package :expanded-second))
            (null (package-use-list p)) (string= (documentation p t) \"Package doc\")
            (eq (nth-value 1 (find-symbol \"PRIVATE-SYMBOL\" p)) :internal)
            (eq (nth-value 1 (find-symbol \"PUBLIC-SYMBOL\" p)) :external)))))")))
    (is (eq t (m:evaluate-string vm "(progn
      (setf (macro-function 'copied-package-definition) (macro-function 'defpackage))
      (let ((name :not-evaluated))
        (declare (ignore name))
        (copied-package-definition copied-package-name (:use))
        (let ((counter *gensym-counter*))
          (defpackage native-package-counter (:use))
          (and (find-package :copied-package-name) (= counter *gensym-counter*)))))")))))

(test correctable-package-imports
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defpackage missing-import-source (:use) (:export :present))")
    (dolist (option '(:import-from :shadowing-import-from))
      (m:evaluate-string vm "(when (find-package 'missing-import-target) (delete-package 'missing-import-target))")
      (is (eq t (m:evaluate vm `(let ((calls 0))
        (handler-bind ((package-error (lambda (condition)
            (unless (eq (package-error-package condition) (find-package 'missing-import-source))
              (error "Wrong source package"))
            (incf calls) (continue condition))))
          (eval '(defpackage missing-import-target (:use)
                   (,option missing-import-source :first-missing :second-missing :present)))
          (and (= calls 2)
               (null (find-symbol "FIRST-MISSING" 'missing-import-source))
               (null (find-symbol "SECOND-MISSING" 'missing-import-source))
               (eq (find-symbol "PRESENT" 'missing-import-source)
                   (find-symbol "PRESENT" 'missing-import-target)))))))))))

(test correctable-package-operations
  (m:with-interpreter (vm)
    (is (eq t (m:evaluate-string vm "(let ((calls 0) (p (make-package :restart-source :use nil)))
      (handler-bind ((package-error (lambda (c) (incf calls) (continue c))))
        (let ((duplicate (make-package :restart-source :use nil))
              (target (make-package :restart-target :use nil :nicknames '(:restart-source))))
          (and (eq duplicate p) (null (package-nicknames target)) (= calls 2)))))")))
    (is (eq t (m:evaluate-string vm "(let* ((source (find-package :restart-source))
          (target (find-package :restart-target)) (a (intern \"A\" source)) (b (intern \"A\" target))
          (calls 0))
      (handler-bind ((package-error (lambda (c)
          (assert (eq (package-error-package c) target)) (incf calls) (continue c))))
        (and (import a target) (eq b (find-symbol \"A\" target))
             (not (eq a b)) (= calls 1))))")))
    (is (eq t (m:evaluate-string vm "(let* ((source (find-package :restart-source))
          (target (find-package :restart-target)) (calls 0))
      (use-package source target)
      (handler-bind ((package-error (lambda (c)
          (assert (eq (package-error-package c) source)) (incf calls) (continue c))))
        (and (delete-package source) (= calls 1) (null (package-use-list target))
             (null (package-name source)) (null (delete-package source)))))")))
    (is (eq t (m:evaluate-string vm "(handler-bind ((package-error (lambda (c) (continue c))))
      (null (delete-package :restart-missing)))")))))

(test native-package-iteration
  (m:with-interpreter (vm)
    (dolist (types '((:internal) (:external) (:inherited) (:internal :external)
                    (:internal :inherited) (:external :inherited)
                    (:internal :external :inherited) (:internal :internal :external)))
      (let ((form
        `(let ((source (make-package "ITERATOR-SOURCE" :use nil))
               (target (make-package "ITERATOR-TARGET" :use nil)))
           (unwind-protect
             (progn
               (dolist (name '("X" "Y" "W")) (export (intern name source) source))
               (shadow "Y" target)
               (use-package source target)
               (export (intern "Z" target) target)
               (let ((result nil))
                 (with-package-iterator (next target ,@types)
                   (loop (multiple-value-bind (more symbol status package) (next)
                     (unless more (return))
                     (assert (eq package target))
                     (assert (eq status (nth-value 1 (find-symbol (symbol-name symbol) target))))
                     (push (list (symbol-name symbol) status) result))))
                 (sort result #'string< :key #'car)))
             (delete-package target) (delete-package source)))))
        (is (equalp (eval form) (m:evaluate vm form)) "Package iterator mismatch: ~S" types)))
    (dolist (form
      '((multiple-value-list (with-package-iterator (next nil :internal)))
        (multiple-value-list (with-package-iterator (next nil :internal) (values)))
        (multiple-value-list (with-package-iterator (next nil :internal) (values 1 2 3)))
        (with-package-iterator (next nil :internal) (multiple-value-list (next)))
        (let ((calls 0)) (with-package-iterator (next (progn (incf calls) nil) :external) calls))
        (block done (let ((x 17))
          (with-package-iterator (next (return-from done x) :internal) (declare (special x)))))))
      (is (equalp (multiple-value-list (eval form)) (multiple-value-list (m:evaluate vm form)))))
    (is (eq t (m:evaluate-string vm
      "(let ((source (make-package \"ITERATOR-IMPORT-SOURCE\" :use nil))
             (target (make-package \"ITERATOR-IMPORT-TARGET\" :use nil)))
         (unwind-protect
           (progn
             (export (intern \"X\" source) source)
             (export (intern \"W\" source) source)
             (use-package source target)
             (import (find-symbol \"X\" source) target)
             (let ((names nil))
               (with-package-iterator (next target :inherited)
                 (loop (multiple-value-bind (more symbol status package) (next)
                   (unless more (return))
                   (assert (and (eq status :inherited) (eq package target)))
                   (push (symbol-name symbol) names))))
               (equal names '(\"W\"))))
           (delete-package target) (delete-package source)))")))
    (is (eq t (m:evaluate-string vm
      "(let ((p (make-package \"ITERATOR-NIL\" :use nil)))
         (unwind-protect
           (progn (import (list nil) p)
             (with-package-iterator (next p :internal)
               (multiple-value-bind (more symbol status package) (next)
                 (and more (null symbol) (eq status :internal) (eq p package)
                      (equal (multiple-value-list (next)) '(nil))))))
           (delete-package p)))")))
    (is (eq t (m:evaluate-string vm
      "(let ((p (make-package \"ITERATOR-NESTED\" :use nil)))
         (unwind-protect
           (progn (intern \"X\" p)
             (with-package-iterator (outer p :internal)
               (with-package-iterator (inner p :internal)
                 (and (outer) (inner) (null (outer)) (null (inner))))))
           (delete-package p)))")))
    (dolist (form '((with-package-iterator) (with-package-iterator (next nil))
                    (with-package-iterator (next nil :invalid))
                    (with-package-iterator (next nil :internal) (next 1))))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))
    (is (eq t (m:evaluate-string vm
      "(handler-case (with-package-iterator (next \"missing-iterator-package\" :internal))
         (package-error () t))")))))
(in-suite :miricl)

(test package-name-index
  (m:with-interpreter (vm)
    ;; These names collide in the package index. Remove the middle, head,
    ;; and tail while preserving imported identities in another package.
    (m:evaluate-string vm "(defpackage :indexed (:use))
                          (setq indexed-symbols
                            (mapcar (lambda (name) (intern name :indexed))
                                    '(\"COLLISION-37\" \"COLLISION-158\" \"COLLISION-284\")))
                          (dotimes (i 40)
                            (make-package (format nil \"INDEX-~D\" i) :use nil))
                          (import indexed-symbols :index-39)")
    (dolist (position '(1 2 0))
      (let ((name (nth position '("COLLISION-37" "COLLISION-158" "COLLISION-284"))))
        (is (eq t (m:evaluate-string vm
                    (format nil "(eq (find-symbol ~S :indexed) (find-symbol ~S :index-39))" name name)))))
      (is (eq t (m:evaluate-string vm
                  (format nil "(unintern (nth ~D indexed-symbols) :indexed)" position))))
      (is (null (m:evaluate-string vm
                  (format nil "(symbol-package (nth ~D indexed-symbols))" position)))))
    (is (equal '(nil nil) (multiple-value-list
                          (m:evaluate-string vm "(find-symbol \"COLLISION-158\" :indexed)"))))
    (is (eq t (m:evaluate-string vm
                "(every (lambda (symbol)
                          (eq symbol (find-symbol (symbol-name symbol) :index-39)))
                        indexed-symbols)")))
    (is (eq t (m:evaluate-string vm "(delete-package :indexed)")))
    (m:evaluate-string vm "(make-package :indexed :use nil)")
    (is (null (m:evaluate-string vm "(eq (intern \"COLLISION-37\" :indexed)
                                       (first indexed-symbols))")))))

(test package-bootstrap
  (m:with-interpreter (vm)
    (dolist (source '("(eq (find-package :cl) (find-package :common-lisp))"
                      "(eq (find-package :cl-user) *package*)"
                      "(not (null (member (find-package :cl) (package-use-list *package*))))"
                      "(loop for p in (package-used-by-list :cl) thereis (eq p *package*))"))
      (is (eq t (m:evaluate-string vm source))))
    (is (equal '(nil nil) (multiple-value-list (m:evaluate-string vm "(find-symbol \"MISSING\")"))))
    (is (equal '(nil :external) (multiple-value-list (m:evaluate-string vm "(find-symbol \"NIL\" :cl)"))))
    (is (equal '(+ :inherited) (multiple-value-list (m:evaluate-string vm "(find-symbol \"+\")"))))
    (is (equal "COMMON-LISP" (m:evaluate-string vm "(package-name (symbol-package nil))")))
    (is (eq t (m:evaluate-string vm "(symbolp nil)")))
    (is (eq t (m:evaluate-string vm "(packagep *package*)")))
    (is (null (m:evaluate-string vm "(find-package :absent)")))))

(test bootstrap-symbol-spellings
  (m:with-interpreter (vm :fuel 100000000)
    ;; Exercise the actual package tables as well as numeric host symbol IDs.
    (let ((expected
            (loop for symbol in m::*runtime-symbols*
                  collect (list (symbol-name symbol)
                            (cond ((keywordp symbol) "KEYWORD")
                                  ((member symbol m::*mop-symbols*) "MIRICL-MOP")
                                  ((member symbol m::*extension-symbols*) "MIRICL")
                                  (t "COMMON-LISP"))))))
      (is (equal (mapcar #'first expected)
                 (m:evaluate vm `(mapcar #'symbol-name ',m::*runtime-symbols*))))
      (is (eq t (m:evaluate vm
        `(every (lambda (entry)
           (multiple-value-bind (symbol status) (find-symbol (first entry) (second entry))
             (and (eq status :external) (string= (symbol-name symbol) (first entry)))))
           ',expected)))))))

(test package-interning
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(make-package :symbols :nicknames '(:syms) :use nil)")
    (is (equal '("NEW" nil "SYMBOLS")
               (m:evaluate-string vm "(multiple-value-bind (s status) (intern \"NEW\" :syms)
                                       (list (symbol-name s) status (package-name (symbol-package s))))")))
    (is (eq :internal (nth-value 1 (m:evaluate-string vm "(intern \"NEW\" :symbols)"))))
    (is (eq t (m:evaluate-string vm "(eq (intern \"NEW\" :syms) (intern \"NEW\" :symbols))")))
    (is (null (nth-value 1 (m:evaluate-string vm "(intern \"new\" :symbols)"))))
    (is (null (m:evaluate-string vm "(eq (intern \"NEW\" :symbols) (intern \"new\" :symbols))")))
    (is (eq t (m:evaluate-string vm "(export (intern \"NEW\" :symbols) :symbols)")))
    (is (eq :external (nth-value 1 (m:evaluate-string vm "(find-symbol \"NEW\" :syms)"))))
    (is (eq t (m:evaluate-string vm "(unexport (intern \"NEW\" :symbols) :symbols)")))
    (is (eq :internal (nth-value 1 (m:evaluate-string vm "(find-symbol \"NEW\" :syms)"))))
    (is (null (nth-value 1 (m:evaluate-string vm "(intern \"FRESH-KEYWORD\" :keyword)"))))
    (is (eq :external (nth-value 1 (m:evaluate-string vm "(intern \"FRESH-KEYWORD\" :keyword)"))))
    (is (eq t (m:evaluate-string vm "(keywordp :fresh-keyword)")))
    (is (eq t (m:evaluate-string vm "(eq :fresh-keyword (intern \"FRESH-KEYWORD\" :keyword))")))
    (is (null (m:evaluate-string vm "(eq (make-symbol \"X\") (make-symbol \"X\"))")))
    (is (null (m:evaluate-string vm "(symbol-package (make-symbol \"X\"))")))))

(test package-visibility
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defpackage :source (:use) (:export :public) (:intern :private))
                          (defpackage :consumer (:use :cl :source))")
    (is (eq :inherited (nth-value 1 (m:evaluate-string vm "(find-symbol \"PUBLIC\" :consumer)"))))
    (is (equal '(nil nil) (multiple-value-list (m:evaluate-string vm "(find-symbol \"PRIVATE\" :consumer)"))))
    (is (eq t (m:evaluate-string vm "(eq (intern \"PUBLIC\" :consumer) (intern \"PUBLIC\" :source))")))
    ;; Re-exporting an inherited symbol makes it present without changing its home.
    (m:evaluate-string vm "(export (find-symbol \"PUBLIC\" :source) :consumer) (unuse-package :source :consumer)")
    (is (eq :external (nth-value 1 (m:evaluate-string vm "(find-symbol \"PUBLIC\" :consumer)"))))
    (is (equal "SOURCE" (m:evaluate-string vm "(package-name (symbol-package (find-symbol \"PUBLIC\" :consumer)))")))
    (is (eq t (m:evaluate-string vm "(unintern (find-symbol \"PUBLIC\" :consumer) :consumer)")))
    (is (null (m:evaluate-string vm "(find-symbol \"PUBLIC\" :consumer)")))
    (is (equal "SOURCE" (m:evaluate-string vm "(package-name (symbol-package (find-symbol \"PUBLIC\" :source)))")))
    (m:evaluate-string vm "(setq saved-symbol (find-symbol \"PUBLIC\" :source)) (unintern saved-symbol :source)")
    (is (null (m:evaluate-string vm "(symbol-package saved-symbol)")))
    (is (null (m:evaluate-string vm "(eq saved-symbol (intern \"PUBLIC\" :source))")))
    (m:evaluate-string vm "(import saved-symbol :consumer)")
    (is (equal "CONSUMER" (m:evaluate-string vm "(package-name (symbol-package saved-symbol))")))))

(test package-conflicts
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defpackage :left (:use) (:export :x))
                          (defpackage :right (:use) (:export :x))
                          (defpackage :both (:use :cl :left))")
    (signals m:interpreter-package-error (m:evaluate-string vm "(use-package :right :both)"))
    (is (eq :inherited (nth-value 1 (m:evaluate-string vm "(find-symbol \"X\" :both)"))))
    (m:evaluate-string vm "(shadow 'x :both) (use-package :right :both)")
    (is (eq :internal (nth-value 1 (m:evaluate-string vm "(find-symbol \"X\" :both)"))))
    (signals m:interpreter-package-error (m:evaluate-string vm "(unintern (find-symbol \"X\" :both) :both)"))
    (is (eq :internal (nth-value 1 (m:evaluate-string vm "(find-symbol \"X\" :both)"))))
    (signals m:interpreter-package-error (m:evaluate-string vm "(import (find-symbol \"X\" :left) :both)"))
    (m:evaluate-string vm "(shadowing-import (find-symbol \"X\" :left) :both)")
    (is (eq t (m:evaluate-string vm "(eq (find-symbol \"X\" :left) (find-symbol \"X\" :both))")))
    ;; A late export must check every user before changing visibility.
    (m:evaluate-string vm "(intern \"LATE\" :left) (intern \"LATE\" :both)")
    (signals m:interpreter-package-error (m:evaluate-string vm "(export (find-symbol \"LATE\" :left) :left)"))
    (is (eq :internal (nth-value 1 (m:evaluate-string vm "(find-symbol \"LATE\" :left)"))))
    (m:evaluate-string vm "(shadow \"LATE\" :both) (export (find-symbol \"LATE\" :left) :left)")
    (is (eq :external (nth-value 1 (m:evaluate-string vm "(find-symbol \"LATE\" :left)"))))))

(test package-source-and-defpackage
  (m:with-interpreter (vm)
    (is (= 42 (m:evaluate-string vm "(defpackage :math-demo (:export :answer) (:nicknames :md) (:use :cl))
                                   (in-package :math-demo)
                                   (defun answer (x) (declare (integer x)) (+ x 2))
                                   (in-package :cl-user) (md:answer 40)")))
    (signals m:interpreter-package-error (m:evaluate-string vm "'md:not-exported"))
    (is (equal "NOT-EXPORTED" (m:evaluate-string vm "(symbol-name 'md::not-exported)")))
    (m:evaluate-string vm "(defpackage :reexport (:use :math-demo) (:export :answer))")
    (is (= 42 (m:evaluate-string vm "(reexport:answer 40)")))
    (m:evaluate-string vm "(defpackage :importer (:export :answer) (:use :cl) (:import-from :md :answer))")
    (is (= 42 (m:evaluate-string vm "(importer:answer 40)")))
    (m:evaluate-string vm "(defpackage :shadow-first (:use :cl) (:export :+) (:shadow :+))")
    (is (null (m:evaluate-string vm "(eq 'shadow-first:+ 'cl:+)")))
    (is (null (find-package "MATH-DEMO")))
    (is (equal "A:B" (m:evaluate-string vm "(symbol-name '|A:B|)")))
    (is (equal "lower" (m:evaluate-string vm "(symbol-name '|lower|)")))
    (is (equal "aBC" (m:evaluate-string vm "(symbol-name '\\abc)")))
    (is (equal "123" (m:evaluate-string vm "(symbol-name '#:123)")))
    (is (null (m:evaluate-string vm "(eq '#:same '#:same)")))
    (is (= #x2a (m:evaluate-string vm "#x2a")))
    (is (= #c(1 2) (m:evaluate-string vm "#c(1 2)")))))

(test package-lifecycle
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(setq saved-package (make-package :old :use nil :nicknames '(:older)))
                          (setq saved-symbol (intern \"KEEP\" saved-package))
                          (rename-package saved-package :new '(:newer))")
    (is (null (m:evaluate-string vm "(find-package :old)")))
    (is (null (m:evaluate-string vm "(find-package :older)")))
    (is (eq t (m:evaluate-string vm "(eq saved-package (find-package :newer))")))
    (is (equal "NEW" (m:evaluate-string vm "(package-name (symbol-package saved-symbol))")))
    (signals m:interpreter-package-error (m:evaluate-string vm "(make-package :new)"))
    (signals m:interpreter-package-error (m:evaluate-string vm "(rename-package saved-package :cl)"))
    (is (eq t (m:evaluate-string vm "(delete-package saved-package)")))
    (is (null (m:evaluate-string vm "(package-name saved-package)")))
    (is (null (m:evaluate-string vm "(symbol-package saved-symbol)")))
    (is (null (m:evaluate-string vm "(find-package :newer)")))
    (is (null (m:evaluate-string vm "(delete-package saved-package)")))
    (is (eq t (m:evaluate-string vm "(packagep (make-package :new :use nil))")))))

(test guest-multiple-values
  (m:with-interpreter (vm)
    (is (equal '(1 2 3) (multiple-value-list (m:evaluate-string vm "(values 1 2 3)"))))
    (is (null (multiple-value-list (m:evaluate-string vm "(values)"))))
    (is (equal '(1 2) (m:evaluate-string vm "(multiple-value-list (values 1 2))")))
    (is (equal '(1 2 nil) (m:evaluate-string vm "(multiple-value-bind (a b c) (values 1 2) (list a b c))")))
    (is (= 2 (m:evaluate-string vm "(nth-value 1 (values 1 2 3))")))
    (is (null (m:evaluate-string vm "(nth-value 9 (values 1 2))")))
    (is (equal '(3) (multiple-value-list (m:evaluate-string vm "(+ (values 1 9) 2)"))))
    (is (equal '(1) (multiple-value-list (m:evaluate-string vm "(setq x (values 1 2))"))))
    (is (equal '(1 2) (multiple-value-list (m:evaluate-string vm "(defun two () (values 1 2)) (two)"))))))

(test package-dynamic-binding
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defpackage :dynamic (:use :cl))
                          (defun current-package-name () (package-name *package*))")
    (is (equal "DYNAMIC" (m:evaluate-string vm "(let ((*package* (find-package :dynamic))) (current-package-name))")))
    (is (equal "COMMON-LISP-USER" (m:evaluate-string vm "(package-name *package*)")))
    (is (equal "COMMON-LISP-USER" (m:evaluate-string vm "(let ((*package* (find-package :dynamic)) (p *package*)) (package-name p))")))
    (is (equal "DYNAMIC" (m:evaluate-string vm "(let* ((*package* (find-package :dynamic)) (p *package*)) (package-name p))")))
    (signals m:interpreter-error (m:evaluate-string vm "(let ((*package* (find-package :dynamic))) (/ 1 0))"))
    (is (equal "COMMON-LISP-USER" (m:evaluate-string vm "(package-name *package*)")))
    (is (equal "DYNAMIC" (m:evaluate-string vm "(let ((*package* (find-package :dynamic))) (package-name (symbol-package (intern \"INSIDE\"))))")))
    (is (eq :internal (nth-value 1 (m:evaluate-string vm "(find-symbol \"INSIDE\" :dynamic)"))))
    (m:evaluate-string vm "(setq *package* (find-package :dynamic))")
    (is (equal "DYNAMIC" (m:evaluate-string vm "(package-name *package*)")))))

(test package-iteration-control
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defpackage :iteration-control (:use) (:export :a :b))")
    (dolist (operator '(do-symbols do-external-symbols do-all-symbols))
      (let ((binding (if (eq operator 'do-all-symbols) '(s) '(s :iteration-control))))
        (dolist (form
          `((multiple-value-list (,operator (,@binding :bad) (return (values :good 17))))
            (multiple-value-list (,operator (,@binding (values :good 17))))))
          (is (equal '(:good 17) (m:evaluate vm form)) "Iteration values ~S" form))
        (is (equal '(:bad) (m:evaluate vm
          `(multiple-value-list (block nil (,operator (,@binding (return (values :good 17)))) :bad)))))
        (is (equal :good (m:evaluate vm
          `(block done
             (tagbody (,operator (,@binding (return-from done :good))
                        (go local-tag) (return-from done :bad) local-tag)
                      local-tag (return-from done :outer-tag))))))
        (is (equal :good (m:evaluate vm
          `(let ((iteration-free :good))
             (declare (special iteration-free))
             (let ((iteration-free :bad))
               (,operator (,@binding iteration-free) (declare (special iteration-free))))))))
        (is (equal :good (m:evaluate vm
          `(let ((s :good)) (declare (special s))
             (block done
               (,operator (,@binding :bad) (declare (special s))
                 (return-from done (if (symbolp s) :good :bad))))))))
        (is (eq t (m:evaluate vm
          `(let ((macro (macro-function ',operator)))
             (and macro
               (handler-case (funcall macro '(,operator (s)) nil nil) (program-error () t))
               (multiple-value-bind (form expanded) (macroexpand-1 '(,operator (,@binding 17)))
                 (and expanded (consp form) (= (eval form) 17))))))))))
    (is (equal '(1 2) (m:evaluate-string vm
      "(let ((calls 0))
         (do-symbols (s (progn (incf calls) :iteration-control) (list calls 2))
           (declare (ignore s))))")))
    (is (eq :good (m:evaluate-string vm
      "(let ((s :iteration-control)) (do-symbols (s s :good) (declare (ignore s))))")))
    (is (eq :good (m:evaluate-string vm
      "(let ((*package* (find-package :iteration-control)))
         (do-symbols (s) (return :good)))")))))

(test package-iteration
  (m:with-interpreter (vm)
    (m:evaluate-string vm "(defpackage :iterate-source (:use) (:export :x :y))
                          (defpackage :iterate (:use :iterate-source) (:shadow :x) (:export :public) (:intern :private))")
    (is (= 4 (m:evaluate-string vm "(let ((n 0)) (do-symbols (s :iterate n) (declare (symbol s)) (setq n (1+ n))))")))
    (is (= 1 (m:evaluate-string vm "(let ((n 0)) (do-external-symbols (s :iterate n) (setq n (1+ n))))")))
    (is (null (m:evaluate-string vm "(do-symbols (s :iterate s))")))
    (is (eq t (m:evaluate-string vm "(let ((n 0)) (do-all-symbols (s (> n 100)) (setq n (1+ n))))")))
    (is (eq t (m:evaluate-string vm "(typep *package* 'package)")))
    (is (eq t (m:evaluate-string vm "(typep :x 'symbol)")))
    (is (eq t (m:evaluate-string vm "(typep \"X\" 'string)")))
    (is (equal '(#\A #\Space) (m:evaluate-string vm "(list #\\A #\\Space)")))
    (is (equal "A" (m:evaluate-string vm "(package-name (make-package #\\A :use nil))")))))

(test package-symbol-enumeration-membership
  (m:with-interpreter (vm :fuel 1000000000)
    (m:evaluate-string vm "(defpackage :seen-a (:use) (:export :shared))
      (defpackage :seen-b (:use)) (defpackage :seen-c (:use))")
    (is (equal '(1 1 t) (m:evaluate-string vm
      "(let ((shared (find-symbol \"SHARED\" :seen-a)) (n 0) (nil-count 0))
         (import (list shared nil) :seen-b) (import (list shared nil) :seen-c)
         (unintern shared :seen-a)
         (do-all-symbols (s) (when (eq s shared) (incf n)) (when (null s) (incf nil-count)))
         (list n nil-count (null (symbol-package shared))))")))
    (m:evaluate-string vm "(defpackage :seen-origin (:use) (:export :inherited))
      (defpackage :seen-route (:use) (:import-from :seen-origin :inherited) (:export :inherited))
      (defpackage :seen-target (:use :seen-origin :seen-route))")
    (is (= 1 (m:evaluate-string vm
      "(let ((n 0)) (do-symbols (s :seen-target n) (incf n)))")))
    (m:evaluate-string vm "(shadow \"INHERITED\" :seen-target)")
    (is (equal '(1 t) (m:evaluate-string vm
      "(let ((n 0) (local nil))
         (do-symbols (s :seen-target) (incf n) (setq local (eq (symbol-package s) (find-package :seen-target))))
         (list n local))")))
    (m:evaluate-string vm "(defparameter *removed-enumeration-symbol* (intern \"GONE\" :seen-a))
      (delete-package :seen-a)
      (defparameter *uninterned-enumeration-symbol* (make-symbol \"UNINTERNED\"))")
    (is (equal '(nil nil) (m:evaluate-string vm
      "(let ((removed nil) (uninterned nil))
         (do-all-symbols (s)
           (when (eq s *removed-enumeration-symbol*) (setq removed t))
           (when (eq s *uninterned-enumeration-symbol*) (setq uninterned t)))
         (list removed uninterned))")))
    (m:evaluate-string vm "(defpackage :large-enumeration (:use))
      (dotimes (i 10000) (intern (format nil \"S~D\" i) :large-enumeration))")
    (is (= 10000 (m:evaluate-string vm
      "(let ((p (find-package :large-enumeration)) (n 0))
         (do-all-symbols (s n) (when (eq (symbol-package s) p) (incf n))))")))
    (is (= 10000 (m:evaluate-string vm
      "(let ((n 0)) (do-symbols (s :large-enumeration n) (incf n)))")))))

(test package-option-validation
  (m:with-interpreter (vm)
    (dolist (source '("(defpackage :bad (:size 1) (:size 2))"
                      "(defpackage :bad (:documentation \"one\") (:documentation \"two\"))"
                      "(defpackage :bad (:intern :x) (:export :x))"
                      "(defpackage :bad (:shadow :x) (:intern :x))"
                      "(defpackage :bad (:import-from :cl :x) (:shadow :x))"
                      "(defpackage :bad (:size -1))"
                      "(defpackage :bad (:documentation 42))"
                      "(defpackage :bad (:unsupported))"))
      (signals m:interpreter-error (m:evaluate-string vm source))
      (is (null (m:evaluate-string vm "(find-package :bad)"))))
    (m:evaluate-string vm "(defpackage :exports (:use) (:export :x)) (defpackage :inherits (:use :exports))")
    (is (eq t (m:evaluate-string vm "(unexport (find-symbol \"X\" :inherits) :inherits)")))
    (is (eq :inherited (nth-value 1 (m:evaluate-string vm "(find-symbol \"X\" :inherits)"))))
    (m:evaluate-string vm "(shadow \"X\" :exports)")
    (is (eq :external (nth-value 1 (m:evaluate-string vm "(find-symbol \"X\" :exports)"))))
    (signals m:interpreter-package-error (m:evaluate-string vm "(use-package :keyword :inherits)"))
    (signals m:interpreter-package-error (m:evaluate-string vm "(use-package :inherits :keyword)"))))

(test package-string-vectors
  (m:with-interpreter (vm)
    (dolist (form
      '((symbol-name (make-symbol (make-array 5 :element-type 'character :initial-contents "nAmEX" :fill-pointer 4)))
        (let* ((name (make-array 6 :element-type 'character :initial-contents "ABCXYZ" :fill-pointer 3))
               (p (make-package name :use nil)))
          (unwind-protect
            (let* ((text (make-array 5 :element-type 'character :initial-contents "nAmEX" :fill-pointer 4))
                   (symbol (intern text p)))
              (list (package-name p) (eq p (find-package name))
                    (symbol-name symbol) (eq symbol (find-symbol text p))
                    (nth-value 1 (intern text p))))
            (delete-package p)))
        (let ((s (make-array 8 :element-type 'character :initial-contents "ZZaλ🙂ZZZ")))
          (symbol-name (make-symbol (make-array 3 :element-type 'character :displaced-to s :displaced-index-offset 2))))))
      (is (equalp (eval form) (m:evaluate vm form)) "String-vector name mismatch for ~S" form))))

(test transient-symbol-lifetimes
  (m:with-interpreter (vm :fuel 100000000)
    (let ((symbol (m:evaluate vm '(make-symbol "host-exported"))))
      (m:evaluate vm '(dotimes (i 10000) (make-symbol "temporary")))
      (is (eq t (m:evaluate vm `(eq ',symbol ',symbol))))
      (is (equal "host-exported" (m:evaluate vm `(symbol-name ',symbol))))
      (is (null (m:evaluate vm `(symbol-package ',symbol))))
      (is (equal 17 (m:evaluate vm `(progn (set ',symbol 17) (symbol-value ',symbol)))))
      (is (equal 17 (m:evaluate vm symbol))))
    (is (equal 42 (m:evaluate vm '(let ((name (gensym)))
      (let ((fn (eval (list 'let (list (list name 42)) (list 'lambda nil name)))))
        (dotimes (i 10000) (gensym)) (funcall fn))))))
    (is (eq t (m:evaluate vm '(let ((name (make-symbol "unbound-transient")))
      (handler-case (eval name) (unbound-variable (c) (eq name (cell-error-name c))))))))))

(test sparse-symbol-binding-indexes
  (m:with-interpreter (vm :fuel 100000000)
    (let ((anchor (m:evaluate vm '(make-symbol "early-export"))))
      (m:evaluate vm '(dotimes (i 5000) (gensym)))
      (let ((late (m:evaluate vm '(make-symbol "late-export"))))
        (is (equal "early-export" (m:evaluate vm `(symbol-name ',anchor))))
        (is (equal "late-export" (m:evaluate vm `(symbol-name ',late))))
        (is (= 17 (m:evaluate vm `(progn (set ',late 17) (symbol-value ',late)))))
        (is (eq t (m:evaluate vm
      '(let ((name (gensym)))
         (eval (list 'define-compiler-macro name nil 31))
         (let ((function (compiler-macro-function name)))
           (and (functionp function) (= (funcall function (list name) nil) 31)
             (progn (setf (compiler-macro-function name) nil)
               (null (compiler-macro-function name)))))))))
    (is (eq t (m:evaluate vm
      '(let ((name (gensym)))
         (eval (list 'define-symbol-macro name 31))
         (and (= (eval name) 31) (not (boundp name))
           (progn (eval (list 'define-symbol-macro name 41)) (= (eval name) 41)))))))
    (is (eq t (m:evaluate vm `(progn (makunbound ',late) (not (boundp ',late))))))
        (is (= 23 (m:evaluate vm `(progn (set ',late 23) (symbol-value ',late)))))))
    (is (eq t (m:evaluate vm
      '(let ((name (gensym)))
         (eval (list 'defun name '(x) '(1+ x)))
         (eval (list 'defun (list 'setf name) '(value x) '(+ value x)))
         (and (= (funcall name 4) 5)
              (= (funcall (fdefinition (list 'setf name)) 3 4) 7)
              (progn (fmakunbound name) (not (fboundp name)))
              (fboundp (list 'setf name)))))))
    (is (eq t (m:evaluate vm
      '(let ((name (gensym)))
         (eval (list 'defmacro name nil 31))
         (and (= (eval (list name)) 31)
           (progn (setf (macro-function name) nil) (null (macro-function name))))))))
    (is (eq t (m:evaluate vm
      '(let ((name (gensym)))
         (eval (list 'deftype name nil '(quote integer)))
         (and (typep 1 name) (not (typep :x name))
           (progn (eval (list 'deftype name nil '(quote symbol)))
             (and (typep :x name) (not (typep 1 name)))))))))
    (is (eq t (m:evaluate vm
      '(let ((name (gensym)))
         (eval (list 'defclass name nil nil))
         (and (typep (make-instance name) name)
           (progn (setf (find-class name) nil) (not (find-class name nil))))))))
    (is (eq t (m:evaluate vm
      '(let ((name (gensym)))
         (proclaim (list 'special name))
         (= 17 (eval (list 'let (list (list name 17))
           (list 'funcall (list 'function (list 'lambda nil name))))))))))))

(defun reference-apropos-list (pattern &optional package)
  ;; Enumerate accessible symbols independently: ECL's APROPOS-LIST can
  ;; include a shadowed inherited symbol instead of the accessible one.
  (let ((pattern (string pattern)) (symbols nil))
    (flet ((collect (symbol)
             (when (search pattern (symbol-name symbol) :test #'char-equal)
               (pushnew symbol symbols))))
      (if package (do-symbols (symbol package) (collect symbol))
          (do-all-symbols (symbol) (collect symbol))))
    symbols))

(test apropos-package-search
  (m:with-interpreter (vm)
    (dolist (pattern '("FOO" "foo" "oo" "" #\o #:foo))
      (let ((form `(let ((p (make-package "MIRICL-APROPOS-TEST" :use nil)))
                     (unwind-protect
                       (progn (intern "FOO" p) (intern "BAR" p) (intern "BAZ" p)
                         (sort (mapcar #'symbol-name (apropos-list ',pattern p)) #'string<))
                       (delete-package p)))))
        (is (equal (eval form) (m:evaluate vm form)))))
    (dolist (form
      '((let ((p (make-package "MIRICL-APROPOS-TEST" :use nil)))
          (unwind-protect
            (progn (intern "ΛΑΜΔΑ" p)
              (list (length (apropos-list "λαμ" p))
                    (length (apropos-list (make-array 8 :element-type 'character :initial-contents "FOOextra" :fill-pointer 3) p))
                    (length (apropos-list (make-array 2 :element-type 'character :displaced-to "xλαμy" :displaced-index-offset 1) p))))
            (delete-package p)))
        (let* ((base (make-package "MIRICL-APROPOS-BASE" :use nil))
               (child (make-package "MIRICL-APROPOS-CHILD" :use (list base))))
          (unwind-protect
            (let ((inherited (intern "MATCH-INHERITED" base))
                  (hidden (intern "MATCH-HIDDEN" base))
                  (private (intern "MATCH-PRIVATE" base))
                  (imported (intern "MATCH-IMPORTED" base)))
              (export (list inherited hidden) base) (shadow "MATCH-HIDDEN" child)
              (import imported child) (intern "MATCH-LOCAL" child)
              (let ((symbols (apropos-list "match" child)))
                (list (sort (mapcar #'symbol-name symbols) #'string<)
                  (not (null (member inherited symbols))) (not (null (member imported symbols)))
                  (null (member hidden symbols)) (null (member private symbols)))))
            (delete-package child) (delete-package base)))
        (let ((p (make-package "MIRICL-APROPOS-TEST" :use nil)))
          (unwind-protect
            (progn (intern "FOO" p)
              (list (length (apropos-list "foo" (package-name p)))
                    (length (apropos-list "foo" (make-symbol (package-name p))))))
            (delete-package p)))
        (let ((p (make-package "MIRICL-APROPOS-TEST" :use nil)) (returned 1))
          (unwind-protect
            (progn (intern "FOO" p)
              (let ((text (with-output-to-string (*standard-output*)
                            (setq returned (multiple-value-list (apropos "foo" p))))))
                (list (not (null (search "FOO" text :test #'char-equal))) returned)))
            (delete-package p)))
        (let ((p (make-package "MIRICL-APROPOS-TEST" :use nil)))
          (unwind-protect
            (list (with-output-to-string (*standard-output*) (apropos "no-match" p))
                  (length (multiple-value-list (apropos-list "" p))))
            (delete-package p)))
        (let ((symbol (intern "MIRICL-APROPOS-GLOBAL-MATCH" :keyword)))
          (not (null (member symbol (apropos-list "miricl-apropos-global-match" nil)))))))
      (is (equalp (eval (subst 'reference-apropos-list 'apropos-list form)) (m:evaluate vm form))
          "APROPOS mismatch for ~S" form))
    (dolist (form '((apropos) (apropos-list) (apropos "x" nil nil) (apropos-list "x" nil nil)))
      (is (eq t (m:evaluate vm `(handler-case ,form (program-error () t))))))
    (dolist (form '((apropos 17) (apropos-list 17)))
      (is (eq t (m:evaluate vm `(handler-case ,form (type-error () t))))))))

(test find-all-symbols-string-views-and-deletion
  (m:with-interpreter (vm)
    (dolist (name '("NIL" nil :nil
                    (make-array 5 :element-type 'character :initial-contents "NILxy" :fill-pointer 3)
                    (make-array 3 :element-type 'base-char :initial-contents "NIL" :adjustable t)
                    (make-array 3 :element-type 'character :displaced-to "xNILy" :displaced-index-offset 1)))
      (is (eq t (m:evaluate vm `(let ((symbols (find-all-symbols ,(if (consp name) name `',name))))
        (and (every (lambda (s) (string= (symbol-name s) "NIL")) symbols) (not (null (member nil symbols)))))))))
    (is (eq t (m:evaluate vm
      '(let ((p (make-package "MIRICL-FIND-ALL-DELETED" :use nil)))
         (intern "PRIVATE" p) (delete-package p)
         (and (not (null (member 'car (find-all-symbols "CAR"))))
              (not (null (member nil (find-all-symbols "NIL")))))))))
    (is (equal '(1 1 nil) (m:evaluate vm
      '(let* ((a (make-package "MIRICL-FIND-ALL-A" :use nil))
              (b (make-package "MIRICL-FIND-ALL-B" :use nil))
              (symbol (intern "MiRiCl-Find-All" a)))
         (unwind-protect
           (progn (import symbol b)
             (list (count symbol (find-all-symbols "MiRiCl-Find-All"))
                   (count symbol (find-all-symbols symbol))
                   (not (null (member symbol (find-all-symbols "MIRICL-FIND-ALL"))))))
           (delete-package b) (delete-package a))))))
    (is (= 1 (length (multiple-value-list (m:evaluate vm '(find-all-symbols "NIL"))))))))

(test bootstrap-symbol-identities
  (is (= (length m::*runtime-symbols*) (length (remove-duplicates m::*runtime-symbols*))))
  (m:with-interpreter (vm)
    (dolist (keyword (remove-duplicates (remove-if-not #'keywordp m::*runtime-symbols*)))
      (is (eq t (m:evaluate vm `(eq ',keyword (intern ,(symbol-name keyword) :keyword))))
          "Host/reader keyword mismatch for ~S" keyword))))

(test standard-symbol-namespace
  (m:with-interpreter (vm :fuel 100000000)
    ;; Names without an implementation still belong to CL. Do not install
    ;; placeholder function bindings merely to complete its namespace.
    (is (eq t (m:evaluate vm
      `(every (lambda (name)
                (multiple-value-bind (symbol status) (find-symbol name :cl)
                  (and (eq status :external)
                       (eq (symbol-package symbol) (find-package :cl)))))
              ',(mapcar #'symbol-name m::*standard-common-lisp-symbols*)))))
    (is (eq t (m:evaluate-string vm
      "(and (constantp 'lambda-list-keywords)
            (every (lambda (keyword) (member keyword lambda-list-keywords))
                   '(&optional &rest &key &allow-other-keys &aux &body &whole &environment)))")))))

(test packed-bootstrap-metadata
  (let* ((private (make-symbol "PRIVATE"))
         (primitive (make-symbol "PRIMITIVE"))
         (symbols (list private :test primitive)))
    (is (equalp #(0 0 7 80 82 73 86 65 84 69
                  1 0 4 84 69 83 84
                  2 0 9 80 82 73 77 73 84 73 86 69)
                (m::bootstrap-private-symbol-bytes symbols (list primitive)))))
  (let* ((name (make-string 255 :initial-element #\A))
         (bytes (m::bootstrap-private-symbol-bytes
                  (list (make-symbol name) (make-symbol name)
                        (make-symbol (concatenate 'string (subseq name 0 128) "B"))))))
    (is (= 265 (length bytes)))
    (is (equalp #(0 255 0 0 128 1 66) (subseq bytes 258))))
  ;; Public records retain ID order and distinguish all namespaces.
  (let ((bytes (m::bootstrap-public-symbol-bytes)) (position 0) (previous ""))
    (dolist (symbol m::*runtime-symbols*)
      (is (= (cond ((keywordp symbol) 1)
                   ((member symbol m::*mop-symbols*) 2)
                   ((member symbol m::*extension-symbols*) 3) (t 0))
             (logand 3 (aref bytes position))))
      (is (eq (not (null (or (member symbol m::*runtime-special-operators*)
                             (member symbol m::*runtime-macros*))))
              (logbitp 2 (aref bytes position))))
      (incf position)
      (let* ((prefix (aref bytes position))
             (length (aref bytes (1+ position))))
        (incf position 2)
        (setf previous (concatenate 'string (subseq previous 0 prefix)
                        (map 'string #'code-char (subseq bytes position (+ position length)))))
        (is (string= (symbol-name symbol) previous))
        (incf position length)))
    (is (= position (length bytes)))))

(test private-library-case-data
  (dolist (operator '(case ecase ccase typecase etypecase ctypecase))
    (let ((key (if (member operator '(case ecase ccase))
                   '(null not) '(or null (cons null t)))))
      (is (equal
        (m::lower-runtime-library
          `(,operator (null input)
             (,key (null value))
             (null (null other) :result)
             (t (when flag (null final)))))
        `(,operator (not input)
           (,key (not value))
           (null (not other) :result)
           (t (if flag (progn (not final))))))))))

(test private-library-form-prefixes
  (dolist (entry (remove-if-not #'fourth m::*runtime-library-prefixes*))
    (destructuring-bind (tag symbol length namespace) entry
      (let* ((table (ecase namespace (:clos m::*clos-library-symbols*) (:loop m::*loop-library-symbols*)))
             (wire-tag (ecase namespace (:clos 24) (:loop 25)))
             (form (cons symbol (make-list (1- length))))
             (bytes (m::runtime-library-bytes (list form) table wire-tag))
             (value (m::runtime-library-prefix-value entry)))
        (is (= tag (aref bytes 0)))
        (is (= (position symbol table) (ldb (byte 27 3) value)))
        (is (= length (ldb (byte 3 0) value)))
        ;; A caller-supplied symbol table uses explicit references, preserving
        ;; its own index space instead of silently using the embedded table.
        (is (/= tag (aref (m::runtime-library-bytes (list form) (copy-list table) wire-tag) 0)))))))

(test bootstrap-prefix-boundaries
  (let* ((name (make-string 255 :initial-element #\A))
         (symbols (list (make-symbol name) (make-symbol name)
                        (make-symbol (concatenate 'string (subseq name 0 128) "B"))))
         (m::*clos-library*
           (append m::*clos-library*
             (list (m::lower-runtime-library `(defparameter *compile-file-pathname* ',symbols)))))
         (m::*clos-library-symbols*
           (m::clos-library-symbols m::*clos-library*))
         (m::*interpreter-wasm* nil))
    (m:with-interpreter (vm)
      (is (equal '(255 255 129) (m:evaluate-string vm
        "(mapcar (lambda (s) (length (symbol-name s))) *compile-file-pathname*)")))
      (is (eq t (m:evaluate-string vm
        "(let ((symbols *compile-file-pathname*))
           (and (not (eq (first symbols) (second symbols)))
                (string= (symbol-name (first symbols)) (symbol-name (second symbols)))
                (every (lambda (s) (null (symbol-package s))) symbols)
                (char= (char (symbol-name (third symbols)) 128) #\\B)))"))))))
