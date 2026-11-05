;;;; JavaScript values remain opaque until explicitly converted.
(in-package #:miricl-js)

(defconstant +null+ '%null)
(defconstant +undefined+ '%undefined)

(define-condition javascript-error (error)
  ((value :initarg :value :reader javascript-error-value))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "JavaScript operation failed." stream))))
(define-condition javascript-unavailable (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "This host does not provide JavaScript." stream))))
(defun %raise-error (value) (error 'javascript-error :value value))
(defun %unavailable () (error 'javascript-unavailable))

(defun available-p () (miricl::%%javascript 0 nil nil nil))
(defun valuep (object) (miricl::%%javascript 1 object nil nil))
(defun global (&optional name) (miricl::%%javascript 2 name nil nil))
(defun property (object name) (miricl::%%javascript 3 object name nil))
(defun (setf property) (value object name) (miricl::%%javascript 4 object name value))
(defun call (function &rest arguments) (miricl::%%javascript 5 function +undefined+ arguments))
(defun call-with-this (function receiver &rest arguments)
  (miricl::%%javascript 5 function receiver arguments))
(defun invoke (object name &rest arguments) (miricl::%%javascript 6 object name arguments))
(defun new (constructor &rest arguments) (miricl::%%javascript 7 constructor nil arguments))
(defun to-js (object) (miricl::%%javascript 8 object nil nil))
(defun from-js (object) (miricl::%%javascript 9 object nil nil))
(defun typeof (object) (miricl::%%javascript 10 object nil nil))
(defun strict-equal (a b) (miricl::%%javascript 11 a b nil))
(defun callback (function)
  (check-type function function)
  (miricl::%%javascript 12 function nil nil))
(defun array (&rest elements) (miricl::%%javascript 13 elements nil nil))

;; These literal descriptors survive COMPILE/COMPILE-FILE. The adapter caches
;; a resolved function, receiver, and conversion plan by descriptor identity.
(defun %import-type-p (type resultp)
  (or (member type '(:value :string :boolean :number :bigint :i32 :u32 :i64 :f32 :f64))
      (and resultp (eq type :void))))
(defmacro define-import (name binding parameter-types result-type)
  (unless (and (symbolp name) name (not (keywordp name))
               (listp binding) (= (length binding) 2) (every #'stringp binding)
               (listp parameter-types) (every (lambda (type) (%import-type-p type nil)) parameter-types)
               (%import-type-p result-type t))
    (error 'program-error))
  (let ((parameters (mapcar (lambda (type) (declare (ignore type)) (gensym)) parameter-types)))
    (list 'defun name parameters
      (list 'miricl::%%javascript 14
        (list 'quote (list (first binding) (second binding) parameter-types result-type))
        (cons 'list parameters) nil))))

;; Public symbols have distinct host identities while the packed bootstrap
;; initially creates private, uninterned guest symbols. Import those identities
;; only after the library is defined, just as other private bootstrap libraries.
(let ((package (make-package "MIRICL-JS" :nicknames '("JS") :use '("CL")))
      (symbols '(available-p value valuep global property call call-with-this invoke new
                 to-js from-js typeof strict-equal callback array +null+ +undefined+
                 javascript-error javascript-error-value javascript-unavailable define-import)))
  (shadowing-import symbols package)
  (export symbols package))
(miricl::%boot-class 'value (list (find-class t)) (find-class 'built-in-class))
