;;;; WasmGC accessors for persistent images, generated from the runtime layout.
(in-package #:miricl)
(named-readtables:in-readtable :miricl-wat)

(defvar *core-types*)
(defvar *core-roots*)

;; Evaluation stacks, host handles, and scheduler state belong to the new
;; process. These roots are the mutable, persistent state of the Lisp system.
(defparameter *core-root-names*
  '($object-serial $character-pages $nil-package $nil-name $next-symbol $symbols $symbol-pages
    $package-indexes $mop-package $implementation-package $packages $package-local-names
    $cl-package $keyword-package $package-bound $current-package $next-package
    $value-bindings $function-bindings $setf-bindings $nil-plist $gentemp-counter $setf-functions $specials
    $derived-types $derived-type-first $condition-classes $condition-class-index $class-bindings $clos-library-symbols
    $macro-bindings $compiler-macros $setf-expanders $symbol-macros $loop-library-symbols
    $loop-expansions $loop-expansion-slot $source-files $source-directories $source-deleted
    $standard-dispatch-functions $character-alpha-ranges $character-names $character-name-index
    $two-over-pi $random-seeder))

(defun core-mut-p (type)
  (and (consp type) (string-equal (car type) 'mut)))

(defun core-unmut (type)
  (if (core-mut-p type) (second type) type))

(defun core-wat-declaration (form)
  (destructuring-bind (kind &rest args) form
    (cond ((string-equal kind 'rec) (mapc #'core-wat-declaration args))
          ((string-equal kind 'type)
           (destructuring-bind (name definition) args
             (let ((layout (if (string-equal (car definition) 'sub)
                               (car (last definition)) definition)))
               (when (member (car layout) '(struct array) :test #'string-equal)
                 (push (cons name layout) *core-types*)))))
          ((and (string-equal kind 'global) (member (first args) *core-root-names* :test #'string-equal))
           (destructuring-bind (name type initial) args
             (push (list name (core-unmut type)) *core-roots*)
             (return-from core-wat-declaration `(,kind ,name (mut ,(core-unmut type)) ,initial))))))
  form)

(defun core-field-code (type)
  (cond ((consp type) (if (member (car (last type)) '($native-entry $native-direct-entry) :test #'string-equal) #\e #\r))
        ((string-equal type 'i32) #\i)
        ((or (string-equal type 'i64) (string-equal type 'f64)) #\l)
        (t (error "Unsupported core image field type ~S" type))))

(defun core-field-signature (types)
  (map 'string #'core-field-code types))

(defun core-external-type (type)
  (ecase (core-field-code type)
    (#\e 'funcref) (#\r 'externref) (#\i 'i32) (#\l 'i64)))

(defun core-field-read (type expression)
  (case (core-field-code type)
    (#\r `(extern.convert_any ,expression))
    (otherwise (if (and (symbolp type) (string-equal type 'f64))
                   `(i64.reinterpret_f64 ,expression) expression))))

(defun core-field-write (type expression)
  (case (core-field-code type)
    (#\e `(ref.cast ,type ,expression))
    (#\r `(ref.cast ,type (any.convert_extern ,expression)))
    (otherwise (if (and (symbolp type) (string-equal type 'f64))
                   `(f64.reinterpret_i64 ,expression) expression))))

(defun core-field-default (type)
  (cond ((atom type) `(,(intern (format nil "~A.CONST" (string-upcase type)) :miricl) 0))
        ((string-equal (second type) 'null) `(ref.null ,(third type)))
        (t
         (let ((name (second type)))
           (assert (string-equal (second (assoc name *core-types* :test #'string-equal)) 'array))
           `(array.new_default ,name (i32.const 0))))))

(defun core-field-variable (index)
  (make-symbol (format nil "$value~D" index)))

(defun core-root-functions ()
  (assert (= (length *core-root-names*) (length *core-roots*)))
  (let* ((roots (reverse *core-roots*)) (types (mapcar #'second roots)))
    `((func (export ,(concatenate 'string "core-roots:" (core-field-signature types)))
        (result ,@(mapcar #'core-external-type types))
        ,@(loop for (name type) in roots collect (core-field-read type `(global.get ,name))))
      (func (export "core-set-roots")
        ,@(loop for type in types for index from 0
                collect `(param ,(core-field-variable index) ,(core-external-type type)))
        ,@(loop for (name type) in roots for index from 0
                collect `(global.set ,name ,(core-field-write type `(local.get ,(core-field-variable index)))))))))

(defun core-array-functions (name id type)
  (let ((value `(ref.cast (ref ,name) (any.convert_extern (local.get $array)))))
    `((func (export ,(format nil "core-array:~D:~C" id (core-field-code type)))
        (param $array externref) (result i32) (array.len ,value))
      (func (export ,(format nil "core-read:~D" id))
        (param $array externref) (param $index i32) (result ,(core-external-type type))
        ,(core-field-read type `(array.get ,name ,value (local.get $index))))
      (func (export ,(format nil "core-new:~D" id))
        (param $length i32) (result externref)
        (extern.convert_any (array.new_default ,name (local.get $length))))
      (func (export ,(format nil "core-set:~D" id))
        (param $array externref) (param $index i32) (param $element ,(core-external-type type))
        (array.set ,name ,value (local.get $index) ,(core-field-write type '(local.get $element)))))))

(defun core-struct-functions (name id fields)
  (let* ((types (mapcar (lambda (field) (car (last field))) fields))
         (value `(ref.cast (ref ,name) (any.convert_extern (local.get $object))))
         (signature (map 'string (lambda (type)
                      (let ((code (core-field-code (core-unmut type))))
                        (if (core-mut-p type) (char-upcase code) code))) types)))
    `((func (export ,(format nil "core-read:~D:~A" id signature))
        (param $object externref) (result ,@(mapcar (lambda (type) (core-external-type (core-unmut type))) types))
        ,@(loop for field in types for index from 0
                collect (core-field-read (core-unmut field) `(struct.get ,name ,index ,value))))
      (func (export ,(format nil "core-new:~D" id))
        ,@(loop for field in types for index from 0 unless (core-mut-p field)
                collect `(param ,(core-field-variable index) ,(core-external-type field)))
        (result externref)
        (extern.convert_any (struct.new ,name
          ,@(loop for field in types for index from 0 collect
              (if (core-mut-p field) (core-field-default (core-unmut field))
                  (core-field-write field `(local.get ,(core-field-variable index))))))))
      (func (export ,(format nil "core-set:~D" id)) (param $object externref)
        ,@(loop for field in types for index from 0 when (core-mut-p field)
                collect `(param ,(core-field-variable index) ,(core-external-type (core-unmut field))))
        ,@(loop for field in types for index from 0 when (core-mut-p field)
                collect `(struct.set ,name ,index ,value
                            ,(core-field-write (core-unmut field) `(local.get ,(core-field-variable index)))))))))

(defun core-image-runtime-wat ()
  (let ((types (remove-if (lambda (type)
                 (member (car type) '($bootstrap-bytes $hir-decoded-instruction) :test #'string-equal)) *core-types*))
        (excluded '($green-thread $thread-context $thread-scratch-block $javascript-value)))
    (runtime-wat-forms
      (append
        (core-root-functions)
        `((export "core-restart" (func $evaluate-source))
          (func (export "core-object-type") (param $object externref) (result i32)
            (local $value (ref null any))
            (local.set $value (any.convert_extern (local.get $object)))
            ,@(loop for (name) in types for id from 0 collect
                `(if (ref.test (ref ,name) (local.get $value))
                   (then (return (i32.const ,(if (member name excluded :test #'string-equal) -1 id))))))
            (i32.const -1))
          (func (export "core-checkpoint-safe") (result i32)
            (ref.is_null (call $cdr (global.get $threads))))
          (func (export "core-native-code") (param $storage externref) (param $start i32) (param $size i32)
            (result externref) (local $value (ref null eq)) (local $data (ref $digits)) (local $index i32)
            (local.set $value (ref.cast (ref null eq) (any.convert_extern (local.get $storage))))
            (if (i32.eq (call $tag (local.get $value)) (i32.const 10))
              (then (return (extern.convert_any (struct.get $obj $digits (ref.cast (ref $obj) (local.get $value)))))))
            (local.set $data (array.new_default $digits (local.get $size)))
            (block $done (loop $copy
              (br_if $done (i32.ge_u (local.get $index) (local.get $size)))
              (array.set $digits (local.get $data) (local.get $index)
                (i32.load8_u (i32.add (local.get $start) (local.get $index))))
              (local.set $index (i32.add (local.get $index) (i32.const 1))) (br $copy)))
            (extern.convert_any (local.get $data))))
        (loop for (name kind . fields) in types for id from 0
              unless (member name excluded :test #'string-equal) append
                (if (string-equal kind 'array)
                    (core-array-functions name id (core-unmut (first fields)))
                    (core-struct-functions name id fields)))))))
