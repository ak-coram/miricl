;;;; Checked macro boundaries are native; construction stays ordinary CL.
(in-package #:miricl-runtime)
(export '(expand-do-form))

(declaim (inline expand-do-parallel-step))
(defun expand-do-parallel-step (pairs)
  "Construct parallel assignments from the worker's fresh variable/form pairs."
  (declare (type list pairs) (optimize (safety 0) (debug 0))
           (inline car cdr cons list gensym reverse append))
  (values
   (let ((bindings nil) (assignments nil))
     (tagbody next
        (if pairs nil (go done))
        (let ((variable (car (the cons pairs))) (temporary (gensym)))
          (setq pairs (cdr (the cons pairs))
                bindings (cons (list temporary (car (the cons pairs))) bindings)
                assignments (cons (list 'setq variable temporary) assignments)
                pairs (cdr (the cons pairs))))
        (go next)
      done)
     (cons 'let (cons (reverse bindings) (append (reverse assignments) (list nil)))))))

(defun expand-do-form (form sequential loop-operator)
  "Expand DO/DO* after validating its form, variable, spec and end-clause spines."
  (declare (type cons form) (optimize (safety 0) (debug 0))
           (inline car cdr consp symbolp eq cons list reverse append
                   expand-do-parallel-step))
  (values
   (let* ((arguments (cdr form)) (variables (car (the cons arguments)))
          (tail (cdr (the cons arguments))) (end (car (the cons tail)))
          (body (cdr (the cons tail)))
          (bindings nil) (steps nil) (declarations nil) (scan body) (tags nil))
     (tagbody variable
        (if variables nil (go variables-done))
        (let ((spec (car (the cons variables))))
          (if (symbolp spec) (setq bindings (cons (list spec nil) bindings))
              (let* ((name (car (the cons spec))) (init (cdr (the cons spec)))
                     (step (if init (cdr (the cons init)))))
                (setq bindings (cons (list name (if init (car (the cons init)))) bindings))
                (if step (setq steps (cons (car (the cons step)) (cons name steps)))))))
        (setq variables (cdr (the cons variables)))
        (go variable)
      variables-done)
     (tagbody declaration
        (if scan nil (go declarations-done))
        (let ((item (car (the cons scan))))
          (if (if (consp item) (eq (car (the cons item)) 'declare))
              (setq declarations (cons item declarations) scan (cdr (the cons scan)))
              (go declarations-done)))
        (go declaration)
      declarations-done)
     (let ((forms scan))
       (tagbody tag
          (if forms nil (go tags-done))
          (if (consp (car (the cons forms))) nil (setq tags t))
          (setq forms (cdr (the cons forms)))
          (go tag)
        tags-done))
     (setq steps (reverse steps))
     (list 'block nil
       (cons (if sequential 'let* 'let) (cons (reverse bindings)
         (append (reverse declarations)
           (list (list loop-operator (list 'not (car (the cons end)))
                   (cons (if tags 'tagbody 'progn) scan)
                   (if sequential (cons 'setq steps) (expand-do-parallel-step steps)))
                 (cons 'progn (cdr (the cons end)))))))))))
(export '(expand-iteration-form))
(defun expand-iteration-form (var source result options)
  "Build DOLIST, DOTIMES and package iteration expansions after spine checks.
OPTIONS holds the body, mode (0=list, 1=count, 2=symbols), and target operators."
  (declare (type simple-vector options) (optimize (safety 0) (debug 0))
           (inline svref car cdr consp eq eql cons list list* gensym reverse append rplacd))
  (values
   (block done
     (let* ((body (svref options 0)) (mode (svref options 1))
            (loop-operator (svref options 2)) (symbol-operator (svref options 3))
            (counting (eql mode 1)) (state (gensym)) (counter (gensym))
            (declarations nil) (scan body) (tags nil) (forms nil))
       (tagbody declaration
          (if scan nil (go declarations-done))
          (let ((form (car (the cons scan))))
            (if (if (consp form) (eq (car (the cons form)) 'declare))
                (setq declarations (cons form declarations) scan (cdr (the cons scan)))
                (go declarations-done)))
          (go declaration)
        declarations-done)
       (setq declarations (reverse declarations) forms scan)
       (tagbody tag
          (if forms nil (go tags-done))
          (if (consp (car (the cons forms))) nil (setq tags t))
          (setq forms (cdr (the cons forms)))
          (go tag)
        tags-done)
       (if (eql mode 2)
           (return-from done
             (list 'block nil
               (cons 'let (cons (list (list state source) (list var nil))
                 (append declarations
                   (list (cons symbol-operator (cons var (cons state
                           (if tags (list (cons 'tagbody scan)) scan))))
                         (list 'setq var nil) result)))))))
       (let ((expansion
               (if (if declarations t tags)
                   (list 'block nil
                     (list 'let (if counting
                                    (list (list state (list 'the 'integer source)) (list counter 0))
                                    (list (list state source)))
                       (list loop-operator (if counting (list '< counter state) state)
                         (cons 'let (cons (list (list var (if counting counter (list 'car state))))
                           (append declarations (list (cons (if tags 'tagbody 'progn) scan)))))
                         (list 'setq (if counting counter state)
                           (list (if counting '1+ 'cdr) (if counting counter state))))
                       (if result (cons 'let (cons (list (list var (if counting counter nil)))
                                         (append declarations (list result)))) nil)))
                   (list 'block nil
                     (list 'let (list (list state (if counting (list 'the 'integer source) source))
                                     (list var (if counting 0 nil)))
                       (cons loop-operator
                         (append (list (if counting (list '< var state) state))
                           (if counting nil (list (list 'setq var (list 'car state))))
                           (list (cons 'progn body) (list 'setq (if counting var state)
                                   (list (if counting '1+ 'cdr) (if counting var state))))))
                       (if counting result (list 'progn (list 'setq var nil) result)))))))
         (if counting nil
             (let ((binding (car (the cons (cdr (the cons (cdr (the cons expansion))))))))
               (rplacd (the cons (cdr (the cons binding)))
                 (cons (list 'declare (list 'type 'list state))
                       (cdr (the cons (cdr (the cons binding))))))))
         expansion)))))
