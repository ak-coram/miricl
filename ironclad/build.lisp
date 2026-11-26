(in-package :cl-user)

(assert (eq miricl-compiler::*compiler-state* :ready))
(declaim (optimize (space 2) (speed 1)))

(js:define-import ironclad-build-progress ("ironcladBuild" "progress") (:string :i32) :void)

(defun ironclad-build-compile-file (source output)
  (multiple-value-bind (file warnings failure)
      (compile-file source :output-file output)
    (declare (ignore warnings))
    (when (or failure (null file))
      (error "Ironclad compilation failed for ~A." source))
    (load file))
  (values))

(defun ironclad-build-native-bundle (input output)
  ;; COMPILE-FILE has already expanded the source macros and LOAD has set up
  ;; the library's packages, types and registrations in this environment.
  (miricl-compiler:compile-native-bundle input output
    :progress (lambda (file count)
                (ironclad-build-progress (or file "<library>") count)))
  (values))
