// Retain the last member of each adjacent run, as in the logical definition.
// Every output cons is fresh, including the last one; dotted tails are ignored.
export const iterativeAdjacentDuplicates = (name, test) => `(defun ${name} (x)
  (let ((result nil))
    (loop while (consp x) do
      (unless (and (consp (cdr x)) (${test} (car x) (cadr x)))
        (push (car x) result))
      (setq x (cdr x)))
    (nreverse result)))`;
