;;; Summary --- day8.el
;;; Commentary:
;;; Code:

(defvar sample-data
  "30373
25512
65332
33549
35390
")

(require 'seq)

(defun visible-from-side (side here)
  "Return non-nil if HERE is visible from the SIDE."
  (not (seq-some (lambda (tree) (>= tree here)) side)))

(visible-from-side '(2) 5)
(visible-from-side '(5 1 2) 5)

(defun visible-from-row (left here right)
  "Return non-nil if HERE is visible from LEFT or RIGHT."
  (or (visible-from-side left here)
      (visible-from-side right here)))

(visible-from-row '(2) 5 '(5 1 2)) ; t
(visible-from-row '(2 5) 5 '(1 2)) ; t
(visible-from-row '(2 5 5) 1 '(2)) ; nil
(visible-from-row '(1 2 3) 4 '(3 2 1)) ; t
(visible-from-row '(5 5 5) 1 '(5 5 5)) ; nil



;;; day8.el ends here
