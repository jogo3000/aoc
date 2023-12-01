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

(defun visible-from-sidep (side here)
  "Return non-nil if HERE is visible from the SIDE."
  (not (seq-some (lambda (tree) (>= tree here)) side)))

(visible-from-sidep '(2) 5)
(visible-from-sidep '(5 1 2) 5)

(defun visible-from-rowp (left here right)
  "Return non-nil if HERE is visible from LEFT or RIGHT."
  (or (visible-from-sidep left here)
      (visible-from-sidep right here)))

(visible-from-rowp '(2) 5 '(5 1 2)) ; t
(visible-from-rowp '(2 5) 5 '(1 2)) ; t
(visible-from-rowp '(2 5 5) 1 '(2)) ; nil
(visible-from-rowp '(1 2 3) 4 '(3 2 1)) ; t
(visible-from-rowp '(5 5 5) 1 '(5 5 5)) ; nil

(require 'subr-x)

(defun parse-data (data)
  "Parse DATA into a seq of seqs."
  (thread-last
    data
    string-chop-newline
    (string-lines)
    (mapcar (lambda (row)
              (mapcar (apply-partially #'+ -48) (string-to-list row))))))

(defun focus-on-coords (data x y)
  "Return row and column from DATA focused on X and Y."
  (let ((row (seq-elt data y))
        (col (mapcar (lambda (row) (seq-elt row x)) data)))
    (list (list (seq-take row x) (seq-elt row x) (seq-drop row (+ 1 x)))
          (list (seq-take col y) (seq-elt col y) (seq-drop col (+ 1 y))))))

(defun tree-visible-p (data x y)
  "Test if tree is visible in DATA point X, Y."
  (seq-let (row col) (focus-on-coords data x y)
    (or (apply #'visible-from-rowp row)
        (apply #'visible-from-rowp col))))

(thread-first
  sample-data
  parse-data
  (tree-visible-p 3 1))


(defun count-visible-trees (input)
  "Count visible trees from INPUT."
  (let ((forest (parse-data (string-chop-newline input))))
    (thread-last
      forest
      (seq-map-indexed
       (lambda (row y)
         (seq-map-indexed
          (lambda (_ x)
            (if (tree-visible-p forest x y) 1 0)) row)))
      (apply #'seq-concatenate 'list)
      (apply #'+))))

(count-visible-trees sample-data)


(with-current-buffer (find-file-noselect "./input")
  (count-visible-trees (buffer-substring-no-properties (point-min) (point-max))))

;; 1782

;; Part 2

(defun count-scenic-score-on-one-side (side here)
  "Count scenic score from SIDE starting from HERE."
  (cadr (seq-reduce (lambda (acc tree)
                      (seq-let (found count) acc
                        (if found acc
                          (list (>= tree here) (+ 1 count)))))
                    side
                    (list nil 0))))

(defun count-scenic-score-on-both-sides (left here right)
  "Count scenic score from HERE to LEFT and RIGHT."
  (* (count-scenic-score-on-one-side (seq-reverse left) here)
     (count-scenic-score-on-one-side right here)))

(count-scenic-score-on-both-sides '(3 1) 3 '(1 4))

(defun count-scenic-score (data x y)
  "Count scenic score in DATA coords X, Y."
  (seq-let (row col) (focus-on-coords data x y)
    (* (apply #'count-scenic-score-on-both-sides row)
       (apply #'count-scenic-score-on-both-sides col))))

(thread-first
  sample-data
  string-chop-newline
  parse-data
  (count-scenic-score 2 3))

(defun max-scenic-score-from-input (input)
  "Find maximum scenic score from INPUT."
  (let ((forest (parse-data (string-chop-newline input))))
    (thread-last
      forest
      (seq-map-indexed
       (lambda (row y)
         (seq-map-indexed
          (lambda (_ x)
            (count-scenic-score forest x y)) row)))
      (apply #'seq-concatenate 'list)
      (apply #'max))))


(max-scenic-score-from-input sample-data)

(with-current-buffer (find-file-noselect "./input")
  (max-scenic-score-from-input (buffer-substring-no-properties (point-min) (point-max))))

;; 474606


;;; day8.el ends here
