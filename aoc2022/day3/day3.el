;;; Summary --- day3.el
;;; Commentary:
;;; Code:

(require 'seq)

(defvar sample-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defun priority (item)
  "Count priority of ITEM."
  (if (>= item (string-to-char "a"))
      (+ 1 (- item (string-to-char "a")))
    (+ 27 (- item (string-to-char "A")))))


(defun mispacked-items (pack)
  "Return seq of mispacked items in PACK."
  (let* ((pack-size (seq-length pack))
         (compartment-size (/ pack-size 2)))
    (seq-uniq
     (seq-intersection (seq-subseq pack 0 compartment-size)
                       (seq-subseq pack compartment-size)))))

(defun sum-of-priorities (input)
  "Count sum of priorities of mispacked items in INPUT."
  (seq-reduce
   (lambda (acc line)
     (+ acc
        (apply #'+ (mapcar #'priority (mispacked-items line)))))
   (string-lines input)
   0))

(with-current-buffer
  (find-file-noselect "./input")
  (sum-of-priorities
   (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

;; -> 7691


;;; part 2

(require 'cl-macs)


(with-current-buffer
    (find-file-noselect "./input")
  (let* ((input (string-trim (buffer-substring-no-properties (point-min) (point-max))))
         (groups (seq-partition (string-lines input) 3)))
    (seq-reduce
     (lambda (acc group)
       (+
        acc
        (priority (car
                   (cl-destructuring-bind (a b c) group
                     (seq-uniq (seq-intersection (seq-intersection a b) c)))))))
     groups
     0)))

;; -> 2508






;;; day3.el ends here
