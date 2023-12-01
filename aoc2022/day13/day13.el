;;; Summary --- day13.el
;;; Commentary:
;;; Code:

(defun load-puzzle-input (f)
  "Return contents of F."
  (with-current-buffer (find-file-noselect f)
    (buffer-substring-no-properties (point-min) (point-max))))

(require 'subr-x)
(require 'seq)

(defun parse-day13-input-from (f)
  "Parse day13 input from F."
  (seq-partition
   (thread-last (load-puzzle-input f)
                (string-replace "," " ")
                (string-join (list "(" ")"))
                (read-from-string)
                (car)) 2))

(parse-day13-input-from "./small-input")

(require 'dash)

(defun decisive-order? (left right)
  "Return non-nil if LEFT and RIGHT are in right / wrong order."
  (cond
   (;; Both are integers, lower should come first
    (and (integerp left)
         (integerp right))
    (progn
      (message "numbers: %s %s" left right)
      (cond ((= left right) nil)
            ((< left right) :right)
            ((> left right) :wrong))))

   (;; Both are lists, compare one at a time
    (and (vectorp left)
         (vectorp right))
    (progn
      (message "vectors\n %s\n %s" left right)
      (let ((sub-comparison
             (seq-some (lambda (pair)
                         (let ((left-sub (car pair))
                               (right-sub (cdr pair)))
                           (decisive-order? (if (listp left-sub) (vconcat left-sub nil) left-sub)
                                            (if (listp right-sub) (vconcat right-sub nil) right-sub))))
                       (-zip (append left nil)
                             (append right nil)))))
        (pcase sub-comparison
          ('nil (cond
                 ((< (seq-length left) (seq-length right)) :right)
                 ((> (seq-length left) (seq-length right)) :wrong)
                 (t nil)))
          (n n)))))

   (;; Other is list, other is number
    (or (vectorp left)
        (vectorp right))
    (progn
      (message "other list\n %s\n %s" left right)
      (decisive-order? (if (vectorp left) left (vector left))
                       (if (vectorp right) right (vector right)))))))

(defun day13-part1 (f)
  "Calculate result from F."
  (thread-last
    (parse-day13-input-from f)
    (seq-map-indexed
     (lambda (pair i)
       (message "pair: %s - %s" i pair)
       (list (+ i 1) (decisive-order? (car pair) (cadr pair)))))
    (seq-filter (lambda (pair) (equal :right (cadr pair))))
    (seq-map 'car)
    (apply '+)))

(day13-part1 "./small-input") ; 13
;; 1 right
;; 2 right
;; 3 wrong
;; 4 right
;; 5 wrong
;; 6 right
;; 7 wrong
;; 8 wrong


(day13-part1 "./input") ; 6240 ; 6251 <- too high!

;; part 2
(defun parse-day13-input-from-2 (f)
  "Parse day13 input from F."
  (thread-last (load-puzzle-input f)
               (string-replace "," " ")
               (string-join (list "(" ")"))
               (read-from-string)
               (car)))


(defun day13-part2 (f)
  "Solve puzzle for F."
  (let ((ordered
         (sort
          (thread-last
            (parse-day13-input-from-2 f)
            (append (list [[2]] [[6]])))
          (lambda (a b) (equal (decisive-order? a b) :right)))))
    (*
     (+ (seq-position ordered [[2]]) 1)
     (+ (seq-position ordered [[6]]) 1))))

(day13-part2 "./small-input") ; 140 <- correct!

(day13-part2 "./input") ; 23142



;;; day13.el ends here
