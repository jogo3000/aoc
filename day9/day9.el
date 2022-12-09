;;; Summary --- day9.el
;;; Commentary:
;;; Code:

(defvar sample-input
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

;; ..##..
;; ...##.
;; .####.
;; ....#.
;; s###..

;; So, there are 13 positions the tail visited at least once.

(require 'subr-x)
(require 'seq)
(require 'cl-lib)

(defun parse-line (line)
  "Parse command on LINE."
  (seq-let (dir n) (split-string line)
    (make-list (car (read-from-string n)) dir)))

(defun maybe-adjust-tail (head tail)
  "Return next TAIL's position so it follows HEAD."
  (let ((rowdiff (- (car head) (car tail)))
        (coldiff (- (cdr head) (cdr tail))))
    (cond
     ((< 1 (abs rowdiff)) `(,(if (cl-plusp rowdiff)
                                 (+ (car tail) 1)
                               (- (car tail) 1))
                            .
                            ,(+ (cdr tail) coldiff)))
     ((< 1 (abs coldiff)) `(,(+ (car tail) rowdiff)
                            .
                            ,(if (cl-plusp coldiff)
                                 (+ (cdr tail) 1)
                               (- (cdr tail) 1))))
     (t tail))))


;;; Making sure tail moves correctly
(maybe-adjust-tail '(0 . 0) '(0 . 0))
(maybe-adjust-tail '(1 . 0) '(0 . 0))
(maybe-adjust-tail '(2 . 0) '(0 . 0))
(maybe-adjust-tail '(2 . 1) '(0 . 0))
(maybe-adjust-tail '(1 . 1) '(0 . 0))
(maybe-adjust-tail '(0 . 1) '(0 . 0))
(maybe-adjust-tail '(0 . 2) '(0 . 0))
(maybe-adjust-tail '(1 . 2) '(0 . 0))


(defun run-simulation (initial-state input)
  "Count positions the tail will visit with given INPUT."
  (let ((final-state
         (seq-reduce
          (lambda (acc line)
            (let ((moves (parse-line line)))
              (seq-reduce
               (lambda (acc move)
                 (let-alist acc
                   ;; move the head
                   (pcase move
                     ("R" (setcar .head (+ (car .head) 1)))
                     ("L" (setcar .head (- (car .head) 1)))
                     ("U" (setcdr .head (+ (cdr .head) 1)))
                     ("D" (setcdr .head (- (cdr .head) 1))))

                   ;; maybe move the tail
                   (let ((new-tail (maybe-adjust-tail .head .tail)))
                     (setf (alist-get 'tail acc) new-tail)

                     ;; Store the new position
                     (setf (alist-get 'positions acc) (cons new-tail .positions))))
                 acc)
               moves
               acc)))
          (string-lines
           (string-chop-newline input))
          initial-state)))
    (let-alist final-state
      (seq-uniq .positions))))


(seq-length
 (run-simulation
  '((head . (0 . 0))
    (tail . (0 . 0))
    (positions . nil))
  sample-input))


(with-current-buffer (find-file-noselect "./input")
  (seq-length
   (run-simulation '((head . (0 . 0))
                     (tail . (0 . 0))
                     (positions . nil))
                   (buffer-substring-no-properties (point-min) (point-max)))))

;; 6314

;; part 2





;;; day9.el ends here
