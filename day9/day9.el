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

; .H.    .H.
; ... -> .T.
; T..    ...


(maybe-adjust-tail '(2 . 2) '(1 . 0))

; ..H    .TH
; T.. -> ...
; ...    ...


(defun run-simulation (initial-state input)
  "Given INITIAL-STATE, count positions the tail will visit with given INPUT."
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

;; 13


(with-current-buffer (find-file-noselect "./input")
  (seq-length
   (run-simulation '((head . (0 . 0))
                     (tail . (0 . 0))
                     (positions . nil))
                   (buffer-substring-no-properties (point-min) (point-max)))))

;; 6314

;; part 2

(defun knot-distance (p1 p2)
  "Return distance from P1 to P2."
  (max (- (car p1) (car p2))
       (- (cdr p1) (cdr p2))))

(defun run-simulation-v2 (initial-state input)
  "Given INITIAL-STATE, count positions the tail will visit with given INPUT."
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

                   ;; move the other knots
                   (let* ((new-t1 (maybe-adjust-tail .head .t1))
                          (new-t2 (maybe-adjust-tail new-t1 .t2))
                          (new-t3 (maybe-adjust-tail new-t2 .t3))
                          (new-t4 (maybe-adjust-tail new-t3 .t4))
                          (new-t5 (maybe-adjust-tail new-t4 .t5))
                          (new-t6 (maybe-adjust-tail new-t5 .t6))
                          (new-t7 (maybe-adjust-tail new-t6 .t7))
                          (new-t8 (maybe-adjust-tail new-t7 .t8))
                          (new-t9 (maybe-adjust-tail new-t8 .t9)))

                     (when (> (knot-distance new-t9 .t9) 1)
                       (message "Error when head is at %s" .head)
                       (message "knot distance %s" (knot-distance new-t9 .t9)))

                     (setf (alist-get 't1 acc) new-t1)
                     (setf (alist-get 't2 acc) new-t2)
                     (setf (alist-get 't3 acc) new-t3)
                     (setf (alist-get 't4 acc) new-t4)
                     (setf (alist-get 't5 acc) new-t5)
                     (setf (alist-get 't6 acc) new-t6)
                     (setf (alist-get 't7 acc) new-t7)
                     (setf (alist-get 't8 acc) new-t8)
                     (setf (alist-get 't9 acc) new-t9)

                     ;; Store the new position
                     (setf (alist-get 'positions acc) (cons new-t9 .positions))))
                 acc)
               moves
               acc)))
          (string-lines
           (string-chop-newline input))
          initial-state)))
    (let-alist final-state
      (seq-uniq .positions))))

(seq-length
   (run-simulation-v2 '((head . (0 . 0))
                        (t1 . (0 . 0))
                        (t2 . (0 . 0))
                        (t3 . (0 . 0))
                        (t4 . (0 . 0))
                        (t5 . (0 . 0))
                        (t6 . (0 . 0))
                        (t7 . (0 . 0))
                        (t8 . (0 . 0))
                        (t9 . (0 . 0))
                        (positions . nil))
                      sample-input))

(seq-length
 (run-simulation-v2 '((head . (0 . 0))
                      (t1 . (0 . 0))
                      (t2 . (0 . 0))
                      (t3 . (0 . 0))
                      (t4 . (0 . 0))
                      (t5 . (0 . 0))
                      (t6 . (0 . 0))
                      (t7 . (0 . 0))
                      (t8 . (0 . 0))
                      (t9 . (0 . 0))
                      (positions . nil))
                    "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"))

;; 36 - correct!

(with-current-buffer (find-file-noselect "./input")
  (seq-length
   (run-simulation-v2 '((head . (0 . 0))
                        (t1 . (0 . 0))
                        (t2 . (0 . 0))
                        (t3 . (0 . 0))
                        (t4 . (0 . 0))
                        (t5 . (0 . 0))
                        (t6 . (0 . 0))
                        (t7 . (0 . 0))
                        (t8 . (0 . 0))
                        (t9 . (0 . 0))
                        (positions . nil))
                      (buffer-substring-no-properties (point-min) (point-max)))))

;; Initial result 2567 - too high !
;; made a renderer, which helped me spot the problem

(defun render-day9-positions (positions)
  "Render POSITIONS graphically."
  (let ((lowest-x (apply #'min (mapcar #'car positions)))
        (lowest-y (apply #'min (mapcar #'cdr positions)))
        (highest-x (apply #'max (mapcar #'car positions)))
        (highest-y (apply #'max (mapcar #'cdr positions))))
    (seq-reduce
     (lambda (acc row)
       (string-join
        (list acc "\n"
              (string-join
               (mapcar
                (lambda (col)
                  (if (seq-contains-p positions `(,(+ row lowest-x) . ,(+ col lowest-y))) "#" "."))
                (number-sequence 0 (- highest-y lowest-y)))))))
     (number-sequence 0 (- highest-x lowest-x))
     "")))

(number-sequence 0 (- 6 -5))

(render-day9-positions
 (run-simulation-v2 '((head . (0 . 0))
                      (t1 . (0 . 0))
                      (t2 . (0 . 0))
                      (t3 . (0 . 0))
                      (t4 . (0 . 0))
                      (t5 . (0 . 0))
                      (t6 . (0 . 0))
                      (t7 . (0 . 0))
                      (t8 . (0 . 0))
                      (t9 . (0 . 0))
                      (positions . nil))
                    "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"))


(run-simulation-v2 '((head . (0 . 0))
                      (t1 . (0 . 0))
                      (t2 . (0 . 0))
                      (t3 . (0 . 0))
                      (t4 . (0 . 0))
                      (t5 . (0 . 0))
                      (t6 . (0 . 0))
                      (t7 . (0 . 0))
                      (t8 . (0 . 0))
                      (t9 . (0 . 0))
                      (positions . nil))
                    "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")


;;; day9.el ends here
