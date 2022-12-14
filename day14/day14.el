;;; Summary --- day14.el
;;; Commentary:
;;; Code:

;; Rules for Scan
;; Your scan traces the path of each solid rock structure and reports the x,y
;; coordinates that form the shape of the path, where x represents distance to
;; the right and y represents distance down. Each path appears as a single line
;; of text in your scan. After the first point of each path, each point
;; indicates the end of a straight horizontal or vertical line to be drawn from
;; the previous point.
;; (as in x,y, the first example coord is x=498, y=4
;; For example:
;; 498,4 -> 498,6 -> 496,6
;; 503,4 -> 502,4 -> 502,9 -> 494,9

(require 'seq)
(require 'subr-x)

(defun parse-scanner-data (input)
  "Parse scanner INPUT."
  (thread-last
    (string-lines input)
    (seq-map (lambda (row)
               (thread-last
                 (split-string row (rx whitespace "->" whitespace))
                 (seq-map (lambda (pair)
                            (seq-let (x y) (split-string pair ",")
                              `(,(string-to-number x)
                                .
                                ,(string-to-number y))))))))))

(parse-scanner-data "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")

(defun points-between (here there)
  "Return list of points between HERE and THERE."
  (let ((x1 (car here))
        (y1 (cdr here))
        (x2 (car there))
        (y2 (cdr there)))
    (cond
     ((= x1 x2) (seq-map (lambda (y) `(,x1 . ,y))
                         (number-sequence y1 y2 (if (< y1 y2) 1 -1))))
     ((= y1 y2) (seq-map (lambda (x) `(,x . ,y1))
                         (number-sequence x1 x2 (if (< x1 x2) 1 -1))))
     (t (error (format "Huh?! Shouldn't be possible xs: %s-%s ys: %s-%s" x1 x2 y1 y2))))))

(defun interpret-path (path)
  "Return all points in PATH."
  (seq-reduce
   (lambda (acc point)
     (if (not acc)
         (cons point acc)
       (let ((new-points (points-between (car acc) point)))
         (seq-reduce (lambda (acc p) (cons p acc)) new-points (cdr acc)))))
   path
   nil))

(defun play-area (points)
  "Return whole play area from POINTS."
  (seq-reduce
   (lambda (acc p)
     (thread-first acc
       (plist-put 'min-x (min (plist-get acc 'min-x) (car p)))
       (plist-put 'max-x (max (plist-get acc 'max-x) (car p)))
       (plist-put 'min-y (min (plist-get acc 'min-y) (cdr p)))
       (plist-put 'max-y (max (plist-get acc 'max-y) (cdr p)))))
   points
   (list 'min-x 100000 'max-x 0 'min-y 0 'max-y 0)))


(thread-last (parse-scanner-data "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")
             (seq-mapcat 'interpret-path)
             play-area)

(with-current-buffer (get-buffer-create "*day14-render*")
  (let* ((map (thread-last (parse-scanner-data "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")
                           (seq-mapcat 'interpret-path)))
         (limits (play-area map)))
    (delete-region (point-min) (point-max))
    (seq-do
     (lambda (y)
       (seq-do (lambda (x)
                 (insert (if (seq-find (lambda (p) (equal p `(,x . ,y))) map)
                             "#"
                           ".")))
               (number-sequence (plist-get limits 'min-x) (plist-get limits 'max-x)))
       (insert "\n"))
     (number-sequence (plist-get limits 'min-y) (plist-get limits 'max-y)))))


;; Sand is pouring from 500,0

;; This kind of structure
;;   4     5  5
;;   9     0  0
;;   4     0  3
;; 0 ......+...
;; 1 ..........
;; 2 ..........
;; 3 ..........
;; 4 ....#...##
;; 5 ....#...#.
;; 6 ..###...#.
;; 7 ........#.
;; 8 ........#.
;; 9 #########.

;; The RULES:
;; Sand is produced one unit at a time, and the next unit of sand is not
;; produced until the previous unit of sand comes to rest. A unit of sand is
;; large enough to fill one tile of air in your scan.

;; A unit of sand always falls down one step if possible. If the tile
;; immediately below is blocked (by rock or sand), the unit of sand attempts to
;; instead move diagonally one step down and to the left. If that tile is
;; blocked, the unit of sand attempts to instead move diagonally one step down
;; and to the right. Sand keeps moving as long as it is able to do so, at each
;; step trying to move down, then down-left, then down-right. If all three
;; possible destinations are blocked, the unit of sand comes to rest and no
;; longer moves, at which point the next unit of sand is created back at the
;; source.

;; Final configuration of sample data
;; .......+...
;; .......~...
;; ......~o...
;; .....~ooo..
;; ....~#ooo##
;; ...~o#ooo#.
;; ..~###ooo#.
;; ..~..oooo#.
;; .~o.ooooo#.
;; ~#########.
;; ~..........
;; ~..........
;; ~..........

;;; day14.el ends here
