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

(defun place-blocked? (map p)
  "Return non-nil when position P is blocked on MAP."
  (seq-find (lambda (pn) (equal pn p)) map))

(defun initialize-map (rocks)
  "Initialize map from ROCKS."
  (let ((limits (play-area rocks)))
    (vconcat
     (seq-map
      (lambda (y)
        (vconcat
         (seq-map (lambda (x)
                    (cond
                     ((place-blocked? rocks `(,x . ,y)) "#")
                     (t ".")))
                  (number-sequence (plist-get limits 'min-x) (plist-get limits 'max-x)))))
      (number-sequence (plist-get limits 'min-y) (plist-get limits 'max-y))))))

(let ((rocks
       (thread-last (parse-scanner-data "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")
                    (seq-mapcat 'interpret-path))))
  (day14-render (initialize-map rocks)))

(defun day14-render (map)
  "Render MAP to a new buffer."
  (with-current-buffer (get-buffer-create "*day14-render*")
    (delete-region (point-min) (point-max))
    (seq-do
     (lambda (row)
       (seq-do
        (lambda (c)
          (insert c))
        row)
       (insert "\n"))
     map)))

(defun corrected-point (p limits)
  "Return point P corrected to LIMITS."
  `(,(- (car p) (plist-get limits 'min-x))
    .
    ,(cdr p)))

(defun point-taken? (map limits p)
  "Return non-nil if P is already taken on MAP with LIMITS."
  (let ((corrected-p (corrected-point p limits)))
    (when (and (<= 0 (car corrected-p))
               (>= (plist-get limits 'max-y) (cdr corrected-p)))
      (not (equal "." (aref (aref map (cdr p)) (car corrected-p)))))))

(defun off-the-map? (limits point)
  "Return non-nil if POINT is off LIMITS."
  (< (plist-get limits 'max-y) (cdr point)))

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

(defun find-resting-place (map limits grain)
  "Return final resting place of GRAIN on MAP with LIMITS."
  (let ((last-pos nil)
        (current-pos grain)
        (rounds 0))
    (while (and
            (< rounds 1000000) ;; avoid infinite loop
            (not (equal last-pos current-pos)) ;; stop when at rest
            (not (< (plist-get limits 'max-y)
                    (cdr current-pos)))) ;; stop if it drops off the map
      (setq last-pos current-pos)
      (setq rounds (+ rounds 1))
      (let ((down `(,(car current-pos) . ,(+ (cdr current-pos) 1))))
        (if (not (point-taken? map limits down))
            (setq current-pos down)
          (let ((down-left `(,(- (car down) 1) . ,(cdr down))))
            (if (not (point-taken? map limits down-left))
                (setq current-pos down-left)
              (let ((down-right `(,(+ (car down) 1) . ,(cdr down))))
                (when (not (point-taken? map limits down-right))
                  (setq current-pos down-right))))))))
    (if (off-the-map? limits current-pos)
        :off-the-map
      current-pos)))

(day14-solve-part1-on "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")

(defun put-grain-on-map (grain map limits)
  "Insert GRAIN to MAP with LIMITS."
  (let ((corrected-grain (corrected-point grain limits)))
    (aset (aref map (cdr corrected-grain))
          (car corrected-grain)
          "o")))

(defun day14-solve-part1-on (s)
  "Solve puzzle for S."
  (let* ((rocks
          (thread-last (parse-scanner-data s)
                       (seq-mapcat 'interpret-path)))
         (map (initialize-map rocks))
         (limits (play-area rocks))
         (rounds -1)
         (new-grain nil))
    (while (and (< rounds 10000)           ; Don't go forever
                (not (equal new-grain :off-the-map)))
      (setq rounds (+ rounds 1))
      ;; Sand is pouring from 500,0
      (setq new-grain (find-resting-place map limits '(500 . 0)))
      (when (not (equal new-grain :off-the-map))
        (put-grain-on-map new-grain map limits)))
    (day14-render map)
    rounds))


;; Sample data starts with this kind of structure
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

(day14-solve-part1-on "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")

;; 24 -- Correct!


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

(let ((puzzle-input
       (with-current-buffer (find-file-noselect "./input")
         (buffer-substring-no-properties (point-min) (point-max)))))
  (day14-solve-part1-on (string-chop-newline puzzle-input)))

805


;;; day14.el ends here
