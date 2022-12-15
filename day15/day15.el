;;; Summary --- day15.el
;;; Commentary:

;; Sample data holds a picture like this
;;                1    1    2    2
;;      0    5    0    5    0    5
;; -2 ..........#.................
;; -1 .........###................
;;  0 ....S...#####...............
;;  1 .......#######........S.....
;;  2 ......#########S............
;;  3 .....###########SB..........
;;  4 ....#############...........
;;  5 ...###############..........
;;  6 ..#################.........
;;  7 .#########S#######S#........
;;  8 ..#################.........
;;  9 ...###############..........
;; 10 ....B############...........
;; 11 ..S..###########............
;; 12 ......#########.............
;; 13 .......#######..............
;; 14 ........#####.S.......S.....
;; 15 B........###................
;; 16 ..........#SB...............
;; 17 ................S..........B
;; 18 ....S.......................
;; 19 ............................
;; 20 ............S......S........
;; 21 ............................
;; 22 .......................B....
;;; Code:

(require 'seq)

(defun parse-beacon-row (row)
  "Parse stuff from beacon ROW."
  (seq-let (_ xs ys _ xb yb) (split-string row (rx (or "x=" ", y=" ":")))
    (list
     (string-to-number xs)
     (string-to-number ys)
     (string-to-number xb)
     (string-to-number yb))))

(require 'subr-x)

(defun parse-puzzle-input (input)
  "Parse INPUT."
  (thread-last
    (string-chop-newline input)
    (string-lines)
    (seq-map 'parse-beacon-row)))

(defun sensor-covered-area (sensor)
  "Return area covered by SENSOR."
  (seq-let (xs ys xb yb) sensor
    (let ((dx (abs (- xs xb)))
          (dy (abs (- ys yb))))
      (+ dx dy))))

(defun manhattan-distance (p1 p2)
  "Return manhattan distance between P1 and P2."
  (let ((dx (abs (- (car p1) (car p2))))
        (dy (abs (- (cdr p1) (cdr p2)))))
    (+ dx dy)))

(defun read-puzzle-input-from (f)
  "Read puzzle input from F."
  (with-current-buffer (find-file-noselect f)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun point-visible? (p sensors)
  "Return non-nil if point P is visible from SENSORS."
  (seq-some
   (lambda (sensor)
     (let ((distance-to-sensor (manhattan-distance
                                p
                                `(,(cadr sensor) . ,(caddr sensor)))))
       (<= distance-to-sensor (car sensor))))
   sensors))

(defun find-beacons (sensors)
  "Return beacons from SENSORS."
  (seq-map
   (lambda (sensor)
     `(,(seq-elt sensor 2) . ,(seq-elt sensor 3)))
   sensors))

(defun count-covered-points (sensors row start end)
  "Counts points covered by SENSORS on ROW, searcing from START to END."
  (let* ((dsensors (seq-map (lambda (sensor)
                              (cons (sensor-covered-area sensor)
                                    sensor))
                            sensors))
         (beacons (seq-map
                   (lambda (sensor)
                     `(,(seq-elt sensor 2) . ,(seq-elt sensor 3)))
                   sensors))
         (x start)
         (count 0))
    (while (< x end)
      (let ((point `(,x . ,row)))
        (when (and
               (point-visible? point dsensors)
               (not (seq-some (lambda (beacon) (equal point beacon)) beacons)))
          (setq count (+ count 1)))
        (setq x (+ x 1))))
    count))

(thread-first
  (read-puzzle-input-from "./sample-input")
  (parse-puzzle-input)
  (count-covered-points 10 -15 35)
  )

;; 26 - correct!

(thread-first
  (read-puzzle-input-from "./input")
  (parse-puzzle-input)
  (count-covered-points 2000000 -1700000 5000000)
  )
;; 4582667 - Correct!
;; 4208426 - clearly missing the mark
;; 1208426 - too low

;; 41


(let ((beacons
       (thread-first
         (read-puzzle-input-from "./input")
         (parse-puzzle-input)
         find-beacons
         )))
  (list (find-min-x beacons)
        (find-max-x beacons)))

(-1236383 3691788)



(defun find-min-x (points)
  "Find min x from POINTS."
  (apply 'min (seq-map 'car points)))

(defun find-max-x (points)
  "Find max X from POINTS."
  (apply 'max (seq-map 'car points)))

(defun find-min-y (points)
  "Find min Y from POINTS."
  (apply 'min (seq-map 'cdr points)))

(defun find-max-y (points)
  "Find max y from POINTS."
  (apply 'max (seq-map 'cdr points)))


;; sample solution:
;;; In this example, in the row where y=10, there are 26 positions where a beacon cannot be present.

(defun day15-render (map)
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


;;; day15.el ends here
