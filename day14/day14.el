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
;;
;; For example:
;; 498,4 -> 498,6 -> 496,6
;; 503,4 -> 502,4 -> 502,9 -> 494,9

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

;;; day14.el ends here
