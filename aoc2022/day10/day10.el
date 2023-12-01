;;; Summary --- day10.el
;;; Commentary:
;;; Code:

(require 'seq)
(require 'subr-x)

(defvar sample-input nil)

(defun initialize-state ()
  "Return a new program state."
  (let ((state (list)))
    (setf (alist-get 'cycle state) 0)
    (setf (alist-get 'x state) 1)
    (setf (alist-get 'running state) nil)
    (setf (alist-get 'out state) nil)
    state))

(defun run-day10-program (input)
  "Run program in INPUT."
  (let ((program (thread-last input
                              string-chop-newline
                              string-lines))
        (state (initialize-state)))

    (dotimes (clock 221)
      (let-alist state
        (cond
         ((= clock 0) nil)
         ((and (not program) (not .running)) nil)
         (.running (seq-let (command params) .running
                     (message "%s stopping %s: %s - %s" clock command params .x)
                     (pcase command
                       ("addx" (setf (alist-get 'x state)
                                     (+ .x (car (read-from-string params))))))
                     (setf (alist-get 'running state)
                           nil)))
         ((not .running) (let ((next-command (pop program)))
                           (seq-let (command params) (split-string next-command)
                             (message "%s starting %s: %s - %s" clock command params .x)
                             (pcase command
                               ("noop" nil)
                               ("addx" (setf (alist-get 'running state)
                                             (list command params))))))))
        (when (seq-find (lambda (x) (= x clock)) '(20 60 100 140 180 220))
          (setf (alist-get 'out state) (cons `(,clock . ,.x) .out)))
        (setf (alist-get 'cycle state) clock)))
    state))


(run-day10-program
 "noop
addx 3
addx -5")

(run-day10-program sample-input)

;; ((out (220 . 18) (180 . 16) (140 . 21) (100 . 18) (60 . 19) (20 . 21)) (running) (x . 19) (cycle . 220))

(defun count-signal-strengths (vals)
  "Count signal strengths in VALS."
  (seq-reduce
   (lambda (acc val)
     (+ acc
        (* (car val) (cdr val))))
   vals
   0))


(let-alist (run-day10-program sample-input)
  (count-signal-strengths .out))

(with-current-buffer (find-file-noselect "./input")
  (let-alist (run-day10-program (buffer-substring-no-properties (point-min) (point-max)))
    (count-signal-strengths .out)))

;; 12520

;; part 2

(require 'map)

(defun initialize-state&screen ()
  "Initialize program state and screen."
  (map-insert (initialize-state) 'row nil))

(defun run-day10-program-render (input)
  "Run program in INPUT."
  (let ((program (thread-last input
                              string-chop-newline
                              string-lines))
        (state (initialize-state)))

    (dotimes (clock 241)
      (let ((col (mod clock 40)))
        (let-alist state
          (cond
           ((= clock 0) nil)
           ((and (not program) (not .running)) nil)
           (.running (seq-let (command params) .running
                       (pcase command
                         ("addx" (setf (alist-get 'x state)
                                       (+ .x (car (read-from-string params))))))
                       (setf (alist-get 'running state)
                             nil)))
           ((not .running) (let ((next-command (pop program)))
                             (seq-let (command params) (split-string next-command)
                               (pcase command
                                 ("noop" nil)
                                 ("addx" (setf (alist-get 'running state)
                                               (list command params))))))))
          (setf (alist-get 'row state) (cons (if (<= .x col (+ 2 .x)) "#" ".") .row))
          (when (= col 0)
            (setf (alist-get 'out state) (cons (alist-get 'row state) .out))
            (setf (alist-get 'row state) (list)))
          (setf (alist-get 'cycle state) clock))))
    state))

(insert
 (let-alist (run-day10-program-render sample-input)
   (let ((rows (mapcar (lambda (row) (string-join (reverse row))) (reverse .out))))
     (string-join rows "\n"))))

;; output
;; ##..##..##..##..##..##..##..##..##..##..
;; ###...###...###...###...###...###...###.
;; ####....####....####....####....####....
;; #####.....#####.....#####.....#####.....
;; ######......######......######......###.
;; #######.......#######.......#######.....

;; comparison
;; ##..##..##..##..##..##..##..##..##..##..
;; ###...###...###...###...###...###...###.
;; ####....####....####....####....####....
;; #####.....#####.....#####.....#####.....
;; ######......######......######......####
;; #######.......#######.......#######.....


(insert
 (with-current-buffer (find-file-noselect "./input")
   (let-alist (run-day10-program-render (buffer-substring-no-properties (point-min) (point-max)))
     (let ((rows (mapcar (lambda (row) (string-join (reverse row))) (reverse .out))))
       (string-join rows "\n")))))

;; ####.#..#.###..####.###....##..##..#....
;; #....#..#.#..#....#.#..#....#.#..#.#...#
;; ###..####.#..#...#..#..#....#.#....#...#
;; #....#..#.###...#...###.....#.#.##.#...#
;; #....#..#.#....#....#....#..#.#..#.#....
;; ####.#..#.#....####.#.....##...###.####.

;; If we scrap the rightmost column:
;; ####.#..#.###..####.###....##..##..#...
;; #....#..#.#..#....#.#..#....#.#..#.#...
;; ###..####.#..#...#..#..#....#.#....#...
;; #....#..#.###...#...###.....#.#.##.#...
;; #....#..#.#....#....#....#..#.#..#.#...
;; ####.#..#.#....####.#.....##...###.####

;; Old broken inputs

;; v1 I think it's upside down
;; ####.###..###....#######....#.#.#######
;; ..####.####..#....#...##....#.#.##....#
;; ...#.######....####...#..######..#....#
;; ...#...####....#.##..#..##..#.####..###
;; ...#.#.####....#.####....#.####..#....#
;; ..##..##.###...#########.######.#######

;; v2 I think it's mirrored
;; ..##..##.###...#########.######.#######
;; ...#.#.####....#.####....#.####..#....#
;; ...#...####....#.##..#..##..#.####..###
;; ...#.######....####...#..######..#....#
;; ..####.####..#....#...##....#.#.##....#
;; ####.###..###....#######....#.#.#######

;; Still mangled somehow
;; #######.######.#########...###.##..##..
;; #....#..####.#....####.#....####.#.#...
;; ###..####.#..##..#..##.#....####...#...
;; #....#..######..#...####....######.#...
;; #....##.#.#....##...#....#..####.####..
;; #######.#.#....#######....###..###.####






(setq sample-input
      "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

;;; day10.el ends here
