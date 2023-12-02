;;; Summary --- day16.el
;;; Commentary:
;;; Code:

;; Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
;; Valve BB has flow rate=13; tunnels lead to valves CC, AA
;; Valve CC has flow rate=2; tunnels lead to valves DD, BB
;; Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
;; Valve EE has flow rate=3; tunnels lead to valves FF, DD
;; Valve FF has flow rate=0; tunnels lead to valves EE, GG
;; Valve GG has flow rate=0; tunnels lead to valves FF, HH
;; Valve HH has flow rate=22; tunnel leads to valve GG
;; Valve II has flow rate=0; tunnels lead to valves AA, JJ
;; Valve JJ has flow rate=21; tunnel leads to valve II

(defun day16-sample-input ()
  "Return sample input."
  (string-replace ";; " "" (buffer-substring-no-properties 53 644)))

(require 'seq)
(require 'subr-x)

(defun parse-input (s)
  "Parse input from S."
  (thread-last
    (string-chop-newline s)
    (string-lines)
    (seq-map
     (lambda (row)
       (seq-let (left right)
           (split-string row ";")
         (let ((valve (substring left 6 8))
               (rate (substring left 23))
               (tunnels (cadr (split-string right
                                            (rx "valve" (zero-or-one "s" whitespace)) t (rx whitespace)))))
           (list valve
                 (string-to-number rate)
                 (split-string tunnels "," t (rx whitespace)))))))))


(defun optimized-parse (s)
  (let* ((rows (parse-input s))
         (keys (seq-map-indexed (lambda (row n)
                                  `(,(car row) . ,n))
                                rows)))
    (plist-get
     (seq-reduce
      (lambda (acc row)
        (let ((pos (plist-get acc 'pos))
              (maze (plist-get acc 'maze)))
          (aset maze pos (vconcat
                          (list
                           'closed
                           (cadr row)
                           (seq-map
                            (lambda (exit)
                              (message "%s" exit)
                              (alist-get exit keys nil nil 'string-equal))
                            (elt row 2)))))
          (plist-put acc 'pos (+ pos 1))
          (plist-put acc 'maze maze)
          acc))
      rows
      (list 'pos 0
            'maze
            (make-vector (seq-length rows) nil)))
     'maze)))

(optimized-parse (day16-sample-input))

(defun walk (maze pos flow score rounds)
  (if (= rounds 0)
      (+ score flow)
    (progn
      (let* ((posinfo (plist-get maze pos))
             (rate (car posinfo)))
        (when (< 0 rate)
          (walk maze pos (+ flow rate) (+ score flow) (- rounds 1))))))
  score)


(defun deep-copy-vector (v)
  (if (vectorp v)
      (apply 'vconcat (seq-map 'deep-copy-vector (append v nil)))
    v))

(let* ((a (vector 0))
       (b (vector a))
       (c (deep-copy-vector b)))
  (aset a 0 10)
  (list a b c))

(append [1] nil)





(pos "AA")
(flow 0)
(score 0)
(rounds 30)

;;; day16.el ends here
