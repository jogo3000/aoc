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
                 (list
                  (string-to-number rate)
                  (split-string tunnels "," t (rx whitespace))))))))))

(parse-input (day16-sample-input))



;;; day16.el ends here
