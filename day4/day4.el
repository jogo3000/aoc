;;; package --- Summary
;;; Commentary:
;;; Code:

(defvar sample-data
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(require 'cl-macs)

(defun parse-range (range-string)
  "Parse RANGE-STRING."
  (mapcar #'car
          (mapcar #'read-from-string (split-string range-string "-"))))

(defun parse-pair (s)
  "Parse pair's data from S."
  (mapcar #'parse-range (split-string s "," )))

(defun ranges-fully-overlap-p (r1 r2)
  "Return non-nil if ranges R1 and R2 overlap fully."
  (or (and (<= (car r1) (car r2))
           (>= (cadr r1) (cadr r2)))
      (and (>= (car r1) (car r2))
           (<= (cadr r1) (cadr r2)))))


(defun count-fully-overlapping-pairs (data)
  "Return count of fully overlapping pairs in DATA."
  (seq-reduce
   (lambda (acc pair)
     (if (apply #'ranges-fully-overlap-p (parse-pair pair))
         (+ acc 1)
       acc))
   (string-lines data)
   0))


(with-current-buffer (find-file-noselect "./input")
  (count-fully-overlapping-pairs
   (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

;; -> 477

;; part 2

(defun ranges-overlap-p (r1 r2)
  "Return non-nil if ranges R1 and R2 overlap at all."
  (or (<= (car r1) (car r2) (cadr r1))
      (<= (car r2) (car r1) (cadr r2))))

(defun count-overlapping-pairs (data)
  "Return count of overlapping pairs in DATA."
  (seq-reduce
   (lambda (acc pair)
     (if (apply #'ranges-overlap-p (parse-pair pair))
         (+ acc 1)
       acc))
   (string-lines data)
   0))

(with-current-buffer (find-file-noselect "./input")
  (count-overlapping-pairs
   (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

;; -> 830


;;; day4.el ends here
