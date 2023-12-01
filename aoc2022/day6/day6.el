;;; Summary --- day6.el
;;; Commentary:
;;; Code:

(require 'seq)

(defvar sample-data
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defun find-marker (s n)
  "Find packet marker from S the count of N."
  (let ((pos n))
    (while (and (< pos (seq-length s))
                (< (seq-length (seq-uniq (seq-subseq s (- pos n) pos))) n))
      (setq pos (+ 1 pos)))
    pos))

(defun find-sop-marker (s)
  "Find start of packet marker from S.

It is 4 unique characters in a row."
  (find-marker s 4))

(find-marker sample-data 4)

(find-sop-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")
(find-sop-marker "nppdvjthqldpwncqszvftbrmjlhg")
(find-sop-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")

(find-sop-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(with-current-buffer (find-file-noselect "./input")
  (find-sop-marker (buffer-substring-no-properties (point-min) (point-max))))

;; 1920

;; Part 2

(defun find-som-marker (s)
  "Find start of message marker from S.

It is 14 unique characters in a row."
  (find-marker s 14))

(find-som-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(with-current-buffer (find-file-noselect "./input")
  (find-som-marker (buffer-substring-no-properties (point-min) (point-max))))

;; 2334



;;; day6.el ends here
