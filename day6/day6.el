;;; Summary --- day6.el
;;; Commentary:
;;; Code:

(require 'seq)

(defvar sample-data
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defun find-marker (s)
  "Find start of packet marker from S."
  (let ((pos 4))
    (while (and (< pos (seq-length s))
                (< (seq-length (seq-uniq (seq-subseq s (- pos 4) pos))) 4))
      (setq pos (+ 1 pos)))
    pos))

(find-marker sample-data)

(find-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")
(find-marker "nppdvjthqldpwncqszvftbrmjlhg")
(find-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")

(find-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(with-current-buffer (find-file-noselect "./input")
  (find-marker (buffer-substring-no-properties (point-min) (point-max))))

;; 1920

;; Part 2

(defun find-som-marker (s)
  "Find start of message marker from S."
  (let ((pos 14))
    (while (and (< pos (seq-length s))
                (< (seq-length (seq-uniq (seq-subseq s (- pos 14) pos))) 14))
      (setq pos (+ 1 pos)))
    pos))

(find-som-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(with-current-buffer (find-file-noselect "./input")
  (find-som-marker (buffer-substring-no-properties (point-min) (point-max))))

;; 2334



;;; day6.el ends here
