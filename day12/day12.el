;;; Summary --- day12.el
;;; Commentary:
;;; Looks like it'll make a lot of sense using buffers as data structures.
;;; I can get the character at point with =char-to-string= and characters next
;;; to it by using arithmetic or =next-line=, =previous-line=, =backward-char=,
;;; =forward-char=
;;;
;;; Code:

(with-current-buffer (find-file-noselect "./sample-input")
  (char-to-string (char-after)))

;;; day12.el ends here
