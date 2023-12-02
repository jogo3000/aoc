;;; Summary --- day20.el
;;; Commentary:
;;; Code:

(let ((workb (get-buffer-create "*day20-buffer*")))
  (with-current-buffer workb
    (delete-region (point-min) (point-max)))
  (with-current-buffer (find-file-noselect "./sample-input")
    (copy-to-buffer workb (point-min) (point-max)))

  (with-current-buffer workb
    (let ((last-line (line-number-at-pos (point-max))))
      (goto-char (point-min))

      (dotimes (round (- last-line 1))
        (insert (number-to-string round) "-")
        (forward-line))

      (insert (number-to-string (- last-line 1)) "-")

      (dotimes (round (- last-line 1))
        (goto-char (point-min))
        (search-forward (format "%d-" round))
        (delete-char (- 0 (+ 1 (length (number-to-string round)))))
        (kill-line)
        (let* ((curr-line (line-number-at-pos (point)))
               (n (car kill-ring))
               (new-pos (if (> 0 (+ curr-line (string-to-number n)))
                            (mod (+ curr-line (string-to-number n))
                                 (- last-line 1))
                            (mod (+ (- last-line 1) curr-line (string-to-number n))
                                 last-line))))
          (kill-line)
          (goto-char (point-min))
          (forward-line new-pos)
          (insert n "->" (format "%d,%d,%s,%d" last-line curr-line n new-pos) "\n"))))))

;;; day20.el ends here
