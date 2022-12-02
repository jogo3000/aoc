;; test-input

;; "1000
;; 2000
;; 3000

;; 4000

;; 5000
;; 6000

;; 7000
;; 8000
;; 9000

;; 10000"

(let ((input
       (with-current-buffer
           (find-file-noselect "./input")
         (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
  (apply #'max
         (mapcar (lambda (s)
                   (seq-reduce (lambda (acc x) (+ acc (car x)))
                               (mapcar #'read-from-string
                                       (split-string s "\n"))
                               0))
                 (split-string input "\n\n"))))

;; --> 69795


(let* ((input
        (with-current-buffer
            (find-file-noselect "./input")
          (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
       (chunks-separated (split-string input "\n\n"))
       (elves-calories (mapcar (lambda (s)
                           (seq-reduce (lambda (acc x) (+ acc (car x)))
                                       (mapcar #'read-from-string
                                               (split-string s "\n"))
                                       0))
                         chunks-separated))
       (top-three (seq-take (sort elves-calories #'>) 3)))

  (apply #'+ top-three))

;; -> 208437




;;; day1.el ends here
