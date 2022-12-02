;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'cl-macs)

(defun parse-choice (choice)
  "Parse CHOICE to keyword."
  (pcase choice
    ("A" :rock)
    ("B" :paper)
    ("C" :scissors)
    ("X" :rock)
    ("Y" :paper)
    ("Z" :scissors)))

(defun shape->points (x)
  "Return points for a given shape X."
  (pcase x
    (:rock 1)
    (:paper 2)
    (:scissors 3)))

(defun round->points (their-choice my-choice)
  "Calculate points for a round of THEIR-CHOICE and MY-CHOICE."
  (+
   (shape->points my-choice)
   (pcase my-choice
     (:rock (pcase their-choice
              (:rock 3)
              (:paper 0)
              (:scissors 6)))
     (:paper (pcase their-choice
               (:rock 6)
               (:paper 3)
               (:scissors 0)))
     (:scissors (pcase their-choice
                  (:rock 0)
                  (:paper 6)
                  (:scissors 3))))))

(with-current-buffer
    (find-file-noselect "./input")
  (let* ((input (string-trim (buffer-substring-no-properties (point-min) (point-max))))
         (input-lines (string-lines input)))
    (seq-reduce
     (lambda (acc round)
       (cl-destructuring-bind (their mine) (split-string round " ")
         (+ acc (round->points (parse-choice their) (parse-choice mine)))))
     input-lines
     0)))

;;; 11906


(defun round->points-v2 (their-choice result)
  "V2 version of point calculation based on THEIR-CHOICE and desired RESULT."
  (let ((parsed-result (pcase result
                         ("X" :lose)
                         ("Y" :draw)
                         ("Z" :win))))
    (pcase parsed-result
      (:draw (round->points their-choice their-choice))
      (:lose (round->points their-choice (pcase their-choice
                                           (:rock :scissors)
                                           (:paper :rock)
                                           (:scissors :paper))))
      (:win (round->points their-choice (pcase their-choice
                                          (:rock :paper)
                                          (:paper :scissors)
                                          (:scissors :rock)))))))

(with-current-buffer
    (find-file-noselect "./input")
  (let* ((input (string-trim (buffer-substring-no-properties (point-min) (point-max))))
         (input-lines (string-lines input)))
    (seq-reduce
     (lambda (acc round)
       (cl-destructuring-bind (their result) (split-string round " ")
         (+ acc (round->points-v2 (parse-choice their) result))))
     input-lines
     0)))

;; -> 11186







;;; day2.el ends here
