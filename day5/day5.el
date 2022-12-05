;;; Summary ---  day5.el
;;; Commentary:
;;; Code:

(require 'cl-macs)
(require 'seq)

(defvar sample-input
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defun parse-stacks (stacks-part)
  "Parse initial stacks configuration from STACKS-PART."
  (let* ((stacks-reversed (seq-reverse (string-lines stacks-part)))
         (number-of-stacks (car
                            (read-from-string (car (last (split-string (string-trim (car stacks-reversed))
                                                                       "\s\s"
                                                                       nil
                                                                       "\s")))))))
    (seq-reduce
     (lambda (acc row)
       (seq-reduce
        (lambda (acc-inner numbered-row)
          (cl-destructuring-bind (n container?) numbered-row
            (let ((v? (substring container? 1 2)))
              (if (not (string-equal " " v?))
                  (apply #'seq-concatenate
                         'list
                         (seq-take acc-inner n)
                         (list (cons v? (seq-elt acc-inner n)))
                         (list (seq-drop acc-inner (+ 1 n))))
                acc-inner))))
        (seq-map-indexed (lambda (v i)
                           (list i v)) (seq-partition row 4))
        acc))
     (cdr stacks-reversed)
     (make-list number-of-stacks (list)))))

(defun stack-set (stack v n)
  "Set element V to STACK in position N."
  (apply #'seq-concatenate
         'list
         (seq-take stack n)
         (list v)
         (list (seq-drop stack (+ 1 n)))))

(defun run-instruction (stack instruction)
  "Run INSTRUCTION on STACK."
  (cl-destructuring-bind (amount from to)
      (seq-map
       #'read-from-string
       (split-string instruction (rx (or "move" "from" "to")) t "\s"))

    (let* ((from (- (car from) 1))
           (to (- (car to) 1))
           (amount (car amount))
           (original-from (seq-elt stack from))
           (original-to (seq-elt stack to))
           (new-to (seq-concatenate 'list (seq-reverse (seq-take original-from amount)) original-to))
           (new-from (seq-drop original-from amount))

           (from-replaced (stack-set stack new-from from)))
      (stack-set from-replaced new-to to))))

(require 'subr-x)

(defun run-simulation (input)
  "Run simulation in INPUT."
  (cl-destructuring-bind (stacks-part instructions) (split-string input "\n\n")
    (let* ((stacks
            (parse-stacks stacks-part))

           (reorganized-stacks
            (seq-reduce
             #'run-instruction
             (string-lines instructions)
             stacks)))

      (string-join (mapcar #'car reorganized-stacks)))))


(run-simulation sample-input)
;; "CMZ"


(with-current-buffer (find-file-noselect "./input")
  (run-simulation (string-trim-right (buffer-substring-no-properties (point-min) (point-max)))))

;; "TDCHVHJTG"


;; part 2

;;; day5.el ends here
