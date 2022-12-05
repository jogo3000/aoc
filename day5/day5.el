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

(defun parse-number-of-stacks (row)
  "Parse number of stacks from ROW."
  (string-match (rx (seq (0+ (seq (0+ whitespace)
                                  (1+ numeric)
                                  (0+ whitespace)))
                         (group (1+ numeric))
                         (0+ whitespace))) row)
  (car (read-from-string (match-string 1 row))))

(defun stack-set (stack v n)
  "Set element V to STACK in position N."
  (apply #'seq-concatenate
         'list
         (seq-take stack n)
         (list v)
         (list (seq-drop stack (+ 1 n)))))

(defun parse-stacks-row (stacks row)
  "Parse stack state defining ROW into STACKS."
  (seq-reduce
   (lambda (acc-inner numbered-row)
     (cl-destructuring-bind (n container?) numbered-row
       (let ((v? (substring container? 1 2)))
         (if (not (string-equal " " v?))
             (stack-set acc-inner (cons v? (seq-elt acc-inner n)) n)
           acc-inner))))
   (seq-map-indexed (lambda (v i)
                      (list i v)) (seq-partition row 4))
   stacks))

(defun parse-stacks (stacks-part)
  "Parse initial stacks configuration from STACKS-PART."
  (let* ((stacks-reversed (seq-reverse (string-lines stacks-part)))
         (numbers-row (car stacks-reversed))
         (number-of-stacks (parse-number-of-stacks numbers-row)))
    (seq-reduce
     #'parse-stacks-row
     (cdr stacks-reversed)
     (make-list number-of-stacks (list)))))

(defun run-instruction (stack instruction)
  "Run INSTRUCTION on STACK."
  (cl-destructuring-bind (amount from to)
      (mapcar #'car
              (mapcar
               #'read-from-string
               (split-string instruction (rx (or "move" "from" "to")) t "\s")))

    (let* ((from (- from 1))
           (to (- to 1))
           (amount amount)
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

(defun run-instruction-2 (stack instruction)
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
           (new-to (seq-concatenate 'list (seq-take original-from amount) original-to))
           (new-from (seq-drop original-from amount))

           (from-replaced (stack-set stack new-from from)))
      (stack-set from-replaced new-to to))))

(defun run-simulation-2 (input)
  "Run simulation in INPUT."
  (cl-destructuring-bind (stacks-part instructions) (split-string input "\n\n")
    (let* ((stacks
            (parse-stacks stacks-part))

           (reorganized-stacks
            (seq-reduce
             #'run-instruction-2
             (string-lines instructions)
             stacks)))

      (string-join (mapcar #'car reorganized-stacks)))))


(run-simulation-2 sample-input)

(with-current-buffer (find-file-noselect "./input")
  (run-simulation-2 (string-trim-right (buffer-substring-no-properties (point-min) (point-max)))))

;; "NGCMPJLHV"



;;; day5.el ends here
