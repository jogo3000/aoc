;;; Summary --- day11.el
;;; Commentary:
;;; Code:

(defvar sample-input nil)

(require 'subr-x)
(require 'seq)

(defun split-to-monkey-parts (s)
  "Split initial S to separate monkey parts."
  (split-string (string-trim s) "\n\n"))

(defun parse-op-part (op-part)
  "Make a function outta OP-PART."
  (let ((fn-part (thread-first
                   op-part
                   (split-string "=")
                   cadr)))
    (car
     (read-from-string
      (format "(lambda (old) (%s %s %s))"
              (substring fn-part 5 6)
              (substring fn-part 1 4)
              (substring fn-part 7 (string-bytes fn-part)))))))



(defun parse-test-part (test-part)
  "Make a function outta TEST-PART."
  (car (read-from-string
        (format "(lambda (x) (= 0 (mod x %s)))"
                (substring
                 test-part
                 (string-bytes "Test: divisible by ")
                 (string-bytes test-part))))))

(defun parse-starting-condition (s)
  "Parse starting condition from S."
  (thread-last
    (split-to-monkey-parts s)
    (mapcar
     (lambda (monkey-state)
       (seq-let (monkey-n item-part op-part test-part then-part else-part)
           (mapcar #'string-trim (string-lines (string-trim monkey-state)))
         `((name . ,monkey-n)
           ,(cons 'inspected-items 0)
           (items . ,(let ((parts (thread-first
                                    item-part
                                    (split-string ":")
                                    cadr
                                    (split-string ","))))
                       (mapcar #'string-to-number parts)))
           (operation . ,(parse-op-part op-part))
           (test . ,(parse-test-part test-part))
           (true-op . ,(string-to-number
                        (string-remove-prefix
                         "If true: throw to monkey "
                         then-part)))
           (false-op . ,(string-to-number
                         (string-remove-prefix
                          "If false: throw to monkey "
                          else-part)))))))))

(parse-starting-condition sample-input)

(defun perform-monkey-round (state monkey)
  "Perform actions of a single MONKEY on STATE."
  (let-alist monkey
    (seq-reduce
     (lambda (state item)
       (let* ((new-worry-level (thread-first (funcall .operation item) (/ 3)))
              (target-monkey
               (if (funcall .test new-worry-level)
                   (elt state .true-op)
                 (elt state .false-op)))
              (target-items
               (alist-get 'items target-monkey)))
         (setf (alist-get 'inspected-items monkey)
               (+ (alist-get 'inspected-items monkey) 1))
         (setf (alist-get 'items target-monkey)
               (append target-items (list new-worry-level)))
         (setf (alist-get 'items monkey)
               nil)
         state))
     .items
     state)
    state))

(defun perform-round (state)
  "Do round of monkey business on STATE."
  (seq-reduce #'perform-monkey-round state state))


(let ((state (parse-starting-condition sample-input)))
  (perform-monkey-round state (elt state 0)))

(thread-first
  (parse-starting-condition sample-input)
  perform-round ; seems to work!
  perform-round
  perform-round)

(defun monkey-business-for (input rounds)
  "Play ROUNDS of monkey business on INPUT."
  (let* ((initial-state (parse-starting-condition input))
         (final-state (thread-last
                        (seq-reduce
                         (lambda (state _)
                           (perform-round state))
                         (number-sequence 1 rounds)
                         initial-state))))

    (apply '*
           (thread-first
             (mapcar (lambda (monkey) (alist-get 'inspected-items monkey)) final-state)
             (sort #'>)
             (seq-take 2)))))

(monkey-business-for sample-input 20)
;; 10605 - ok!

(with-current-buffer (find-file-noselect "./input")
  (monkey-business-for (buffer-substring-no-properties (point-min)
                                                       (point-max))
                       20))

;; 66802

;; Part 2

(defun parse-test-part-v2 (test-part)
  "Get modcount from TEST-PART."
  (string-to-number
   (string-remove-prefix "Test: divisible by " test-part)))

(parse-test-part-v2 "Test: divisible by 19")

(defun install-worry-regulators (state)
  "Install worry regulators for STATE."
  (let ((worry-regulators (mapcar (lambda (monkey) (alist-get 'test monkey)) state)))
    (mapcar
     (lambda (monkey)
       (setf (alist-get 'worry-regulators monkey) worry-regulators)
       (setf (alist-get 'items monkey)
             (mapcar (lambda (item) (make-list (seq-length worry-regulators)
                                               item))
                     (alist-get 'items monkey)))
       monkey)
     state)))

(defun parse-starting-condition-v2 (s)
  "Parse starting condition from S."
  (thread-last
    (split-to-monkey-parts s)
    (mapcar
     (lambda (monkey-state)
       (seq-let (monkey-n item-part op-part test-part then-part else-part)
           (mapcar #'string-trim (string-lines (string-trim monkey-state)))
         `((name . ,(thread-last
                      monkey-n
                      (string-remove-prefix "Monkey ")
                      (string-remove-suffix ":")
                      string-to-number))
           ,(cons 'worry-regulators nil)
           ,(cons 'inspected-items 0)
           (items . ,(let ((parts (thread-first
                                    item-part
                                    (split-string ":")
                                    cadr
                                    (split-string ","))))
                       (mapcar #'string-to-number parts)))
           (operation . ,(parse-op-part op-part))
           (test . ,(parse-test-part-v2 test-part))
           (true-op . ,(string-to-number
                        (string-remove-prefix
                         "If true: throw to monkey "
                         then-part)))
           (false-op . ,(string-to-number
                         (string-remove-prefix
                          "If false: throw to monkey "
                          else-part)))))))
    install-worry-regulators))

(parse-starting-condition-v2 sample-input)

(defun perform-monkey-round-v2 (state monkey)
  "Perform actions of a single MONKEY on STATE."
  (let-alist monkey
    (seq-reduce
     (lambda (state worry-levels)
       (let* ((new-worry-levels (mapcar (lambda (item)
                                          (funcall .operation item))
                                        worry-levels))
              (moderated-worry-levels
               (seq-map-indexed
                (lambda (item n)
                  (let ((worry-regulator (elt .worry-regulators n)))
                    (mod item worry-regulator)))
                new-worry-levels))
              (target-monkey
               (if (= 0 (mod
                         (elt
                          moderated-worry-levels
                          .name)
                         .test))
                   (elt state .true-op)
                 (elt state .false-op)))
              (target-items
               (alist-get 'items target-monkey)))
         (setf (alist-get 'inspected-items monkey)
               (+ (alist-get 'inspected-items monkey) 1))
         (setf (alist-get 'items target-monkey)
               (append target-items (list
                                     moderated-worry-levels)))
         (setf (alist-get 'items monkey)
               nil)
         state))
     .items
     state)
    state))

(defun perform-round-v2 (state)
  "Do round of monkey business on STATE."
  (seq-reduce #'perform-monkey-round-v2 state state))

(defun monkey-business-v2-for (input rounds)
  "Play ROUNDS of monkey business on INPUT."
  (let* ((initial-state (parse-starting-condition-v2 input))
         (final-state (thread-last
                        (seq-reduce
                         (lambda (state _)
                           (perform-round-v2 state))
                         (number-sequence 1 rounds)
                         initial-state))))

    (apply '*
           (thread-first
             (mapcar (lambda (monkey) (alist-get 'inspected-items monkey)) final-state)
             (sort #'>)
             (seq-take 2)))))

(let ((state (parse-starting-condition-v2 sample-input)))
  (seq-reduce
   (lambda (state monkey)
     (perform-monkey-round-v2 state monkey))
   (seq-take state 1)
   state)
  )

(monkey-business-v2-for sample-input 1)
;; Should be:
;; == After round 1 ==
;; Monkey 0 inspected items 2 times.
;; Monkey 1 inspected items 4 times.
;; Monkey 2 inspected items 3 times.
;; Monkey 3 inspected items 6 times.
;; (+ 2 4 3 6) ; 15

(monkey-business-v2-for sample-input 2)

(monkey-business-v2-for sample-input 20)
;; == After round 20 ==
;; Monkey 0 inspected items 99 times.
;; Monkey 1 inspected items 97 times.
;; Monkey 2 inspected items 8 times.
;; Monkey 3 inspected items 103 times.
;; (+ 99 97 8 103) -> 307
;; (+ 96 100 5 102) -> 303

(monkey-business-v2-for sample-input 1000) ;; Arithmetic overflow!

;; == After round 1000 ==
;; Monkey 0 inspected items 5204 times.
;; Monkey 1 inspected items 4792 times.
;; Monkey 2 inspected items 199 times.
;; Monkey 3 inspected items 5192 times.

(monkey-business-v2-for sample-input 10000)
;; 2713310158
;; 2713310158 -- correct!
;; == After round 10000 ==
;; Monkey 0 inspected items 52166 times.
;; Monkey 1 inspected items 47830 times.
;; Monkey 2 inspected items 1938 times.
;; Monkey 3 inspected items 52013 times.

(with-current-buffer (find-file-noselect "./input")
  (monkey-business-v2-for (buffer-substring-no-properties (point-min) (point-max))
                          10000))

;; 21800916620



(setq sample-input
      "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

;;; day11.el ends here
