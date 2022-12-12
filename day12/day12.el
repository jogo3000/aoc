;;; Summary --- day12.el
;;; Commentary:
;;; Looks like it'll make a lot of sense using buffers as data structures.
;;; I can get the character at point with =char-to-string= and characters next
;;; to it by using arithmetic or =next-line=, =previous-line=, =backward-char=,
;;; =forward-char=
;;;
;;; Code:

(require 'subr-x)

(defun find-marker (marker)
  "Find MARKER in buffer of BUFFER-WIDTH."
  (setf (point) (point-min))
  (let ((starting-point (search-forward marker)))
    (backward-char)
    (point)))

(defun above (p width)
  "Return position above P in a buffer of WIDTH."
  (let ((new-pos (- p (+ width 1))))
    (when (> new-pos 0)
      new-pos)))

(defun below (p width)
  "Return position below P in a buffer of WIDTH."
  (+ p (+ width 1)))

(defun left (p width)
  "Return position left of P in a buffer of WIDTH."
  (when (> (mod p (+ width 1)) 1)
    (- p 1)))

(left 1 8)
(left 8 8)
(left 9 8)
(left 17 8)
(left 18 8)

(defun right (p width)
  "Return position right of P in a buffer of WIDTH."
  (when (> (mod p (+ width 1)) 0)
    (+ p 1)))

(right 27 8)

(defun some-found (tasks)
  "Return non-nil when some of the TASKS has found the goal."
  (seq-some
   (lambda (x)
     (when (= 83 ;; S
              (char-after (car x)))
       x))
   tasks))

(require 'seq)

(defun legal-move? (here there)
  "Return non-nil if move is legal between HERE and THERE.

This works because characters in alphabet are in correct order:
[97 98 99 100 101 102 103 104 105 106 107 108 109
 110 111 112 113 114 115 116 117 118 119 120 121 122]."
  (when (and here there)
    ;(message "looking at %s:%s" (char-to-string here) (char-to-string there))

    (let ((regulated-there (pcase there
                             (83 97)  ;; Start level with 'a'
                             (69 122) ;; End level with 'z'
                             (_ there)))
          (regulated-here (pcase here
                            (83 97)
                            (69 122)
                            (_ here))))
      (<= (- regulated-here regulated-there) 1))))

(let ((width 8)
      (heigth 5))
  (with-current-buffer (find-file-noselect "./sample-input")
    (let* ((start-marker (find-marker "S"))
           (end-marker (find-marker "E"))
           (tasks (list (list end-marker)))
           (rounds 0))
      (while (and (< rounds 100) (not (some-found tasks)))
        (setq rounds (+ rounds 1))
        (setq tasks
              (seq-uniq
               (mapcan (lambda (task)
                         (let* ((pos (car task))
                                (current-char (char-after pos)))
                           (seq-reduce
                            (lambda (acc new-pos)
                              (if (and new-pos
                                       (not (> new-pos (point-max)))
                                       (not (seq-contains-p task new-pos))
                                       (legal-move? current-char (char-after new-pos)))
                                  (cons (cons new-pos task) acc)
                                acc))
                            (list
                             (left pos width)
                             (right pos width)
                             (above pos width)
                             (below pos width))
                            nil))) tasks))))
      (some-found tasks))))

(let ((width 180))
  (with-current-buffer (find-file-noselect "./input")
    (let* ((start-marker (find-marker "S"))
           (end-marker (find-marker "E"))
           (tasks (list (list end-marker)))
           (rounds 0))
      (while (and (< rounds 100) (not (some-found tasks)))
        (setq rounds (+ rounds 1))
        (setq tasks
              (seq-uniq
               (mapcan (lambda (task)
                         (let* ((pos (car task))
                                (current-char (char-after pos)))
                           (seq-reduce
                            (lambda (acc new-pos)
                              (if (and new-pos
                                       (not (> new-pos (point-max)))
                                       (not (seq-contains-p task new-pos))
                                       (legal-move? current-char (char-after new-pos)))
                                  (cons (cons new-pos task) acc)
                                acc))
                            (list
                             (left pos width)
                             (right pos width)
                             (above pos width)
                             (below pos width))
                            nil))) tasks))))
      (seq-length (some-found tasks)))))


;;; day12.el ends here
