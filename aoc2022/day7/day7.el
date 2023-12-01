;;; Summary --- day7.el
;;; Commentary:
;;; Code:

(defvar sample-data
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(require 'seq)

(defun split-into-commands (s)
  "Splits S into commands (and thier outputs)."
  (seq-rest (mapcar #'string-trim (split-string s (rx (and line-start  "$"))))))

(require 'map)

(defun parse-ls-output (output-lines)
  "Parse ls OUTPUT-LINES into alist."
  (mapcar (lambda (line)
            (seq-let (type-or-size name) (split-string line)
              (pcase type-or-size
                ("dir" (list name nil))
                (_ (list name (car (read-from-string type-or-size)))))))
          output-lines))

(defun current-dir (stack)
  (caaar stack))

(defun handle-cd-/ (root stack command output-lines)
  (list root))

(defun handle-ls (root stack command output-lines)
  (map-put! (car stack) (current-dir stack)
            (list (parse-ls-output output-lines)))
  stack)

(defun handle-cd.. (root stack command output-lines)
  (cdar stack))

(defun handle-cd-dir (root stack command output-lines)
  (seq-let (_ dir) (split-string command)
    (list (cons (assoc dir (cadr (assoc (current-dir stack) (car stack)))) stack))))

(defvar day7-root nil)
(defvar day7-stack nil)

(defun parse-data (s)
  "Form a tree structure from S."
  (setq day7-root (list (list "/" 0)))
  (setq day7-stack (list day7-root))

  (seq-do (lambda (command-and-output)
            (seq-let (command &rest output-lines) (string-lines command-and-output)
              (let ((new-stack
                     (pcase command
                       ("cd /" (handle-cd-/ day7-root day7-stack command output-lines))
                       ("ls" (handle-ls day7-root day7-stack command output-lines))
                       ("cd .." (handle-cd.. day7-root day7-stack command output-lines))
                       ((and (rx (seq "cd" whitespace (one-or-more alphanumeric)))
                             cd-command)
                        (seq-let (_ dir) (split-string cd-command)
                          (handle-cd-dir day7-root day7-stack cd-command output-lines))))))
                (setq day7-stack new-stack))))
          (split-into-commands s))
  day7-root)

(parse-data sample-data)

(defvar day7-sizes nil)

(require 'subr-x)

(defun day7-file-content-sizes (dir)
  (apply #'+
         (mapcar #'cadr
                 (map-filter (lambda (k v)
                               (integerp (car v))) dir))))

(defun day7-du (here dir)
  "Find size of DIR in HERE."
  (let ((file-content-size (apply #'+
                                  (mapcar #'cadr
                                          (map-filter (lambda (k v)
                                                        (integerp (car v))) dir))))
        (subdir-sizes (thread-last
                        dir
                        (map-filter (lambda (k v) (and (listp (car v))
                                                       (< 0 (seq-length (car v))))))
                        (map-apply (lambda (k v)
                                     (day7-du k (car v)))))))

    (message "here %s dir %s" here dir)
    (message "here %s - file-content %d -- subdir %s" here file-content-size subdir-sizes)
    (if (not subdir-sizes)
        (list here file-content-size)
        (apply #'seq-concatenate 'list
         (list
          here
          (+ file-content-size
             (thread-last
               subdir-sizes
               (mapcar #'cadr)
               (seq-filter #'integerp)
               (apply #'+))))
         subdir-sizes))))


(day7-du "/" (parse-data sample-data))
;; ("/" 48381165 "/" 48381165 "a" 94853 "e" 584 "d" 24933642)


(thread-last
  sample-data
  parse-data
  (day7-du "/")
  (seq-filter (lambda (v) (and (integerp v)
                                        (>= 100000 v))))
  (apply #'+))

(with-current-buffer (find-file-noselect "./input")
  (thread-last
    (string-trim-right (buffer-substring-no-properties (point-min) (point-max)))
    parse-data
    (day7-du "/")
    (seq-filter (lambda (v) (and (integerp v)
                                 (>= 100000 v))))
    (apply #'+)))

;; 1844187
;; 4978279

;;; part 2

(let* ((total-space 70000000)
       (needed-space 30000000)
       (du
        (thread-last
          sample-data
          parse-data
          (day7-du "/")))
       (free-space (- total-space (lax-plist-get du "/"))))
  (apply #'min (seq-filter (lambda (v) (and (integerp v) (<= needed-space (+ free-space v)))) du)))


(with-current-buffer (find-file-noselect "./input")
  (let* ((total-space 70000000)
         (needed-space 30000000)
         (du
          (thread-last
            (string-trim-right (buffer-substring-no-properties (point-min) (point-max)))
            parse-data
            (day7-du "/")))
         (free-space (- total-space (lax-plist-get du "/"))))
    (apply #'min (seq-filter (lambda (v) (and (integerp v) (<= needed-space (+ free-space v)))) du))))

;; 4978279




;;; day7.el ends here
