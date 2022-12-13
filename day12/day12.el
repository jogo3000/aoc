;;; Summary --- day12.el
;;; Commentary:
;;; Looks like it'll make a lot of sense using buffers as data structures.
;;; I can get the character at point with =char-to-string= and characters next
;;; to it by using arithmetic or =next-line=, =previous-line=, =backward-char=,
;;; =forward-char=
;;;
;;; Code:

(require 'subr-x)

(defun above (p width)
  "Return position above P in a buffer of WIDTH."
  (let ((new-pos (- p width)))
    (when (>= new-pos 0)
      new-pos)))

(defun below (p width)
  "Return position below P in a buffer of WIDTH."
  (+ p width))

(defun left (p width)
  "Return position left of P in a buffer of WIDTH."
  (when (>= (mod p width) 1)
    (- p 1)))

(defun right (p width)
  "Return position right of P in a buffer of WIDTH."
  (when (or (= 0 p) (> (mod p width) 0))
    (+ p 1)))

(let ((input "SabqponmabcryxxlaccszExkacctuvwjabdefghi"))
 ;; test area
  (string-match "S" input)
  )

(require 'seq)

(defun legal-move? (here there)
  "Return non-nil if move is legal between HERE and THERE.

This works because characters in alphabet are in correct order:
[97 98 99 100 101 102 103 104 105 106 107 108 109
 110 111 112 113 114 115 116 117 118 119 120 121 122]."
  (when (and here there)

    (let ((regulated-there (pcase there
                             (83 97)  ;; Start level with 'a'
                             (69 122) ;; End level with 'z'
                             (_ there)))
          (regulated-here (pcase here
                            (83 97)
                            (69 122)
                            (_ here))))
      (<= (- regulated-there regulated-here) 1))))

;; Let's try Djiktra's

;;  1  function Dijkstra(Graph, source):
;;  2
;;  3      for each vertex v in Graph.Vertices:
;;  4          dist[v] ← INFINITY
;;  5          prev[v] ← UNDEFINED
;;  6          add v to Q
;;  7      dist[source] ← 0
;;  8
;;  9      while Q is not empty:
;; 10          u ← vertex in Q with min dist[u]
;; 11          remove u from Q
;; 12
;; 13          for each neighbor v of u still in Q:
;; 14              alt ← dist[u] + Graph.Edges(u, v)
;; 15              if alt < dist[v]:
;; 16                  dist[v] ← alt
;; 17                  prev[v] ← u
;; 18
;; 19      return dist[], prev[]

(require 'cl-seq)
(left 1 181)

(defun djikstra (input width)
  (setq case-fold-search nil)
  (let ((max-pos (seq-length input))
        (source (string-match "S" input))
        (target (string-match "E" input))
        (map (string-to-list input))
        (prev (make-list (seq-length input) nil))
        (dist (make-list (seq-length input) most-positive-fixnum)))
    (message "target here: %s" target)
    (setf (elt dist source) 0)

    (let ((Q (number-sequence 0 (- (seq-length input) 1)))
          (found nil))
      (while (not (or (seq-empty-p Q) found))
        (let ((u (cl-reduce
                  (lambda (acc n)
                    (let ((dist-acc (elt dist acc))
                          (dist-n (elt dist n)))
                      (if (< dist-n dist-acc) n acc)))
                  Q)))
          (when (= u target)
            (progn (message "found!")
                   (setq found t)))
          (setq Q (delete u Q))

          (seq-do
           (lambda (v)
             (let ((alt (+ (elt dist u) 1)))
               (when (< alt (elt dist v))
                 (setf (elt dist v) alt)
                 (setf (elt prev v) u))))
           (seq-filter
            (lambda (v) (and v
                             (seq-contains-p Q v)
                             (legal-move? (elt input u)
                                          (elt input v))))
            (list
             (left u width)
             (right u width)
             (above u width)
             (below u width))))))
      (list dist prev))))

;; 1  S ← empty sequence
;; 2  u ← target
;; 3  if prev[u] is defined or u = source:          // Do something only if the vertex is reachable
;; 4      while u is defined:                       // Construct the shortest path with a stack S
;; 5          insert u at the beginning of S        // Push the vertex onto the stack
;; 6          u ← prev[u]                           // Traverse from target to source

(defun shortest-path (input dist prev)
  (let ((S nil)
        (u (string-match "E" input))
        (source (string-match "S" input)))
    (message "%s" (seq-length (seq-filter 'identity prev)))
    (message "%s - %s - %s" u (elt prev u) source)
    (when (or (elt prev u) (= u source))
      (message "Tänne tultiin %s" (elt prev u))
      (while u
        (setq S (cons u S))
        (setq u (elt prev u))))
    S))

(with-current-buffer (find-file-noselect "./sample-input")
  (let ((input (thread-last (buffer-substring-no-properties (point-min) (point-max))
                            (string-replace "\n" ""))))
    (seq-let (dist prev) (djikstra input 8)
      (- (seq-length (shortest-path input dist prev)) 1)
      )))


(defvar day12-djikstra
  (with-current-buffer (find-file-noselect "./input")
    (setq case-fold-search nil)
    (let ((input (thread-last (buffer-substring-no-properties (point-min) (point-max))
                              (string-replace "\n" ""))))
      (djikstra input 181))))


(with-current-buffer (find-file-noselect "./input")
  (setq case-fold-search nil)
  (let ((input (thread-last (buffer-substring-no-properties (point-min) (point-max))
                            (string-replace "\n" ""))))

    (seq-let (dist prev) day12-djikstra
      ; dist
       (- (seq-length (shortest-path input dist prev)) 1)
      )))

(with-current-buffer (find-file-noselect "./input")
  (setq case-fold-search nil)
  (let ((input (thread-last (buffer-substring-no-properties (point-min) (point-max))
                            )))
    (seq-length (string-to-list (car (string-lines input))))))

(with-current-buffer (find-file-noselect "./sample-2")
  (let ((input (thread-last (buffer-substring-no-properties (point-min) (point-max))
                            (string-replace "\n" ""))))
    (seq-let (dist prev) (djikstra input 74)
      (seq-length (shortest-path input dist prev)))))

(with-current-buffer (find-file-noselect "./sample-input3")
  (setq case-fold-search nil)
  (let ((input (thread-last (buffer-substring-no-properties (point-min) (point-max))
                            (string-replace "\n" ""))))
    (seq-let (dist prev) (djikstra input 10)
      (shortest-path input dist prev))))

;;; day12.el ends here
