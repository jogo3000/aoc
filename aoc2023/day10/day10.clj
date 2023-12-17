(ns day10
  (:require [clojure.string :as str]))

(def sample-1 ".....
.S-7.
.|.|.
.L-J.
.....")

(defn parse-input [s]
  (->> s
       (str/split-lines)
       (mapv vec)))

(defn find-start [m]
  (let [y (keep-indexed
           (fn [i row]
             (let [pos
                   (keep-indexed (fn [j c]
                                   (when (= c \S)
                                     [i j])) row)]
               (when-not (empty? pos)
                 pos))) m)]
    (ffirst y)))

(defn north [[y x]]
  [(dec y) x])

(defn south [[y x]]
  [(inc y) x])

(defn west [[y x]]
  [y (dec x)])

(defn east [[y x]]
  [y (inc x)])

(defn possible-steps [m [y x :as pos]]
  (let [pipe (get-in m [y x])]
    (case pipe
      \| [(north pos) (south pos)]
      \- [(east pos) (west pos)]
      \L [(north pos) (east pos)]
      \J [(north pos) (west pos)]
      \7 [(south pos) (west pos)]
      \F [(south pos) (east pos)]
      \S (into []
               (comp
                (filter (fn [pos'] (every? #(>= % 0) pos')))
                (map (juxt identity #(possible-steps m %)))
                (filter (comp (complement empty?) second))
                (keep (fn [[pos' dirs]]
                       (when (some (fn [p] (= p pos)) dirs)
                         pos'))))
               [(north pos) (east pos) (south pos) (west pos)])
      \. []
      (throw (Exception. (str "How did I end up in " pos))))))

(-> sample-1
    parse-input
    (possible-steps [1 1]))

(defn follow-pipe [m pos dir]
  (->> (possible-steps m dir)
       (filter #(not= % pos))
       first))

(defn find-furthest [input]
  (let [m (parse-input input)
        [y x] (find-start m)
        pipe-ends
        (possible-steps m [y x])]
    (loop [dist 1
           pipe1 (list (first pipe-ends) [y x])
           pipe2 (list (second pipe-ends) [y x])]
      (if (= (first pipe1) (first pipe2))
        dist
        (recur (inc dist)
               (list (follow-pipe m (second pipe1) (first pipe1)) (first pipe1))
               (list (follow-pipe m (second pipe2) (first pipe2)) (first pipe2)))))))

(find-furthest sample-1)

(def sample-2
  "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(-> sample-2
    (find-furthest))

(def sample-3
  "..F7.
.FJ|.
SJ.L7
|F--J
LJ...")

(-> sample-3
    parse-input
    (possible-steps [2 0]))

(find-furthest sample-3)

(def sample-4 "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(find-furthest sample-4)

(find-furthest (slurp "day10/input.txt"))
;;6927

;; Part deux
(defn find-pipe [input]
  (let [m (parse-input input)
        [y x] (find-start m)
        pipe-ends
        (possible-steps m [y x])]
    (loop [dist 1
           pipe1 (list (first pipe-ends) [y x])
           pipe2 (list (second pipe-ends) [y x])]
      (if (= (first pipe1) (first pipe2))
        (into pipe1 (rest pipe2))
        (recur (inc dist)
               (cons (follow-pipe m (second pipe1) (first pipe1)) pipe1)
               (cons (follow-pipe m (second pipe2) (first pipe2)) pipe2))))))

(find-pipe sample-1)

(def sample-5
  "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(find-pipe sample-5)
'([1 1] [1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [1 8] [1 9] [2 9] [3 9] [4 9] [5 9] [6 9] [7 9] [7 8] [7 7] [7 6] [6 6] [5 6] [5 7] [5 8] [4 8] [3 8] [2 8] [2 7] [2 6] [2 5] [2 4] [2 3] [2 2] [3 2] [4 2] [5 2] [5 3] [5 4] [6 4] [7 4] [7 3] [7 2] [7 1] [6 1] [5 1] [4 1] [3 1] [2 1] [1 1])

(defn direction [[y x] [y' x']]
  (cond
    (and (= y y') (= (inc x) x')) :east
    (and (= y y') (= (dec x) x')) :west
    (and (= (inc y) y') (= x x')) :south
    (and (= (dec y) y') (= x x')) :north
    :else (throw (Exception. (str "Should not happen!" [y x] [y' x'])))))

(defn left [pos dir]
  (case dir
    :east (north pos)
    :west (south pos)
    :south (east pos)
    :north (west pos)))

(defn right [pos dir]
  (case dir
    :east (south pos)
    :west (north pos)
    :south (west pos)
    :north (east pos)))


(defn neighbouring-cells [pipe side]
  (let [pipe-cells (set pipe)]
    (disj (into #{}
                (mapcat identity)
                (for [[p p'] (partition 2 1 pipe)]
                  (let [r (side p (direction p p'))
                        r' (side p' (direction p p'))]
                    [(when-not (or (pipe-cells r)
                                   (some neg? r)) r)
                     (when-not (or (pipe-cells r')
                                   (some neg? r')) r')]))) nil)))

(defn find-chirality [pipe]
  (->> pipe
       (partition 3 1)
       (map (fn [[p x n]]
              (case [(direction p x)
                     (direction x n)]
                [:north :east] :right
                [:north :west] :left
                [:east :south] :right
                [:east :north] :left
                [:south :west] :right
                [:south :east] :left
                [:west :north] :right
                [:west :south] :left
                nil)))
       (remove nil?)
       (frequencies)
       (apply max-key second)
       first))

(def sample-6 ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(defn visualize-part-with [input part c]
  (str/join \newline
            (map-indexed (fn [y row]
                           (str/join (map-indexed (fn [x c']
                                                    (if (part [y x])
                                                      c c')) row)))
                         (str/split-lines input))))

(defn visualize-pipe [input]
  (let [pipe (set (find-pipe input))]
    (visualize-part-with input pipe \*)))

(visualize-pipe sample-6)

(let [pipe (find-pipe sample-6)
      left-sides (set (neighbouring-cells pipe left))
      right-sides (set (neighbouring-cells pipe right))]
  (->>
   (interleave
    (visualize-part-with sample-6 (set pipe) \*)
    (visualize-part-with sample-6 left-sides \O)
    (visualize-part-with sample-6 right-sides \I))
   (partition 3)
   (map (fn [[a b c]]
          (cond
            (= c \I) c
            (= b \O) b
            :else a)))
   str/join
   println ))

(println sample-6)

(println (visualize-pipe sample-6))

(defn find-interior-cells [input]
  (let [pipe (find-pipe input)
        chirality (find-chirality pipe)
        some-interior-cells (neighbouring-cells pipe
                                                (if (= chirality :left)
                                                  left
                                                  right))
        pipe-cells (set pipe)
        queue (into '() some-interior-cells)]
    (loop [covered #{}
           queue (into '() some-interior-cells)]
      (if (empty? queue)
        covered
        (recur (conj covered (first queue))
               (into (rest queue)
                     (filter #(and (not (pipe-cells %))
                                   (not (covered %))))
                     (let [head (first queue)]
                       [(north head)
                        (west head)
                        (south head)
                        (east head)])))))))

(find-interior-cells sample-1)
(find-interior-cells sample-3)

(find-interior-cells sample-5)
(find-interior-cells sample-6)

(def sample-7 ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(count (find-interior-cells sample-7))

(def sample-8 "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(count (find-interior-cells sample-8))

(->> (slurp "day10/input.txt")
     find-interior-cells
     count)

;; 467
