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

(find-furthest (slurp "/home/jogo3000/git/aoc2022/aoc2023/day10/input.txt"))
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
    (disj (set
           (for [[p p'] (partition 2 1 pipe)]
             (let [r (side p (direction p p'))]
               (when-not (or (pipe-cells r)
                             (some neg? r)) r)))) nil)))

(let [pipe (find-pipe sample-1)]
  [(neighbouring-cells pipe left)
   (neighbouring-cells pipe right)])

[#{[2 2]}
 #{[1 0] [3 4] [4 2] [4 1] [0 3] [2 4] [0 2] [2 0]}]

(let [pipe (find-pipe sample-3)]
  [(neighbouring-cells pipe left)
   (neighbouring-cells pipe right)])

[#{[2 2]}
 #{[4 3] [1 0] [4 2] [1 4] [5 0] [3 5] [0 1]}]

(let [pipe (find-pipe sample-5)]
  [(neighbouring-cells pipe left)
   (neighbouring-cells pipe right)])

[#{[6 7] [6 3] [6 8] [6 2]}
 #{[8 8] [8 7] [4 3] [1 0] [8 3] [0 6] [3 3] [5 10] [0 5] [3 4] [8 6] [3 0] [4 7] [4 10] [6 5] [0 9] [8 2] [8 1] [0 3] [0 7] [5 5] [3 6] [7 10] [0 2] [2 0] [0 4] [3 10] [6 10] [4 4] [3 7] [2 10] [7 5] [5 0] [6 0] [3 5] [0 8] [4 0]}]


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

(let [pipe (find-pipe sample-6)]
  [pipe
   (neighbouring-cells pipe left)
   (neighbouring-cells pipe right)])

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
    (visualize-part-with sample-6 left-sides \I)
    (visualize-part-with sample-6 right-sides \O))
   (partition 3)
   (map (fn [[a b c]]
          (cond
            (and (= b \I) (= c\O)) \!
            (= b \I) b
            (= c \O) c
            :else a)))
   str/join
   println ))

(println sample-6)

(println (visualize-pipe sample-6))
