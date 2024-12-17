(ns day16
  (:require [clojure.string :as str]))

;; The Reindeer start on the Start Tile (marked S) facing East and need to reach
;; the End Tile (marked E). They can move forward one tile at a time (increasing
;; their score by 1 point), but never into a wall (#). They can also rotate
;; clockwise or counterclockwise 90 degrees at a time (increasing their score by
;; 1000 points).

;; Analyze your map carefully. What is the lowest score a Reindeer could
;; possibly get?


(def sample-maze-1
  "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
") ;; best score 7036

(def sample-maze-2
  "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
") ;; best score 11048

(defn parse-map [input]
  (->> input
       str/split-lines
       (mapv vec)))

(defn west [[y x]]
  [y (dec x)])

(defn east [[y x]]
  [y (inc x)])

(defn north [[y x]]
  [(dec y) x])

(defn south [[y x]]
  [(inc y) x])

(defn find-symbol [S m]
  (for [y (range (count m))
        x (range (count (first m)))
        :when (= S (get-in m [y x]))]
    [y x]))

(defn find-start [m]
  (first (find-symbol \S m)))

(defn find-end [m]
  (first (find-symbol \E m)))

(def valid-dest? #{\E \.})

(defn find-paths [maze]
  (let [m (parse-map maze)
        start (find-start m)
        end (find-end m)]
    (loop [queue [{:path []
                   :pos start
                   :dir east}]
           paths []]
      (if (empty? queue) paths
          (let [[{:keys [path pos dir]} & todo] queue]
            #_(println pos (count queue) (count paths))
            (if (= pos end) (recur todo (conj paths path))
                (let [forward-tile (get-in m (dir pos))
                      left ({north west west south south east east north} dir)
                      right ({north east east south south west west north} dir)
                      left-tile (get-in m (left pos))
                      right-tile (get-in m (right pos))]
                  (recur (-> todo
                             (into (when (and (valid-dest? left-tile)
                                              (not (fn? (peek path))))
                                     [{:path (conj path left)
                                       :pos pos
                                       :dir left}]))
                             (into (when (and (valid-dest? right-tile)
                                              (not (fn? (peek path))))
                                     [{:path (conj path right)
                                       :pos pos
                                       :dir right}]))
                             (into (when (and (valid-dest? forward-tile)
                                              (not ((into #{} path) (dir pos))))
                                     [{:path (conj path (dir pos))
                                       :pos (dir pos)
                                       :dir dir}])))
                         paths))))))))

(defn evaluate-path [path]
  (reduce (fn [acc move]
            (if (fn? move) (+ acc 1000)
                (inc acc))) 0 path))

(->> sample-maze-1
     find-paths
     (map evaluate-path)
     sort
     first) ; 7036, correct

(->> sample-maze-2
     find-paths
     (map evaluate-path)
     sort first) ; 11048, correct


#_(->> (slurp "day16/input")
     find-paths
     (map evaluate-path)
     sort first)
