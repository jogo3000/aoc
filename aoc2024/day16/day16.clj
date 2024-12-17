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

(defn left [[y x]]
  [y (dec x)])

(defn right [[y x]]
  [y (inc x)])

(defn up [[y x]]
  [(dec y) x])

(defn down [[y x]]
  [(inc y) x])


(let [m (parse-map sample-maze-1)
      start (for [y (range (count m))
                  x (range (count (first m)))
                  :when (= \S (get-in m [y x]))]
              [y x])]
  start)
