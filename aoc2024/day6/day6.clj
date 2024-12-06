(ns day6
  (:require [clojure.string :as str]))

(def sample-map
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

(defn next-dir [dir]
  (case dir
    [-1  0] [ 0  1]
    [0  1] [ 1  0]
    [1  0] [ 0 -1]
    [0 -1] [-1  0]))

(defn find-route [input]
  (let [m (->> (str/split-lines input)
               (map vec)
               vec)
        starting-loc (first (for [i (range (count m))
                                  j (range (count (first m)))
                                  :let [c (get-in m [i j])]
                                  :when (= \^ c)]
                              [i j]))
        starting-dir [-1 0]]
    (loop [positions []
           loc starting-loc
           dir starting-dir]
      (let [adj-loc (map + loc dir)
            adj-cell (get-in m adj-loc)]
        (case adj-cell
          nil (conj positions loc)
          \# (recur positions loc (next-dir dir))
          (recur (conj positions loc) adj-loc dir))))))

(defn solve-1 [input]
  (->> input find-route distinct count))

(solve-1 sample-map) ; 41

(solve-1 (slurp "day6/input")) ; 4973
