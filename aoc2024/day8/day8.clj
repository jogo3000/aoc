(ns day8
  (:require [clojure.string :as str]))

(def sample-input
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")

(def puzzle-input (slurp "day8/input"))

(defn find-antennas [input]
  (let [m (->> input str/split-lines (map vec) vec)
        height (count m)
        width (count (first m))]
    (for [x (range height)
          y (range width)
          :let [tile (get-in m [x y])]
          :when (not= tile \.)]
      [tile [x y]])))


(defn group-antenna-locations [antennas]
  (update-vals
   (->> antennas
        (group-by first))
   #(map second %)))

(defn make-antenna-pairs [antennas]
  (let [n (count antennas)]
    (apply concat
           (apply concat
                  (for [i (range n)]
                    (for [j (range (inc i) n)]
                      [[(nth antennas i) (nth antennas j)]
                       [(nth antennas j) (nth antennas i)]]))))))


(defn find-antinode [[[x1 y1] [x2 y2]]]
  (let [xd (- x2 x1)
        yd (- y2 y1)]
    [(+ x2 xd) (+ y2 yd)]))

(defn find-all-antinodes [input]
  (let [max-x (count input)
        max-y (count (first (str/split-lines input)))]
    (->> (update-vals
          (->> input
               find-antennas
               group-antenna-locations)
          make-antenna-pairs)
         vals
         (apply concat)
         (map find-antinode)
         (filter (fn [[a b]] (and (>= (dec max-x) a 0) (>= (dec max-y) b 0))))
         (into #{})
         count)))

(find-all-antinodes sample-input) ; 14

(find-all-antinodes puzzle-input) ; 401
