(ns day11
  (:require [clojure.string :as str]))

(def sample-1 "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
")

(def sample-1-expansion "....#........
.........#...
#............
.............
.............
........#....
.#...........
............#
.............
.............
.........#...
#....#.......
")

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv #(mapv identity %))))

(def original-1 (parse-input sample-1))

(defn find-voids [m]
  (let [rows (set (range (count m)))
        cols (set (range (count (first m))))]
    (reduce (fn [acc [y x]]
              [(disj (first acc) y)
               (disj (second acc) x)])
            [rows cols]
            (for [y (range (count m))
                  x (range (count (first m)))]
              (when (= \# (get-in m [y x]))
                [y x])))))

(defn fill-col-voids [m col-voids]
  (let [height (count m)
        width (count (first m))]
    (->> col-voids
         sort
         ((fn [vs]
            (concat [0] vs [height])))
         (partition 2 1)
         (map #(subvec m (first %) (second %)))
         (reduce (fn [acc m]
                   (-> acc
                       (into m)
                       (into [(vec (repeat width \.))])))
                 []))))

(defn fill-row-voids [row row-voids]
  (let [width (count row)]
    (->> row-voids
         sort
         ((fn [vs]
            (concat [0] vs [width])))
         (partition 2 1)
         (map #(subvec row (first %) (second %)))
         (reduce (fn [acc m]
                   (-> acc
                       (into m)
                       (into [\.])))
                 []))))

(defn expand-universe [m]
  (let [[col-voids row-voids] (find-voids m)]
    (mapv #(fill-row-voids % row-voids)
          (fill-col-voids m col-voids))))

;; Expansion adds a column and a row but it does not matter

(defn find-shortest-paths [universe]
  (let [expanded (expand-universe universe)
        height (count expanded)
        width (count (first expanded))
        galaxies
        (for [y (range height)
              x (range width)
              :when (= \# (get-in expanded [y x]))]
          [y x])]
    (reduce +
            (loop [distances []
                   [galaxy & other-galaxies] galaxies]
              (if (empty? galaxy) distances
                  (recur (into distances
                               (for [[y2 x2] other-galaxies]
                                 (+ (abs (- y2 (first galaxy)))
                                    (abs (- x2 (second galaxy))))))
                         other-galaxies))))))

(->> sample-1
     parse-input
     find-shortest-paths) ; 374, correct

(->> (slurp "/home/uusitalo/git/aoc/aoc2023/day11/input.txt")
     parse-input
     find-shortest-paths) ; 9947476
