(ns day4
  (:require [clojure.string :as str]))

(def sample-input
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
")

(def puzzle-input (slurp "day4/input"))


(defn parse-input [s]
  (->> s str/trim
       str/split-lines
       (map vec)
       vec))

(defn solve-part1 [input]
  (apply +
         (let [m (parse-input input)]
           (for [x (range (count (first m)))
                 y (range (count m))
                 :let [down [(get-in m [x y])
                             (get-in m [(+ x 1) y])
                             (get-in m [(+ x 2) y])
                             (get-in m [(+ x 3) y])]
                       up [(get-in m [x y])
                           (get-in m [(- x 1) y])
                           (get-in m [(- x 2) y])
                           (get-in m [(- x 3) y])]
                       right [(get-in m [x y])
                              (get-in m [x (+ y 1)])
                              (get-in m [x (+ y 2)])
                              (get-in m [x (+ y 3)])]
                       left [(get-in m [x y])
                             (get-in m [x (- y 1)])
                             (get-in m [x (- y 2)])
                             (get-in m [x (- y 3)])]
                       up-left [(get-in m [x y])
                                (get-in m [(- x 1) (- y 1)])
                                (get-in m [(- x 2) (- y 2)])
                                (get-in m [(- x 3) (- y 3)])]
                       up-right [(get-in m [x y])
                                 (get-in m [(- x 1) (+ y 1)])
                                 (get-in m [(- x 2) (+ y 2)])
                                 (get-in m [(- x 3) (+ y 3)])]
                       down-left [(get-in m [x y])
                                  (get-in m [(+ x 1) (- y 1)])
                                  (get-in m [(+ x 2) (- y 2)])
                                  (get-in m [(+ x 3) (- y 3)])]
                       down-right [(get-in m [x y])
                                   (get-in m [(+ x 1) (+ y 1)])
                                   (get-in m [(+ x 2) (+ y 2)])
                                   (get-in m [(+ x 3) (+ y 3)])]]]
             (count (filter #(= "XMAS" (str/join %)) [down up right left up-left up-right down-left down-right]))))))


(solve-part1 sample-input) ; 18

(solve-part1 puzzle-input) ; 2618
