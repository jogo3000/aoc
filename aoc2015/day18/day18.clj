(ns day18
  (:require [clojure.string :as str]))

(def puzzle-input (->> (slurp "day18/input.txt")
                       str/trim))

(def sample-input ".#.#.#
...##.
#....#
..#...
#.#..#
####..")

(defn parse-input [s]
  (->> s str/split-lines (map vec) vec))

(def on \#)
(def off \.)

(defn on? [game coords]
  (if (= (get-in game coords off) on)
    1 0))

(defn next-state [game]
  (->> game
       (map-indexed
        (fn [y row]
          (vec
           (for [x (range (count row))]
             (let [neighbors (+ (on? game [(dec y) (dec x)])
                                (on? game [(dec y) x])
                                (on? game [(dec y) (inc x)])
                                (on? game [y (dec x)])
                                (on? game [y (inc x)])
                                (on? game [(inc y) (dec x)])
                                (on? game [(inc y) x])
                                (on? game [(inc y) (inc x)]))
                   this (get-in game [y x])]
               (cond
                 (and (= this off) (= neighbors 3)) \#
                 (and (= this on) (<= 2 neighbors 3)) \#
                 :else \.))))))
       vec))

(defn count-lights-that-are-on [game]
  (count (filter #(= on %) (flatten game))))

(count-lights-that-are-on (nth (iterate next-state (parse-input sample-input)) 4))

(count-lights-that-are-on (nth (iterate next-state (parse-input puzzle-input)) 100))1061


;; part deux

(defn santa's-game-rules [game]
  (->> game
       (map-indexed
        (fn [y row]
          (vec
           (for [x (range (count row))]
             (let [neighbors (+ (on? game [(dec y) (dec x)])
                                (on? game [(dec y) x])
                                (on? game [(dec y) (inc x)])
                                (on? game [y (dec x)])
                                (on? game [y (inc x)])
                                (on? game [(inc y) (dec x)])
                                (on? game [(inc y) x])
                                (on? game [(inc y) (inc x)]))
                   this (get-in game [y x])]
               (cond
                 (or (= y x 0)
                     (= y x (dec (count game)))
                     (and (= y 0) (= x (dec (count game))))
                     (and (= y (dec (count game))) (= 0 x)))
                 \#
                 (and (= this off) (= neighbors 3)) \#
                 (and (= this on) (<= 2 neighbors 3)) \#
                 :else \.))))))
       vec))


(def new-sample "##.#.#
...##.
#....#
..#...
#.#..#
####.#")

(count-lights-that-are-on (nth (iterate santa's-game-rules (parse-input new-sample)) 5))

(count-lights-that-are-on (nth (iterate santa's-game-rules (parse-input puzzle-input)) 100)) ; 1006

;; That was surprisingly easy
