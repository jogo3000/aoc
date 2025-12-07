(ns day4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample
  "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(def roll \@)

(defn parse-input [input]
  (->> input str/split-lines (mapv vec)))

(defn adjacent-cells [x y]
  [[(dec x) (inc y)]
   [(dec x) y]
   [(dec x) (dec y)]
   [x (dec y)]
   [x (inc y)]
   [(inc x) (inc y)]
   [(inc x) y]
   [(inc x) (dec y)]])

(let [grid (parse-input (slurp "day4/input"))]
  (->>
   (for [y (range (count grid))
         x (range (count (first grid)))
         :when (= \@ (get-in grid [y x]))]
     (->> (adjacent-cells x y)
          (reduce (fn [acc [x y]]
                    (if (= roll (get-in grid [y x]))
                      (inc acc)
                      acc)) 0)))
   (filter #(< % 4))
   count)) ; 1626

; Part 2

(defn find-rolls [grid]
  (into #{}
        (for [y (range (count grid))
              x (range (count (first grid)))
              :when (= roll (get-in grid [y x]))]
          [y x])))

(defn find-removable [rolls]
  (->> rolls
       (filter (fn [[y x]]
                 (let [adjacent-rolls
                       (->> (adjacent-cells y x)
                            (filter #(contains? rolls %)))]
                   (< (count adjacent-rolls) 4))))
       (into #{})))

(count
 (let [grid (parse-input (slurp "day4/input"))
       rolls (find-rolls grid)]
   (loop [rolls rolls
          removed #{}]
     (let [removable (find-removable rolls)]
       (if (empty? removable) removed
           (recur (set/difference rolls removable)
                  (set/union removed removable))))))) ; 9173
