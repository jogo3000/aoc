(ns day5
  (:require [clojure.string :as str]))

(def sample "3-5
10-14
16-20
12-18

1
5
8
11
17
32
")

(defn parse-input [input]
  (let [[ranges ingredients] (-> input str/trim (str/split #"\n{2}"))]
    [(->> ranges str/split-lines (map #(->> (str/split % #"\-") (map parse-long))))
     (->> ingredients str/split-lines (map parse-long))]))

(defn fresh? [ranges ingredient]
  (some (fn [[start end]] (<= start ingredient end)) ranges))

(let [[ranges ingredients] (parse-input (slurp "day5/input"))]
  (->> ingredients (filter (partial fresh? ranges)) count)) ; 848

;; Part 2
;; Naive solution runs out of heap
#_(let [[ranges _] (parse-input (slurp "day5/input"))]
  (count
   (reduce (fn [acc [start end]]
             (into acc (range start (inc end)))) #{} ranges)))

(defn can-merge? [[s1 e1] [s2 e2]]
  (or (<= s1 s2 e1)
      (<= s1 e2 e1)
      (<= s2 s1 e2)
      (<= s2 e1 e2)))

(let [[ranges _] (parse-input (slurp "day5/input"))
      merged-ranges
      (loop [ranges ranges]
        (if-let [mergeable
                 (some (fn [range]
                         (when-let [mergeable (first (filter #(and (not= range %) (can-merge? range %)) ranges))]
                           [range mergeable])) ranges)]
          (let [[[s1 e1] [s2 e2]] mergeable]
            (recur (-> (into #{} ranges)
                       (disj [s1 e1])
                       (disj [s2 e2])
                       (conj
                        [(min s1 s2) (max e1 e2)]))))
          ranges))]
  (reduce (fn [acc [s e]]
            (+ acc 1 (- e s))) 0 merged-ranges)) ; 334714395325710
