(ns day3
  (:require [clojure.string :as str]))

(def sample "987654321111111
811111111111119
234234234234278
818181911112111"
)

(defn optimal-power [s]
  (let [[pos best-first]
        (->> s butlast
             (map-indexed (fn [i c]
                            [i (-> c str parse-long)]))
             (reduce (fn [[i x] [j n]]
                       (if (> n x) [j n] [i x]))))]
    (max (-> s (subs (- (count s) 2)) parse-long)
     (+ (* 10 best-first)
        (->> s (drop (inc pos)) (map (comp parse-long str))(apply max))))))

(->> (slurp "day3/input")
     str/split-lines
     (map optimal-power)
     (reduce +)) ; 17142
