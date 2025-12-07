(ns day3
  "https://adventofcode.com/2025/day/3"
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

;; part 2

(defn optimal-power+ [s n]
  (if (zero? n) 0
      (let [len (count s)
            [pos best-first]
            (->> s
                 (take (- len (dec n)))
                 (map-indexed (fn [i c]
                                [i (-> c str parse-long)]))
                 (reduce (fn [[i x] [j n]]
                           (if (> n x) [j n] [i x]))))]
        (+ (* (java.lang.Math/pow 10 (dec n)) best-first)
           (optimal-power+ (subs s (inc pos)) (dec n))))))

(->> sample
     str/split-lines
     (map (fn [s] (optimal-power+ s 12)))
     (reduce +)) ; 3.121910778619E12

(->> (slurp "day3/input")
     str/split-lines
     (map (fn [s] (optimal-power+ s 12)))
     (reduce +)) ; 1.69935154100102E14
