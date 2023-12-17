(ns day9
  (:require [clojure.string :as str]))

(def sample-input-1 "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

;; 0 3 6 9 12 15
;;   3 3 3  3  3
;;     0 0  0  0

(defn differences [values]
  (->> values
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(defn triangulate [line]
  (let [values
        (->> (str/split line #"\s+")
             (map parse-long))]
    (->> values
         (iterate differences)
         (take-while #(not-every? zero? %)))))

(defn predict [line]
  (let [ts (reverse (triangulate line))]
    (reduce (fn [acc n]
              (+ acc (last n))) 0 ts)))

(->> sample-input-1
     str/split-lines
     (map predict)
     (reduce +))


(->> (slurp "day9/input.txt")
     str/trim
     str/split-lines
     (map predict)
     (reduce +))

;; 1916822650

;; Part 2

(defn extrapolate [line]
  (let [ts (reverse (triangulate line))]
    (reduce (fn [acc n]
              (* -1 (- acc (first n)))) 0 ts)))

(->> sample-input-1
     str/split-lines
     (map extrapolate)
     (reduce +))

(->> (slurp "day9/input.txt")
     str/trim
     str/split-lines
     (map extrapolate)
     (reduce +))

; 966
