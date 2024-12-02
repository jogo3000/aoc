(ns day2
  (:require [clojure.string :as str]))

(def sample-input
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
")

(defn parse-input [s]
  (->> s
       str/split-lines
       (map #(str/split % #"\s"))
       (map #(map parse-long %))))

(parse-input sample-input)

(defn too-big-jumps? [report]
  (reduce (fn [acc a]
            (let [diff (abs (- acc a))]
              (if (<= 1 diff  3)
                a
                (reduced :unsafe)))) report))

(defn all-increasing-or-decreasing? [report]
  (let [steps (partition 2 1 report)]
    (or (every? (fn [[a b]] (> a b)) steps)
        (every? (fn [[a b]] (< a b)) steps))))

(defn is-safe? [report]
  (and (not= :unsafe (too-big-jumps? report))
       (all-increasing-or-decreasing? report)))

(map is-safe? (parse-input sample-input))

(defn find-safe-reports [s]
  (->> s parse-input
       (map is-safe?)
       (filter true?)
       count))

(find-safe-reports sample-input) ; 2

(find-safe-reports (slurp "day2/input")) ; 287

;; Part 2

;; I think brute force will still do it

(defn produce-dampened-candidates [report]
  (for [n (range (count report))]
    (concat (subvec (vec report) 0 n)
            (subvec (vec report) (inc n)))))

(defn find-safe-reports-dampened [s]
  (count
   (for [report (parse-input s)
         :let [cands (produce-dampened-candidates report)]
         :when (some is-safe? cands)]
     :safe)))

(find-safe-reports-dampened sample-input)

(find-safe-reports-dampened (slurp "day2/input")) ; 354
