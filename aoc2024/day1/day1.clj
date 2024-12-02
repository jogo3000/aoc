(ns day1
  (:require [clojure.string :as str]))

;; Part 1

(def sample-input
  "3   4
4   3
2   5
1   3
3   9
3   3
")

(defn parse-input [sample-input]
  (apply merge-with into
         (for [line
               (->> sample-input
                    str/split-lines
                    )]
           (let [[left right] (str/split line #"\s+")]
             {:left [left] :right [right]}))))


(defn solve-part1 [input]
  (let [{:keys [left right]} (parse-input input)
        sorted-left (sort left)
        sorted-right (sort right)]
    (apply +
           (for [[l r] (map list sorted-left sorted-right)]
             (abs (- (parse-long l) (parse-long r)))))))


(solve-part1 (slurp "day1/puzzle")) ; 2164381

;; Part 2
(defn solve-part2 [input]
  (let [{:keys [left right]} (parse-input input)
        counts
        (reduce (fn [m n] (update m n (fnil inc 0))) {} right)]
    (apply +
           (for [l left]
             (* (parse-long l)
                (counts l 0))))))

(solve-part2 (slurp "day1/puzzle")) ; 20719933
