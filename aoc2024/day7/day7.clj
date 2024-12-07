(ns day7
  (:require [clojure.string :as str]))

(def sample-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
")


(defn parse-input [input]
  (->> input
       str/split-lines
       (map (fn [s] (->> (str/split s #":*\s")
                         (map parse-long))))))


(defn op-seqs [n ops]
  (let [nops
        (into
         (map #(cons * %) ops)
         (map #(cons + %) ops))]
    (if (= n 1)
      nops
      (op-seqs (dec n) nops))))

(defn valid? [eq]
  (let [[y & xs] eq
        operator-permutations (op-seqs (dec (count xs)) '(()))]
    (some (fn [opseq]
            (= y
               (reduce (fn [acc [operator n]]
                         (if (> acc y) (reduced :over)
                             (operator acc n)))
                       (first xs)
                       (partition 2 (interleave opseq (rest xs))))))
          operator-permutations)))

(defn solve-1 [input]
  (->> input
       parse-input
       (filter valid?)
       (map first)
       (apply +)))

(solve-1 sample-input) ; 3749 so far so good
(time (solve-1 (slurp "day7/input"))) ; 3119088655389
