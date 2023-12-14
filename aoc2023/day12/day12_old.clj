(ns day12-old
  (:require [clojure.string :as str]))

(def operational \.)
(def damaged \#)
(def unknown \?)

(def sample1 "#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1")

(def sample-2 "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn find-unknowns [row]
  (keep-indexed #(when (= unknown %2) %1) row))

(defn to-binary [row]
  (->> row
       (keep-indexed (fn [n c]
                       (when (= damaged c)
                         n)))
       (reduce (fn [acc n]
                 (bit-set acc n)) 0)))

(defn to-row [n]
  (loop [n n
         s ""]
    (if (zero? n)
      s
      (recur (bit-shift-right (bit-clear n 0) 1)
             (str s (if (bit-test n 0)
                      damaged
                      operational))))))

(defn parse-row [row]
  (let [[springs & counts] (str/split row #"[\s,]+")]
    [springs
     (map parse-long counts)]))

(defn check-arrangement [row counts]
  (->> row
       (re-seq #"#+")
       (map count)
       (= counts)))

(defn row-permutations [row-binary permutations unknowns]
  (map to-row
       (for [p permutations]
         (loop [pn p
                u' unknowns
                n' row-binary]
           (if (zero? pn)
             n'
             (recur (bit-shift-right (bit-clear pn 0) 1)
                    (rest u')
                    (if (bit-test pn 0)
                      (bit-set n' (first u'))
                      (bit-clear n' (first u')))))))))

(defn count-arrangements [row]
  (let [[line counts] (parse-row row)
        unknowns (find-unknowns line)
        permutations
        (-> (java.lang.Math/pow 2 (count unknowns))
            range)]
    (reduce (fn [acc n]
              (if (check-arrangement n counts)
                (inc acc)
                acc))
            0
            (row-permutations (to-binary line)
                              permutations
                              unknowns))))

(defn count-total-arrangements [input]
  (->> input
       str/split-lines
       (map count-arrangements)
       (reduce +)))

(defn total-arrangements-per-line [input]
  (->> input
       str/split-lines
       (map-indexed (fn [i line]
                      [i (count-arrangements line)]))))

(->> (slurp "/home/uusitalo/git/aoc/aoc2023/day12/input.txt")
     count-total-arrangements)

; 7173

;; part deux
