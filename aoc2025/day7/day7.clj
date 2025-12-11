(ns day7
  (:require [clojure.string :as str]))

(def sample
  ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
")

(defn parse-input [input]
  (->> input str/trim str/split-lines (mapv vec)))

(def empty-space \.)
(def splitter \^)
(def source \S)

(defn locate-source [m]
  (let [first-row (first m)]
    (first (for [i (range (count first-row))
                 :when (= source (get-in m [0 i]))]
             [0 i]))))

(defn at-splitter? [m [x y]]
  (= splitter (get-in m [x y])))

(let [m (parse-input (slurp "day7/input"))
      depth (count m)]
  (loop [beams [(locate-source m)]
         splits 0]
    (if (>= (ffirst beams) depth) splits
        (let [{splitting true continuing false}
              (->> beams
                   (map (comp (partial into {}) vector (juxt (partial at-splitter? m) vector)))
                   (apply merge-with concat))]
          (recur
           (->> splitting
                (mapcat (fn [[x y]] [[x (dec y)] [x (inc y)]]))
                (into continuing)
                distinct
                (map (fn [[x y]] [(inc x) y])))
           (+ splits (count splitting))))))) ; 1516


;; This approach works theoretically speaking, but is way too slow
#_
(count
 (let [m (parse-input (slurp "day7/input"))
       depth (count m)]
   (loop [beams [(list (locate-source m))]]
     (if (>= (first (ffirst beams)) depth) beams
         (let [{splitting true continuing false}
               (->> beams
                    (map (comp (partial into {}) vector (juxt (comp (partial at-splitter? m) first) vector)))
                    (apply merge-with into))]
           (recur
            (->> splitting
                 (mapcat (fn [xs]
                           (let [[[x y] & _] xs]
                             [(cons [x (dec y)] xs) (cons [x (inc y)] xs)])))
                 (into continuing)
                 distinct
                 (map (fn [xs]
                        (let [[[x y] & _] xs]
                          (cons [(inc x) y] xs)))))))))))
