(ns day13
  (:require [clojure.string :as str]))

(def sample1 "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.")

(def sample2 "#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn parse-input [input]
  (let [rows  (str/split-lines input)
        width (->> rows count)
        cols (->> rows
                  (apply interleave)
                  (partition width)
                  (mapv str/join))]
    [rows cols]))

(defn find-mirror-pos [parts]
  (let [reflection-points
        (->> parts
             (partition 2 1)
             (keep-indexed (fn [i [a b]] (when (= a b) (inc i)))))]
    (println (vec reflection-points))
    (for [rp reflection-points
          :let [[left right] (split-at rp parts)]
          :when (->> (interleave (reverse left) right)
                     (partition 2)
                     (every? #(= (first %) (second %))))]
      rp)))

(defn appraise [[horizontal vertical]]
  (let [ns (find-mirror-pos vertical)
        ks (find-mirror-pos horizontal)]
    (println ns ks)
    (+ (reduce + ns)
       (->> ks (map #(* 100 %)) (reduce +)))))

(let [[horizontal vertical] (parse-input sample1)]
  (list (find-mirror-pos vertical)
        (find-mirror-pos horizontal)))

(defn parse-puzzle-input [input]
  (str/split input #"\n{2}"))

(->> (slurp "/home/uusitalo/git/aoc/aoc2023/day13/input.txt")
     str/trim
     parse-puzzle-input
     (map parse-input)
     (map appraise)
     (reduce +))

; 35210

;; Part deux
