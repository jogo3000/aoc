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
;;
;; Looks like maybe the smudges gonna be on the line not
;; covered. Otherwise every change is only going to make things worse,
;; eh?

(defn uncovered-by-mirror [len n]
  (if (= (/ len n) 2)
    [] ;; Nothing uncovered
    (let [d (- len n)]
      (if (> n d)
        [1 (if (= d (dec n)) 2 (- n d))]
        [(* 2 n) len]))))

(defn swap [[a b]]
  [b a])

;; Huh? the first reflecton is still there?
(let [[hz vr] (parse-input sample1)
      width (count vr)
      height (count hz)]
  [(appraise [hz vr])
   (for [n (->> vr
                find-mirror-pos
                first
                (uncovered-by-mirror width)
                (apply range))]
     (let [uncovered (get vr (dec n))]
       (keep-indexed
        (fn [x c]
          (let [new-map
                (str/join \newline
                          (update vr (dec n)
                                  (fn [line] (str/join (assoc (vec line) x (if (= \. c) \# \.))))))]
            (appraise (swap (parse-input new-map))))) uncovered)))])
