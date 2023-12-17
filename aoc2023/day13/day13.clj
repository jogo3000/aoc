(ns day13
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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
    (for [rp reflection-points
          :let [[left right] (split-at rp parts)]
          :when (->> (interleave (reverse left) right)
                     (partition 2)
                     (every? #(= (first %) (second %))))]
      rp)))

(defn reflection-lines [[hz vr]]
  [(find-mirror-pos vr)
   (find-mirror-pos hz)])

(defn appraise-lines [[hls vls]]
  (+ (reduce + vls)
       (->> hls (map #(* 100 %)) (reduce +))))

(defn appraise [[horizontal vertical]]
  (appraise-lines (reflection-lines [horizontal vertical])))

(let [[horizontal vertical] (parse-input sample1)]
  (list (find-mirror-pos vertical)
        (find-mirror-pos horizontal)))

(defn parse-puzzle-input [input]
  (str/split input #"\n{2}"))

(->> (slurp "day13/input.txt")
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

(appraise (parse-input sample2))

(defn rearrange-mirror [input]
  (let [[hz vr] (parse-input input)
        height (count hz)
        width (count vr)
        [old-hls old-vls] (reflection-lines [hz vr])]
    (set
     (for [y (range height)
           x (range width)
           :let [[up [row & down]] (split-at y hz)
                 [left [c & right]] (split-at x row)
                 permutation
                 (str/join \newline
                           (-> (vec up)
                               (into [(str/join (-> (vec left) (into [(if (= c \.) \# \.)]) (into right)))])
                               (into down)))
                 [new-hls new-vls] (reflection-lines (parse-input permutation))]]
       (appraise-lines [(set/difference (set new-vls) (set old-vls))
                        (set/difference (set new-hls) (set old-hls))])))))

(println (rearrange-mirror sample1))

(->> (slurp "day13/input.txt")
     str/trim
     parse-puzzle-input
     (map rearrange-mirror)
     (reduce (fn [acc n]
               (+ (if (coll? acc)
                    (apply + acc)
                    acc) (apply + n)))))

;; 31974
