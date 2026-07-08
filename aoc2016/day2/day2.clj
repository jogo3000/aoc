(ns day2
  (:require [clojure.string]))

(def grid [["1" "2" "3"]
           ["4" "5" "6"]
           ["7" "8" "9"]])

(def sample "ULL
RRDDD
LURDL
UUUUD")

(def puzzle-input (slurp "input"))

(defn read-button [[x y]]
  (-> grid (nth y) (nth x)))

(defn up [[x y]]
  [x (max 0 (dec y))])

(defn down [[x y]]
  [x (min 2 (inc y))])

(defn left [[x y]]
  [(max 0 (dec x)) y])

(defn right [[x y]]
  [(min 2 (inc x)) y])

(defn solve [input]
  (let [rows (->> input
                  clojure.string/split-lines)
        start [1 1]]
    (->>
     (loop [rows rows
            pos start
            locations []]
       (if (empty? rows) locations
           (let [pos (reduce (fn [acc dir]
                               (case dir
                                 \U (up acc)
                                 \D (down acc)
                                 \L (left acc)
                                 \R (right acc)))
                             pos
                             (first rows))]
             (recur (rest rows)
                    pos
                    (conj locations pos)))))
     (map read-button)
     (clojure.string/join))))

(solve sample)

(solve puzzle-input) ; "84452"

;; part 2

(def grid2 (->> (slurp "grid2")
                (clojure.string/split-lines)
                (map vec)))


(defn read-button2 [[x y]]
  (-> grid2 (nth y) (nth x)))

(defn on-button? [[x y]]
  (try
    (not= \space (read-button2 [x y]))
    (catch IndexOutOfBoundsException _
      false)))

(defn up2 [[x y]]
  (let [y' (max 0 (dec y))]
    (if (on-button? [x y'])
      [x y']
      [x y])))

(defn down2 [[x y]]
  (let [y' (min 4 (inc y))]
    (if (on-button? [x y'])
      [x y']
      [x y])))

(defn left2 [[x y]]
  (let [x' (max 0 (dec x))]
    (if (on-button? [x' y])
      [x' y]
      [x y])))

(defn right2 [[x y]]
  (let [x' (min 4 (inc x))]
    (if (on-button? [x' y])
      [x' y]
      [x y])))

(defn solve2 [input]
  (let [rows (->> input
                  clojure.string/split-lines)
        start [0 2]]
    (->>
     (loop [rows rows
            pos start
            locations []]
       (if (empty? rows) locations
           (let [pos (reduce (fn [acc dir]
                               (case dir
                                 \U (up2 acc)
                                 \D (down2 acc)
                                 \L (left2 acc)
                                 \R (right2 acc)))
                             pos
                             (first rows))]
             (recur (rest rows)
                    pos
                    (conj locations pos)))))
     (map read-button2)
     (clojure.string/join))))

(solve2 sample)

(solve2 puzzle-input) "D65C3"
