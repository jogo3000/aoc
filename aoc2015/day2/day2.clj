(ns day2
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "day2/input.txt"))

(defn area [x y]
  (* x y))

(defn needed-wrapper [[l w h]]
  (let [l*w (area l w)
        w*h (area w h)
        l*h (area l h)]
    (+ l*w w*h l*h
       l*w w*h l*h
       (min l*w w*h l*h))))

(needed-wrapper [2 3 4])
(needed-wrapper [1 1 10])

(defn box-sizes [input]
  (->> input
       str/trim
       str/split-lines
       (map (fn [s] (->> (str/split s #"x")
                         (map parse-long))))))

(->> puzzle-input
     box-sizes
     (map needed-wrapper)
     (reduce +)) ; 1586300


(defn ribbon-for-wrap [[l w h :as sides]]
  (->> sides sort (take 2) (apply +) (* 2)))

(defn ribbon-for-bow [[l w h]]
  (* l w h))

(->> puzzle-input
     box-sizes
     (map (juxt ribbon-for-bow ribbon-for-wrap))
     (map #(apply + %))
     (reduce +)) ; 3737498
