(ns day11
  (:require [clojure.string :as str]))

(def sample-data [0 1 10 99 999])

(def puzzle-data [112 1110 163902 0 7656027 83039 9 74])

(defn blink [stones]
  (->> stones
       (reduce (fn [acc stone]
                 (cond
                   (zero? stone)
                   (conj acc 1)

                   (even? (count (str stone)))
                   (let [digits (vec (str stone))
                         n (count digits)
                         [a b] (split-at (/ n 2) digits)]
                     (into acc [(parse-long (str/join a)) (parse-long (str/join b))]))

                   :else
                   (conj acc (* 2024 stone)))) [])))

(blink sample-data)

(->> puzzle-data
     (iterate blink)
     (take 26)
     last
     count) ; 183620
