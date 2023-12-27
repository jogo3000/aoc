(ns day3
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "day3/input.txt"))

(def north \^)
(def south \v)
(def east \<)
(def west \>)

(->> puzzle-input
     str/trim
     (reduce (fn [{:keys [houses x y] :as acc} c]
               (let [acc'
                     (-> (condp = c
                           north (update acc :y inc)
                           south (update acc :y dec)
                           east (update acc :x inc)
                           west (update acc :x dec)
                           acc))]
                 (update acc' :houses conj [(:x acc') (:y acc')])))
             {:houses #{[0 0]}
              :x 0
              :y 0})
     :houses
     count)

; 2565

;; Part II

(->> puzzle-input
     str/trim
     (reduce (fn [{:keys [turn] :as acc} c]
               (let [x-coord (if (= turn :santa) :santa-x :robo-x)
                     y-coord (if (= turn :santa) :santa-y :robo-y )
                     acc'
                     (-> (condp = c
                           north (update acc y-coord inc)
                           south (update acc y-coord dec)
                           east (update acc x-coord inc)
                           west (update acc x-coord dec)
                           acc))]
                 (-> acc'
                     (assoc :turn (if (= turn :santa) :robo-x :santa))
                     (update :houses conj [(:santa-x acc') (:santa-y acc')])
                     (update :houses conj [(:robo-x acc') (:robo-y acc')]))))
             {:houses #{[0 0]}
              :santa-x 0
              :santa-y 0
              :robo-x 0
              :robo-y 0
              :turn :santa})
     :houses
     count)

;; 2639
