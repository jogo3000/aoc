(ns day2
  (:require [clojure.string :as str]))

(def sample "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(def input "9595822750-9596086139,1957-2424,88663-137581,48152-65638,12354817-12385558,435647-489419,518494-609540,2459-3699,646671-688518,195-245,295420-352048,346-514,8686839668-8686892985,51798991-51835611,8766267-8977105,2-17,967351-995831,6184891-6331321,6161577722-6161678622,912862710-913019953,6550936-6625232,4767634976-4767662856,2122995-2257010,1194-1754,779-1160,22-38,4961-6948,39-53,102-120,169741-245433,92902394-92956787,531-721,64-101,15596-20965,774184-943987,8395-11781,30178-47948,94338815-94398813")

(defn invalid? [s]
  (let [len (count s)
        half (quot len 2)]
    (and (zero? (mod len 2))
         (= (subs s half) (subs s 0 half)))))

(->> (str/split input #",")
     (reduce
      (fn [acc range-expr]
        (let [[start end] (->> (str/split range-expr #"-") (map parse-long))]
          (+ acc
             (->> (range start (inc end))
                  (filter #(invalid? (str %)))
                  (reduce +)))))
      0))  ; 40398804950

; Part 2

(defn invalid2? [s]
  (->> (range 1 (count s))
       (some (fn [len]
               (let [parts (partition len len nil s)]
                 (apply = parts))))))


(->> (str/split input #",")
     (reduce
      (fn [acc range-expr]
        (let [[start end] (->> (str/split range-expr #"-") (map parse-long))]
          (+ acc
             (->> (range start (inc end))
                  (filter #(invalid2? (str %)))
                  (reduce +)))))
      0)) ; 65794984339
