(ns day6
  (:require [clojure.string :as str]))

(def sample-data
  "Time:      7  15   30
Distance:  9  40  200")

(defn parse-input [s]
  (let [[times distances]
        (->> s
             (str/split-lines)
             (map #(->> (str/split % #"\s+")
                        (drop 1)
                        (map parse-long))))]
    (map (fn [t d] {:time t :distance d}) times distances)))

(parse-input sample-data)

(defn win? [{:keys [time distance]} charge]
  (let [time-left (- time charge)
        speed charge
        my-distance (* speed time-left)]
    (> my-distance distance)))

(defn count-wins [{t :time :as game}]
  (count (filter true? (for [charge-time (range (inc t))]
                       (win? game charge-time)))))

(->> sample-data
     parse-input
     (map count-wins)
     (reduce *))

(def puzzle-input
  "Time:        62     73     75     65
Distance:   644   1023   1240   1023")

(->> puzzle-input
     parse-input
     (map count-wins)
     (reduce *)) ; 393120, correct

;; part 2

(defn parse2 [s]
  (->> s
       (str/split-lines)
       (map #(filter (fn [c] (Character/isDigit c)) %))
       (map (comp parse-long str/join))
       (apply (fn [t d] {:time t :distance d}))))

(parse2 sample-data)

(count-wins (parse2 puzzle-input))
