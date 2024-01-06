(ns day14
  (:require [clojure.string :as str]))

(defn parse-line [s]
  (let [[_ reindeer-name speed capacity rest-period]
        (re-find
         #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\."
         s)]
    {:name reindeer-name
     :speed (parse-long speed)
     :capacity (parse-long capacity)
     :rest-period (parse-long rest-period)}))

(def puzzle-input (->> (slurp "day14/input.txt")
                       str/trim
                       str/split-lines
                       (map parse-line)))

(def sample-input [{:name "Comet" :speed 14 :capacity 10 :rest-period 127}
                   {:name "Dancer" :speed 16 :capacity 11 :rest-period 162}])

(def race-duration 2503)

(def simpler-input [{:name "Test" :speed 1 :capacity 3 :rest-period 2}])

(defn run-race [race-duration input]
  (loop [time-left race-duration
         reindeers (->> input
                        (map #(assoc % :distance 0 :state :flying
                                     :rest-time 0 :flight-time 0)))]
    #_(println (:state (first reindeers)) (:distance (first reindeers)))
    (if (zero? time-left)
      reindeers
      (recur
       (dec time-left)
       (->> reindeers
            (map (fn [reindeer]
                   (if (= :resting (:state reindeer))
                     (if (>= (:rest-time reindeer) (:rest-period reindeer))
                       (-> (assoc reindeer :flight-time 1 :rest-time 0 :state :flying)
                           (update :distance + (:speed reindeer)))
                       (update reindeer :rest-time inc))
                     (if (>= (:flight-time reindeer) (:capacity reindeer))
                       (assoc reindeer :state :resting :rest-time 1)
                       (-> (update reindeer :flight-time inc)
                           (update :distance + (:speed reindeer))))))))))))

(run-race 1000 sample-input)
(apply max-key :distance (run-race 2503 puzzle-input))

;; part 2

(defn award-sprint-points [reindeers]
  (let [leader (apply max-key :distance reindeers)]
    (map (fn [competitor]
           (if (= (:distance leader) (:distance competitor))
             (update competitor :points inc)
             competitor)) reindeers)))

(defn run-race-with-sprint-goals [race-duration input]
  (loop [time-left race-duration
         reindeers (->> input
                        (map #(assoc % :distance 0 :state :flying
                                     :rest-time 0 :flight-time 0 :points 0)))]
    (if (zero? time-left)
      reindeers
      (recur
       (dec time-left)
       (->> reindeers
            (map (fn [reindeer]
                   (if (= :resting (:state reindeer))
                     (if (>= (:rest-time reindeer) (:rest-period reindeer))
                       (-> (assoc reindeer :flight-time 1 :rest-time 0 :state :flying)
                           (update :distance + (:speed reindeer)))
                       (update reindeer :rest-time inc))
                     (if (>= (:flight-time reindeer) (:capacity reindeer))
                       (assoc reindeer :state :resting :rest-time 1)
                       (-> (update reindeer :flight-time inc)
                           (update :distance + (:speed reindeer)))))))
            award-sprint-points)))))

(run-race-with-sprint-goals 1000 sample-input)

(apply max-key :points (run-race-with-sprint-goals 2503 puzzle-input))

;; That made Rudolph win. Santa's favourite
