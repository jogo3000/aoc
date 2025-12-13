(ns day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
")

(defn parse-goal [s]
  (->> s (drop 1) butlast (mapv {\. false \# true})))

(defn parse-switches [coll]
  (->> coll (map #(str/replace % #"[\(\)]" "")) (map #(str/split % #"," ))
       (map #(map parse-long %))))

(defn parse-joltages [s]
  (->> (str/split s #"[\{,\}]")
       (drop 1)
       (mapv parse-long)))

(defn parse-input [input]
  (->> input str/trim str/split-lines
       (map (fn [s]
              (let [parts (str/split s #"\s")
                    goal (->> parts first parse-goal)
                    switches (->> parts (drop 1) butlast parse-switches)
                    joltages (->> parts last parse-joltages)]
                {:goal goal :switches switches :joltages joltages})))))

(defn shortest-distance [m distances]
  (let [nearest (reduce (fn [n1 n2] (min-key distances n1 n2)) m)]
    [nearest (distances nearest)]))

(defn flick [state switches]
  (reduce (fn [state switch]
            (update state switch not)) state switches))

(defn solve-machine [machine]
  (loop [unvisited #{(:goal machine)}
         visited #{}
         distances {(:goal machine) 0}]
    (let [[state distance] (shortest-distance unvisited distances)]
      (if (every? false? state) distance
          (let [neighbours (map (partial flick state) (:switches machine))]
            (recur (-> unvisited (into neighbours) (disj state) (set/difference visited))
                   (conj visited state)
                   (reduce (fn [distances n]
                             (assoc distances n
                                    (min (inc distance)
                                         (get distances n Integer/MAX_VALUE))))
                           distances
                           neighbours)))))))

(->> (slurp "day10/input")
     parse-input
     (map solve-machine)
     (reduce +)) ; 522


;; Part 2
(defn power-flick [state switches]
  (reduce (fn [state switch] (update state switch dec)) state switches))

(defn solve-joltage [machine]
  (println machine)
  (loop [unvisited #{(:joltages machine)}
         visited #{}
         distances {(:joltages machine) 0}]
    (let [[state distance] (shortest-distance unvisited distances)]
      (if (every? zero? state) distance
          (let [neighbours (->> (map (partial power-flick state) (:switches machine))
                                (filter #(not-any? neg? %)))]
            (recur (-> unvisited (into neighbours) (disj state) (set/difference visited))
                   (conj visited state)
                   (reduce (fn [distances n]
                             (assoc distances n
                                    (min (inc distance)
                                         (get distances n Integer/MAX_VALUE))))
                           distances
                           neighbours)))))))

;; This works, but is too slow for part 2
(->> #_(slurp "day10/input")
     sample
     parse-input
     (map solve-joltage)
     (reduce +))
