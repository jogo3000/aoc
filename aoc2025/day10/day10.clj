(ns day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

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

(defn power-flock [state switches]
  (reduce (fn [state switch] (update state switch inc)) state switches))

(defn distance-from-goal [joltages]
  (reduce + joltages))

(defn h [unvisited distances]
  (let [s (reduce (fn [s1 s2]
                    (min-key (fn [s] (+ (distances s) (distance-from-goal s)))
                             s1 s2)) unvisited)]
    [s (distances s)]))

(defn solve-joltage [machine]
  (println machine)
  (loop [unvisited #{(:joltages machine)}
         visited #{}
         distances {(:joltages machine) 0}]
    (let [[state distance] (h unvisited distances)]
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
#_
(->> #_(slurp "day10/input")
     sample
     parse-input
     (map solve-joltage)
     (reduce +))

(defn solve-joltage2 [machine]
  (let [{:keys [joltages switches]} machine
        switches (->> switches (sort-by count) reverse)]
    (loop [state joltages
           switches switches
           flicks 0]
      (cond
        (every? zero? state) flicks
        (empty? switches) :error
        :else
        (let [switch (first switches)
              [new-state new-flicks]
              (loop [state state
                     flicks flicks]
                (let [new-state (power-flick state switch)]
                  (cond
                    (every? zero? new-state) [new-state (inc flicks)]
                    (some neg? new-state) [state flicks]
                    :else
                    (recur new-state (inc flicks)))))]
          (recur new-state (rest switches) new-flicks))))))

;; Brute forcing like this can't solve every case
#_(->> sample
     parse-input
     (map solve-joltage2))


(defn solve-joltage3 [machine]
  (println machine)
  (let [s (:joltages machine)
        t (-> s count (repeat 0) vec)]
    (loop [qf #{s}
           Sf #{}
           df {s 0}
           qt #{t}
           St #{}
           dt {t 0}
           u Integer/MAX_VALUE]
      (let [best-f (shortest-distance qf df)
            best-t (shortest-distance qt dt)]
        (cond
          (>= (+ (second best-f) (second best-t)) u)
          u

          (< (second best-f) (second best-t))
          (let [[state distance] best-f]
            (let [neighbours (->> (map (partial power-flick state) (:switches machine))
                                  (filter #(not-any? neg? %)))
                  [best-u df'] (reduce (fn [[u distances] n]
                                         (let [d' (min (inc distance)
                                                       (get distances n Integer/MAX_VALUE))]
                                           [(min u (+ distance 1 (get dt n Integer/MAX_VALUE)))
                                            (assoc distances n d')]))
                                       [u df]
                                       neighbours)]
              (recur (-> qf (into neighbours) (disj state) (set/difference Sf))
                     (conj Sf state)
                     df'
                     qt
                     St
                     dt
                     best-u)))

          :else
          (let [[state distance] best-t]
            (let [neighbours (->> (map (partial power-flock state) (:switches machine))
                                  (filter (fn [state]
                                            (->> (map vector state s)
                                                 (not-any? #(apply > %))))))
                  [best-u dt'] (reduce (fn [[u distances] n]
                                         (let [d' (min (inc distance)
                                                       (get distances n Integer/MAX_VALUE))]
                                           [(min u (+ distance 1 (get df n Integer/MAX_VALUE)))
                                            (assoc distances n d')]))
                                       [u dt]
                                       neighbours)]
              (recur qf
                     Sf
                     df
                     (-> qt (into neighbours) (disj state) (set/difference St))
                     (conj St state)
                     dt'
                     best-u))))))))

;; Bi-directional djikstra works too, but still too slow
#_(->> #_sample
     (slurp "day10/input")
     parse-input
     (map solve-joltage3)
     (reduce +))

(defn solve-joltage4 [machine]
  (println machine)
  (let [{:keys [joltages switches]} machine]
    (loop [unvisited #{joltages}
           visited #{}
           distances {joltages 0}]
      (let [[state distance] (shortest-distance unvisited distances)
            #_(h unvisited distances)]
        (if (every? zero? state) distance
            (let [neighbour->distance
                  (->> switches
                       (mapcat (fn [switch]
                                 (loop [state state
                                        neighbours '()
                                        d distance]
                                   (let [neighbor (power-flick state switch)]
                                     (if (not-any? neg? neighbor)
                                       (recur neighbor
                                              (cons [neighbor (inc d)] neighbours)
                                              (inc d))
                                       neighbours))))))]
              (recur (-> unvisited (into (map first neighbour->distance)) (disj state) (set/difference visited))
                     (conj visited state)
                     (reduce (fn [distances [n d]]
                               (assoc distances n
                                      (min d (get distances n Integer/MAX_VALUE))))
                             distances
                             neighbour->distance))))))))

;; Works, but is still too slow
#_(->> #_sample
     (slurp "day10/input")
     parse-input
     (map solve-joltage4))

(defn solve-joltage5 [f joltage switches presses]
  (cond (every? zero? joltage) presses
        (some neg? joltage) Integer/MAX_VALUE
        :else
        (reduce (fn [best switch]
                  (let [j' (power-flick joltage switch)
                        p' (f f j' switches (inc presses))]
                    (if (< p' best) p' best)))
                Integer/MAX_VALUE switches)))

(def m-solver (memoize solve-joltage5))

;; Brute force is too slow
#_(->> #_sample
       (slurp "day10/input")
       parse-input
       (map (fn [{:keys [joltages switches]}]
              (println joltages)
              (m-solver m-solver joltages switches 0)))
       (reduce +))
