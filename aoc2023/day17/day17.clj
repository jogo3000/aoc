(ns day17
  (:require [clojure.string :as str]))

(def sample-input (slurp "day17/sample.txt"))
(def ^String puzzle-input (slurp "day17/input.txt"))

(defn parse-input [s]
  (->> s
       str/trim
       str/split-lines
       (map #(vec (map (comp abs (partial - (int \0)) int) %)))
       vec))

(defn north [[y x]]
  [(dec y) x])

(defn south [[y x]]
  [(inc y) x])

(defn east [[y x]]
  [y (inc x)])

(defn west [[y x]]
  [y (dec x)])

(defn may-move? [m pos dir]
  (let [height (count m)
        width (count (first m))
        [y x] (dir pos)]
    (and (<= 0 y (dec height))
         (<= 0 x (dec width)))))

(def dirs {nil [south east]
           west [west north south]
           east [east north south]
           north [north east west]
           south [south east west]})

(defn safe+ [& args]
  (reduce (fn [acc n]
            (if (or (= Long/MAX_VALUE acc)
                    (= Long/MAX_VALUE n))
              (reduced Long/MAX_VALUE)
              (+ acc n))) args))

(defn direction [[y x] [y' x']]
  (if (= y y')
    (cond
      (< x x') east
      (> x x') west
      :else nil)
    (cond
      (< y y') south
      (> y y') north
      :else nil)))

(defn follow-path [prev u]
  (->> (iterate prev u)
       (take 3)
       (partition 2 1)
       (map #(direction (first %)
                        (second %)))))

(defn djikstra [m]
  (let [source [0 0]
        height (count m)
        width (count (first m))
        target [(dec height) (dec width)]
        dist (-> (into {}
                       (for [y (range height)
                             x (range width)]
                         [[y x] Long/MAX_VALUE]))
                 (assoc source 0))
        prev {}]
    (loop [Q (-> (into #{}
                       (for [y (range height)
                             x (range width)]
                         [y x])))
           dist dist
           prev prev]
      (let [u (reduce (fn [acc u]
                        (if (> (dist acc) (dist u))
                          u acc)) Q)
            Q (reduce (fn [acc q] (if (= u q)
                                    acc (conj acc q))) [] Q)

            previous (prev u)
            path-here (when previous
                        (follow-path prev u))
            speed-limit (= (count (set path-here)) 3)
            dir (when previous
                  (direction previous u))
            possible-directions (->> (dirs dir)
                                     (filter #(if speed-limit
                                                (not= % dir)
                                                %))
                                     (filter #(may-move? m u %)))
            neighbours (map #(% u) possible-directions)
            new-state (reduce (fn [state v]
                                (let [alt (+ (dist u) (get-in m v))]
                                  (if (< alt (dist v))
                                    (-> state
                                        (update :dist (fn [d] (assoc d v alt)))
                                        (update :prev (fn [d] (assoc d v u))))
                                    state)))
                              {:dist dist
                               :prev prev}
                              neighbours)]
        (if (= u target)
          [dist prev]
          (recur Q
                 (:dist new-state)
                 (:prev new-state)))))))

(defn reconstruct-path [came-from current]
  (loop [total-path (list current)
         current current]
    (if-let [c' (came-from current)]
      (recur (cons c' total-path)
             c')
      total-path)))

(def result (djikstra (parse-input sample-input)))
((nth result 2) (first result))
(reconstruct-path (second result) [12 12])

#_(search-path  (parse-input puzzle-input))

(let [m (parse-input sample-input)]
  (reduce (fn [acc pos]
            (+ acc (get-in m pos)))
          0
          (reconstruct-path (second result) [12 12])))

(defn visualize [m steps dirs]
  (let [height (count m)
        width (count (first m))]
    (str/join
     \newline
     (for [y (range height)]
       (str/join
        (for [x (range width)]
          (if (contains? steps [y x])
            (let [d (last (dirs [y x]))]
              (cond
                (= west d) \<
                (= east d) \>
                (= north d) \^
                (= south d) \v
                :else \#))
           (get-in m [y x]))))))))

(let [m (parse-input sample-input)
      p (set (reconstruct-path (second result) (first result)))]
  (println "---------")
  (println (visualize m p (second result))))
