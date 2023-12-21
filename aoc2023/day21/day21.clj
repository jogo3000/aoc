(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input (str/trim (slurp "day21/sample1.txt")))
(def puzzle-input (str/trim (slurp "day21/input.txt")))

(def starting-position \S)
(def garden-plot \.)
(def rock \#)

(defn parse-input [s]
  (->> s
       str/split-lines
       (mapv vec)))

(defn north [[y x]]
  [(dec y) x])

(defn south [[y x]]
  [(inc y) x])

(defn east [[y x]]
  [y (inc x)])

(defn west [[y x]]
  [y (dec x)])

(defn inside-map? [max-y max-x [y' x']]
  (and (<= 0 y' max-y)
       (<= 0 x' max-x)))

(defn may-move? [m pos dir]
  (let [max-x (dec (count (first m)))
        max-y (dec (count m))
        [y' x' :as pos'] (dir pos)]
    (when (and (<= 0 y' max-y)
               (<= 0 x' max-x)
               (not= rock (get-in m pos')))
      pos')))

(defn find-starting-position [m]
  (let [width (count (first m))
        height (count m)]
    (first (for [y (range height)
                 x (range width)
                 :when (= starting-position (get-in m [y x]))]
             [y x]))))

(defn count-possible-steps [input steps]
  (let [m (parse-input input)
        starting-position (find-starting-position m)]
    (->> (range steps)
         (reduce (fn [{:keys [queue visited]} _]
                   (let [new-spots
                         (set
                          (mapcat identity
                                  (for [spot queue]
                                    (let [allowed-positions
                                          (keep (partial may-move? m spot) [north south east west])]
                                      allowed-positions))))]
                     {:queue (remove #(contains? visited %) new-spots)
                      :visited (into (update-vals visited not)
                                     (map (juxt identity (constantly true)))
                                     new-spots)}))
                 {:queue [starting-position]
                  :visited {starting-position true}})
         :visited
         (filter second)
         (map first)
         (into #{}))))

(count (count-possible-steps sample-input 6))

(defn visualize [m steps]
  (let [steps (set steps)]
    (println "---- visualization -----")
    (println
     (str/join
      "\n"
      (for [y (range (count m))]
        (str/join
         (for [x (range (count (first m)))]
           (if (steps [y x])
             \O
             (get-in m [y x])))))))))


(let [m (parse-input sample-input)]
  (visualize m (count-possible-steps sample-input 2)))

(let [m (parse-input sample-input)]
  (doseq [n (range 100)]
    (println n (count (count-possible-steps sample-input n)))))

#_(count (count-possible-steps puzzle-input 64)); 3776 - This works

;; Part deux

(def target-steps 26501365)
(+ 26501365 26501365 26501365 26501365) ; 106 005 460 Something much more manageable
(* target-steps target-steps);; ->  702 322 346 863 225  search area too large to hold in memory

(defn wrapped-get [m height width [y x]]
  (let [mod-y (mod y height)
        mod-x (mod x width)]
    (get-in m [mod-y mod-x])))

(defn may-move-wrap? [m height width pos dir]
  (let [[y' x' :as pos'] (dir pos)
        mod-y (mod y' height)
        mod-x (mod x' width)]
    (when (not= rock (get-in m [mod-y mod-x]))
      pos')))

(defn count-possible-steps2 [input steps]
  (let [m (parse-input input)
        height (count m)
        width (count (first m))
        starting-position (find-starting-position m)]
    (->> (range steps)
         (reduce (fn [{:keys [queue visited]} _]
                   (let [new-spots
                         (set
                          (mapcat identity
                                  (for [spot queue]
                                    (let [allowed-positions
                                          (keep (partial may-move-wrap? m height width spot) [north south east west])]
                                      allowed-positions))))]
                     {:queue (remove #(contains? visited %) new-spots)
                      :visited (into (update-vals visited not)
                                     (map (juxt identity (constantly true)))
                                     new-spots)}))
                 {:queue [starting-position]
                  :visited {starting-position true}})
         :visited
         (filter second)
         (map first)
         (into #{}))))

(defn count-steps-to-visit-all [input]
  (let [m (parse-input input)
        height (count m)
        width (count (first m))
        starting-position (find-starting-position m)
        garden-spots (into #{} (for [y (range height)
                                     x (range width)
                                     :when (not= rock (get-in m [y x]))]
                                 [y x]))]
    (loop [{:keys [queue visited]} {:queue [starting-position]
                                    :visited {starting-position true}}
           steps 0]
      (if (or (> steps (* 2 width))
           (= garden-spots (set/intersection (set (keys visited)) garden-spots)))
        [steps visited]
        (let [new-spots
              (set
               (mapcat identity
                       (for [spot queue]
                         (let [allowed-positions
                               (keep (partial may-move-wrap? m height width spot) [north south east west])]
                           allowed-positions))))]
          (recur
           {:queue (remove #(contains? visited %) new-spots)
            :visited (into (update-vals visited not)
                           (map (juxt identity (constantly true)))
                           new-spots)}
           (inc steps)))))))

(def steps-all (count-steps-to-visit-all puzzle-input))

(let [m (parse-input puzzle-input)]
  (visualize m (->> (second steps-all)
                    (filter second)
                    (map first)
                    (into #{}))))

(def parsed-puzzle (parse-input puzzle-input))

(->> (second steps-all)
     (filter second) ;; Näistä lukemista pitää poistaa ruudukon ulkopuolelle jäävä kama
     (map first)
     (filter (partial inside-map? (dec (count parsed-puzzle))
                      (dec (count (first parsed-puzzle)))))
     count); 7597

(->> (second steps-all)
     (filter (complement second))
     (map first)
     (filter (partial inside-map? (dec (count parsed-puzzle))
                      (dec (count (first parsed-puzzle)))))
     count); 7689

(count
 (for [y (range (count parsed-puzzle))
       x (range (count (first parsed-puzzle)))
       :when (= rock (get-in parsed-puzzle [y x]))]
   [y x])) ; 1870

(* (count parsed-puzzle)
   (count (first parsed-puzzle))) ; 17161




(def steps-sample (count-steps-to-visit-all sample-input))

(let [m (parse-input sample-input)
      steps
      (count-possible-steps2 sample-input 20)]
  (println "---- visualization -----")
  (println
   (str/join
    "\n"
    (for [y (range -30 30)]
      (str/join
       (for [x (range -30 30)]
         (if (steps [y x])
           \O
           (wrapped-get m (count m) (count (first m)) [y x]))))))))
