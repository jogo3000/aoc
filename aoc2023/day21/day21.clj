(ns day21
  (:require [clojure.string :as str]))

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
  (doseq [n (range 10)]
    (visualize m (count-possible-steps sample-input n))))

#_(count (count-possible-steps puzzle-input 64)); 3776 - This works

;; Part deux

(def target-steps 26501365)

(* target-steps target-steps);; ->  702 322 346 863 225  search area too large to hold in memory
