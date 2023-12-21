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
(quot target-steps 130) ; 203856 full screens wide

(* target-steps target-steps);; ->  702 322 346 863 225  search area too large to hold in memory
(+ 26501365 26501365 26501365 26501365) ; 106 005 460 Something much more manageable

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

(def steps-on-on-map
  (->> (second steps-all)
       (filter second)
       (map first)
       (filter (partial inside-map? (dec (count parsed-puzzle))
                        (dec (count (first parsed-puzzle)))))
       count))                             ; 7597

(def steps-on-off-map
  (->> (second steps-all)
       (filter (complement second))
       (map first)
       (filter (partial inside-map? (dec (count parsed-puzzle))
                        (dec (count (first parsed-puzzle)))))
       count))                             ; 7689

(def stones-whole-map
  (count
   (for [y (range (count parsed-puzzle))
         x (range (count (first parsed-puzzle)))
         :when (= rock (get-in parsed-puzzle [y x]))]
     [y x]))) ; 1870

(* (count parsed-puzzle)
   (count (first parsed-puzzle))) ; 17161

(count parsed-puzzle)




(def steps-sample (count-steps-to-visit-all sample-input))

(let [n 20
      m (parse-input sample-input)
      steps
      (count-possible-steps2 sample-input n)]
  (println "---- visualization -----")
  (println
   (str/join
    "\n"
    (for [y (range (- n) (+ n 10))]
      (str/join
       (for [x (range (- n) (+ n 10))]
         (if (steps [y x])
           \O
           (wrapped-get m (count m) (count (first m)) [y x]))))))))


(let [n 200
      m (parse-input puzzle-input)
      steps
      (count-possible-steps2 puzzle-input n)]
  (println "---- visualization -----")
  (println
   (str/join
    "\n"
    (for [y (range (- n) (+ n 10))]
      (str/join
       (for [x (range (- n) (+ n 10))]
         (if (steps [y x])
           \O
           (wrapped-get m (count m) (count (first m)) [y x]))))))))

(let [n 7
      sample-input ".....\n.....\n..S..\n.....\n....."
      m (parse-input sample-input)
      steps
      (count-possible-steps2 sample-input n)]
  (println "---- visualization -----")
  (println
   (str/join
    "\n"
    (for [y (range (- n) (+ n 10))]
      (str/join
       (for [x (range (- n) (+ n 10))]
         (if (steps [y x])
           \O
           (wrapped-get m (count m) (count (first m)) [y x]))))))))


(apply + (map #(* 4 %) (range 7 0 -2))) 64

(apply + (map #(* 4 %) (range 5 0 -2))) 36

(apply + (map #(* 4 %) (range 5000 0 -2))) 25010000

(apply + (map #(* 4 %) (range target-steps 0 -2))) ;; without rocks removed count is 702 322 399 865 956

(quot target-steps (count parsed-puzzle)) (* 202300 202300) ; 40 925 290 000 <- still way too many whole maps to go through
(rem target-steps (count parsed-puzzle)) ; 65 <- exactly half of the puzzle!

;; This means the walk ends to the edge of the puzzle. I can see there is a clearance area that can be walked

;; The map can be divided into a central area, the nw, ne, sw and se quadrants

;; This means the distance traveled west is 202300 maps
;; distance traveled east is 202300 maps
;; distance traveled north is 202300 maps
;; distance traveled south is 202300 maps
;; 1 map for the tips of the corners
;;


;; Ok middle row is 202300 maps of "on", 202300 maps of "off



;; let's see how the quadrants look like

;; top left quadrant
(defn only-top-left [places]
  (->> places
       (filter (fn [[[y x] _]]
                 (< x (- 65 y))))))

(visualize parsed-puzzle
           (->> (only-top-left (second steps-all))
                (filter second)
                (map first)
                (into #{})))

(defn remove-top-left [places]
  (->> places
       (filter (fn [[[y x] _]]
                 (>= x (- 65 y))))))

(visualize parsed-puzzle
           (->> (remove-top-left (second steps-all))
                (filter second)
                (map first)
                (into #{})))

(println
 (str/join "\n"
           (for [y (range 131)]
             (str/join
              (for [x (range 131)]
                (if (> x (- 65 y))
                  \.
                  (get-in parsed-puzzle [y x])))))))

(def stones-top-left
  (reduce +
          (for [y (range 65)]
            (reduce +
                    (for [x (range 65)
                          :when (and (< x (- 65 y))
                                     (= rock (get-in parsed-puzzle [y x])))]
                      1))))) ; 234

;; top right quadrant
(defn remove-top-right [places]
  (->> places
       (filter (fn [[[y x] _]]
                 (<= x (- 130 (- 65 y)))))))

(visualize parsed-puzzle
           (->> (remove-top-right (second steps-all))
                (filter second)
                (map first)
                (into #{})))



(println
 (str/join "\n"
           (for [y (range 131)]
             (str/join
              (for [x (range 131)]
                (if (< x (- 130 (- 65 y)))
                  \.
                  (get-in parsed-puzzle [y x])))))))

(def stones-top-right
  (reduce +
          (for [y (range 131)]
            (reduce +
                    (for [x (range 131)
                          :when (and  (>= x (- 130 (- 65 y)))
                                      (= rock (get-in parsed-puzzle [y x])))]
                      1))))) ; 231

;; bottom left quadrant
(defn remove-bottom-left [places]
  (->> places
       (filter (fn [[[y x] _]]
                 (>= x (- y 65))))))

(visualize parsed-puzzle
           (->> (remove-bottom-left (second steps-all))
                (filter second)
                (map first)
                (into #{})))

(println
 (str/join "\n"
           (for [y (range 131)]
             (str/join
              (for [x (range 131)]
                (if (>= x (- y 65))
                  \.
                  (get-in parsed-puzzle [y x])))))))

(def stones-bottom-left
  (reduce +
          (for [y (range 131)]
            (reduce +
                    (for [x (range 131)
                          :when (and (< x (- y 65))
                                     (= rock (get-in parsed-puzzle [y x])))]
                      1))))) ; 263

;; bottom right quadrant
(defn remove-bottom-right [places]
  (->> places
       (filter (fn [[[y x] _]]
                 (<= x (- 130 (- y 65)))))))

(visualize parsed-puzzle
           (->> (remove-bottom-right (second steps-all))
                (filter second)
                (map first)
                (into #{})))


(println
 (str/join "\n"
           (for [y (range 131)]
             (str/join
              (for [x (range 131)]
                (if (< x (- 130 (- y 65)))
                  \.
                  (get-in parsed-puzzle [y x])))))))


(def stones-bottom-right
  (reduce +
          (for [y (range 131)]
            (reduce +
                    (for [x (range 131)
                          :when (and
                                 (> x (- 130 (- y 65)))
                                 (= rock (get-in parsed-puzzle [y x])))]
                      1))))) ; 224


(def stones-middle (- stones-whole-map
                      stones-top-right
                      stones-top-left
                      stones-bottom-right
                      stones-bottom-left))

(visualize parsed-puzzle
           (->> (second steps-all)
                (remove-bottom-left)
                (remove-top-left)
                (filter second)
                (map first)
                (into #{})))


(visualize parsed-puzzle
           (->> (second steps-all)
                (remove-bottom-right)
                (remove-top-right)
                (filter second)
                (map first)
                (into #{})))
