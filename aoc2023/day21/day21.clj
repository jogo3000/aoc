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

(def parsed-puzzle (parse-input puzzle-input))

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

(defn visualize-str [m steps]
  (let [steps (set steps)]
    (str/join
     "\n"
     (for [y (range (count m))]
       (str/join
        (for [x (range (count (first m)))]
          (if (steps [y x])
            \O
            (get-in m [y x]))))))))

(defn visualize [m steps]
  (let [steps (set steps)]
    (println "---- visualization -----")
    (println
     (visualize-str m steps))))


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

(defn count-steps-to-hit-point [input target]
  (let [m (parse-input input)
        height (count m)
        width (count (first m))
        starting-position (find-starting-position m)]
    (loop [{:keys [queue visited]} {:queue [starting-position]
                                    :visited {starting-position true}}
           steps 0]
      (if (or (> steps (* 2 width))
              (contains? visited target))
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

(def steps-to-hit-edge (count-steps-to-hit-point puzzle-input [65 0]))
(def steps-to-fill-map (count-steps-to-hit-point puzzle-input [65 -130]))
(def steps-to-fill-map-off (count-steps-to-hit-point puzzle-input [65 -133]))

(def on-steps-full (->> (second steps-to-fill-map)
                        (filter second)
                        (map first)
                        (filter (partial inside-map? (dec 131)
                                         (dec 131)))
                        (into #{})))

(def on-steps-full-count (count on-steps-full))  ;; 7597

(def off-steps-full (->> (second steps-to-fill-map-off)
                         (filter second)
                         (map first)
                         (filter (partial inside-map? (dec 131)
                                          (dec 131)))
                         (into #{})))

(def off-steps-full-count (count off-steps-full)) ; 7689


(def stones-whole-map
  (count
   (for [y (range (count parsed-puzzle))
         x (range (count (first parsed-puzzle)))
         :when (= rock (get-in parsed-puzzle [y x]))]
     [y x]))) ; 1870

; (apply + (map #(* 4 %) (range target-steps 0 -2))) ;; without rocks removed count is 702 322 399 865 956

; (quot target-steps (count parsed-puzzle)) ;; 202300
;; (* 202300 202300) ; 40 925 290 000 <- still way too many whole maps to go through
; (rem target-steps (count parsed-puzzle)) ; 65 <- exactly half of the puzzle!

;; This means the walk ends to the edge of the puzzle. I can see there is a clearance area that can be walked

;; The map can be divided into a central area, the nw, ne, sw and se quadrants

;; This means the distance traveled west is 202300 maps
;; distance traveled east is 202300 maps
;; distance traveled north is 202300 maps
;; distance traveled south is 202300 maps
;;
;;


;; Ok middle row is 202300 maps of "on", 202300 maps of "off, plus one in the middle "on"



;; let's see how the quadrants look like

;; top left quadrant
(defn only-top-left [places]
  (->> places
       (filter (fn [[y x]]
                 (< x (- 65 y))))))

(defn join-maps-horizontal [& maps]
  (->> maps
       (map #(map str/trim (str/split-lines %)))
       (apply interleave)
       (partition (count maps))
       (map #(apply str %))
       (str/join "\n")))

(defn join-maps-vertical [& maps]
  (str/join "\n" maps))

(defn remove-top-left [places]
  (->> places
       (remove (fn [[y x]]
                 (< x (- 65 y))))))

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
       (filter (fn [[y x]]
                 (<= x (- 130 (- 65 y)))))))

(defn only-top-right [places]
  (->> places
       (remove (fn [[y x]]
                 (<= x (- 130 (- 65 y)))))))

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
       (filter (fn [[y x]]
                 (>= x (- y 65))))))

(defn only-bottom-left [places]
  (->> places
       (remove (fn [[y x]]
                 (>= x (- y 65))))))

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
       (filter (fn [[y x]]
                 (<= x (- 130 (- y 65)))))))

(defn only-bottom-right [places]
  (->> places
       (remove (fn [[y x]]
                 (<= x (- 130 (- y 65)))))))

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

(def left-tip (->> on-steps-full
                   (remove-bottom-left)
                   (remove-top-left)
                   (into #{})
                   count
                   ;inc "on", so middle position unvisited
                   ))

(def right-tip-full (->> on-steps-full
                    (remove-bottom-right)
                    (remove-top-right)
                    (into #{})))

(def right-tip (count right-tip-full))

(def bottom-tip-full
  (->> on-steps-full
       (remove-bottom-right)
       (remove-bottom-left)
       (into #{})))

(def bottom-tip (count bottom-tip-full))

(def top-tip-full                            ; 5723
  (->> on-steps-full
       (remove-top-right)
       (remove-top-left)
       (into #{})))

(def top-tip (count top-tip-full))

(def only-bottom-left-off-full
  (->> off-steps-full
       (only-bottom-left)
       (into #{})))

(def only-bottom-left-off
  (->> off-steps-full
       (only-bottom-left)
       (into #{})
       count))

(def only-bottom-right-off-full
  (->> off-steps-full
       (only-bottom-right)
       (into #{})))

(def only-bottom-right-off
  (->> off-steps-full
       (only-bottom-right)
       (into #{})
       count))

(def only-top-right-off-full
  (->> off-steps-full
       (only-top-right)
       (into #{})))

(def only-top-right-off
  (->> off-steps-full
       (only-top-right)
       (into #{})
       count))

(def only-top-left-off-full
  (->> off-steps-full
       (only-top-left)
       (into #{})))

(def only-top-left-off
  (->> off-steps-full
       (only-top-left)
       (into #{})
       count))


(def top-right-removed-on ; 6661
  (->> on-steps-full
       (remove-top-right)
       (into #{})
       count ; inc
       ))

(def bottom-right-removed-on
  (->> on-steps-full
       (remove-bottom-right)
       (into #{})
       count ; inc
       ))

(def top-left-removed-on
  (->> on-steps-full
       (remove-top-left)
       (into #{})
       count ; inc
       ))

(def bottom-left-removed-on ; 6694
  (->> on-steps-full
       (remove-bottom-left)
       (into #{})
       count ; inc
       ))


(println
 (join-maps-horizontal
  (visualize-str parsed-puzzle only-bottom-right-off-full)
  (visualize-str parsed-puzzle top-tip-full)
  (visualize-str parsed-puzzle only-bottom-left-off-full)))



;; To be clear, first map is on and because the amount traveled right is
;; even (202300), the last is "on" too

;; Same goes for the top and bottom tips

;; AND that means that there are (- 202300 1) = 202299 full maps to all
;; directions. One "off" map in both ends makes (/ (- 202299 1) 2) = 101149
;; full "on" and 101150 "off" maps (+ 101149 101150 1) => 202300 matches

;; Because the amount of steps 26501365 is odd, the "on" map is the one with the
;; starting position unvisited

;; Ok middle row is 202300 maps of "on", 202300 maps of "off
;; does my calculation match then (+ 101150 101150) (+ 101149 101149) 202298
;; It does match
(let [on-steps on-steps-full-count
      off-steps off-steps-full-count
      rounds 202300]

  (letfn [(wing [extra1 extra2]
           (loop [steps 0
                  level 1]
             (if (not= (+ 2 level) rounds)
               (let [even-steps (if (odd? level) (- rounds level 2)
                                    (- rounds level 2 1))]
                 (recur (+ steps
                           (if (even? level) off-steps 0)
                           (* (/ even-steps 2) on-steps)
                           (* (/ even-steps 2) off-steps)
                           extra1
                           extra2)
                        (inc level)))

               ;; remaining extra step
               (+ steps extra2))))]

    (+ ;; starting map 1 "on" in the middle
     on-steps
     ;; pillars
     ;; right hand side 202300 maps right to the center, with 1 as the tip
     (* 101149 on-steps) (* 101150 off-steps) right-tip

     ;; top pillar is the same as right hand side but tip is different
     (* 101149 on-steps) (* 101150 off-steps) top-tip

     ;; bottom pillar
     (* 101149 on-steps) (* 101150 off-steps) bottom-tip

     ;; left pillar
     (* 101149 on-steps) (* 101150 off-steps) left-tip

     ;; right to top
     ;; Starting with 1 "on", final two maps are remove top right "on", only bottom left "off"
     ;; so amount of maps lessens by two each round until we reach the position its only the two and then one
     ;; (- 202300 2) 202298 which means half are "on" half are "off"
     (wing top-right-removed-on only-bottom-left-off)


     ;; left to top
     ;; Here the final ones are 1 only-bottom-right "off", 1 remove top left "on"
     (wing top-left-removed-on only-bottom-right-off)

     ;; left to bottom
     ;; Here final ones are only-top-right "off", remove-bottom-left "on"
     (wing bottom-left-removed-on only-top-right-off)

     ;; right to bottom
     ;; rightmost two are now remove bottom right "on" and only-top-left "off"
     (wing bottom-right-removed-on only-top-left-off)

     )))

;; 625580912434268N
;; dang, still not right. What the hell?
;; Forgot tho compensate for the not even steps
;; 625584004730924N Not right. But this seems closer to truth

;; 312798177808091 ;; No, wtf?


;; 312798177716352 ;; NO!


;; 312798177918652 ;; NO! Are my assumptions correct even


;; 312900492155132 ;; not right either


;; 312900491952832 ;; not right, but it didn't say too high or too low


;; Too low? 312798177716352 This is within the high limit but it does seem to be
;; a bit lower than expected. the map is pretty sparse

;; It seems that I overshoot badly on the wings
;; pillars + row  632 432 723 408 366
;; high limit     702 322 399 865 956
;; just the pillars is 50 015 348 991

;; so the left pillar was actually missing, which actually makes things a lot worse
;; 2 529 580 857 802 658
;; 2 529 568 353 980 941 Still too high, this is actually way too high!
;; The upper limit is below, so I'm roughly overshooting at least three to four times
;; well, in the ballpark
;;   702 322 399 865 956


;; 2529609279877830 is too high

                                        ; 1 kerros 202300 - 2, alkaa "on" loppuu "off" + reunapalat
                                        ; 2 kerros 202298 - 2, alkaa off loppuu off + reunapalat

#_(println
   (let [rounds 101]
     (loop [steps ""
            level 1]
       (if (not (= (+ 2 level) rounds))
         (recur (str (str
                      (str/join (remove nil?
                                        (cons (if-not (odd? level) \. nil)
                                              (take (if (odd? level) (- rounds level 2)
                                                        (- rounds level 2 1))
                                                    (cycle [\O \.])))))
                      "T"
                      "B")
                     \newline
                     steps)
                (inc level))

         ;; right to top remaining map
         (str "B\n" steps)))))
