(ns day24
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "day24/input.txt"))

(def sample-input
"19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
")

(defrecord Hailstone [px py pz vx vy vz])

(defn parse-hailstone-coordinates [row]
  (->> (str/split row #"[,@\s]+")
       (map parse-long)
       (apply ->Hailstone)))

(defn parse-input [input]
  (->> input str/trim str/split-lines (map parse-hailstone-coordinates)))

(parse-input sample-input)

(def sample-boundaries {:min-x 7 :max-x 27
                        :min-y 7 :max-y 27
                        :min-z 7 :max-z 27})

;; how many checks I have to do?
(defn estimate-path-checks [hailstones]
  (loop [checks 0
         hailstones (dec hailstones)] ;; decrease because don't need to check against itself
    (if (zero? hailstones)
      checks
      (recur (+ checks hailstones)
             (dec hailstones)))))

;; for the sample data
(estimate-path-checks 10) ;; 10  checks

;; Full data
(estimate-path-checks 300) ;; 44850 ok not so bad, but the test area is big!

(defn estimate-area-checks [{:keys [min-x max-x min-y max-y]}]
  (* (- max-y min-y)
     (- max-x min-x)))

(estimate-area-checks sample-boundaries) ; 400 manageable

(def puzzle-boundaries {:min-x 200000000000000M
                        :max-x 400000000000000M
                        :min-y 200000000000000M
                        :max-y 400000000000000M
                        :min-z 200000000000000M
                        :max-z 400000000000000M})

(estimate-area-checks puzzle-boundaries) ; 40000000000000000000000000000M

;; That is clearly unmanageable

;; Also it states that disregard the z axis but. Why are they giving the z axis?
;; Part 2 is likely same thing in 3d?

;; How abouth some maths instead of brute forcing it?

;; Path of a hailstone is calculated by (in 2d)

;; xn = x + n*vx
;; yn = y + n*vy

;; So we want to have a place where two hailstones share xn, yn but with any n
;; because it is asking to diregard time
;; A equation pair then?

;; We need a way to rephrase the coordinate and velocity so that we get an
;; equation where zero is on the right side

(first (parse-input sample-input))
#day24.Hailstone{:px 19, :py 13, :pz 30, :vx -2, :vy 1, :vz -2}

;; That works out as
;; k = 1/-2 because k = delta-y / delta-x

;; y = kx + b
;; y - kx -b = 0 ;; This could work as a basis of solving the equation pair

;; y − y1 = m(x − x1)
;; y - y1 - m(x - x1) = 0
;; y - y1 - (mx - mx1) = 0
;; y - mx - y1 + mx1 = 0
;; y - mx + mx1 - y1 = 0

;; y = mx - mx1 + y1
;; y - kx + c = 0

(defn line-x->y [l x]
  (+ (* (:x-factor l) x)
     (:c-factor l)))

(defn to-line [h]
  (let [k (/ (:vy h) (:vx h))
        c (- (:py h) (* k (:px h)))]
    {:y-factor 1
     :x-factor k
     :c-factor c}))

(line-x->y (to-line #day24.Hailstone{:px 19, :py 13, :pz 30, :vx -2, :vy 1, :vz -2})
           19) ;;13N

(defn gcd [a b]
  (loop [a a
         b b]
    (if (zero? (mod (max a b) (min a b)))
      (min a b)
      (recur (min a b) (mod (max a b) (min a b))))))

(defn greatest-common-divisor [ps]
  (reduce gcd ps))

(defn lcm [a b]
  (let [gcd (gcd (abs a) (abs b))]
    (/ (* a b)
       (if (and (neg? a) (neg? b))
         (- gcd) gcd))))

(defn least-common-multiple [xs]
  (reduce lcm xs))

;; y - kx - c = 0 : * l
;; y - jx - d = 0

;; -ly + jx - lc = 0
;;   y - jx + d  = 0

;; (1 - l)y + (d - lc) = 0
;; (1 - l)y = - (d - lc)
;; y = - (d - lc) / (1 - l)

(defn solve-equation-pair [l1 l2]
  (let [lcm-x (lcm (abs (:x-factor l1)) (abs (:x-factor l2)))
        [la lb] (sort-by (comp abs :x-factor) [l1 l2])
        x-equalized-la
        (update-vals la #(* (/ lcm-x (:x-factor la)) %))
        x-equalized-lb
        (update-vals lb #(* (/ lcm-x (:x-factor lb)) %))
        same-sign? (or (and (pos? (:x-factor x-equalized-la))
                            (pos? (:x-factor x-equalized-lb)))
                       (and (neg? (:x-factor x-equalized-la))
                            (neg? (:x-factor x-equalized-lb))))
        x-equalized-lb (if same-sign?
                         (update-vals x-equalized-lb #(* -1 %))
                         x-equalized-lb)
        y (let [{:keys [y-factor c-factor]} (merge-with + x-equalized-la x-equalized-lb)]
            (/ c-factor
               y-factor))

        y-equalized-la
        la
        y-equalized-lb (update-vals lb #(* -1 %))

        x (let [{:keys [x-factor c-factor]} (merge-with + y-equalized-la y-equalized-lb)]
            (/ c-factor (- x-factor)))]
    [x y]))

(solve-equation-pair (to-line  #day24.Hailstone{:px 20 :py 19 :pz 25 :vx 1 :vy -5 :vz -3})
                     (to-line #day24.Hailstone{:px 20, :py 25, :pz 34, :vx -2, :vy -2, :vz -4}))


(line-x->y (to-line  #day24.Hailstone{:px 20 :py 19 :pz 25 :vx 1 :vy -5 :vz -3})
           19)

(line-x->y (to-line #day24.Hailstone{:px 20, :py 25, :pz 34, :vx -2, :vy -2, :vz -4})
           20)

(to-line  #day24.Hailstone{:px 20 :py 19 :pz 25 :vx 1 :vy -5 :vz -3})
{:y-factor 1, :x-factor -5, :c-factor 119}

(to-line #day24.Hailstone{:px 20, :py 25, :pz 34, :vx -2, :vy -2, :vz -4})
{:y-factor 1, :x-factor 1, :c-factor 5} (+ 119 5) (/ 124 6)
;; [19 -24]

;; [19 47/2]

;; [19 62]

(let [l (to-line #day24.Hailstone{:px 20 :py 19 :pz 25 :vx 1 :vy -5 :vz -3})]
  (line-x->y l 19)) ;; 24 - clearly not correct


(defn round-for-comparison [a]
  (if-not (ratio? a) a
          (try
            (.divide
             (BigDecimal. (numerator a))
             (BigDecimal. (denominator a)))
            (catch ArithmeticException _
              #_(println a)
              (double a)))))

(defn point-in-future? [h x y]
  ;; #day24.Hailstone{:px 19, :py 13, :pz 30, :vx -2, :vy 1, :vz -2}
  (let [decision (and (if (pos? (:vx h))
                        (>= x (:px h))
                        (<= x (:px h)))
                      (if (pos? (:vy h))
                        (>= y (:py h))
                        (<= y (:py h))))]
    #_(when-not decision
      (println "paths cross in the past for " h x y))
    decision))

#_(hailstones-paths-cross-within-area?
 sample-boundaries
 #day24.Hailstone{:px 20 :py 19 :pz 25 :vx 1 :vy -5 :vz -3}
 #day24.Hailstone{:px 20, :py 25, :pz 34, :vx -2, :vy -2, :vz -4}
 )


(defn hailstones-paths-cross-within-area? [area h1 h2]
  (let [l1 (to-line h1)
        l2 (to-line h2)]
    (and (not (= (:x-factor l1) (:x-factor l2))) ;; they can't be parallel
         (let [[x y :as _cross-point]
               (solve-equation-pair l1 l2)
               x (round-for-comparison x)
               y (round-for-comparison y)]
           (and (point-in-future? h1 x y)
                (point-in-future? h2 x y)
                (<= (:min-x area) x (:max-x area))
                (<= (:min-y area) y (:max-y area)))))))

(defn count-potentially-crossing-hailstones-2d [input boundaries]
  (let [hailstones (vec (parse-input input))]
    (count
     (into []
           (mapcat identity
                   (for [h1 (range (count hailstones))]
                     (for [h2 (range (inc h1) (count hailstones))
                           :when (hailstones-paths-cross-within-area? boundaries
                                                                      (get hailstones h1)
                                                                      (get hailstones h2))]
                       [h1 h2])))))))

(count-potentially-crossing-hailstones-2d sample-input sample-boundaries)

(count-potentially-crossing-hailstones-2d puzzle-input puzzle-boundaries) ; 12783


;; part deux

;; Ok, so we need to find a beam that would hit all of the rocks, z-dimension
;; included. This seems daunting, but I suppose I could find beams that would
;; hit a pair of hailstones and then count off every one that wouldn't hit the
;; rest of them?

;; We need to make the z-included hit detection as well, though. Let's start
;; with that. Here goes.

;; Example that would hit Hailstone 1
(defn solve-equation-pair-3d [h1 h2]
  (let [l1 (to-line h1)
        l2 (to-line h2)]
    (if (= (:x-factor l1) (:x-factor l2)) ;; parallel
      nil
      (let [[x y] (solve-equation-pair l1 l2)
            l3 (to-line (->Hailstone (:px h1) (:pz h1) nil (:vx h1) (:vz h1) nil))
            l4 (to-line (->Hailstone (:px h2) (:pz h2) nil (:vx h2) (:vz h2) nil))]
        (if (= (:x-factor l3) (:x-factor l4)) ;; parallel
          nil
          (let [[x' z] (solve-equation-pair l3 l4)]

            #_(assert (= y y') (str "These should match, otherwise it doesn't work " y  " " y'))
            (when (= x x')
              [x y z])))))))

(solve-equation-pair-3d
 (->Hailstone 19 13 30 -2 1 -2)
 (->Hailstone 24 13 10 -3 1  2)) ; [9N 18N 20] This seems to work. What about if it doesn't hit?

(solve-equation-pair-3d
 (->Hailstone 18 19 22 -1 -1 -2)
 (->Hailstone 24 13 10 -3 1  2))

(solve-equation-pair-3d
 (->Hailstone 19 13 30 -2 -1 -2)
 (->Hailstone 18, 19, 22 -1, -1, -2)) ; nil works, they shouldn't hit based on the puzzle description





;; Example: If a straight line is passing through the two fixed points in the
;; 3-dimensional whose position coordinates are P (2, 3, 5) and Q (4, 6, 12)
;; then its cartesian equation using the two-point form is given by

;; l = (4 – 2), m = (6 – 3), n = (12 – 5)

;; l = 2, m = 3, n = 7

;; Choosing the point P (2, 3, 5)

;; The required equation of the line

;; L : (x – 2) / 2 = (y – 3) /  3 = (z – 5) / 7


(defn beam-between [h1 h2 dt]
  (let [l (- (:px h2) (:px h1))
        m (- (:py h2) (:py h1))
        n (- (:pz h2) (:pz h1))]
    (->Hailstone (:px h1) (:py h1) (:pz h1) (/ l dt) (/ m dt) (/ n dt))))

(beam-between (->Hailstone 21 14 12 1 -5 -2)
              (->Hailstone 15 16 16 -1 -1 -2)
              2)

#day24.Hailstone{:px 21, :py 14, :pz 12, :vx -3, :vy 1, :vz 0}



;; This has the problem that it doesn't take into account the starting
;; point... But I think I will burn that bridge when I get there. Assuming this
;; can even find a line that would cross all the paths, I could backtrack to
;; find the points and determine the starting point from there

;; I really hope going tick by tick is enough
(defn move-hailstone [h]
  (-> h
      (update :px + (:vx h))
      (update :py + (:vy h))
      (update :pz + (:vz h))))

(defn move-hailstone-backwards [h]
  (-> h
      (update :px - (:vx h))
      (update :py - (:vy h))
      (update :pz - (:vz h))))

;; Desired result 24, 13, 10 and velocity -3, 1, 2

(defn find-candidates [hstones1 hstones2 dt]
  (remove
   nil?
   (mapcat identity
           (for [i (range (count hstones1))]
             (for [j (range (count hstones2))
                   :let [h1 (get hstones1 i)
                         h2 (get hstones2 j)]
                   :when (not= (:name h1) (:name h2))]
               (let [candidate-beam
                     (move-hailstone-backwards (beam-between h1 h2 dt))
                     comparison-stones (remove (fn [h] (= (mapv #(% candidate-beam) [:vx :vy :vz])
                                                          (mapv #(% h) [:vx :vy :vz]))) hstones2)]
                 #_(when (= candidate-beam #day24.Hailstone{:px 24, :py 13, :pz 10, :vx -3, :vy 1, :vz 2})
                   (def *hstones hstones1) (def *hstones2 hstones2)
                   (def *dt dt)
                   (def *comparison-stones comparison-stones))
                 (when (and (not (zero? (:vx candidate-beam)))
                            (not (zero? (:vy candidate-beam)))
                            (not (zero? (:vz candidate-beam)))
                            (not (ratio? (:px candidate-beam)))
                            (not (ratio? (:py candidate-beam)))
                            (not (ratio? (:pz candidate-beam)))
                            (every? #(solve-equation-pair-3d candidate-beam %)
                                    comparison-stones))
                   candidate-beam)))))))

(defn name-hs [hs]
  (map #(assoc % :name (gensym))
       hs))

#_(def res
    (let [hailstones (vec (map #(assoc % :name (gensym)) (parse-input puzzle-input)))
          c (count hailstones)
          max-iterations 10000
          hailstones-path (take (* max-iterations
                                   max-iterations)
                                (iterate #(mapv move-hailstone %) hailstones))]
      (loop [d0 0]
        (if (> d0 max-iterations)
          :fail
          (let [res
                (loop [dt 1]
                  (if (> dt max-iterations) :fail
                      (do (when (zero? (mod (* (inc d0) dt) 10))
                            (println "round " d0, dt))
                          (let [candidates
                                (find-candidates (nth hailstones-path d0)
                                                 (nth hailstones-path (+ d0 dt))
                                                 dt)]
                            (if (seq candidates) candidates
                                (recur (inc dt)))))))]
            (if (= :fail res)
              (recur (inc d0))
              (do (println res)
                  res)))))))


(def sample-hailstones (parse-input sample-input))

(def res
  (let [hailstones (vec (map #(assoc %
                                     :name (str (gensym))
                                     :generation 0) (parse-input sample-input)))]
    (loop [current-hailstone-positions hailstones
           all-hailstone-positions (into #{} current-hailstone-positions)
           round 1]
      (if (> round 10) :fail
          (let [new-hailstone-posisions (map (comp #(assoc % :generation round) move-hailstone)
                                             current-hailstone-positions)
                amassed-hailstone-positions (into all-hailstone-positions new-hailstone-posisions)
                candidate-pairs (->> amassed-hailstone-positions
                                     (partition 2 1)
                                     (remove (fn [[h1 h2]] (= (:generation h1) (:generation h2)))))]
            (if-let [result
                     (->> candidate-pairs
                          (some (fn [[h1 h2]]
                                  (let [older (min-key :generation h1 h2)
                                        newer (max-key :generation h1 h2)
                                        candidate-beam (->> (beam-between older newer (- (:generation newer)
                                                                                         (:generation older)))
                                                            move-hailstone-backwards)
                                        comparison-hailstones
                                        (remove (fn [h] (and (= (:vx candidate-beam)
                                                                (:vx h))
                                                             (= (:vy candidate-beam)
                                                                (:vx h))
                                                             (= (:vz candidate-beam)
                                                                (:vz h))))
                                                hailstones)]
                                    (when (and (not (zero? (:vx candidate-beam)))
                                               (not (zero? (:vy candidate-beam)))
                                               (not (zero? (:vz candidate-beam)))
                                               (not (ratio? (:px candidate-beam)))
                                               (not (ratio? (:py candidate-beam)))
                                               (not (ratio? (:pz candidate-beam)))
                                               (every? #(solve-equation-pair-3d candidate-beam %)
                                                       comparison-hailstones))
                                      candidate-beam)))))]
              result
              (recur new-hailstone-posisions
                     amassed-hailstone-positions
                     (inc round))))))))


(comment
  ;; Can I solve it in one dimension at a time?
  (defn max-x [hs]
    (reduce (fn [acc h]
              (max-key :px acc h)) hs))

  (defn max-vx [hs]
    (reduce (fn [acc h]
              (max-key :vx acc h)) hs))

  (defn min-x [hs]
    (reduce (fn [acc h]
              (min-key :px acc h)) hs))

  (defn distance [[x1 y1] [x2 y2]]
    (Math/sqrt (+ (Math/pow (double (- x2 x1)) (double 2)) (Math/pow (double (- y2 y1)) (double 2)))))

  (defn manhattan-distance [[x1 y1] [x2 y2]]
    (+ (abs (- x2 x1)) (abs (- y2 y1))))

  (defn project-to-x-dimension [hs]
    (->Hailstone (:px hs) 0 0 (:vx hs) 1 0))
  ;; I know this should work with max-x so start with that

  (defn search-x [hailstones]
    (let [projected-hailstones
          (map project-to-x-dimension hailstones)]
      (loop [search-x (inc (:px (max-x projected-hailstones)))
             dx (dec (:vx (max-vx projected-hailstones)))]

        (println search-x dx)
        (cond
          (= dx (- search-x)) (recur (inc search-x) (dec (:vx (max-vx projected-hailstones))))
          (zero? dx) (recur search-x (dec dx))
          (not-any? #(> (:vx %) dx) projected-hailstones) :fail2
          (some #(= dx (:vx %)) projected-hailstones)
          (recur search-x (dec dx))

          :else
          (let [candidate-hs (->Hailstone search-x 0 0 dx 1 0)
                candidate (to-line candidate-hs)
                intersections
                (sort (map #(solve-equation-pair candidate (to-line %)) projected-hailstones))
                #_ (println intersections)
                distances (map #(apply manhattan-distance %) (partition 2 1 intersections))
                #_ (println distances)
                gcd (when (every? pos? distances)
                      (greatest-common-divisor distances))]
            (if (> (or gcd 0) 1) candidate-hs
                (recur search-x (dec dx))))))))


  (search-x sample-hailstones)          ; Idea works


  #_(search-x (parse-input puzzle-input)) ; Unfortunately it is too slow for the real puzzle

  ;; NEED to pare down search space

  (def x-hailstones-sample (map project-to-x-dimension sample-hailstones))

  (defn left-edge [hs]
    (reduce (fn [acc h]
              (cond
                (< (:px acc) (:px h)) acc
                (> (:px acc) (:px h)) h
                :else
                (min-key :px acc h))) hs))

  (defn right-edge [hs]
    (reduce (fn [acc h]
              (cond
                (> (:px acc) (:px h)) acc
                (< (:px acc) (:px h)) h
                :else
                (max-key :vx acc h)))
            hs))

  (defn hailstones-collide? [hs]
    (let [x-positions (into #{} (map :px hs))]
      (< (count x-positions) (count hs))))

  #_(let [hailstones (->> (parse-input puzzle-input)
                          (map project-to-x-dimension)
                          name-hs)
          left (left-edge hailstones)
          right (right-edge hailstones)
          start-time 0]

      ;; Try out the right side first, because in the sample that is where the solution is found

      (loop [hailstones hailstones
             world-clock start-time]
        (let [rightmost (right-edge hailstones)
              others (disj (set hailstones) rightmost)]
          (if ;; Is there a way to identify this won't find a hit? I guess if the rock would have to travel so fast that it will be faster than some parallel lines
              ;; Many hailstones in the same position, this can't be it
              (contains? (->> others (map :px) set) (:px rightmost))
              (recur (map move-hailstone hailstones) (inc world-clock))

              (let [candidate
                    (loop [next-level (map move-hailstone others)
                           relative-clock 1]
                      (let [right-next (right-edge next-level)
                            others-next (disj (set next-level) right-next)]
                        (if (contains? (->> others-next (map :px) set) (:px right-next))
                          (recur (map move-hailstone next-level) (inc relative-clock))
                          (let [candidate (beam-between rightmost right-next relative-clock)
                                candidate-line (to-line candidate)
                                hit-points (map (partial solve-equation-pair candidate-line)
                                                (map to-line others-next))]

                            ;; If we find a nil, means that furthering the beam can't produce more hits
                            (if
                                (some nil? hit-points) nil

                                (let [hit-distances (->> hit-points
                                                         (into [[(:px rightmost) (:py rightmost)]])
                                                         sort
                                                         (partition 2 1)
                                                         (map #(apply manhattan-distance %))
                                                         set)]
                                  (if (and (seq hit-distances)
                                           (> (greatest-common-divisor hit-distances) 1))
                                    (update candidate :px - (:vx candidate))
                                    (recur (map move-hailstone next-level) (inc relative-clock)))))))))]
                (if candidate
                  candidate
                  (recur (map move-hailstone hailstones) (inc world-clock))))))))
  )
(comment
(to-line (->Hailstone 19 1 0 -2 1 0))


(let [hailstones (->> (parse-input sample-input)
                      (map project-to-x-dimension)
                      name-hs)
      left (left-edge hailstones)
      right (right-edge hailstones)
      start-time 0]

  ;; Try out the right side first, because in the sample that is where the solution is found

  (loop [hailstones hailstones
         world-clock start-time]
    ;; Advance world clock until all rocks are distinct
    (let [rightmost (right-edge hailstones)
          others (disj (set hailstones) rightmost)]
      (if ;; Many hailstones in the same position, this can't be it
          (contains? (->> others (map :px) set) (:px rightmost))
          (recur (map move-hailstone hailstones) (inc world-clock))

          (loop [others others
                 h-cand (right-edge others)
                 delta-v (- (:vx h-cand)
                            (:vx rightmost))]
            delta-v)))))

(apply min-key :vx (parse-input puzzle-input))
#day24.Hailstone{:px 455160135943041, :py 416020055225069, :pz 478237633141809, :vx -331, :vy -236, :vz -306}
(apply max-key :vx (parse-input puzzle-input))

#day24.Hailstone{:px 171893378980836, :py 226953722759994, :pz 236802876450239, :vx 289, :vy 102, :vz 121}

(apply min-key (comp abs :vx) (parse-input puzzle-input))

;; Slowest absolute speed
#day24.Hailstone{:px 249599421151031, :py 394069474260792, :pz 366619266165953, :vx 5, :vy -181, :vz -98}

(apply max-key (comp abs :vx) (parse-input puzzle-input))
;; Fastest absolute speed
#day24.Hailstone{:px 455160135943041, :py 416020055225069, :pz 478237633141809, :vx -331, :vy -236, :vz -306}

(sort-by :vx (parse-input puzzle-input))

(sort-by :px (parse-input puzzle-input))

8831802251766
16801569357148
20220675054975
455160135943041

(- 455160135943041 8831802251766)

; 446 328 333 691 275


(def parallel-by-vx (group-by :vx (parse-input puzzle-input)))

(->> (sort-by :px (parallel-by-vx -105))
     (map :px)
     (partition 2 1)
     (map (comp abs #(apply - %)))
     greatest-common-divisor) 225


(->> (sort-by :px (parallel-by-vx -12))
     (map :px)
     (partition 2 1)
     (map (comp abs #(apply - %)))
     greatest-common-divisor) ; 205425316258398 only 1 of these


(greatest-common-divisor
 (into
  (into
   (->> (sort-by :px (parallel-by-vx -105))
        (map :px)
        (partition 2 1)
        (map (comp abs #(apply - %))))

   (->> (sort-by :px (parallel-by-vx -12))
        (map :px)
        (partition 2 1)
        (map (comp abs #(apply - %)))))

  (->> (sort-by :px (parallel-by-vx 128))
       (map :px)
       (partition 2 1)
       (map (comp abs #(apply - %))))))


(reduce + (map :x-factor (map to-line (map project-to-x-dimension sample-hailstones)))) ; -2N
(reduce + (map :c-factor (map to-line (map project-to-x-dimension sample-hailstones)))) ; 59/2

(reduce + (map :vx sample-hailstones)) ; -5

(reduce + (map :vy sample-hailstones)) ; -9

(reduce + (map :vz sample-hailstones)) ; -12

(least-common-multiple (map :vx sample-hailstones)) -2

(least-common-multiple (map :x-factor (map to-line (map project-to-x-dimension sample-hailstones)))) -1N

(greatest-common-divisor (map :x-factor (map to-line (map project-to-x-dimension sample-hailstones))))
)


(reduce * (map :vx sample-hailstones)) ; 4

(greatest-common-divisor (map :px (parse-input puzzle-input)))

(sort-by second (frequencies (map :px (parse-input puzzle-input))))

(sort-by second (frequencies (map :py (parse-input puzzle-input))))

(sort-by second (frequencies (map :pz (parse-input puzzle-input))))

(map (juxt identity #(apply max-key % (parse-input puzzle-input))) [:vx :vy :vz])

; vx -331 289
; vy -88 842
; vz -909 878


(map (juxt identity #(apply min-key % (parse-input puzzle-input))) [:vx :vy :vz])

(* (- 289 -331) (- 842 -88) (- 878 -909)) ; k-space 1 030 384 200

(defn hit-positions
  "Checked 13.09"
  [candidate hailstone-lines]
  (let [candidate-line (to-line candidate)]
    (->> (map
          (fn [hl]
            (if (= (:x-factor hl) ;; Don't try with parallel lines
                   (:x-factor candidate-line)) :ignore
                (solve-equation-pair candidate-line hl)))
          hailstone-lines)
         (remove #(= :ignore %)))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn check-for-hits [h1 i hailstone-lines comparison-positions amount t]
  (loop [j 0]
    (if (>= j amount) nil
        ;; Don't compare with itself
        (if (= i j) (recur (inc j))
            (let [h2 (get comparison-positions j)
                  candidate (beam-between h1 h2 t)
                  hit-points (hit-positions candidate hailstone-lines)]
              (println h1 h2 candidate)

              (if (or (some nil? hit-points)
                      (some #(neg? (second %)) hit-points)) (recur (inc j))
                  (let [distances (->> hit-points
                                       (into [[(:px h1) (:py h1)]])
                                       sort
                                       (partition 2 1)
                                       (map #(apply manhattan-distance %))
                                       (map abs)
                                       set)]
                    (if (and (not-any? zero? distances)
                             (seq distances)
                             (> (greatest-common-divisor distances) 1))
                      (update candidate :px - (:vx candidate))
                      (recur (inc j))))))))))


(defn find-hits-in-next-rounds [max-rounds initial-positions hailstone-lines amount]
  (loop [c 1
         comparison-positions initial-positions]
    (if (> c max-rounds) nil
        (let [comparison-positions (mapv move-hailstone comparison-positions)
              hits
              (->>
               (map-indexed
                (fn [i h1]
                  (check-for-hits h1 i hailstone-lines comparison-positions amount (inc c)))
                initial-positions)
               (remove nil?))]
          (if (seq hits) (first hits)
              (recur (inc c) comparison-positions))))))

;; Try to iterate _once more_
#_(let [hailstones sample-hailstones
      amount (count hailstones)
      hailstones (mapv project-to-x-dimension hailstones)
      hailstone-lines (mapv to-line hailstones)
      max-rounds 10]
  (loop [initial-positions (mapv move-hailstone hailstones)
         c 1]
    (if (> c max-rounds)
      nil
      (let [hit
            (find-hits-in-next-rounds max-rounds initial-positions hailstone-lines amount)]
        (if hit hit
            (recur (map move-hailstone initial-positions)
                   (inc c)))))))


(map to-line sample-hailstones)
