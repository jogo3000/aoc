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

(def sample-boundaries {:min-x 7 :max-x 27
                        :min-y 7 :max-y 27})

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
                        :max-y 400000000000000M})

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
            l3 (to-line (->Hailstone (:py h1) (:pz h1) nil (:vy h1) (:vz h1) nil))
            l4 (to-line (->Hailstone (:py h2) (:pz h2) nil (:vy h2) (:vz h2) nil))]
        (if (= (:x-factor l3) (:x-factor l4))
          nil
          (let [[y' z] (solve-equation-pair l3 l4)]

            (assert (= y y') "These should match, otherwise it doesn't work")
            [x y z]))))))

(solve-equation-pair-3d
 (->Hailstone 19 13 30 -2 1 -2)
 (->Hailstone 24 13 10 -3 1  2)) ; [9N 18N 20] This seems to work. What about if it doesn't hit?

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
        n (- (:pz h2) (:pz h2))]
    (->Hailstone (:px h1) (:py h1) (:pz h1) (/ l dt) (/ m dt) (/ n dt))))

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
                     (beam-between h1 h2 dt)]
                 (when (and (pos? (:vx candidate-beam))
                            (pos? (:vy candidate-beam))
                            (pos? (:vz candidate-beam)))
                   (every? #(solve-equation-pair-3d candidate-beam %)
                           (remove #(or (= (:name %) (:name h1))
                                        (= (:name %) (:name h2))) hstones1)))))))))

(defn name-hs [hs]
  (map #(assoc % :name (gensym))
       hs))

#_(let [hailstones (vec (map #(assoc % name (gensym)) (parse-input sample-input)))
      c (count hailstones)]
  (loop [dt 1
         hailstones-path [hailstones (mapv move-hailstone hailstones)]]
    (if (> dt 10000) :fail
        (let [candidates
              (find-candidates (get hailstones-path 0)
                               (get hailstones-path dt)
                               dt)]
          (if (seq candidates) candidates
              (recur (inc dt) (conj hailstones-path (mapv move-hailstone hailstones))))))))


(def sample-hailstones (parse-input sample-input))

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


(search-x sample-hailstones) ; Idea works


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

(let [hailstones (->> sample-hailstones
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
      (if
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
                            candidate-line (to-line candidate)]
                        ;; This found a good candidate, but I still need to prove it.
                        (map (partial solve-equation-pair candidate-line)
                             (map to-line others-next))))))]
            (if candidate
              candidate
              (recur (map move-hailstone hailstones) (inc world-clock))))))
    )

  )
