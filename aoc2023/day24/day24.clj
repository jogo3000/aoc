(ns day24
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "day24/input.txt"))

(def sample-input "19, 13, 30 @ -2,  1, -2
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

(defn lcm [a b]
  (let [gcd (gcd (abs a) (abs b))]
    (/ (* a b)
       (if (and (neg? a) (neg? b))
         (- gcd) gcd))))

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

(hailstones-paths-cross-within-area?
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


;; Maybe I'm getting values from the past now
;; 5749 -- got rid of some rounding errors
;; 5638 too low? Maybe rounding errors?
