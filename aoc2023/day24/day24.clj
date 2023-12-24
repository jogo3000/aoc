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
       (map #(BigDecimal. %))
       (apply ->Hailstone)))

(defn parse-input [input]
  (->> input str/trim str/split-lines (map parse-hailstone-coordinates)))

(parse-input sample-input)

(def sample-boundaries (update-vals {:min-x 7 :max-x 27
                                     :min-y 7 :max-y 27
                                     :min-z 7 :max-z 27}
                                    #(BigDecimal/valueOf %)))

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

(defn divide [a b]
  (try (.divide a b)
       (catch ArithmeticException _
         (BigDecimal/valueOf (/ (double a) (double b)))
         #_(.divide a b 6 BigDecimal/ROUND_HALF_UP))))

(defn to-line [h]
  (let [k (divide (:vy h) (:vx h))
        c (.subtract (.multiply k (:px h)) (:py h))]
    {:y-factor 1
     :x-factor k
     :c-factor c}))

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

(defn solve-equation-pair [l1 l2]
  (let [lcm-x (lcm (:x-factor l1) (:x-factor l2))
        [la lb] (sort-by :x-factor [l1 l2])
        x-equalized-la
        (update-vals la #(* (divide lcm-x (:x-factor la)) %))
        x-equalized-lb (if (and (pos? (:x-factor x-equalized-la))
                                (pos? (:x-factor lb)))
                         (update-vals lb #(* -1M %))
                         lb)
        y (let [{:keys [y-factor c-factor]} (merge-with + x-equalized-la x-equalized-lb)]
            (divide (- c-factor) y-factor))

        y-equalized-la
        la
        y-equalized-lb (update-vals lb #(* -1M %))

        x (let [{:keys [x-factor c-factor]} (merge-with + y-equalized-la y-equalized-lb)]
            (divide (- c-factor) (- x-factor)))]
    [x y]))

(defn point-in-future? [h x y]
  ;; #day24.Hailstone{:px 19, :py 13, :pz 30, :vx -2, :vy 1, :vz -2}
  (and (if (pos? (:vx h))
         (> x (:px h))
         (< x (:px h)))
       (if (pos? (:vy h))
         (> y (:py h))
         (< y (:py h)))))

(defn hailstones-paths-cross-within-area? [area h1 h2]
  (let [l1 (to-line h1)
        l2 (to-line h2)]
    (and (not (= (:x-factor l1) (:x-factor l2))) ;; they can't be parallel
         (let [[x y :as _cross-point]
               (solve-equation-pair l1 l2)]
           (try
             (and (point-in-future? h1 x y)
                  (point-in-future? h2 x y)
                  (<= (:min-x area) x (:max-x area))
                  (<= (:min-y area) y (:max-y area)))
             (catch ArithmeticException _
               (println x y)))))))

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

(count-potentially-crossing-hailstones-2d puzzle-input puzzle-boundaries) ; 955

(let [[h1 h2] (take 2 (parse-input sample-input))]
  (hailstones-paths-cross-within-area? sample-boundaries h1 h2))

;; 5749 -- got rid of some rounding errors

;; 5638 too low? Maybe rounding errors?
