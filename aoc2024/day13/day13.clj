(ns day13
  (:require [clojure.string :as str]))

(def sample-data
  "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
")

(defn parse-input [s]
  (->> (str/split s #"\n\n")
       (map (fn [s] (->> s str/split-lines
                         (map #(str/split % #":|,\w*"))
                         (map (fn [[role x y]]
                                {:role role
                                 :x (parse-long (subs x 3))
                                 :y (parse-long (subs y 3))}))
                         (partition 3)
                         (map (fn [[A B Prize]]
                                {:A (select-keys A [:x :y])
                                 :B (select-keys B [:x :y])
                                 :prize (select-keys Prize [:x :y])})))))))

(defn find-solutions [game]
  (let [{:keys [A B prize]} game]
    (for [a (range 100)
          b (range 100)
          :let [x (+ (* a (:x A))
                     (* b (:x B)))
                y (+ (* a (:y A))
                     (* b (:y B)))]
          :when (= prize {:x x :y y})]
      [a b])))

(defn cost [[a b]]
  (+ (* 3 a) b))

(->> sample-data
     parse-input
     (map find-solutions)
     (map #(map cost %))
     (keep not-empty)
     (map #(apply min %))
     (apply +)) ; 480 - seems to work


(->> (slurp "day13/input")
     parse-input
     (map find-solutions)
     (map #(map cost %))
     (keep not-empty)
     (map #(apply min %))
     (apply +)) ; 37686
