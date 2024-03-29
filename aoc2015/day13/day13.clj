(ns day13
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "day13/input.txt"))

(def example-input "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.
")

;; Optimal total change of happiness for sample is 330

(defn parse-line [s]
  (let [[_ person-a gain-or-lose units person-b]
        (re-find #"(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)\." s)]
    [person-a person-b (case gain-or-lose
                "gain" (parse-long units)
                "lose" (- (parse-long units)))]))

(defn parse-happiness-key [input]
  (->> (str/split-lines input)
       (map parse-line)
       (reduce (fn [acc [person-a person-b units]]
                 (assoc-in acc [person-a person-b] units))
               {})))

(defn orderings [persons]
  (if-not (seq persons)
    '(())
    (mapcat (fn [p]
           (let [others (filter (partial not= p) persons)]
             (map #(cons p %)
                  (orderings others)))) persons)))

(defn tally-happiness [happiness-key seating]
  (let [all-pairs (concat (cons (first seating) (into seating (rest seating)))
                          (list (first seating)))]
    (->> all-pairs
         (partition 2 1)
         (map #(get-in happiness-key %))
         (reduce +))))

(let [happiness-key (parse-happiness-key example-input)
      orderings (orderings (keys happiness-key))]
  (reduce (fn [best ordering]
            (max best (tally-happiness happiness-key ordering)))
          0 orderings)) ; 330, works

(let [happiness-key (parse-happiness-key puzzle-input)
      orderings (orderings (keys happiness-key))]
  (reduce (fn [best ordering]
            (max best (tally-happiness happiness-key ordering)))
          0 orderings)) ;  733


;; Part two

(let [happiness-key (parse-happiness-key puzzle-input)
      with-me (-> happiness-key
                  (update-vals #(assoc % "Me" 0))
                  (assoc "Me" (into {}
                                    (map (fn [n] [n 0]))
                                    (keys happiness-key))))
      orderings (orderings (keys with-me))]
  (reduce (fn [best ordering]
            (max best (tally-happiness with-me ordering)))
          0 orderings)) ; 725

;; Quite sad that I made it worse
