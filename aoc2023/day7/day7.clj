(ns day7
  (:require [clojure.string :as str]))

(def card-strengths '[\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2])

(def sample-input
  "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(def strengths [:five-of-a-kind
                :four-of-a-kind
                :full-house
                :three-of-a-kind
                :two-pair
                :one-pair
                :high])

(defn eval-hand [hand]
  (let [cards (->> hand
                   frequencies
                   (sort-by second)
                   reverse)]
    [(cond
       (= 5 (second (first cards))) :five-of-a-kind
       (= 4 (second (first cards))) :four-of-a-kind
       (and (= 3 (second (first cards)))
            (= 2 (second (second cards)))) :full-house
       (and (= 3 (second (first cards)))
            (= 1 (second (second cards)))) :three-of-a-kind
       (and (= 2 (second (first cards)))
            (= 2 (second (second cards)))) :two-pair
       (and (= 2 (second (first cards)))
            (= 1 (second (second cards)))) :one-pair
       :else :high) hand]))

(defn compare-hands [hand1 hand2]
  (let [[hand1-value hand1-cards]
        (eval-hand hand1)
        [hand2-value hand2-cards]
        (eval-hand hand2)
        type-comp (- (.indexOf strengths hand1-value)
                     (.indexOf strengths hand2-value))]
    (if-not (zero? type-comp)
      type-comp
      (loop [hand1-left hand1-cards
             hand2-left hand2-cards]
        (let [h1-high (first hand1-left)
              h2-high (first hand2-left)
              val-comp (- (.indexOf card-strengths h1-high)
                          (.indexOf card-strengths h2-high))]
          (if-not (zero? val-comp)
            val-comp
            (recur (rest hand1-left)
                   (rest hand2-left))))))))

(defn count-winnings [input]
  (let [hand-bids (->> input str/split-lines (map #(str/split % #"\s+")))
        sorted (reverse (sort-by first compare-hands hand-bids))]
    (->> sorted
         (map-indexed (fn [rank [cs bid]]
                        (* (inc rank) (parse-long bid))))
         (reduce +))))


(count-winnings sample-input)

(let [hand-bids (->> (str/trim (slurp "/home/uusitalo/git/aoc/aoc2023/day7/input.txt"))
                     str/split-lines (map #(str/split % #"\s+")))
      sorted (reverse (sort-by first compare-hands hand-bids))]
  (->>
   sorted
   (map (fn [[hand bid]]
          [(sort-by #(.indexOf card-strengths %) hand) bid]))))


(count-winnings (str/trim (slurp "/home/uusitalo/git/aoc/aoc2023/day7/input.txt")))

;; 251121738
