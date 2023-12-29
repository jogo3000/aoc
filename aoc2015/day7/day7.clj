(ns day7
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk])
  (:import [java.lang Math]))

(def sample "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
")

(def sample-result '{d 72
                     e 507
                     f 492
                     g 114
                     h 65412
                     i 65079
                     x 123
                     y 456})

(defn NOT [n]
  (reduce (fn [acc n]
          (bit-flip acc n)) n (range 16)))

(defn my-read-string [s]
  (loop [[s & symbols] (edn/read-string (str "[" s "]"))
         exprs []
         working-on nil]
    (let [[unprocessed expr leftover-expr]
          (case s
            NOT [(rest symbols) nil `(NOT ~(first symbols))]
            -> [(rest symbols) `(~(first symbols) ~working-on) nil]
            AND [(rest symbols) nil `(bit-and ~working-on ~(first symbols))]
            OR [(rest symbols) nil `(bit-or ~working-on ~(first symbols))]
            RSHIFT [(rest symbols) nil `(bit-shift-right ~working-on ~(first symbols))]
            LSHIFT [(rest symbols) nil `(bit-shift-left ~working-on ~(first symbols))]
            ;; Number, symbol
            [symbols nil s]
            )]
      (if-not (seq symbols) exprs
              (recur unprocessed
                     (if expr (conj exprs expr) exprs)
                     leftover-expr)))))

(defn eval-wires [input]
  (loop [wires (into {} (map vec) (my-read-string input))]
    (if (every? number? (vals wires)) wires
        (recur
         (reduce (fn [acc [v _]]
                   (update acc v
                           #(walk/postwalk
                             (fn [v]
                               (cond
                                 (number? v) v
                                 (symbol? v) (get acc v v)
                                 (seq? v) (if (every? number? (rest v))
                                            (eval v)
                                            v)
                                 :else v)) %)))
                 wires
                 wires)))))

(def puzzle-input (slurp "day7/input.txt"))

(comment
  (eval-wires puzzle-input)

  (int (Character/MAX_VALUE)))
