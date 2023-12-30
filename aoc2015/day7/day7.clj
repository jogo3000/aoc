(ns day7
  (:require [clojure.edn :as edn]))

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

(defn RSHIFT [s n]
  (bit-and (bit-shift-right s n) (int (Character/MAX_VALUE))))

(defn LSHIFT [s n]
  (bit-and (bit-shift-left s n) (int (Character/MAX_VALUE))))

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
            RSHIFT [(rest symbols) nil `(RSHIFT ~working-on ~(first symbols))]
            LSHIFT [(rest symbols) nil `(LSHIFT ~working-on ~(first symbols))]
            ;; Number, symbol
            [symbols nil s]
            )]
      (if-not (seq symbols) exprs
              (recur unprocessed
                     (if expr (conj exprs expr) exprs)
                     leftover-expr)))))

;; OK OK I will evaluate it as an AST

(def puzzle-input (slurp "day7/input.txt"))

(def program (->> (my-read-string puzzle-input)
                   (map vec)
                   (into {})))

(letfn [(eval-expr [eval-expr expr]
          #_(Thread/sleep 200)
          (let [res
                (cond
                  (number? expr) expr
                  (symbol? expr) (eval-expr eval-expr (get program expr))
                  (seq? expr) (eval (cons (first expr) (map (partial eval-expr eval-expr) (rest expr))))
                  :else (throw (Exception. (str "wad dis" expr))))]
            (println expr "->" res)
            res))]
  (let [mem (memoize eval-expr)]
    (mem mem (get program 'a)))) ;; 3176

;; Part 2

(def override (assoc program 'b 3176))

(letfn [(eval-expr [eval-expr expr]
          #_(Thread/sleep 200)
          (let [res
                (cond
                  (number? expr) expr
                  (symbol? expr) (eval-expr eval-expr (get override expr))
                  (seq? expr) (eval (cons (first expr) (map (partial eval-expr eval-expr) (rest expr))))
                  :else (throw (Exception. (str "wad dis" expr))))]
            (println expr "->" res)
            res))]
  (let [mem (memoize eval-expr)]
    (mem mem (get override 'a)))); 14710
