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


(defn my-read-string [s]
  (loop [[s & symbols] (edn/read-string (str "[" s "]"))
         exprs []
         working-on nil]
    (let [[unprocessed expr leftover-expr]
          (case s
            NOT [(rest symbols) nil `(bit-not ~(first symbols))]
            -> [(rest symbols) `(def ~(first symbols) ~working-on) nil]
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

(defn remove-expr [program n]
  (let [[head tail] (split-at n program)]
    (into (vec head)
          (rest tail))))

(defn reorder-exprs [program] ;; FIXME
  (let [root-val-locs
        (->> program
             (map-indexed (fn [i [_ _ expr]]
                            (when (number? expr) i)))
             (remove nil?))
        root-vals (mapv #(get program %) root-val-locs)
        unordered-program (reduce
                           (fn [program i]
                             (remove-expr program i))
                           program
                           (reverse root-val-locs))

        def-locs
        (into {} (map-indexed (fn [i exp]
                                (let [[_def v _e] exp]
                                  [v i])))
              unordered-program)
        dependencies
        (->> unordered-program
             (map-indexed (fn [i [_ _var expr]]
                            (cond (number? expr) []
                                  (symbol? expr) [{expr i}]
                                  (seq? expr)
                                  (mapv (fn [symbol] {symbol i}) (rest expr)))))
             (mapcat identity)
             (apply merge-with min)
             (remove (comp number? first))
             (sort-by second))
        prioritized-definitions
        (into []
              (comp
               (map (fn [[sym loc]]
                      (when-let [def-loc (get def-locs sym loc)]
                        (let [definition (get unordered-program def-loc)]
                          definition))))
               (remove nil?))
              dependencies)]

    (-> (vec root-vals)
        (into prioritized-definitions)
        (into (reduce (fn [program [sym loc]]
                        (if-let [def-loc (get def-locs sym)]
                          (remove-expr program def-loc)
                          program))
                      unordered-program
                      (reverse (sort-by (comp def-locs first) dependencies)))))))

(defn get-list-of-symbols [program]
  (->> program
       flatten
       (filter symbol?)
       (remove #{`def `bit-and `bit-not `bit-or `bit-shift-right `bit-shift-left})))

(defn eval-string [s]
  (let [program (->> (my-read-string s)
                     (reorder-exprs))
        symbols (get-list-of-symbols program)
        declarations (mapv #(list `declare %) symbols)]
    (doseq [expr declarations]
      (eval expr))
    (doseq [expr program]
      (eval expr))))

(def puzzle-input (slurp "day7/input.txt"))

#_(eval-string puzzle-input)

(comment
  (eval-string sample)

  (def misordered-sample "x AND y -> d
x OR y -> e
123 -> x
456 -> y
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
")

  (reorder-exprs (my-read-string misordered-sample)) ;; TODO
  (reorder-exprs (my-read-string puzzle-input))




  )
