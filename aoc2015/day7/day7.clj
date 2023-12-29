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

(defn find-root-vals [program]
  (->> program
       (map-indexed (fn [i [_ _ expr]]
                      (when (number? expr) i)))
       (remove nil?)))

(defn remove-expr [program loc]
  (let [[head tail] (split-at loc program)]
    (into (vec head)
          (rest tail))))

(defn remove-exprs [program locs]
  (reduce (fn [program loc]
            (remove-expr program loc))
          program
          (reverse locs)))

(defn definition-locations [program]
  (into {} (map-indexed (fn [i exp]
                          (let [[_def v _e] exp]
                            [v i])))
        program))

(defn dependencies [program]
  (->> program
       (map-indexed (fn [i [_ s expr]]
                      (cond (number? expr) []
                            (symbol? expr) [{s [expr]}]
                            (seq? expr)
                            (mapv (fn [symbol] (when (symbol? symbol)
                                                 {s [symbol]})) (rest expr)))))
       (mapcat identity)
       (remove nil?)
       (apply (partial merge-with into))))

(defn prioritized-definitions [program dependencies def-locs]
  (->> (loop [[head & queue] (keys dependencies)
              chain '()]
         (if-not head chain
                 (let [deps (get dependencies head)]
                   (recur (into (vec queue) (remove (set chain) deps))
                          (-> (cons head chain))))))
       (map-indexed (fn [i c] {c i}))
       (apply (partial merge-with min))
       (sort-by second)
       (map (fn [[sym _]]
              (when-let [def-loc (get def-locs sym)]
                (get program def-loc))))
       (remove nil?)))

(defn symbol-def-locations [def-locations symbols]
  (keep #(get def-locations %) symbols))

(let [program (my-read-string puzzle-input)
      root-val-locs (find-root-vals program)
      unordered (remove-exprs program root-val-locs)
      def-locs (definition-locations unordered)
      dependencies (dependencies (remove-exprs program root-val-locs))]
  (prioritized-definitions2 unordered
                            dependencies
                            def-locs))

(defn reorder-exprs [program]
  (let [root-val-locs (find-root-vals program)
        root-vals (mapv #(get program %) root-val-locs)
        unordered-program (remove-exprs program root-val-locs)

        def-locs (definition-locations unordered-program)
        dependencies (dependencies unordered-program)

        prioritized-definitions
        (prioritized-definitions unordered-program dependencies def-locs)

        prioritized-def-locations (symbol-def-locations def-locs (keys dependencies))

        remaining-exprs (remove-exprs unordered-program prioritized-def-locations)]

    (-> (vec root-vals)
        (into prioritized-definitions)
        (into remaining-exprs))))

  (reorder-exprs (my-read-string puzzle-input))

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

  (reorder-exprs (my-read-string misordered-sample))
  (reorder-exprs (my-read-string puzzle-input))




  )
