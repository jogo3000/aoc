(ns day12
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :as walk]))

(def puzzle-input (slurp "day12/input.txt"))

(def parsed (edn/read-string (str/replace puzzle-input #":" "")))

(let [numbers (atom 0)]
  (walk/postwalk (fn [v]
                   (when (number? v)
                     (swap! numbers + v))
                   v) parsed)
  @numbers); 191164

;; part deux

(let [numbers (atom 0)]
  (walk/prewalk (fn [v]
                  (when (number? v)
                    (swap! numbers + v))
                  (if (and (map? v)
                           (contains? (set (vals v)) "red"))
                    {}
                    v)) parsed)
  @numbers) ; 87842
