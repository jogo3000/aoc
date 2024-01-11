(ns day16
  (:require [clojure.string :as str]))

(def MFCSAM-analysis "children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
")

(defn parse-analysis [s]
  (read-string (str "{"
                    (str/replace s #":" "")
                    "}")))

(def puzzle-input (slurp "day16/input.txt"))

(let [mfcsam (parse-analysis MFCSAM-analysis)]
  (->> puzzle-input
       str/trim
       str/split-lines
       (keep (fn [s]
               (let [[sue-no memo] (str/split s #":" 2)
                     parsed-memo (parse-analysis memo)]
                 (when (every? (fn [[k v]] (= v (get mfcsam k))) parsed-memo)
                   sue-no)))))) ;; ("Sue 373")

;; part 2
(let [mfcsam (parse-analysis MFCSAM-analysis)]
  (->> puzzle-input
       str/trim
       str/split-lines
       (keep (fn [s]
               (let [[sue-no memo] (str/split s #":" 2)
                     parsed-memo (parse-analysis memo)]
                 (when (every? (fn [[k v]]
                                 (condp = k
                                   'cats (> v (get mfcsam k))
                                   'pomeranians (< v (get mfcsam k))
                                   'goldfish (< v (get mfcsam k))
                                   (= v (get mfcsam k)))) parsed-memo)
                   sue-no)))))) ;; ("Sue 260")

;; That was easy?
