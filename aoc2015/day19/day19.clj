(ns day19
  (:require [clojure.string :as str]))

(def sample-input "H => HO
H => OH
O => HH

HOH")

(defn parse-input [s]
  (let [[replacements-str input] (str/split s #"\n\n")]
    [(->> replacements-str str/split-lines (map #(str/split % #" => ")) (map #(map vec %)))
     (vec input)]))

(defn count-replacements [og]
  (let [[replacements input] (parse-input og)]
    (->>
     (for [i (range (count input))
           :let [prefix (subvec input 0 i)
                 c (subvec input i (inc i))
                 postfix (subvec input (inc i))]]
       (->> replacements
            (map #(if (= (first %) c) (second %) c))
            (map #(concat prefix % postfix))
            (remove #(= (count input) (count %)))))
     (reduce into)
     distinct
     count)))

(count-replacements sample-input)

;; Samples work

(count-replacements (slurp "day19/input")) ;; 189 - too low

;; Oh yeah, the actual data contains replacements longer than 1 character
