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
     (for [r replacements
           i (range (count input))
           :let [rc (count (first r))
                 prefix (subvec input 0 i)
                 c (subvec input i (min (+ i rc) (count input)))
                 postfix (subvec input (min (+ i rc) (count input)))]]
       (->> replacements
            (map #(if (= (first %) c) (second %) nil))
            (remove nil?)
            (map #(concat prefix % postfix))))
     (reduce into)
     distinct
     count)))

(count-replacements sample-input)

;; Samples work

(count-replacements (slurp "day19/input")) ;; 509
