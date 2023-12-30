(ns day8
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def puzzle-input (slurp "day8/input.txt"))


(let [s puzzle-input]
  (-
   (count (str/replace s #"\s+" ""))
   (->>
    (edn/read-string
     (str "["
          (str/replace s #"\Q\x\E" "\\\\u00")
          "]"))
    (map count)
    (reduce +))

   )

  ) ; 1326 too low

(defn decode [s]
  (-> s
      (str/replace #"\Q\\\E" "_")
      (str/replace #"\\x\p{XDigit}{2}" "_")
      (str/replace #"\\\"" "_")
      (str/replace #"\"" "")))

(let [s (str/trim puzzle-input)
      parts (map str/trim (str/split-lines s))]
  (- (->> parts
          (map count)
          (reduce +))
     (->> parts
          (map (comp count decode))
          (reduce +)))) 1342


;; Part deux
