(ns day8
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def puzzle-input (slurp "day8/input.txt"))


(let [s (slurp "day8/input.txt")]
                                        ;(-
  #_(count (str/replace s #"\n" ""))
  (->>
   (edn/read-string
    (str "["
         (str/replace s #"\\x" "\\\\u00")
         "]"))
   #_#_(map count)
   (reduce +))

                                        ;   )

  ) ; 1326 too low
