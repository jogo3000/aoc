(ns day15
  (:require [clojure.string :as str]))

(def puzzle-input "Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8
")

(def parsed-input
  (->> puzzle-input
       str/trim
       str/split-lines
       (map (fn [s]
              (let [[_ spice properties] (re-find #"(\w+): (.+)" s)]
                {spice (read-string (str "{" properties "}"))})))
       (apply merge)))

(defn eval-score [ingredients property a b c d]
  (+ (* a (get-in ingredients ["Sprinkles" property]))
     (* b (get-in ingredients ["PeanutButter" property]))
     (* c (get-in ingredients ["Frosting" property]))
     (* d (get-in ingredients ["Sugar" property]))))

(apply max
       (for [a (range 101)
             b (range 101)
             c (range 101)
             d (range 101)
             :when (= 100 (+ a b c d))]
         (let [capacity-score (eval-score parsed-input 'capacity a b c d)
               durability-score (eval-score parsed-input 'durability a b c d)
               flavor-score (eval-score parsed-input 'flavor a b c d)
               texture-score (eval-score parsed-input 'texture a b c d)]
           (* (max 0 capacity-score)
              (max 0 durability-score)
              (max 0 flavor-score)
              (max 0 texture-score))))) ; 13882464


;; Part two

(apply max
       (for [a (range 101)
             b (range 101)
             c (range 101)
             d (range 101)
             :when (= 100 (+ a b c d))]
         (let [capacity-score (eval-score parsed-input 'capacity a b c d)
               durability-score (eval-score parsed-input 'durability a b c d)
               flavor-score (eval-score parsed-input 'flavor a b c d)
               texture-score (eval-score parsed-input 'texture a b c d)
               calories-score (eval-score parsed-input 'calories a b c d)]
           (if (= 500 calories-score)
             (* (max 0 capacity-score)
                (max 0 durability-score)
                (max 0 flavor-score)
                (max 0 texture-score))
             0)))) ; 11171160
