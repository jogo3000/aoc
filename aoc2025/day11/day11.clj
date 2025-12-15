(ns day11
  (:require [clojure.string :as str]))

(def sample "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
")

(defn parse-input [input]
  (->> input str/trim str/split-lines
       (map (fn [s] (let [[source & sinks] (str/split s #"[:\s]+")]
                      [source
                       sinks])))
       (into {})))

(count
 (let [network (parse-input (slurp "day11/input"))]
   (loop [qf #{(list "you")}
          paths #{}]
     (if (empty? qf) paths
         (let [current (first qf)
               neighbours (get network (first current))
               unvisited-neighbours (->> neighbours (remove #(= % "out")))
               complete-paths (->> neighbours (filter #(= % "out")))]
           (recur (-> qf (disj current)
                      (into (->> unvisited-neighbours (map #(cons % current)))))
                  (into paths (map #(cons % current) complete-paths)))))))) ; 448
