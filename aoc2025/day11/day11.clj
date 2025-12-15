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

;; Part 2
(def sample2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
")

(defn looping? [path node]
  (some #(= node %) path))

;; Something odd about the graph, takes super long

(defn parse-and-reverse [input]
  (->> (parse-input input)
       (reduce (fn [edges [source sinks]]
                 (->> sinks
                      (map #(hash-map % [source]))
                      (reduce (fn [edges sink->source]
                                (merge-with into edges sink->source)) edges))) {})))

;; Tried to go from end to start, didn't work. Too slow still
#_
(count
 (let [network (parse-and-reverse (slurp "day11/input")
                                  #_sample2)
       start-node "out"
       goal-node "svr"]
   (loop [qf #{(list start-node)}
          paths #{}]
     (if (empty? qf) paths
         (let [current (first qf)
               neighbours (get network (first current))
               unvisited-neighbours (->> neighbours (remove #(= % goal-node)))
               complete-paths (->> neighbours (filter #(= % goal-node)))]
           (recur (->> unvisited-neighbours
                       (remove (partial looping? current))
                       (map #(cons % current))
                       (into (disj qf current)))
                  (->> (map #(cons % current) complete-paths)
                       (remove #(not (and (some (fn [node] (= node "dac")) %)
                                          (some (fn [node] (= node "fft")) %))))
                       (into paths))))))))
