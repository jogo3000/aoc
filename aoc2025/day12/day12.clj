(ns day12
  (:require [clojure.string :as str]))

(def sample (slurp "day12/sample"))

(defn parse-shape [s]
  (let [id (->> s first str parse-long)
        shape (->> (subs s 3) str/split-lines (mapv vec))]
    [id shape]))

(defn parse-region [s]
  (let [[size & counts] (str/split s #"\s")]
    {:size (->> (str/split size #"[x:]") (map parse-long))
     :counts (->> counts (map-indexed (fn [i s] [i (parse-long s)])) (into {}))}))

(defn parse-input [input]
  (let [segments (-> input str/trim (str/split #"\n\n"))
        shapes (->> segments butlast (map parse-shape) (into {}))
        regions (->> segments last str/split-lines (map parse-region))]
    [shapes regions]))

(parse-input sample)
