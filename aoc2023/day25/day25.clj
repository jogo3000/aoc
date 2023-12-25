(ns day25
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
")

(defn parse-row [s]
  (let [[head & tail] (str/split s #":*\s+")]
    (for [c tail]
      #{head c})))

(defn parse-input [input]
  (->> input str/trim str/split-lines (mapcat parse-row) set))

(defn travel-netrowk [network starting-point]
  (loop [queue (list starting-point)
         visited #{}]
    (if (empty? queue)
      visited
      (let [[a b] (vec (first queue))
            connections-from-a (into #{}
                                     (comp
                                      (filter #(contains? % a))
                                      (remove #(visited %)))
                                     network)
            all-connections (into connections-from-a
                                  (comp
                                   (filter #(contains? % b))
                                   (remove #(visited %)))
                                  network)]
        (recur (into (rest queue)
                     all-connections)
               (into visited
                     all-connections))))))

(def sample-network (parse-input sample-input))

(def sample-pared-down
  (disj sample-network
        #{"hfx" "pzl"}
        #{"bvb" "cmg"}
        #{"nvd" "jqt"}))

(defn find-segments [network]
  (loop [network network
         segments #{}]
    (if (empty? network) segments
        (let [subnet (travel-netrowk network (first network))]
          (recur (set/difference network subnet)
                 (conj segments subnet))))))

(defn subsets [n items]
  (cond
    (= n 0) '(())
    (empty? items) '()
    :else (lazy-cat (map
                     #(cons (first items) %)
                     (subsets (dec n) (rest items)))
                    (subsets n (rest items)))))

(defn subnet-sizes [subnets]
  (->> subnets
       (map #(reduce (fn [acc c]
                       (into acc c))
                     %))
       (map count)))

(->> (find-segments sample-pared-down)
     subnet-sizes)

(take 3 (subsets 3 sample-network))

(defn find-answer [network]
  (->>
   (some #(let [subnets (find-segments (set/difference network %))]
            (when (= 2 (count subnets))
              subnets))
         (subsets 3 sample-network))
   subnet-sizes))

(find-answer sample-network)

(defn find-answer2 [network]
  (let [network-v (vec network)
        size (count network)]
    (subnet-sizes
     (loop [i 0]
       (when-not (>= i size)
         (if-let [result
                  (let [network' (disj network (get network-v i))]
                    (loop [j i]
                      (when-not (>= j size)
                        (if-let [result
                                 (let [network'' (disj network' (get network-v j))]
                                   (loop [k j]
                                     (when-not (>= k size)
                                       (let [network''' (disj network'' (get network-v k))
                                             segments
                                             (find-segments network''')]
                                         (if (= (count segments) 2)
                                           segments
                                           (recur (inc k)))))))]
                          result
                          (recur (inc j))))))]
           result
           (recur (inc i))))))))

(find-answer2 sample-network)

#_(def sizes-part1
    (find-answer (parse-input (slurp "day25/input.txt"))))

(def puzzle-network (parse-input (slurp "day25/input.txt")))

(doseq [edge sample-network]
  (let [[a b] (vec edge)]
    (println a "--" b)))

;; Graphviz don't know how to to do it. But wait. What if I take the full graph and start moving edges to the other side and test if they stay separated

;; Other idea would be to find chokepoints in the network and remove them

;; How about Karger's algo?

sample-network

(defrecord KargerEdge [original edge])

(defn Karger [network]
  (let [edges (map #(if (= KargerEdge (type %)) %
                        (->KargerEdge % %)) network)
        e (rand-nth edges)
        connected (->> edges
                       (filter #(seq (set/intersection (:edge e) (:edge %))))
                       (remove #(= e %)))
        new-vertex (str/join (:edge e))
        new-connections (->> connected
                             (map (fn [e2]
                                    (update e2 :edge (fn [edge]
                                                       (conj (set/difference edge (:edge e))
                                                             new-vertex)))))
                             (remove #(= (count (:edge %)) 1)))]
    (-> (set edges)
        (disj e)
        (set/difference connected)
        (into new-connections))))

(drop-while #(> (count %) 3) (iterate Karger sample-network))

;; Not working exactly like it should
