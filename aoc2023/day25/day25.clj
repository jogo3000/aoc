(ns day25
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:import [java.security SecureRandom]))

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

(set! *warn-on-reflection* true)

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

(doseq [edge puzzle-network]
  (let [[a b] (vec edge)]
    (println a "--" b)))

;; Graphviz don't know how to to do it. But wait. What if I take the full graph and start moving edges to the other side and test if they stay separated

;; Other idea would be to find chokepoints in the network and remove them

;; How about Karger's algo?

sample-network

(defrecord KargerEdge [original edge])
(def ^SecureRandom random (SecureRandom.))

(sort-by first
         (frequencies (take 1000 (repeatedly #(.nextInt random (count sample-network))))))

(defn Karger [network]
  (let [edges (map #(if (= KargerEdge (type %)) %
                        (->KargerEdge % %)) network)
        e (nth edges (.nextInt random (count edges)))
        connected (->> edges
                       (filter #(seq (set/intersection (:edge e) (:edge %))))
                       (remove #(= e %))
                       #_(remove #(= (:edge e) (:edge %))))
        new-vertex (str/join (vec (:edge e)) )
        new-connections (->> connected
                             (map (fn [e2]
                                    (-> e2
                                        (update :edge (fn [edge]
                                                        (conj (set/difference edge (:edge e))
                                                              new-vertex))))))
                             (remove #(= (count (:edge %)) 1)))]
    (-> (set edges)
        (disj e)
        (set/difference (set connected))
        (into new-connections))))

(defn count-vertices [network]
  (count (reduce (fn [acc edge]
                   (let [[a b] (vec edge)]
                     (-> acc
                         (conj a)
                         (conj b))))
                 #{} network)))

(defn count-kg-vertices [network]
  (count (reduce (fn [acc edge]
                   (let [[a b] (vec (:edge edge))]
                     (-> acc
                         (conj a)
                         (conj b))))
                 #{} network)))

#_(sort-by second
           (frequencies
            (mapcat identity
                    (for [_ (range 100)]
                      (map :original (last (take-while #(> (count-vertices %) 2) (iterate Karger sample-network))))))))

;; #{"hfx" "pzl"}
;; #{"bvb" "cmg"}
;; #{"nvd" "jqt"}

(defn do-karger [network]
  (loop [knw (Karger network)]
    (if (= (count-kg-vertices knw) 2)
      (map :original knw)
      (recur (Karger knw)))))

(defn travel-network-kgs [network starting-point]
  (loop [queue (list starting-point)
         visited #{}]
    (if (empty? queue)
      visited
      (let [[a b] (vec (:edge (first queue)))
            connections-from-a (into #{}
                                     (comp
                                      (filter (comp #(contains? % a) :edge))
                                      (remove #(visited %)))
                                     network)
            all-connections (into connections-from-a
                                  (comp
                                   (filter (comp #(contains? % b) :edge))
                                   (remove #(visited %)))
                                  network)]
        (recur (into (rest queue)
                     all-connections)
               (into visited
                     all-connections))))))

(defn find-segments-kgs [network]
  (loop [network network
         segments #{}]
    (if (empty? network) segments
        (let [subnet (travel-network-kgs network (first network))]
          (recur (set/difference network subnet)
                 (conj segments subnet))))))

(defn find-answer-karger-stein [network]
  (->> (subsets 3 network)
   (map (juxt identity #(find-segments-kgs (set/difference network (set %)))))
   (remove #(< 2 (count (second %))))
   (reduce (fn [a b]
             (max-key (comp count second) a b)))))

(defn do-karger-stein
  ([network]
   (let [network (map #(if (= KargerEdge (type %)) %
                           (->KargerEdge % %)) network)]
     (do-karger-stein network (/ (count-kg-vertices network) (Math/sqrt 2)))))
  ([network n]
   (if (< n (* 2 (Math/sqrt 2)))
     (find-answer-karger-stein (set network))
     (loop [knw (Karger network)]
       (if-not (< (count-kg-vertices knw) n)
         (recur (Karger knw))
         (let [S1 (do-karger-stein knw)
               S2 (do-karger-stein knw)]
           (min-key (comp count first) S1 S2)))))))

(do-karger-stein sample-network)

(do-karger puzzle-network)

(/ 1212 (Math/sqrt 2)) 857.0134187980956

(defn do-karger-until-three [network]
  (loop []
    (let [kgw (set (do-karger network))]
      (if (= (count kgw) 3)
        kgw
        (recur)))))


(do-karger-until-three sample-network)

;; Not working exactly like it should
(let [network sample-network]
    (loop [c 100]
      (println "iterations to go" c)
      (if (<= c 0) :fail
          (let [candidates
                (do-karger-until-three network)]
            (assert (> (count candidates) 0))
            (if-let [result
                     (->> (subsets 3 candidates)
                          (some (fn [cands]
                                  (let [pared-network (set/difference network cands)
                                        pared-size (count pared-network)
                                        network-size (count network)
                                        _ (assert (< (count pared-network) (count network)))
                                        subnets (find-segments pared-network)]
                                    (when (>= (count subnets) 2)
                                      subnets))))
                          subnet-sizes
                          not-empty)]
              result
              (recur (dec c)))))))


;; edges found with graphviz
;; gbc --- hxr
;; tmt -- pnz
;; xkz -- mvv

(->> (find-segments (disj puzzle-network
                          #{"gbc" "hxr"}
                          #{"tmt" "pnz"}
                          #{"xkz" "mvv"}))
     subnet-sizes)

(* 744 766) 569904




#_(Karger puzzle-network)
