

(def sample-input
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
")

(require '[clojure.string :as str])


(defn parse-input [s]
  (->> s
       str/trim
       str/split-lines
       (reduce (fn [acc row]
                 (assoc acc (subs row 6 8)
                        {:flow (java.lang.Integer/parseInt (subs row 23 (str/index-of row ";")))
                         :tunnels (map str/trim (str/split (second (str/split row #"valves?")) #","))}) )
               {})))

(defn all-valves-opened? [maze]
  (every? #(zero? (:flow %)) (vals maze)))

(defn walk*
  "Walk from start to end. Too slow on big inputs!"
  [maze pos rate score rounds]
  (cond (zero? rounds) (list rate (list pos))
        (all-valves-opened? maze) (list (+ (* rounds rate) score) (list :stopped-ast pos))
        :else
        (let [{:keys [flow tunnels]} (get maze pos)]
          (let [[subscore path]
                (apply (partial max-key first)
                       (keep identity
                             (cons
                              (when (pos? flow)
                                ;; Open valve
                                (let [[subscore path]
                                      (walk* (assoc-in maze [pos :flow] 0)
                                             pos (+ rate flow) (+ rate score) (dec rounds))]
                                  (list subscore (cons :open-valve (rest path)))))
                              (map (fn [tunnel]
                                     (walk* maze
                                            tunnel
                                            rate
                                            (+ rate score)
                                            (dec rounds))) tunnels))))]
            (list (+ rate subscore) (cons pos path))))))


(walk*
 (parse-input sample-input)
 "AA"
 0
 0
 10)

;; A walk of 10 steps would create this (300 ("AA" "DD" :open-valve "AA" "BB" :open-valve "AA" "II" "JJ" :open-valve "II"))

(* 6 6 6 6 6 6)

46656
