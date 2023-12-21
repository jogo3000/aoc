(ns day20
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def puzzle-input (str/trim (slurp "day20/input.txt")))

(def sample-input "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(def sample-input-2 "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")

(def flip-flop \%)
(def conjunction \&)
(def broadcaster "broadcaster")
(def output "output")

(defn parse-row [row]
  (let [[module targets] (str/split row #"\s*->\s*")
        broadcaster? (= "broadcaster" module)
        module-name (if broadcaster? module (subs module 1))
        module-type (if broadcaster? module (first module))
        outputs (str/split targets #"\s*,\s*" )]
    [module-name {:module-type module-type
                  :outputs outputs}]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-row)
       (into {})))

(defn setup-flip-flop [network module-name {:keys [module-type]}]
  (if (= module-type flip-flop)
    (update network module-name assoc :memory :off)
    network))

(defn connect-conjunction-modules [network module-name {:keys [outputs]}]
  (reduce (fn [acc o]
            (if (= conjunction (:module-type (acc o)))
              (update acc o
                      (fn [module]
                        (assoc-in module [:memory module-name] :low)))
              acc))
          network
          outputs))

(defn setup-network [network]
  (-> (reduce (fn [acc [module-name {:keys [module-type
                                            outputs] :as module}]]
                (-> acc
                    (setup-flip-flop module-name module)
                    (connect-conjunction-modules module-name module)))
              network
              network)
      (assoc "output" {:module-type "output" :low 0 :high 0})
      (assoc "rx" {:module-type "rx" :low 0 :high 0})))

(def sample-network (-> (parse-input sample-input)
                        setup-network))

(defn input->network [input]
  (-> (parse-input input)
      setup-network))

(defn process-pulse [network tick pulse-source pulse-target low-high]
  ;; (println pulse-source pulse-target low-high)
  (let [module (network pulse-target)
        outputs (:outputs module)]
    (condp = (:module-type module)
      broadcaster
      [network (map #(vector pulse-target % low-high) outputs)]

      flip-flop
      (cond
        (= :high low-high)
        [network]

        (= :low low-high)
        (let [memory (:memory module)
              sent-pulse (if (= memory :off) :high :low)]
          [(-> network
               (assoc-in [pulse-target :memory] (if (= memory :off) :on :off))
               #_(update-in [pulse-target :pulses] conj [tick sent-pulse]))
           (map #(vector pulse-target % sent-pulse) outputs)]))

      conjunction
      (let [memory (-> (:memory module)
                       (assoc pulse-source low-high))
            sent-pulse (if (every? (partial = :high) (vals memory))
                         :low :high)]
        [(-> network
             (assoc-in [pulse-target :memory] memory)
             #_(update-in [pulse-target :pulses] conj [tick sent-pulse]))
         (map #(vector pulse-target % sent-pulse) outputs)])

      output
      [(update-in network [pulse-target low-high] inc)]

      "rx"
      [(update-in network [pulse-target low-high] inc)]

      ;; Default
      [network])))

(process-pulse sample-network 1 :headquarters "broadcaster" :low)

(defn press-important-button [network tick]
  (let [initial-pulse  [:headquarters "broadcaster" :low]]
    (loop [processed-pulses {:low 0 :high 0}
           pulses [initial-pulse]
           network network
           tick tick]
      (if (empty? pulses)
        [(assoc network :tick tick) processed-pulses]
        (let [[updated-network new-pulses]
              (reduce (fn [[network pulses] pulse]
                        (let [[network' new-pulses] (apply (partial process-pulse network tick) pulse)]
                          [network' (into pulses new-pulses)]))
                      [network []]
                      pulses)]
          (recur
           (reduce (fn [processed-pulses pulse]
                     (update processed-pulses (last pulse) inc))
                   processed-pulses
                   pulses)
           new-pulses
           updated-network
           (inc tick)))))))

(press-important-button sample-network 0)

(defn repeatedly-press-button [input times]
  (loop [network (input->network input)
         processed-pulses {:low 0 :high 0}
         c 0]
    (if (>= c times)
      processed-pulses
      (let [[network' processed-pulses']
            (press-important-button network 0)]
        (recur network'
               (merge-with + processed-pulses processed-pulses')
               (inc c))))))

(repeatedly-press-button sample-input 1000)
{:low 8000, :high 4000}

(repeatedly-press-button sample-input-2 1000)
{:low 4250, :high 2750}

(repeatedly-press-button puzzle-input 1000)
{:low 17194, :high 43218}
; (* 17194 43218) 743090292


;; part deux

;; Find out what configuration is needed

(def puzzle-network (input->network puzzle-input))

;; Naive implementation
(defn press-button-until-rx-low [input]
  (loop [network (input->network input)
         c 0
         tick 0]
    (if (>= (get-in network ["rx" :low]) 1)
      c
      (let [[network' _processed-pulses']
            (press-important-button network (or (:tick network) tick))]
        (recur network'
               (inc c)
               (:tick network))))))

#_(press-button-until-rx-low puzzle-input)

;; too slow. The network can be traversed backwards by sending a single low pulse from rx backwards I think.

(def module-preceding-rx ;; Is a conjunction node in my case
  (->> puzzle-network
       (some #(when (contains? (set (:outputs (second %))) "rx")
                %))))

;; The node sends a low pulse when all connecting nodes have sent a high pulse

;; Backtrack to find next nodes
(->> (:memory (second module-preceding-rx))
     keys
     (map (juxt identity puzzle-network)))

'(["kv" {:module-type \&, :outputs ["ll"], :memory {"hb" :low}}]
  ["kl" {:module-type \&, :outputs ["ll"], :memory {"ff" :low}}]
  ["vb" {:module-type \&, :outputs ["ll"], :memory {"tj" :low}}]
  ["vm" {:module-type \&, :outputs ["ll"], :memory {"th" :low}}])


(defn backtrack-connecting-modules [network module-id]
  (filter #(when (contains? (set (:outputs (second %))) module-id)
             %) network))

(backtrack-connecting-modules puzzle-network "hb")

'(["xp" {:module-type \%, :outputs ["hb" "xf"], :memory :off}]
  ["fx" {:module-type \%, :outputs ["rl" "hb"], :memory :off}]
  ["rl" {:module-type \%, :outputs ["hb"], :memory :off}]
  ["ch" {:module-type \%, :outputs ["hb" "mv"], :memory :off}]
  ["mv" {:module-type \%, :outputs ["hb" "mx"], :memory :off}]
  ["dx" {:module-type \%, :outputs ["hb" "fx"], :memory :off}]
  ["fz" {:module-type \%, :outputs ["hb" "dx"], :memory :off}]
  ["km" {:module-type \%, :outputs ["fz" "hb"], :memory :off}]
  ["tp" {:module-type \%, :outputs ["hb" "sv"], :memory :off}])

(backtrack-connecting-modules puzzle-network "xp")
'(["mx" {:module-type \%, :outputs ["xp"], :memory :off}])

(backtrack-connecting-modules puzzle-network "mx")
'(["mv" {:module-type \%, :outputs ["hb" "mx"], :memory :off}]
  ["hb" {:module-type \&, :outputs ["sv" "xf" "kv" "tp" "mx"], :memory {"xp" :low, "fx" :low, "rl" :low, "ch" :low, "mv" :low, "dx" :low, "fz" :low, "km" :low, "tp" :low}}])

(backtrack-connecting-modules puzzle-network "mv")
'(["ch" {:module-type \%, :outputs ["hb" "mv"], :memory :off}])

(backtrack-connecting-modules puzzle-network "ch")
'(["sv" {:module-type \%, :outputs ["ch"], :memory :off}])

(backtrack-connecting-modules puzzle-network "sv")
'(["hb" {:module-type \&, :outputs ["sv" "xf" "kv" "tp" "mx"], :memory {"xp" :low, "fx" :low, "rl" :low, "ch" :low, "mv" :low, "dx" :low, "fz" :low, "km" :low, "tp" :low}}]
  ["tp" {:module-type \%, :outputs ["hb" "sv"], :memory :off}])

;; making loops, this won't work like this

;; Idea was to backtrack and find a state that would trigger message to "rx"
(defn find-dream-state [network target-module]
  (loop [network-state {}
         visited-modules #{}
         preceding-modules
         (backtrack-connecting-modules network target-module)]
    (if (empty? preceding-modules)
      network-state
      (recur
       (into network-state
             (map (fn [[id module]]
                    (condp = (:module-type module)
                      broadcaster
                      [id module]

                      flip-flop
                      [id (assoc module :memory :off)]

                      conjunction
                      [id (update module :memory #(update-vals % (constantly :high)))])))
             preceding-modules)
       (into visited-modules (keys preceding-modules))
       (->> preceding-modules
            (map first)
            (remove visited-modules)
            (mapcat (partial backtrack-connecting-modules network)))))))

(find-dream-state (input->network sample-input-2) "output")

(press-important-button
 (let [network (input->network sample-input-2)
       dream-state (find-dream-state network "output")]
   (-> dream-state
       (assoc "output" {:module-type "output" :low 0 :high 0}))) 0)

(def target-state (find-dream-state puzzle-network "rx"))

(let [network target-state]
  (-> network
      (assoc "rx" {:module-type "rx" :low 0 :high 0})
      (press-important-button 0)))

;; okay, but all the conjunction nodes could've gotten their memories set on an earlier position of the flip-flops

;; What about repeatedly pressing the button until find out when every node has
;; a period that we can analyze?
(defn periods-done? [network]
  (->> (dissoc network broadcaster "output" "rx" :tick)
       vals
       (every? #(loop [pulses (:pulses %)
                       high false
                       low false]
                  (if (and high low) true
                      (if (empty? pulses)
                        false
                        (recur (rest pulses)
                               (or high (= :high (second (first pulses))))
                               (or low (= :low (second (first pulses)))))))))))

(defn repeatedly-press-button-to-analyze-periods [input times]
  (loop [network (input->network input)
         processed-pulses {:low 0 :high 0}
         c 0
         tick 0M]
    (if (or (periods-done? network)
            (>= c times))
      network
      (let [[network' processed-pulses']
            (press-important-button network (or (:tick network) tick))]
        (recur network'
               (merge-with + processed-pulses processed-pulses')
               (inc c)
               (:tick network))))))

(def periods (repeatedly-press-button-to-analyze-periods puzzle-input 100000))

(periods-done? periods)

(periods "ll")
(:tick periods)

(dissoc periods broadcaster "output" "rx")
758741M

(->> periods
     (filter #(nil? (:pulses (second %)))))

(->> periods
     (map second)
     (map :pulses)
     (map count))

(keys periods)

;; Well this is not working. (It could've have but I didn't realize to remove the important nodes from the system)

;; I analyzed the network with graphviz (see the .svg file in the same directory. It seems that paring down the network makes it easier to analyze

(puzzle-network broadcaster)

(def lp-broadcaster (assoc-in puzzle-network [broadcaster :outputs] ["lp"]))

(def fn-broadcaster (assoc-in puzzle-network [broadcaster :outputs] ["fn"]))

(press-important-button lp-broadcaster 0)

(press-important-button fn-broadcaster 0)

(def pared-down-network (-> puzzle-network
                            (assoc-in [broadcaster :outputs] ["lp"])
                            (assoc-in ["ll" :memory] {"vb" :low})))

(defn try-network [network]
  (loop [network network
         c 0
         tick 0M]
    (if (or (>= (get-in network ["rx" :low]) 1) (> c 10000))
      [c network]
      (let [[network' _processed-pulses']
            (press-important-button network (or (:tick network) tick))]
        (recur network'
               (inc c)
               (:tick network))))))

(try-network pared-down-network) ; 3793

(try-network (-> pared-down-network
                 (assoc-in [broadcaster :outputs] ["tp"])
                 (assoc-in ["ll" :memory] {"kv" :low}))) ; 4013

(try-network (-> pared-down-network
                 (assoc-in [broadcaster :outputs] ["zz"])
                 (assoc-in ["ll" :memory] {"kl" :low}))) ; 3917

(try-network (-> pared-down-network
                 (assoc-in [broadcaster :outputs] ["fn"])
                 (assoc-in ["ll" :memory] {"vm" :low}))); 4051

;; Ah, they have different periods. The periods matching idea will work though

(defn gcd [a b]
  (loop [a a
         b b]
    (if (zero? (mod (max a b) (min a b)))
      (min a b)
      (recur (min a b) (mod (max a b) (min a b))))))

(defn lcm [a b]
  (/ (* a b)
     (gcd a b)))

(defn least-common-multiple [xs]
  (reduce lcm xs))



(least-common-multiple [3793 4013 3917 4051]) ; 241 528 184 647 003
