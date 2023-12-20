(ns day20
  (:require [clojure.string :as str]))

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

(defn process-pulse [network pulse-source pulse-target low-high]
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
        (let [memory (:memory module)]
          [(assoc-in network [pulse-target :memory] (if (= memory :off) :on :off))
           (map #(vector pulse-target % (if (= memory :off) :high :low)) outputs)]))

      conjunction
      (let [memory (-> (:memory module)
                       (assoc pulse-source low-high))]
        [(assoc-in network [pulse-target :memory] memory)
         (map #(vector pulse-target % (if (every? (partial = :high) (vals memory))
                                        :low :high)) outputs)])

      output
      [(update-in network [pulse-target low-high] inc)]

      "rx"
      [(update-in network [pulse-target low-high] inc)]

      ;; Default
      [network])))

(process-pulse sample-network :headquarters "broadcaster" :low)

(defn press-important-button [network]
  (let [initial-pulse  [:headquarters "broadcaster" :low]]
    (loop [processed-pulses {:low 0 :high 0}
           pulses [initial-pulse]
           network network]
      (if (empty? pulses)
        [network processed-pulses]
        (let [[updated-network new-pulses]
              (reduce (fn [[network pulses] pulse]
                        (let [[network' new-pulses] (apply (partial process-pulse network) pulse)]
                          [network' (into pulses new-pulses)]))
                      [network []]
                      pulses)]
          (recur
           (reduce (fn [processed-pulses pulse]
                     (update processed-pulses (last pulse) inc))
                   processed-pulses
                   pulses)
           new-pulses
           updated-network))))))

(press-important-button sample-network)

(defn repeatedly-press-button [input times]
  (loop [network (input->network input)
         processed-pulses {:low 0 :high 0}
         c 0]
    (if (>= c times)
      processed-pulses
      (let [[network' processed-pulses']
            (press-important-button network)]
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

(defn backtrack-connecting-modules [network module-id]
  (filter #(when (contains? (set (:outputs (second %))) module-id)
             %) network))

(defn find-dream-state [network target-module]
  (loop [network-state {}
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
       (mapcat (partial backtrack-connecting-modules network)
               (map first preceding-modules))))))

(press-important-button
 (let [network (input->network sample-input-2)
       dream-state (find-dream-state network "output")]
   (-> dream-state
       (assoc "output" {:module-type "output" :low 0 :high 0}))))

(def target-state (find-dream-state puzzle-network "rx"))

(defn press-button-until-rx-low [input]
  (loop [network (input->network input)
         c 0]
    (if (>= (get-in network ["rx" :low]) 1)
      c
      (let [[network' _processed-pulses']
            (press-important-button network)]
        (recur network'
               (inc c))))))

(press-button-until-rx-low puzzle-input)

;; too slow. The network can be traversed backwards by sending a single low pulse from rx backwards I think.
