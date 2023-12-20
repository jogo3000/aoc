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
      (assoc "output" {:module-type "output" :low 0 :high 0})))

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
