(ns day1)

(def puzzle-input (slurp "day1/input.txt"))

(reduce (fn [acc c]
          (condp = c
            \( (inc acc)
            \) (dec acc)
            acc)) 0 puzzle-input) ; 280


;; part II

(reduce (fn [{:keys [level counter] :as acc} c]
          (if (neg? level) (reduced (dec counter))
              (-> acc
                  (update :level
                          (condp = c
                            \( inc
                            \) dec
                            identity))
                  (update :counter inc)))) {:level 0 :counter 1} puzzle-input); 1797
