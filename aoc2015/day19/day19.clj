(ns day19
  (:require [clojure.string :as str]))

(def sample-input "H => HO
H => OH
O => HH

HOH")

(defn parse-input [s]
  (let [[replacements-str input] (str/split s #"\n\n")]
    [(->> replacements-str str/split-lines (map #(str/split % #" => ")) (map #(map vec %)))
     (vec input)]))

(defn count-replacements [og]
  (let [[replacements input] (parse-input og)]
    (->>
     (for [r replacements
           i (range (count input))
           :let [rc (count (first r))
                 prefix (subvec input 0 i)
                 c (subvec input i (min (+ i rc) (count input)))
                 postfix (subvec input (min (+ i rc) (count input)))]]
       (->> replacements
            (map #(if (= (first %) c) (second %) nil))
            (remove nil?)
            (map #(concat prefix % postfix))))
     (reduce into)
     distinct
     count)))

(count-replacements sample-input)

;; Samples work

(count-replacements (slurp "day19/input")) ;; 509

;; Part deux

;; starting from `e`, what is the shortest path to the medicine

(parse-input (str/trim (slurp "day19/input")))

;; One could start replacing e one by one, but how about going backwards?

(defn parse-v2 [i]
  (-> (parse-input i)
      (update 0 (partial map reverse))))


(def sample-input2 "
e => H
e => O
H => HO
H => OH
O => HH

HOH")


(def sample-input3 "
e => H
e => O
H => HO
H => OH
O => HH

HOHOHO")


(defn possible-replacements [replacements input]
  (reduce into
          (remove #(or (nil? %) (empty? %))
                  (for [[source target] replacements
                        :let [source-length (count source)]]
                    (remove #(or (nil? %) (empty? %))
                            (for [i (range (inc (- (count input) source-length)))
                                  :let [comparison (subvec input i (+ i source-length))]]
                              (when (= comparison source)
                                (-> (subvec input 0 i)
                                    (into target)
                                    (into (subvec input (+ i source-length)))))))))))

(let [[replacements input] (parse-v2 (str/trim sample-input3))]
  (loop [iterations 0
         inputs [input]]
    (if-not (or (> iterations 10)
                (some #(= [\e] %) inputs))
      (recur
       (inc iterations)
       (->> inputs
            (map (fn [input]
                   (possible-replacements replacements input)))
            (apply concat)))
      iterations))) ; 6 <- that's right


(comment ;; Doesn't work, takes too long
  (let [[replacements input] (parse-v2 (str/trim (slurp "day19/input")))]
    (loop [iterations 0
           inputs [input]]
      (if-not (or (> iterations 10)
                  (some #(= [\e] %) inputs))
        (recur
         (inc iterations)
         (->> inputs
              (map (fn [input]
                     (possible-replacements replacements input)))
              (apply concat)))
        iterations))))
