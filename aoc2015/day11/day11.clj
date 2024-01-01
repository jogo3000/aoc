(ns day11
  (:require [clojure.string :as str]))

(def santa's-current-password "cqjxjnds")

(defn increment-char [c]
  (char
   (+ (int \a)
      (mod (inc (- (int c)

                   (int \a)))
           26))))

(defn increment-password [s]
  (str/join
   (loop [pw-chars (vec s)
          pos (dec (count pw-chars))]
     (let [inc-char (increment-char (get pw-chars pos))]
       (if (= inc-char \a)
         (recur (assoc pw-chars pos inc-char) (dec pos))
         (assoc pw-chars pos inc-char))))))

(defn includes-increasing-straight? [s]
  (re-find (re-pattern (->> (partition 3 1 "abcdefghijklmnopqrstuvwxyz")
                            (map str/join)
                            (map #(format "(%s)" %))
                            (str/join \|)))
           s))

(defn no-forbidden-letters? [s]
  (not (re-find #"[iol]" s)))

(defn two-different-non-overlapping-pairs? [s]
  (re-find #"([a-z])\1.*([a-z])\2" s))

(def password-policy (every-pred includes-increasing-straight?
                                 no-forbidden-letters?
                                 two-different-non-overlapping-pairs?))

(first (drop-while (complement password-policy)
                   (iterate increment-password santa's-current-password)));  "cqjxxyzz"


;; part deux

(first (drop-while (complement password-policy)
                   (drop 1 (iterate increment-password "cqjxxyzz")))); "cqkaabcc"
