(ns day5
  (:require [clojure.string :as str]))

(def vowels "aeiou")

(def three-vowels-pattern (re-pattern (str/join ".*" (repeat 3 (str "[" vowels "]")))))

(defn three-vowels? [s]
  (re-find three-vowels-pattern s))

(defn one-letter-twice? [s]
  (re-find #"(.)\1" s))

(defn no-naughty-words? [s]
  (not (re-find #"(ab)|(cd)|(pq)|(xy)" s)))

(no-naughty-words? "ugknbfddgicrmopn")

(->> (slurp "day5/input.txt")
     str/trim
     str/split-lines
     (filter (every-pred three-vowels?
                         one-letter-twice?
                         no-naughty-words?))
     count) ; 236, nice


;; Part II

(defn repeated-pair? [s]
  (re-find #"(.{2}).*\1" s))

(defn separated-letter? [s]
  (re-find #"(.{1}).{1}\1" s))

(->> (slurp "day5/input.txt")
     str/trim
     str/split-lines
     (filter (every-pred repeated-pair?
                         separated-letter?))
     count) ; 51 Nice
