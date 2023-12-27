(ns day4
  (:import [java.security MessageDigest]))

(def md5 (MessageDigest/getInstance "MD5"))

(defn test-candidate [candidate]
  (let [input (.getBytes candidate)
        [a b c] (take 3 (.digest (doto md5
                                   (.reset)
                                   (.update input))))]
    (and (zero? a)
         (zero? b)
         (= c (bit-and 0x0F c)))))


(test-candidate "abcdef609043") ; true good

(test-candidate "pqrstuv1048970") ; true, good

(def puzzle-input "iwrupvqb")

(loop [c 0]
  (if (test-candidate (str puzzle-input c)) c
      (recur (inc c)))) ; 346386 whee!

;; Part deux

(defn test-candidate2 [candidate]
  (let [input (.getBytes candidate)
        [a b c] (take 3 (.digest (doto md5
                                   (.reset)
                                   (.update input))))]
    (and (zero? a)
         (zero? b)
         (zero? c))))

(loop [c 0]
  (if (test-candidate2 (str puzzle-input c)) c
      (recur (inc c)))) ; 9958218
