(ns aoc2023.12.bmemoized
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[brd nums] (str/split line #" ")]
    [(str (str/join "?" (flatten (repeat 5 brd))) ".") 
     (flatten (repeat 5 (map parse-long (re-seq #"\d+" nums))))]))

(defn valid? [chrs n]
  (and (re-matches #"[\?\#]*[\.\?]" (apply str chrs))
       (= (count chrs) n)))

(def ways 
  (memoize
    (fn [line nums]
      (let [[x & more] nums
            len        (inc (or x 0))
            [head rst] (split-at len line)
            calc-rest  (fn [] (if (not= \# (first head))
                                (ways (rest line) nums)
                                0))]
        (cond 
          (empty? nums)       (if (not-any? #{\#} line) 1 0)
          (empty? line)       0
          (valid? head len)   (+ (ways rst more) (calc-rest)) 
          :else               (calc-rest))))))

(defn answer [s]
  (->> (str/split-lines s)
       (map parse-line)
       (map #(apply ways %))
       (apply +)))

(time (answer (slurp "src/aoc2023/12/a.txt")))
