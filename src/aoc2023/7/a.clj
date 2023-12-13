(ns aoc2023.7.a
  (:require [clojure.string :as str]))

(defn parse-hand-bid [s]
  (update (str/split s #" ") 1 parse-long))

(defn hand-strength [h]
  (let [freqs (vec (sort (vals (frequencies h))))]
    (case freqs
      [5]       6
      [1 4]     5
      [2 3]     4
      [1 1 3]   3
      [1 2 2]   2
      [1 1 1 2] 1
      0)))

(defn card-power [c]
  (case c
    \A 100
    \K  99
    \Q  98
    \J  97
    \T  96
    (int c)))

(defn card-compare [h1 h2]
  (let [pwr1 (card-power (first h1))
        pwr2 (card-power (first h2))]
    (if (not= pwr1 pwr2)
      (< pwr1 pwr2)
      (recur (rest h1) (rest h2)))))

(defn smaller-hand? [h1 h2]
  (let [str1 (hand-strength h1)
        str2 (hand-strength h2)]
    (if (= str1 str2)
      (card-compare h1 h2)
      (< str1 str2))))

(defn answer [s]
  (->> (str/split-lines s)
       (map parse-hand-bid)
       (sort-by first smaller-hand?)
       (map second)
       (map-indexed (fn [ind bid] (* bid (inc ind))))
       (reduce +)))

(answer (slurp "src/aoc2023/7/a.txt"))
