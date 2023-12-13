(ns aoc2023.4.a
  (:require [clojure.string :as str]))

(defn get-nums [s]
  (map parse-long (re-seq #"\d+" s)))

(defn parse-game [game-str]
  (let [nums (second (str/split game-str #":"))]
    (update (mapv get-nums (str/split nums #"\|")) 0 set)))

(defn score [n]
  (if (pos? n)
      (reduce * (repeat (dec n) 2))
      0))

(defn score-game [winning? hand]
  (->> (filter winning? hand)
       (count)
       (score)))

(defn answer [s]
  (->> (str/split-lines s)
       (map parse-game)
       (map #(apply score-game %))
       (reduce +)))
  
(answer (slurp "src/aoc2023/4/a.txt"))
