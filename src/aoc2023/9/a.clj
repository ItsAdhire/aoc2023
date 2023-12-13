(ns aoc2023.9.a
  (:require [clojure.string :as str]))

(defn parse-nums [s]
  (map parse-long (re-seq #"-?\d+" s)))

(defn differ [lon]
  (map #(- (apply - %)) 
       (partition 2 1 lon)))

(defn history [lon]
  (->> (iterate differ lon)
       (take-while #(not-every? zero? %))
       (map last)
       (apply +)))

(defn answer [s]
  (let [lines (str/split-lines s)
        nums  (map parse-nums lines)]
    (apply + (map history nums))))

(answer (slurp "src/aoc2023/9/a.txt"))
