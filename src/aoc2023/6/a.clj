(ns aoc2023.6.a
  (:require [clojure.string :as str]))

(defn parse-nums [s]
  (map parse-long (re-seq #"\d+" s)))

(defn ways [duration dist]
  (->> (range duration)
       (map (fn [holds] (* holds (- duration holds))))
       (filter #(< dist %))
       (count)))

(defn answer [s]
  (let [lines (str/split-lines s)
        times (parse-nums (first lines))
        dists (parse-nums (second lines))]
    (reduce * (map ways times dists))))

(answer (slurp "src/aoc2023/6/a.txt"))
