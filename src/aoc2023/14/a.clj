(ns aoc2023.14.a
  (:require [clojure.string :as str]))

(defn transpose [m] (apply mapv vector m))

(defn score [line]
  (let [len  (count line)]
    (first (reduce-kv
              (fn [[t-load o-loc] ind chr]
                (case chr
                  \o [(+ t-load (- len o-loc)) 
                      (inc o-loc)]
                  \. [t-load o-loc]
                  \# [t-load (inc ind)])) 
              [0 0]
              (vec line)))))

(defn answer [s]
  (->> (str/split-lines s)
       (transpose)
       (map score)
       (apply +)))

(time (answer (slurp "src/aoc2023/14/a.txt")))
