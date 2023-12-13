(ns aoc2023.8.a
  (:require [clojure.string :as str]))

(defn parse-turns [turn]
  (map #(case %
          \L 0
          \R 1)
       turn))

(defn parse-map [mapping]
  (let [substr (fn [start end] (subs mapping start end))]
    {(substr 0 3)
     [(substr 7 10) (substr 12 15)]}))
   
(defn parse-maps [maps]
  (apply merge (map parse-map maps)))

(defn steps [turns mappings]
  (reduce (fn [[curr n] turn] (if (= curr "ZZZ")
                                (reduced n)
                                [((mappings curr) turn) (inc n)]))
          ["AAA" 0]
          (cycle turns)))

(defn answer [s]
  (let [lines    (str/split-lines s)
        turns    (parse-turns (first lines))
        mappings (parse-maps (drop 2 lines))]
    (steps turns mappings)))

(answer (slurp "src/aoc2023/8/a.txt"))
