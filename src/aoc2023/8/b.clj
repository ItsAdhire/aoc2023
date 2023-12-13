(ns aoc2023.8.b
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

(defn gcd [x y]
  (if (zero? y)
    x
    (recur y (mod x y))))

(defn lcm [x y]
  (/ (* x y)
     (gcd x y)))

(defn steps [start turns mappings]
  (reduce (fn [[curr n] turn] (if (str/ends-with? curr "Z")
                               (reduced [curr n])
                               [((mappings curr) turn) (inc n)]))
          [start 0]
          (cycle turns)))

(defn answer [s]
 (let [lines    (str/split-lines s)
       turns    (vec (parse-turns (first lines)))
       mappings (parse-maps (drop 2 lines))
       starts   (filter #(str/ends-with? % "A") 
                        (keys mappings))]
   (->> starts
        (map #(steps % turns mappings))
        (map second)
        (reduce lcm))))

(answer (slurp "src/aoc2023/8/a.txt"))
