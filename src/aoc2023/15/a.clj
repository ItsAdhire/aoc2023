(ns aoc2023.15.a
  (:require [clojure.string :as str]))

(defn my-hash [s]
  (let [hash-char #(mod (* 17 (+ %1 (int %2))) 256)]
    (reduce hash-char 0 s)))

(defn answer [s]
  (->> (str/split (str/trim-newline s) #",")
       (map my-hash)
       (apply +)))

(answer (slurp "src/aoc2023/15/a.txt"))
