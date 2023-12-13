(ns aoc2023.6.b
  (:require [clojure.string :as str]))

(defn parse-single-num [s]
  (parse-long (apply str (re-seq #"\d" s))))

(defn ways [^long duration ^long dist]
  (->> (range duration)
       (map (fn [^long holds] (* holds (- duration holds))))
       (filter #(< dist ^long %))
       (count)))

(defn answer [s]
  (let [lines (str/split-lines s)
        dur  (parse-single-num (first lines))
        dist (parse-single-num (second lines))]
    (ways dur dist)))

(answer (slurp "src/aoc2023/6/a.txt"))
