(ns aoc2023.5.a
  (:require [clojure.string :as str]))

(defn parse-nums [s] 
  (map parse-long (re-seq #"\d+" s)))

;; returns a list of conversions which is a list of 
;; triplet value representing 
;; the dest source radius of a mapping
(defn parse-maps [lines]
  (->> lines
       (partition-by #(= "" %))
       (rest)
       (take-nth 2)
       (map #(map parse-nums (rest %)))))

(defn apply-conversion [n mappings]
  (let [in-range? (fn [[_dest source rad]] (<= source n (+ source rad)))
        transform (fn [[dest source _rad]] (+ n (- dest source)))]
    (-> (filter in-range? mappings)
        (first)
        (or [n n 0])
        (transform))))

(defn answer [s]
  (let [lines     (str/split-lines s)
        seeds     (parse-nums (first lines))
        maps      (parse-maps (rest lines))
        seed->loc (fn [seed] (reduce apply-conversion seed maps))]
    (reduce min (map seed->loc seeds))))
  
(answer (slurp "src/aoc2023/5/a.txt"))
