(ns aoc2023.3.a
  (:require [clojure.string :as string]))

(defn neighbors [[row col]]
  (for [x [(dec row) row (inc row)]
        y [(dec col) col (inc col)]]
    [x y]))

(def special? (complement (set ".1234567890")))

(defn special-locs [i row]
  (keep-indexed (fn [j chr]
                  (when (special? chr) [i j]))
                row))

(defn valid-locs [board]
  (->> board
       (map-indexed special-locs)
       (apply concat)
       (map neighbors)
       (apply concat)
       (apply hash-set)))

(defn re-pos [matcher]
  (when (re-find matcher)
    (vector (.group matcher)
            (.start matcher)
            (.end matcher))))

(defn re-seq-pos [re s]
  (let [matcher (re-matcher re s)]
    (take-while some? (repeatedly #(re-pos matcher)))))

(defn pos->locs [row [start end]]
  (map vector (repeat row) (range start end)))

(defn valid-nums [line row valid?]
  (->> line
       (re-seq-pos #"\d+")
       (filter #(some valid? 
                      (pos->locs row (rest %))))
       (map (comp parse-long first))))

(defn answer [board]
  (let [valid?     (valid-locs board)
        line->nums (fn [row line] (valid-nums line row valid?))]
    (->> board
         (map-indexed line->nums)
         (flatten)
         (reduce +))))

(answer (string/split-lines (slurp "src/aoc2023/3/a.txt")))
