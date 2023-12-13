(ns aoc2023.3.b
  (:require [clojure.string :as string]))

(defn neighbors [[row col]]
  (for [x [(dec row) row (inc row)]
        y [(dec col) col (inc col)]]
    [x y]))

(defn gear-loc-row [i row]
  (keep-indexed (fn [j chr]
                  (when (= \* chr) [i j]))
                row))

(defn gear-locs [board]
  (->> board
       (map-indexed gear-loc-row)
       (apply concat)))

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

(defn adjacent? [loc1 loc2]
  (some #{loc2} (neighbors loc1)))

(defn adjacent-numbers [[row col] board]
  (->> [(dec row) row (inc row)]
       (map #(valid-nums (board %) % (partial adjacent? [row col])))
       (flatten)))

(defn gear-ratio [gear-loc board]
  (let [nums (adjacent-numbers gear-loc board)]
    (if (= (count nums) 2)
      (reduce * nums)
      0)))

(defn answer [board]
    (->> board
         (gear-locs)
         (map #(gear-ratio % board))
         (reduce +)))

(answer (string/split-lines (slurp "src/aoc2023/3/a.txt")))
