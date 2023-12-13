(ns aoc2023.6.bfast
  (:require [clojure.string :as str]))

(defn parse-single-num [s]
  (parse-long (apply str (re-seq #"\d" s))))

(defn bs-min-key [start end valid? best]
  (let [mid (long (/ (+ start end) 2))]
    (cond
      (> start end) best
      (valid? mid)  (recur start (dec mid) valid? (min mid best))
      :else         (recur (inc mid) end valid? best))))

(defn answer [s]
  (let [lines (str/split-lines s)
        dur  (parse-single-num (first lines))
        dist (parse-single-num (second lines))
        win? (fn [hold] (< dist (* hold (- dur hold))))
        n    (bs-min-key 0 (/ dur 2) win? ##Inf)]
    (- (inc dur) (* 2 n))))

(answer (slurp "src/aoc2023/6/a.txt"))
