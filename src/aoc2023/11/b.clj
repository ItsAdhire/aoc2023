(ns aoc2023.11.b
  (:require [clojure.string :as str]))

(defn transpose [m]
  (apply map str m))

(defn gal-nums [line]
  (count (re-seq #"\#" line)))

(defn expanded-rows [lines]
  (->> lines
      (keep-indexed #(when (zero? (gal-nums %2)) %1))
      (set)))

(defn expanded-indices [coll expand?]
  (->> coll
       (map-indexed (fn [ind _] (if (expand? ind) 1000000 1)))
       (reductions +))) ;prefix sum

(defn line->pos [line row skip?]
  (->> (expanded-indices line skip?)
       (map vector line)
       (filter #(= \# (first %)))
       (map second)
       (map vector (repeat row))
       (map rseq)))

(defn gal-pos [lines exp-row? exp-col?]
  (mapcat (fn [row line] (line->pos line row exp-col?))
          (expanded-indices lines exp-row?)
          lines))

(defn dists [gal gal-list]
  (->> gal-list
       (map #(map (comp abs -) gal %))
       (flatten)
       (apply +)))
  
(defn answer [s]
  (let [lines     (str/split-lines s)
        exp-row? (expanded-rows lines)
        exp-col? (expanded-rows (transpose lines))
        gal-list (gal-pos lines exp-row? exp-col?)]
    (/ (apply + (map #(dists % gal-list) gal-list)) 
       2)))

(answer (slurp "src/aoc2023/11/a.txt"))
