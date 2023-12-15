(ns aoc2023.13.a
  (:require [clojure.string :as str]))

(defn transpose [m] (apply mapv vector m))

(defn h-reflection [lines]
  (let [rev        (reverse lines)
        len        (count lines)
        reflected? (fn [i] 
                     (every? true? (map = 
                                        (take i (drop i lines))
                                        (take-last i rev))))]
    (first (filter reflected? (range 1 len)))))

(defn refl-index [lines]
  (or (h-reflection (transpose lines))
      (* 100 (h-reflection lines))))

(defn answer [s]
  (->> (str/split s #"\n\n")
       (map str/split-lines)
       (map refl-index)
       (apply +)))

(answer (slurp "src/aoc2023/13/a.txt"))
