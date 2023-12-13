(ns aoc2023.5.b
  (:require [clojure.string :as str]))

(defn parse-nums [s] 
  (map parse-long (re-seq #"\d+" s)))

;; returns a list of pairs for a seed range consisting
;; of start and end seeds
(defn parse-seeds [line]
  (->> line
       (parse-nums)
       (partition 2)
       (map (fn [[start len]] [start (+ start len -1)]))))

;; returns a list of conversions which is a list of 
;; triplet value representing 
;; the dest source radius of a mapping
(defn parse-maps [lines]
  (->> lines
       (partition-by #(= "" %))
       (rest)
       (take-nth 2)
       (map #(map parse-nums (rest %)))))

(defn convert [[start end] [dest source rad]]
  (let [lo source
        hi (+ source rad -1)
        df (- dest source)]
    (cond
      (or (< end lo) 
          (< hi start))    [(list [start end])
                            (list)]
      (<= lo start end hi) [(list)
                            (list [(+ start df) (+ end df)])]
      (<= start lo hi end) [(list [start (dec lo)] [(inc hi) end])
                            (list [(+ lo df) (+ hi df)])]
      (<= start lo end hi) [(list [start (dec lo)])
                            (list [(+ lo df) (+ end df)])]
      (<= lo start hi end) [(list [(inc hi) end])
                            (list [(+ start df) (+ hi df)])]
      :else                [(list)
                            (list)])))

(defn convert-seeds [seeds conversion]
  (->> seeds
    (map #(convert % conversion))
    (reduce #(map into %1 %2))))

(defn apply-map [seeds mappings]
  (loop [unmapped seeds
         mappings mappings
         mapped   []]
    (if (empty? mappings)
      (concat unmapped mapped)
      (let [[leftover result] 
            (convert-seeds unmapped (first mappings))]
        (recur leftover
               (rest mappings)
               (concat result mapped))))))

(defn answer [s]
  (let [lines     (str/split-lines s)
        seeds     (parse-seeds (first lines))
        maps      (parse-maps (rest lines))]
    (->> (reduce apply-map seeds maps)
         (map first)
         (reduce min))))
    
(answer (slurp "src/aoc2023/5/a.txt"))
