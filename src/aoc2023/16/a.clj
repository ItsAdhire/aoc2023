(ns aoc2023.16.a
  (:require [clojure.string :as str]))

(defn init-board [lines]
  (let [coord-pairs (fn [row line] 
                      (map-indexed #(hash-map [%1 row] %2) line))]
    (->> lines
         (map-indexed coord-pairs)
         (map #(apply merge %))
         (apply merge))))

(defn move [beam]
  (update beam :pos #(mapv + % (:dir beam))))

(defn spawn [beam board]
  (case (get board (:pos beam) :out-of-board)
    \. [beam]
    \| (if (zero? (first (:dir beam)))
         [beam]
         [(assoc beam :dir [0 1])
          (assoc beam :dir [0 -1])])
    \- (if (zero? (second (:dir beam))) 
         [beam]
         [(assoc beam :dir [1 0])
          (assoc beam :dir [-1 0])])
    \\ [(update beam :dir (comp vec reverse))]
    \/ [(update beam :dir (comp vec reverse #(map - %)))]
    :out-of-board []))

(defn simulate [seen? beams board]
  (let [new-beams (->> beams
                       (mapcat #(spawn % board))
                       (map move)
                       (remove seen?))
        new-seen? (into seen? new-beams)]
    (if (empty? new-beams)
      seen?
      (recur new-seen? new-beams board))))
       
(defn answer [s]
  (let [lines     (str/split-lines s)
        board     (init-board lines)
        init-beam {:pos [0 0] :dir [1 0]}]
    (->> board
         (simulate #{init-beam} [init-beam])
         (map :pos)
         (filter #(contains? board %))
         (distinct)
         (count))))
     
(time (answer (slurp "src/aoc2023/16/a.txt")))
