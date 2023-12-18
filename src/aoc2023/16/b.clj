(ns aoc2023.16.b
  (:require [clojure.string :as str]))

(defn init-board [lines]
  (let [coord-pairs (fn [row line] 
                      (map-indexed #(hash-map [%1 row] %2) line))]
    (->> lines
         (map-indexed coord-pairs)
         (map #(apply merge %))
         (apply merge))))

(defn edge-beams [lines]
  (let [x-max        (count (first lines))
        y-max        (count lines)]
    (concat
      (map #(hash-map :pos [% 0] :dir [0 1]) (range x-max))
      (map #(hash-map :pos [% (dec y-max)] :dir [0 -1]) (range x-max))
      (map #(hash-map :pos [0 %] :dir [1 0]) (range y-max))
      (map #(hash-map :pos [(dec x-max) %] :dir [-1 0]) (range y-max)))))

(defn move [beam]
  (update beam :pos #(mapv + % (:dir beam))))

(defn spawn [beam board]
  (case (get board (:pos beam) :out-of-board)
    \. [beam]
    \| (if (zero? (get-in beam [:dir 0]))
         [beam]
         [(assoc beam :dir [0 1])
          (assoc beam :dir [0 -1])])
    \- (if (zero? (get-in beam [:dir 1]))
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

(defn energized [init-beam board]
  (->> board
     (simulate #{init-beam} [init-beam]) 
     (map :pos)
     (filter #(contains? board %))
     (distinct)
     (count)))
       
(defn answer [s]
  (let [lines     (str/split-lines s)
        board     (init-board lines)]
    (->> (edge-beams lines)
         (pmap #(energized % board))
         (apply max))))

(time (answer (slurp "src/aoc2023/16/a.txt")))
