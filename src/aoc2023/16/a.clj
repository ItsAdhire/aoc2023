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

(defn answer [s]
  (let [lines (str/split-lines s)
        board (init-board lines)
        turn  (fn [beams] 
                (set (mapcat #(spawn % board) (map move beams))))]
    (apply distinct? (take 1200 (iterate turn [{:pos [-1 0] :dir [1 0]}])))))

(time (answer (slurp "src/aoc2023/16/a.txt")))


#_
(answer ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")
