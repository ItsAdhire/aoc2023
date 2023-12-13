(ns aoc2023.10.a
  (:require [clojure.string :as str]))

(def translate {\| (fn [[x y]] [[x (dec y)]
                                [x (inc y)]])
                \- (fn [[x y]] [[(dec x) y] 
                                [(inc x) y]])
                \L (fn [[x y]] [[x (dec y)] 
                                [(inc x) y]])
                \J (fn [[x y]] [[x (dec y)] 
                                [(dec x) y]])
                \7 (fn [[x y]] [[x (inc y)] 
                                [(dec x) y]])
                \F (fn [[x y]] [[x (inc y)] 
                                [(inc x) y]])
                \S (fn [[x y]] [[x (dec y)]
                                [x (inc y)] 
                                [(dec x) y]
                                [(inc x) y]])
                \. (constantly [])})

(defn init-board [board row line]
  (->> line
       (map-indexed (fn [col chr] 
                      (hash-map [col row] chr)))
       (reduce merge board)))

(defn moves [pos board]
  (let [neighbors (fn [p] ((translate (board p \.)) p))
        mutual?   (fn [p] (some #{pos} (neighbors p)))]
    (filter mutual? (neighbors pos))))

(defn bfs [start board]
  (loop [locs     (list start)
         visited? #{start}
         n        0]
    (let [new-locs (->> locs
                        (mapcat #(moves % board))
                        (remove visited?))]
      (if-not (empty? new-locs)
        (recur new-locs 
               (into visited? (set new-locs)) 
               (inc n))
        n))))
      
(defn answer [s]
  (let [lines (vec (str/split-lines s))
        board (reduce-kv init-board {} lines)
        s-loc (some #(when (= \S (val %)) (key %))
                    board)]
    (bfs s-loc board)))

(answer (slurp "src/aoc2023/10/a.txt"))
