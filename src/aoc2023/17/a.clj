(ns aoc2023.17.a
  (:require [clojure.string :as str])
  (:import [java.util PriorityQueue]))

;; mutating priority queues
(defn pq-pop! [^PriorityQueue pq] (.poll pq))
(defn pq-add! [^PriorityQueue pq elem] (.add pq elem))
(defn pq [coll]
  (let [pq (new PriorityQueue)]
   (doseq [elem coll]
     (pq-add! pq elem))
   pq))

(defn get-in-board [board r c] (get (get board r) c))

(defn neighbors [row col dir]
  (apply map vector 
         (for [i (range 1 4)]
          (if (= :vert dir)
            [[(+ row i) col :horiz]
             [(- row i) col :horiz]]
            [[row (+ col i) :vert]
             [row (- col i) :vert]]))))

(defn relaxed-edges [esp row col dir board]
  (->> (neighbors row col dir)
       (map #(map (fn [[r c d]]
                    (apply vector (get-in-board board r c) [r c d]))
                  %))
       (map #(filter first %))
       (mapcat #(reductions (fn ([] nil)
                                ([[v _ _ _] vert2]
                                 (update vert2 0 + v)))
                           %))
       (keep identity)
       (map #(update % 0 + esp))))

(defn dijkstra [target pq board]
  (loop [seen #{}]
    (let [[esp r c d] (pq-pop! pq)
          position    [r c]
          vertex      [r c d]]
      (if (not= position target)
        (do (when-not (seen vertex)
              (doseq [new-edge (relaxed-edges esp r c d board)]
                 (pq-add! pq new-edge)))
            (recur (conj seen vertex)))
        esp))))

(defn answer [s]
  (let [board (->> (str/split-lines s)
                   (map #(re-seq #"\d" %))
                   (mapv #(mapv parse-long %)))
        targ  [(dec (count board)) 
               (dec (count (first board)))]]
    (dijkstra targ
              (pq [[0 0 0 :vert]
                   [0 0 0 :horiz]])
              board)))
  
(answer (slurp "src/aoc2023/17/a.txt"))
