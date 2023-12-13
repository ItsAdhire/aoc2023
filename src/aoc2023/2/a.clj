(ns aoc2023.2.a
  (:require [clojure.string :as string]))

(defn split [s re]
  (string/split (string/trim s) re))

(defn valid-set? [rolls]
  (and (<= (:r rolls 0) 12)
       (<= (:g rolls 0) 13)
       (<= (:b rolls 0) 14)))

(defn parse-roll [roll]
  {(cond
     (string/includes? roll "red") :r
     (string/includes? roll "green") :g
     (string/includes? roll "blue") :b)
   (parse-long (first (split roll #" ")))})

(defn parse-set [game-set]
  (->> (string/split game-set #",")
       (map parse-roll)
       (reduce merge)))

(defn parse-game [game]
  (let [set-strs (-> game
                     (split #":")
                     (second)
                     (split #";"))]
    (map parse-set set-strs)))

(defn score [game-num sets]
  (if (every? valid-set? sets) (inc game-num) 0))

(defn answer [s]
  (->> (string/split-lines s)
       (map parse-game)
       (map-indexed score)
       (reduce +)))

(answer (slurp "src/aoc2023/2/a.txt"))
