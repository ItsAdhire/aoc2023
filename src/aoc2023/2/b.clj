(ns aoc2023.2.b
  (:require [clojure.string :as string]))

(defn split [s re]
  (string/split (string/trim s) re))

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

(defn minimum-cubes [sets]
  (apply merge-with max sets))

(defn score [sets] 
  (* (:r sets 0) (:g sets 0) (:b sets 0)))

(defn answer [s]
  (->> (string/split-lines s)
       (map (comp score minimum-cubes parse-game))
       (reduce +)))

(answer (slurp "src/aoc2023/2/a.txt"))
