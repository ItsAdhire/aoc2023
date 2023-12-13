(ns aoc2023.4.b
  (:require [clojure.string :as str]))

(defn get-nums [s]
  (map parse-long (re-seq #"\d+" s)))

(defn parse-game [game-str]
  (let [nums (second (str/split game-str #":"))]
    (update (mapv get-nums (str/split nums #"\|")) 0 set)))

(defn won-count [winning? hand]
  (count (filter winning? hand)))

(defn affected-cards [card-num len]
  (map + (range len) (repeat (inc card-num))))

(defn update-sheet [sheet card-num len]
  (let [curr (sheet card-num)]
    (reduce (fn [sht card] (merge-with + sht {card curr})) 
            sheet 
            (affected-cards card-num len))))

(defn init-sheet [len]
  (zipmap (range len) 
          (repeat 1)))

(defn answer [s]
  (let [lines (str/split-lines s)
        sheet (init-sheet (count lines))]
    (->> lines
         (map parse-game)
         (mapv #(apply won-count %))
         (reduce-kv update-sheet sheet)
         (vals)
         (reduce +))))
  
(answer (slurp "src/aoc2023/4/a.txt"))
