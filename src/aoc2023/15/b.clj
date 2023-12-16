(ns aoc2023.15.b
  (:require [clojure.string :as str]))

(defn my-hash [s]
  (let [hash-char #(mod (* 17 (+ %1 (int %2))) 256)]
    (reduce hash-char 0 s)))

(defn hash-box [hmap action]
  (let [[prio s] action
        [l v]    (str/split s #"=") ; v is nil if not = found (del case)
        k        (if v l (apply str (drop-last s)))
        add-fn   (fn [box] (if (contains? box k)
                             (assoc-in box [k 1] v)
                             (assoc box k [prio v])))
        rem-fn   #(dissoc % k)]
    (update hmap (my-hash k) (if v add-fn rem-fn))))

(defn score-box [box]
  (->> box
       (sort-by #(first (val %)))  ; sort on priority
       (map (juxt #(inc (my-hash (key %))) ; get the box number and num value
                  #(parse-long (second (val %)))))
       (map-indexed #(apply * (inc %1) %2)))) ; multiply by 1-based index

(defn answer [s]
  (->> (str/split (str/trim-newline s) #",")
       (map-indexed vector)
       (reduce hash-box {})
       (vals)
       (mapcat score-box)
       (apply +)))

(answer (slurp "src/aoc2023/15/a.txt"))
