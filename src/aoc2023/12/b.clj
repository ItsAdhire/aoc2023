(ns aoc2023.12.b
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[brd nums] (str/split line #" ")]
    [(str (str/join "?" (flatten (repeat 5 brd))) ".") 
     (flatten (repeat 5 (map parse-long (re-seq #"\d+" nums))))]))

(defn valid? [chrs n]
  (and (re-matches #"[\?\#]*[\.\?]" (apply str chrs))
       (= (count chrs) n)))

(defn init-ways [line]
  ; pad initial subproblem so getting outside of range is in vector
  ; and returns 1 for the initial case 
  (let [str-line  (apply str line "..................")
        not-has#? (fn [ind] (not-any? #{\#} (subs str-line ind)))]
    (->> str-line
         (map-indexed (fn [i _] (if (not-has#? i) 1 0)))
         (vec)))) 

(defn ways* [line n prev]
  (let [span      (inc n)
        prev-ways #(get prev (+ % span) 0)
        curr-ways (fn [acc chrs]
                    (if (not= \# (first chrs)) acc 0))]
    (->> line
         (partition-all span 1)
         (map vector (range))
         (reverse)
         (reductions (fn [acc [i chrs]]
                       (+ (curr-ways acc chrs)
                          (if (valid? chrs span) (prev-ways i) 0)))
                     0)
         (rest)
         (reverse)
         (vec))))

(defn ways [line nums]
  (reduce #(ways* line %2 %1) 
          (init-ways line)
          (reverse nums)))

(defn answer [s]
  (->> (str/split-lines s)
       (map parse-line)
       (map #(apply ways %))
       (map first)
       (apply +)))

(time (answer (slurp "src/aoc2023/12/a.txt")))
