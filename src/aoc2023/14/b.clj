(ns aoc2023.14.b
  (:require [clojure.string :as str]))

(defn transpose [m] (apply mapv vector m))

(defn tilt-left [line]
  (let [[tilt-str rem-dots]
        (reduce (fn [[acc dots] chr]
                  (case chr
                    \O [(str acc chr) dots]
                    \. [acc (inc dots)]
                    \# [(apply str acc (concat (repeat dots \.) [chr])) 0]))
                ["" 0]
                line)]
    (apply str tilt-str (repeat rem-dots \.))))

(defn tilt-cycle [lines]
  (->> lines
       (transpose)
       (map tilt-left) ; north
       (transpose)
       (map tilt-left) ; west
       (transpose)
       (map rseq)
       (map tilt-left) ; south
       (map reverse)
       (transpose)     
       (map rseq)
       (map tilt-left) ; east
       (map reverse)))

;; finds the cycle start index 
;; and cycle length
(defn find-cycle [inf-seq]
  (reduce (fn [seen [i v]]
            (if-let [start (seen v)]
              (reduced [start (- i start)])
              (assoc seen v i)))
          {}
          (map vector (range) inf-seq)))

(defn extrapolate-cycle [target inf-seq]
  (let [[c-start c-len] (find-cycle inf-seq)
        c-offset        (mod (- target c-start) c-len)]
    (nth inf-seq (+ c-start c-offset))))

(defn score-lines [lines]
  (let [down-range (range (count lines) 0 -1)
        count-O    #(count (filter #{\O} %))]
    (map #(* %1 (count-O %2)) down-range lines))) 

(defn answer [s]
 (->> (str/split-lines s)
      (iterate tilt-cycle)
      (extrapolate-cycle 1000000000)
      (score-lines)
      (apply +)))

(answer (slurp "src/aoc2023/14/a.txt"))
