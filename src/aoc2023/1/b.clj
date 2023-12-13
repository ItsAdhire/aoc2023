(ns aoc2023.1.b
  (:require [clojure.string :as string]))

(defn combine [a b]
  (Integer/parseInt (str a b)))

(def numbers ["one" "two" "three" "four" 
              "five" "six" "seven" "eight" "nine"
              "1" "2" "3" "4" "5" "6" "7" "8" "9"])

(def str->number
  (zipmap numbers (cycle (range 1 10))))

(defn first-number [s]
  (apply min-key #(or (string/index-of s %) ##Inf) numbers))

(defn last-number [s]
  (apply max-key #(or (string/last-index-of s %) ##-Inf) numbers))

(defn calibration [s]
  (let [f (str->number (first-number s))
        l (str->number (last-number s))]
    (combine f l)))

(defn answer [s]
  (reduce + (map calibration 
                 (string/split-lines s))))

(answer (slurp "src/aoc2023/1/a.txt"))
