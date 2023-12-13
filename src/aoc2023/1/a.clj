(ns aoc2023.1.a
  (:require [clojure.string :as string]))

(defn digit? [c] (Character/isDigit c))

(defn parse-int [c] (- (int c) (int \0)))

(defn combine [a b]
  (Integer/parseInt (str a b)))

(defn calibration [s]
  (let [nums (map parse-int (filter digit? s))
        f    (first nums)
        l    (last nums)]
    (combine f l)))

(reduce + (map calibration 
               (string/split-lines (slurp "src/aoc2023/1/a.txt"))))
