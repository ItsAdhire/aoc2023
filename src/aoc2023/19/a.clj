(ns aoc2023.19.a
  (:require [clojure.string :as str]))

(defn chr-map [chr]
  (case chr
    \< < \> >
    \x 0 \m 1 \a 2 \s 3))

(defn parse-rule [rule]
  (if (some #{\:} rule)
    (let [index (chr-map (first rule))
          op    (chr-map (second rule))
          arg   (parse-long (re-find #"\d+" rule))
          dest  (second (str/split rule #":"))]
      (fn [rating]
        (when (op (rating index) arg)
          dest)))
    (constantly rule)))

(defn parse-flow [workflow]
  (-> (str/split workflow #"\{")
      (update 1 #(apply str (drop-last %)))
      (update 1 #(map parse-rule (str/split % #",")))))

(defn parse-flows [s]
  (->> (str/split-lines s)
       (map parse-flow)
       (apply conj {})))

(defn parse-ratings [s]
  (->> (str/split-lines s)
       (map #(re-seq #"\d+" %))
       (map #(mapv parse-long %))))

(defn score [rating flows]
  (loop [fns (get flows "in")]
    (let [output ((first fns) rating)]
      (case output
        "A" (apply + rating)
        "R" 0
        nil (recur (rest fns))
        (recur (get flows output))))))

(defn answer [s]
  (let [inputs  (str/split s #"\n\n")
        flows   (parse-flows (first inputs))
        ratings (parse-ratings (second inputs))]
   (apply + (map #(score % flows) ratings))))

(answer (slurp "src/aoc2023/19/a.txt"))

#_
(answer "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013")
