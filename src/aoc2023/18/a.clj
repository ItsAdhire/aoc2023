(ns aoc2023.18.a
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (-> (str/split line #" ")
      (subvec 0 2)
      (update 1 parse-long)))

(defn dir->delta [dir]
  (case dir
    "R" [0 1]
    "D" [1 0]
    "L" [0 -1]
    "U" [-1 0]))

(defn dir->locs [start [dir len]]
  (rest (reductions
          #(map + %1 %2)
          start
          (repeat len (dir->delta dir)))))

(defn perimeter [lines]
  (loop [dug #{} curr [0 0] instrs lines]
    (if-not (empty? instrs)
      (let [new-digs (dir->locs curr (first instrs))]
        (recur (into dug new-digs)
               (last new-digs)
               (rest instrs)))
      dug)))

(defn boxed-locs [perim]
  (let [row-min (apply min (map first perim))
        row-max (apply max (map first perim))
        col-min (apply min (map second perim))
        col-max (apply max (map second perim))]
    (for [r (range row-min (inc row-max))
          c (range col-min (inc col-max))]
      [r c])))

(defn neighbors [loc]
  (mapcat #(dir->locs loc [% 1]) ["R" "D" "L" "U"]))

(defn dfs [start dug? level? out-of-board?]
  (loop [seen? #{}
         stack [start]]
    (let [head    (peek stack)
          open?   (some-fn level? out-of-board?)
          ignore? (some-fn seen? dug?)]
      (cond
        (empty? stack) [(reduce into [dug? seen? stack]) level?]
        (open? head)   [dug? (reduce into [level? seen? stack])]
        (ignore? head) (recur seen? (pop stack))
        :else          (recur (conj seen? head) 
                            (apply conj (pop stack) 
                                    (neighbors head)))))))

(defn answer [s]
  (let [perim (->> (str/split-lines s)
                   (map parse-line)
                   (perimeter))
        locs       (boxed-locs perim)
        out-board? (complement (set locs))]
    (->> (reduce (fn [[dug? level?] loc]
                   (dfs loc dug? level? out-board?))
                 [perim #{}] locs)
         (first)
         (count))))

(answer (slurp "src/aoc2023/18/a.txt"))
