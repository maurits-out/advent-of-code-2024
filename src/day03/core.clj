(ns day03.core
  (:require [clojure.java.io :as io]))

(defn part1 [memory]
  (->> (re-seq #"mul\((\d+),(\d+)\)" memory)
       (map (fn [[_ x y]] (* (Integer/parseInt x) (Integer/parseInt y))))
       (reduce +)))

(defn part2 [memory]
  (loop [instructions (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" memory)
         ignore false
         acc 0]
    (if-let [[op x y] (first instructions)]
      (cond
        (= op "don't()") (recur (rest instructions) true acc)
        (= op "do()") (recur (rest instructions) false acc)
        ignore (recur (rest instructions) ignore acc)
        :else (recur (rest instructions) ignore (+ acc (* (Integer/parseInt x) (Integer/parseInt y)))))
      acc)))

(defn -main []
  (let [memory (slurp (io/resource "day03.txt"))]
    (println "Part 1:" (part1 memory))
    (println "Part 2:" (part2 memory))))
