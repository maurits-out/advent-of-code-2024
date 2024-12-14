(ns day01.historian-hysteria
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-line [line]
  (map #(Integer/parseInt %) (string/split line #"\s+")))

(defn parse-input []
  (->> (io/resource "day01.txt")
       slurp
       string/split-lines
       (map parse-line)
       (apply map vector)))

(defn part1 [left right]
  (let [differences (map #(abs (- %1 %2)) (sort left) (sort right))]
    (apply + differences)))

(defn part2 [left right]
  (let [counts (frequencies right)
        similarities (map #(* % (counts % 0)) left)]
    (apply + similarities)))

(defn -main []
  (let [[left right] (parse-input)]
    (println "Part 1:" (part1 left right))
    (println "Part 2:" (part2 left right))))
