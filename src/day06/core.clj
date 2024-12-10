(ns day06.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn find-guard-position [puzzle-map]
  (some (fn [[row-idx row]]
          (some (fn [[col-idx char]]
                  (when (= char \^)
                    [row-idx col-idx]))
                (map-indexed vector row)))
        (map-indexed vector puzzle-map)))

(defn parse-input []
  (let [input (slurp (io/resource "day06.txt"))
        lines (string/split-lines input)
        puzzle-map (mapv #(vec %) lines)
        guard-position (find-guard-position puzzle-map)]
    {:puzzle-map puzzle-map :guard-position guard-position}))

(defn next-location [[row column] direction]
  (case direction
    :up [(dec row) column]
    :down [(inc row) column]
    :left [row (dec column)]
    :right [row (inc column)]))

(defn turn-right [direction]
  (case direction
    :up :right
    :right :down
    :down :left
    :left :up))

(defn part1 [{:keys [puzzle-map guard-position]}]
  (loop [direction :up
         location guard-position
         visited #{guard-position}]
    (let [next (next-location location direction)]
      (case (get-in puzzle-map next)
        nil (count visited)
        \# (recur (turn-right direction) location visited)
        (recur direction next (conj visited next))))))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))))
