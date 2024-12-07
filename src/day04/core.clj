(ns day04.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-input []
  (->> (slurp (io/resource "day04.txt"))
       (string/split-lines)
       (mapv vec)))

(defn has-word [input word row column [dr dc]]
  (loop [current-row row
         current-column column
         [letter & remaining] word]
    (let [current-letter (get-in input [current-row current-column])]
      (cond
        (and (= current-letter letter) (empty? remaining)) true
        (= current-letter letter) (recur (+ current-row dr) (+ current-column dc) remaining)
        :else false))))

(defn has-x-mas [input row column]
  (if (= (get-in input [row column]) \A)
    (let [has-mas-word (partial has-word input "MAS")]
      (or
        (and (has-mas-word (dec row) (dec column) [1 1]) (has-mas-word (dec row) (inc column) [1 -1]))
        (and (has-mas-word (dec row) (dec column) [1 1]) (has-mas-word (inc row) (dec column) [-1 1]))
        (and (has-mas-word (inc row) (dec column) [-1 1]) (has-mas-word (inc row) (inc column) [-1 -1]))
        (and (has-mas-word (dec row) (inc column) [1 -1]) (has-mas-word (inc row) (inc column) [-1 -1]))))))

(defn part1 [input]
  (let [directions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
        size (count input)]
    (count (filter identity
                   (for [r (range size)
                         c (range size)
                         dir directions]
                     (has-word input "XMAS" r c dir))))))

(defn part2 [input]
  (let [size (count input)]
    (count (filter identity
                   (for [r (range size)
                         c (range size)]
                     (has-x-mas input r c))))))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
